/**
 * Generate time series data for demonstration of TS attributes.  Generates four
 * series of values.  A lag period is inferred by the number of start values. The
 * series generated are: flat (a random walk); a slope constructed by adding the
 * additive increment product of the prior value and the multiplicative increment;
 * and the cummulative values of the slope value.
 * @param test records
 * @param start values, seasonality or cyclic behavior is entered with this parameter
 * @param noise mean, the mean value of normally distributed noise which will be added
 * @param noise variance, the variance of the normally distributed noise
 * @param multiply value, the multiplicative increment
 * @param additive value, the additive increment
 * @return the generated time series.
 */
IMPORT Std.system.thorlib;
IMPORT ML;
IMPORT TS;
IMPORT TS.Types;
Co_eff := Types.Co_efficient;

EXPORT Series_Data(DATASET(Co_eff) initial_vals,
                   REAL8 mult_val=1.0,
                   REAL8 add_val=0.0,
                   UNSIGNED2 test_records=100,
                   REAL8 noise_mean=0.0,
                   REAL8 noise_var=1.0) := FUNCTION
  Work0 := RECORD
    UNSIGNED2 rec_id := 0;
    REAL8 val_flat := 0.0;
    REAL8 val_slope := 0.0;
    REAL8 val_accum := 0.0;
  END;
  State_Rec := RECORD
    UNSIGNED2 lags;
    DATASET(Co_eff) start_vals;
    DATASET(Co_eff) prev_vals;
    DATASET(Co_eff) accum_vals;
  END;
  Work1 := RECORD(Work0)
    UNSIGNED2 lags;
    DATASET(Co_eff) start_vals;
    DATASET(Co_eff) prev_vals;
    DATASET(Co_eff) accum_vals;
  END;
  noise_dist := ML.Distribution.Normal(noise_mean, noise_var);
  noise_obs  := ML.Distribution.GenData(test_records, noise_dist, 1);
  Work0 cvtNoise(ML.Types.NumericField noise) := TRANSFORM
    SELF.rec_id := noise.id;
    SELF.val_flat := noise.value;
    SELF := [];
  END;
  base_set := SORT(PROJECT(noise_obs, cvtNoise(LEFT)), rec_id);
  default_start := DATASET([{0.0,1}], Co_eff);
  start_vals := GLOBAL(IF(EXISTS(initial_vals), initial_vals, default_start), FEW);
  Co_eff addZeros(Co_eff b, UNSIGNED c) := TRANSFORM
    SELF.lag  := c;
    SELF.cv := 0.0;
  END;
  Co_eff keepNZ(Co_eff prev, Co_eff curr) := TRANSFORM
    SELF.lag := curr.lag;
    SELF.cv := prev.cv + curr.cv;
  END;
  // NOTE: Use PROCESS(...) after HPCC-11104 is fixed
  // State_Rec makeState(DATASET(Co_eff) starts) := TRANSFORM
    // max_lag := MAX(starts, lag);
    // zeros := NORMALIZE(CHOOSEN(starts,1), max_lag, addZeros(LEFT, COUNTER));
    // all_starts := ROLLUP(SORT(starts+zeros, lag), keepNZ(LEFT,RIGHT), lag);
    // SELF.lags := max_lag;
    // SELF.start_vals := all_starts;
    // SELF.prev_vals := all_starts;
    // SELF.accum_vals := all_starts;
  // END;
  // s_init := ROW(makeState(start_vals));
  // NOTE: Remove ITERATE(...) after HPCC-11104 is fixed
  Work1 makeIter(Work0 base, DATASET(Co_eff) starts) := TRANSFORM
    max_lag := MAX(starts, lag);
    zeros := NORMALIZE(CHOOSEN(starts,1), max_lag, addZeros(LEFT, COUNTER));
    all_starts := ROLLUP(SORT(starts+zeros, lag), keepNZ(LEFT,RIGHT), lag);
    SELF.lags := max_lag;
    SELF.start_vals := all_starts;
    SELF.prev_vals := all_starts;
    SELF.accum_vals := all_starts;
    SELF := base;
  END;
  iter_base := PROJECT(base_set, makeIter(LEFT, start_vals));
  // perform increment
  Co_eff i_vals(Co_eff lr) := TRANSFORM
    SELF.cv := lr.cv*mult_val + add_val;
    SELF.lag := lr.lag;
  END;
  // perform accumulation
  Co_eff c_vals(Co_eff prev_accum, Co_eff this_val) := TRANSFORM
    SELF.cv := prev_accum.cv + this_val.cv;
    SELF.lag := prev_accum.lag;
  END;
  // NOTE: Use PROCESS(...) after HPCC-11104 is fixed
  // calc next value from state
  // Work0 nextVal(Work0 w, State_Rec s) := TRANSFORM
    // period := ((w.rec_id-1) % s.lags) + 1;
    // SELF.rec_id := w.rec_id;
    // SELF.val_flat := w.val_flat + s.start_vals(lag=period)[1].cv;
    // SELF.val_slope := w.val_flat + s.prev_vals(lag=period)[1].cv;
    // SELF.val_accum := w.val_flat + s.accum_vals(lag=period)[1].cv;
  // END;
  // Update state
  // State_Rec nextState(Work0 w, State_Rec s) := TRANSFORM
    // period := ((w.rec_id-1) % s.lags) + 1;
    // this_prev := s.prev_vals(lag=period);
    // this_accum := s.accum_vals(lag=period);
    // new_prev := PROJECT(this_prev, i_vals(LEFT));
    // new_accum:= JOIN(this_accum, new_prev, LEFT.lag=RIGHT.lag, c_vals(LEFT,RIGHT));
    // SELF.accum_vals := s.accum_vals(lag<>period) + new_accum;
    // SELF.prev_vals := s.prev_vals(period<>lag) + new_prev;
    // SELF := s;
  // END;
  // iterate through base
  //series := PROCESS(base_set, s_init, nextVal(LEFT,RIGHT),nextState(LEFT,RIGHT));
  // NOTE: Remove ITERATE(...) after HPCC-11104 is fixed
  Work1 doIter(Work1 prev, Work1 curr) := TRANSFORM
    period := ((curr.rec_id-1) % curr.lags) + 1;
    this_prev := IF(prev.rec_id<>0, prev.prev_vals, curr.prev_vals)(lag=period);
    this_accum := IF(prev.rec_id<>0, prev.accum_vals, curr.accum_vals)(lag=period);
    new_prev := PROJECT(this_prev, i_vals(LEFT));
    new_accum:= JOIN(this_accum, new_prev, LEFT.lag=RIGHT.lag, c_vals(LEFT,RIGHT));
    SELF.accum_vals := IF(prev.rec_id<>0, prev.accum_vals, curr.accum_vals)(lag<>period) + new_accum;
    SELF.prev_vals := IF(prev.rec_id<>0, prev.prev_vals, curr.prev_vals)(period<>lag) + new_prev;
    SELF.val_flat := curr.val_flat + curr.start_vals(lag=period)[1].cv;
    SELF.val_slope := curr.val_flat + IF(prev.rec_id<>0, prev.prev_vals, curr.prev_vals)(lag=period)[1].cv;
    SELF.val_accum := curr.val_flat + IF(prev.rec_id<>0, prev.accum_vals, curr.accum_vals)(lag=period)[1].cv;
    SELF := curr;
  END;
  series := ITERATE(iter_base, doIter(LEFT,RIGHT));
  RETURN PROJECT(series, Work0);
END;
