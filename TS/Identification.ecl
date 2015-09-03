// Model Identification.  Produces autocorrelation function and
//the partial autocorrelation function datasets.
IMPORT PBblas;
IMPORT TS.Types;
IMPORT TS;
ModelObs := Types.ModelObs;

EXPORT Identification(DATASET(Types.UniObservation) series,
                      DATASET(Types.Ident_Spec) spec) := MODULE
  SHARED model_obs := TS.SeriesByModel(series, spec);
  SHARED post_difference := TS.DifferenceSeries(model_obs, spec);
  SHARED LagRec := RECORD
    Types.t_model_id model_id;
    Types.t_time_ord period;
    Types.t_time_ord lag_per;
    Types.t_value v;
    UNSIGNED2 k;
    UNSIGNED4 N;
  END;
  SHARED ACF_Rec := RECORD
    Types.t_model_id model_id;
    UNSIGNED2 k;
    REAL8 av;           // Auto cov, k
    REAL8 ac;           // Auto corr, k
    REAL8 sq;
    REAL8 sum_sq;       // sum of r-squared, k-1 of them
    UNSIGNED4 N;
  END;
  SHARED Cell := RECORD(PBblas.Types.Layout_Cell)
    Types.t_model_id model_id;
  END;
  // Formulae from Bowerman & O'Connell, Forecasting and Time Series, 1979
  EXPORT DATASET(Types.PACF_ACF) Correlations(UNSIGNED2 lags) := FUNCTION
    s_stat := TABLE(post_difference,
                    {model_id, z_bar:=AVE(GROUP,dependent), N:=COUNT(GROUP),
                     f_per:=MIN(GROUP,period), l_per:=MAX(GROUP,period)},
                    model_id, FEW, UNSORTED);
    ObsRec := RECORD(ModelObs)
      REAL8 z_bar;
      UNSIGNED4 N;
      UNSIGNED2 f_per;
      UNSIGNED2 l_per;
    END;
    ObsRec addStats(ModelObs obs, RECORDOF(s_stat) st) := TRANSFORM
      SELF.z_bar := st.z_bar;
      SELF.N := st.N;
      SELF.f_per := st.f_per;
      SELF.l_per := st.l_per;
      SELF := obs;
    END;
    withStats := JOIN(post_difference, s_stat, LEFT.model_id=RIGHT.model_id,
                      addStats(LEFT, RIGHT), LOOKUP);
    LagRec explode(ObsRec rec, UNSIGNED c) := TRANSFORM
      k := (c-1) DIV 2;
      adj := ((c-1) % 2)*k; //no adjustment for this column then adj for lag
      normPeriod := rec.period-rec.f_per+1;
      useRecord := normPeriod > adj AND rec.period + k - adj <= rec.l_per;
      SELF.period := IF(useRecord, rec.period, SKIP);
      SELF.lag_per := rec.period - adj;
      SELF.v := rec.dependent - rec.z_bar;
      SELF.k := k;
      SELF.model_id := rec.model_id;
      SELF.N := rec.N;
    END;
    // replicate for t and t+k multiply, use 0 for var (t and t)
    // t_k = SUM((z_i-zbar)*(z_i+k - zbar);i=1,N-k)
    exploded_t := NORMALIZE(withStats, 2*(lags+1), explode(LEFT, COUNTER));
    g_exploded := GROUP(exploded_t, model_id, ALL);
    LagRec mult(LagRec prev, LagRec curr) := TRANSFORM
      SELF.v := prev.v * curr.v;
      SELF := curr;
    END;
    s_exploded := SORT(g_exploded, k, lag_per, period);
    prod_terms := ROLLUP(s_exploded, mult(LEFT,RIGHT), model_id, k, lag_per);
    LagRec sumt(Lagrec prev, LagRec curr) := TRANSFORM
      SELF.v := prev.v + curr.v;
      SELF := curr;
    END;
    sum_terms := UNGROUP(ROLLUP(prod_terms, sumt(LEFT,RIGHT), model_id, k));
    r_k_denom := sum_terms(k=0);
    r_k_numer := sum_terms(k>0);
    ACF_Rec makeACF(LagRec rec, LagRec denom) := TRANSFORM
      SELF.k := rec.k;
      SELF.av := rec.v;
      SELF.ac := rec.v / denom.v;   // the r_k value
      SELF.sq := (rec.v*rec.v) / (denom.v*denom.v);
      SELF.sum_sq := 0.0;
      SELF.model_id := rec.model_id;
      SELF.N := rec.N;
    END;
    pre_sumsq := JOIN(r_k_numer, r_k_denom, LEFT.model_id=RIGHT.model_id,
                      makeACF(LEFT, RIGHT), LOOKUP);
    ACF_Rec accum_sq(ACF_rec prev, ACF_rec curr) := TRANSFORM
      SELF.sum_sq := IF(prev.model_id=curr.model_id, prev.sum_sq + prev.sq, 0);
      SELF := curr;
    END;
    r_k := UNGROUP(ITERATE(GROUP(pre_sumsq, model_id), accum_sq(LEFT,RIGHT)));
    // Now calculate the partials
    Cell cvt2Cell(ACF_Rec acf) := TRANSFORM
      SELF.x := acf.k;
      SELF.y := 1;
      SELF.v := acf.ac;
      SELF.model_id := acf.model_id;
    END;
    Cell mult_k_kj(Cell r_k, Cell r_kj, UNSIGNED i) := TRANSFORM
      SELF.v := r_k.v * r_kj.v;
      SELF.x := r_kj.x + 1;
      SELF.y := MAP(r_k.x=r_kj.y AND r_k.x=i-r_kj.y => 3,   // both
                    r_k.x=r_kj.y                    => 2,   // divisor
                    1);
      SELF.model_id := r_k.model_id;
    END;
    Cell make_rkk(Cell r_k, DATASET(Cell) pairs) := TRANSFORM
      SELF.v := (r_k.v - SUM(pairs(y<>2), v)) / (1.0 - SUM(pairs(y<>1), v));
      SELF.x := r_k.x;
      SELF.y := r_k.x;
      SELF.model_id := r_k.model_id;
    END;
    Cell reverse_j(Cell r_kj, Cell r_kk) := TRANSFORM
      SELF.v := r_kj.v * r_kk.v;
      SELF.x := r_kj.x;
      SELF.y := r_kj.x+1 - r_kj.y;  // reverse the entries
      SELF.model_id := r_kk.model_id;
    END;
    Cell reduce_kj(Cell r_kj, Cell r_p) := TRANSFORM
      SELF.v := r_kj.v - r_p.v;
      SELF.x := r_kj.x + 1;
      SELF.y := r_kj.y;
      SELF.model_id := r_p.model_id;
    END;
    rk_cells := PROJECT(r_k, cvt2Cell(LEFT));
    init_partial := DATASET([], Cell);
    loop_body(DATASET(Cell) work, UNSIGNED i) := FUNCTION
      work_pairs := JOIN(rk_cells(x<i), work(x=i-1),
                        LEFT.model_id=RIGHT.model_id
                        AND (LEFT.x=i-RIGHT.y OR LEFT.x=RIGHT.y),
                        mult_k_kj(LEFT,RIGHT, i), LOCAL);
      r_kk := DENORMALIZE(rk_cells(x=i), work_pairs,
                          LEFT.model_id=RIGHT.model_id AND LEFT.x=RIGHT.x, GROUP,
                          make_rkk(LEFT, ROWS(RIGHT)), LOCAL);
      r_kj_r_kk := JOIN(work(x=i-1), r_kk, LEFT.model_id=RIGHT.model_id,
                          reverse_j(LEFT, RIGHT), LOCAL);
      r_kj := JOIN(work(x=i-1), r_kj_r_kk,
                   LEFT.model_id=RIGHT.model_id AND LEFT.x=RIGHT.x AND LEFT.y=RIGHT.y,
                   reduce_kj(LEFT,RIGHT), LOOKUP, LOCAL);
      RETURN work & r_kk & r_kj;
    END;
    partials := LOOP(init_partial, lags, loop_body(ROWS(LEFT), COUNTER));
    Types.PACF_ACF calc_t(ACF_Rec acf, Cell pacf) := TRANSFORM
      work := 1 / SQRT(acf.N);
      SELF.lags:= lags;
      SELF.lag := acf.k;
      SELF.av := acf.av;
      SELF.ac := acf.ac;
      SELF.pac := pacf.v;
      SELF.ac_t_like := acf.ac/(SQRT(1+2*acf.sum_sq) * work);
      SELF.pac_t_like := pacf.v / work;
      SELF.model_id := acf.model_id;
    END;
    rslt := JOIN(r_k, partials(x=y),
                 LEFT.model_id=RIGHT.model_id AND LEFT.k=RIGHT.x,
                 calc_t(LEFT, RIGHT), LOOKUP);
    RETURN rslt;
  END;
  EXPORT DATASET(Types.CorrRec) ACF(UNSIGNED2 lags) := FUNCTION
    Types.CorrRec extractCorr(Types.PACF_ACF rec) := TRANSFORM
      SELF.model_id := rec.model_id;
      SELF.lag := rec.lag;
      SELF.corr := rec.ac;
      SELF.t_like := rec.ac_t_like;
    END;
    RETURN PROJECT(Correlations(lags), extractCorr(LEFT));
  END;
  EXPORT DATASET(Types.CorrRec) PACF(UNSIGNED2 lags) := FUNCTION
    Types.CorrRec extractPCorr(Types.PACF_ACF rec) := TRANSFORM
      SELF.model_id := rec.model_id;
      SELF.lag := rec.lag;
      SELF.corr := rec.pac;
      SELF.t_like := rec.pac_t_like;
    END;
    RETURN PROJECT(Correlations(lags), extractPCorr(LEFT));
  END;
END;
