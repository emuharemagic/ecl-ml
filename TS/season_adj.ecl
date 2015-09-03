// Seasonal adjustment of the time series.  The adjustment can be added or
//multiplied.
IMPORT TS;
IMPORT ML;
UniObservation := TS.Types.UniObservation;
t_value := TS.Types.t_value;
NumericField := ML.Types.NumericField;

EXPORT season_adj := MODULE
  SHARED ExtendedObs := RECORD(UniObservation)
    UNSIGNED season;
  END;
  SHARED ExtendedObs markObs(UniObservation obs, UNSIGNED seasons) := TRANSFORM
    SELF.season := ((obs.period-1) % seasons) + 1;
    SELF := obs;
  END;
  SHARED t_value AdjustPrototype(UNSIGNED f, t_value v) := v;
  SHARED t_value MultiplyAdj(UNSIGNED f, t_value v) := f * v;
  SHARED t_value AddAdj(UNSIGNED f, t_value v) := f + v;
  SHARED UniObservation adj_obs(ExtendedObs obs,
                                NumericField f,
                                AdjustPrototype func) := TRANSFORM
    SELF.dependent := IF(f.id=0, obs.dependent, func(f.value, obs.dependent));
    SELF := obs;
  END;
  SHARED DATASET(UniObservation) ApplyAdj(DATASET(UniObservation) ts,
                                          DATASET(NumericField) adj,
                                          UNSIGNED seasons,
                                          UNSIGNED adj_col,
                                          AdjustPrototype func) := FUNCTION
    marked := PROJECT(ts, markObs(LEFT, seasons));
    filtered := adj(id<=seasons AND number=adj_col);
    RETURN JOIN(marked, filtered, LEFT.season=RIGHT.id,
                   adj_obs(LEFT, RIGHT, func), LOOKUP);
  END;
  /**
   * Apply multiplicative seasonal adjustment factor to the series.
   * @param ts the time series
   * @param adj the adjustment factor, the season corresponds to the row
   * @param seasons the number of seasons
   * @param adj_col the column number of the seasonal factor.
   * @return the adjusted time series.
   */
  EXPORT DATASET(UniObservation) Multiply(DATASET(UniObservation) ts,
                                          DATASET(NumericField) adj,
                                          UNSIGNED seasons,
                                          UNSIGNED adj_col) := FUNCTION
    RETURN ApplyAdj(ts, adj, seasons, adj_col, MultiplyAdj);
  END;
  /**
   * Apply additive seasonal adjustment factor to the series.
   * @param ts the time series
   * @param adj the adjustment to be added, the season corresponds to the row
   * @param seasons the number of seasons
   * @param adj_col the column number of the seasonal factor
   */
  EXPORT DATASET(UniObservation) Add(DATASET(UniObservation) ts,
                                     DATASET(NumericField) adj,
                                     UNSIGNeD seasons,
                                     UNSIGNED adj_col) := FUNCTION
    RETURN ApplyAdj(ts, adj, seasons, adj_col, AddAdj);
  END;
END;