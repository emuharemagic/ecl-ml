// Apply the model to generate forecast observations.  All of the
//observations are used for the forecasts for each of the model
//specifications provided.
IMPORT TS;
EXPORT DATASET(TS.Types.ModelObs)
       Forecast(DATASET(TS.Types.UniObservation) obs,
                DATASET(TS.Types.Model_Parameters) parms,
                UNSIGNED2 forecast_periods) := FUNCTION
  // Generate data for each model
  byModel := TS.SeriesByModel(obs, parms);
  // Calculate forecast
  act_fcst := TS.EstimatedObs(byModel, parms, forecast_periods);
  // Produce series for forecast values
  TS.Types.ModelObs cvt(TS.Types.Obs_Estimated obs) := TRANSFORM
    SELF.dependent := obs.estimate;
    SELF := obs;
  END;
  RETURN PROJECT(act_fcst, cvt(LEFT));
END;
