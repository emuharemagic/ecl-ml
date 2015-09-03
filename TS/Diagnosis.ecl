// retrocast series and produce error stats
IMPORT TS;
Model_Score := TS.Types.Model_Score;
Model_Parm := TS.Types.Model_Parameters;

EXPORT DATASET(TS.Types.Model_Score)
       Diagnosis(DATASET(TS.Types.UniObservation) obs,
                 DATASET(TS.Types.Model_Parameters) models) := FUNCTION
  // Generate data for each model
  byModel := TS.SeriesByModel(obs, models);
  // Calculate forecast
  act_fcst := TS.EstimatedObs(byModel, models, 0);
  REAL8 se(REAL8 d, REAL8 e) := (d-e) * (d-e);    // Squared error
  model_sse := TABLE(act_fcst,
                    {model_id, num_obs := COUNT(GROUP), sse:=SUM(GROUP, se(dependent, estimate))},
                    model_id, FEW);
  Model_Score calc_s(Model_Parm p, model_sse s) := TRANSFORM
    SELF.s_measure := SQRT(s.sse / (s.num_obs-p.ar_terms-p.ma_terms));
    SELF.q_measure := 0.0;      // Need to add q measure
    SELF := p;
  END;
  scored := JOIN(models, model_sse,
                 LEFT.model_id=RIGHT.model_id,
                 calc_s(LEFT,RIGHT), LOOKUP);
  RETURN scored;
END;
