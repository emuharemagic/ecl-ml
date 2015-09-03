// Expand a series into a series for each model
IMPORT TS;
ObsRec := TS.Types.UniObservation;
ModelObs := TS.Types.ModelObs;
Ident_Spec := TS.Types.Ident_Spec;
EXPORT DATASET(TS.Types.ModelObs)
       SeriesByModel(DATASET(TS.Types.UniObservation) series,
                     DATASET(TS.Types.Ident_Spec) spec) := FUNCTION
  ModelObs byModel(ObsRec obs, Ident_Spec sp) := TRANSFORM
    SELF.model_id := sp.model_id;
    SELF := obs;
  END;
  exploded := JOIN(series, spec, TRUE, byModel(LEFT, RIGHT), ALL);
  model_obs := SORT(exploded, model_id, period);
  RETURN model_obs;
END;
