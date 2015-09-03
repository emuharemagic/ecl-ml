// Calculate the estimate values for history and future
IMPORT TS;
IMPORT TS.Types;
ModelObs := TS.Types.ModelObs;
Parameter_Extension := TS.Types.Parameter_Extension;
Co_eff := TS.Types.CO_efficient;
ObsRec := TS.Types.UniObservation;
ObsWork := RECORD
    INTEGER period;
    ObsRec.dependent;
END;
EXPORT DATASET(TS.Types.Obs_Estimated)
       EstimatedObs(DATASET(TS.Types.ModelObs) model_obs,
                    DATASET(TS.Types.Model_Parameters) models,
                    UNSIGNED2 forecast_periods=0) := FUNCTION
  degreeOK := ASSERT(models, degree<6, 'degree > 5 not supported', FAIL);
  diffed := TS.DifferenceSeries(model_obs, degreeOK, keepInitial:=TRUE);
  extend_specs := TS.ExtendedParameters(degreeOK);
  // Score the models
  WorkRec := RECORD(Parameter_Extension)
    TS.Types.t_time_ord period;
    TS.Types.t_value dependent;
    TS.Types.t_value estimate;
    BOOLEAN future;
  END;
  // Look at replacing when we go to more data than 1 node.  In the
  //meantime, we need the records on the node with the data.
  DATASET(ObsWork) jumpStart(UNSIGNED t, REAL8 mu) := FUNCTION
    ObsWork genObs(UNSIGNED c) := TRANSFORM
      SELF.period := 1 - c;
      SELF.dependent := mu;
    END;
    dummy := DATASET([{1}], {UNSIGNED1 x});
    rslt := NORMALIZE(dummy, t, genObs(COUNTER));
    RETURN rslt;
  END;  //jumpStart function definition

  HistRec := RECORD
    DATASET(ObsWork) act;
    DATASET(ObsWork) fcst;
  END;
  WorkRec makeBase(ModelObs obs, Parameter_Extension mod) := TRANSFORM
    SELF.estimate := IF(obs.period < mod.degree, obs.dependent, 0.0);
    SELF.future := FALSE;
    SELF := obs;
    SELF := mod;
  END;
  withParm := JOIN(diffed, extend_specs, LEFT.model_id=RIGHT.model_id,
                     makeBase(LEFT,RIGHT), LOOKUP);
  grpdModBase:= GROUP(withParm, model_id, ALL);
  grpdModLast:= TOPN(grpdModBase, 1, -period);
  WorkRec makeFuture(WorkRec lstRec, UNSIGNED c) := TRANSFORM
    SELF.estimate := 0.0;
    SELF.future := TRUE;
    SELF.period := lstRec.period + c;
    SELF.dependent := 0.0;
    SELF := lstRec; // pick up model stuff
  END;
  grpdModFtr := NORMALIZE(grpdModLast, forecast_periods, makeFuture(LEFT, COUNTER));
  grpdModObs := SORT(GROUP(grpdModBase+grpdModFtr, model_id, ALL), model_id, period);
  // Process definition from iteration through observations
  Upd(WorkRec wr, HistRec hr) := MODULE
    SHARED act := IF(EXISTS(hr.act), hr.act, jumpStart(wr.terms, wr.mu));
    SHARED fct := IF(EXISTS(hr.fcst), hr.fcst, jumpStart(wr.terms, wr.mu));
    SHARED actN:= DATASET([{wr.period, wr.dependent}], ObsWork) & act;
    ObsWork prodTerm(Co_eff cof, ObsWork t, INTEGER sgn) := TRANSFORM
      SELF.period := t.period;
      SELF.dependent := cof.cv * t.dependent * (REAL8)sgn;
    END;
    poly1 := JOIN(wr.theta_phi, act,
                  LEFT.lag = wr.period - RIGHT.period,
                  prodTerm(LEFT,RIGHT, 1), LOOKUP);
    poly2 := JOIN(wr.phi, fct,
                  LEFT.lag = wr.period - RIGHT.period,
                  prodTerm(LEFT,RIGHT,-1), LOOKUP);
    SHARED forecast_val := SUM(poly1 + poly2, dependent) + wr.c - wr.mu;
    SHARED fctN:= DATASET([{wr.period, forecast_val}], ObsWork) & fct;
    EXPORT HistRec histUpd() := TRANSFORM
      SELF.act := IF(wr.period>wr.degree, CHOOSEN(actN, wr.terms));
      SELF.fcst:= IF(wr.period>wr.degree, CHOOSEN(fctN, wr.terms));
    END;
    EXPORT WorkRec obsUpd() := TRANSFORM
      SELF.dependent := IF(wr.future, forecast_val, wr.dependent);
      SELF.estimate := IF(wr.period>wr.degree+wr.terms, forecast_val, wr.dependent);
      SELF := wr;
    END;
  END;
  initH := ROW({DATASET([],ObsWork), DATASET([], ObsWork)}, HistRec);
  withEst := PROCESS(grpdModObs, initH, Upd(LEFT,RIGHT).obsUpd(), Upd(LEFT,RIGHT).histUpd());
  //reverse differencing
  WorkRec accumObs(WorkRec prev, WorkRec curr, UNSIGNED2 pass) := TRANSFORM
    no_accum := pass > curr.degree OR curr.degree+1-pass >= curr.period;
    SELF.dependent := IF(no_accum, curr.dependent, curr.dependent+prev.dependent);
    SELF.estimate  := IF(no_accum, curr.estimate, curr.estimate+prev.estimate);
    SELF := curr;
  END;
  accum1 := ITERATE(withEst, accumObs(LEFT, RIGHT, 1));
  accum2 := ITERATE(accum1, accumObs(LEFT, RIGHT, 2));
  accum3 := ITERATE(accum2, accumObs(LEFT, RIGHT, 3));
  accum4 := ITERATE(accum3, accumObs(LEFT, RIGHT, 4));
  accum5 := ITERATE(accum4, accumObs(LEFT, RIGHT, 5));
  RETURN PROJECT(UNGROUP(accum5), TS.Types.Obs_Estimated);
END;
