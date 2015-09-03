//Produce a difference series of degree 0, 1, or 2
IMPORT TS.Types;
ModelObs := Types.ModelObs;
Spec := Types.Ident_Spec;
EXPORT DATASET(ModelObs)
       DifferenceSeries(DATASET(ModelObs) obs,
                        DATASET(Spec) degree_spec,
                        BOOLEAN keepInitial=FALSE) := FUNCTION
  ObsRec := RECORD(ModelObs)
    UNSIGNED2 degree;
    Types.t_value work;
  END;
  ObsRec markDegree(ModelObs obs, Spec sp) := TRANSFORM
    SELF.degree := sp.degree;
    SELF.work := 0.0;
    SELF := obs;
  END;
  degreeOK := ASSERT(degree_spec, degree<6, 'degree > 5 not supported', FAIL);
  marked := JOIN(obs, degreeOK, LEFT.model_id=RIGHT.model_id,
                 markDegree(LEFT, RIGHT), LOOKUP);
  ObsRec deltaObs(ObsRec prev, ObsRec curr, UNSIGNED2 pass) := TRANSFORM
    no_diff := pass > curr.degree OR pass>=curr.period;
    SELF.dependent := IF(no_diff, curr.dependent, curr.dependent-prev.work);
    SELF.work := curr.dependent;
    SELF := curr;
  END;
  grped := GROUP(SORTED(marked, model_id, period), model_id);
  diff1 := ITERATE(grped, deltaObs(LEFT, RIGHT, 1));
  diff2 := ITERATE(diff1, deltaObs(LEFT, RIGHT, 2));
  diff3 := ITERATE(diff2, deltaObs(LEFT, RIGHT, 3));
  diff4 := ITERATE(diff3, deltaObs(LEFT, RIGHT, 4));
  diff5 := ITERATE(diff4, deltaObs(LEFT, RIGHT, 5));
  rslt := UNGROUP(PROJECT(diff5(keepInitial OR period>degree), ModelObs));
  RETURN rslt;
END;
