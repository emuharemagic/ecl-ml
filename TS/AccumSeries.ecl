//Produce a series from a differenced series of degree 0 to 5
IMPORT TS.Types;
ModelObs := Types.ModelObs;
Spec := Types.Ident_Spec;
EXPORT AccumSeries(DATASET(Types.ModelObs) obs,
                        DATASET(Types.Ident_Spec) degree_spec) := FUNCTION
  ObsRec := RECORD(Types.ModelObs)
    UNSIGNED2 degree;
  END;
  ObsRec markDegree(ModelObs obs, Spec sp) := TRANSFORM
    SELF.degree := sp.degree;
    SELF := obs;
  END;
  degreeOK := ASSERT(degree_spec, degree<6, 'degree > 5 not supported', FAIL);
  marked := JOIN(obs, degreeOK, LEFT.model_id=RIGHT.model_id,
                 markDegree(LEFT, RIGHT), LOOKUP);
  ObsRec accumObs(ObsRec prev, ObsRec curr, UNSIGNED2 pass) := TRANSFORM
    no_accum := pass > curr.degree OR curr.degree+1-pass >= curr.period;
    base := IF(prev.model_id<>curr.model_id, 0, prev.dependent);
    SELF.dependent := IF(no_accum, curr.dependent, curr.dependent + base);
    SELF := curr;
  END;
  grped  := GROUP(SORTED(marked, model_id, period), model_id);
  accum1 := ITERATE(grped,  accumObs(LEFT, RIGHT, 1));
  accum2 := ITERATE(accum1, accumObs(LEFT, RIGHT, 2));
  accum3 := ITERATE(accum2, accumObs(LEFT, RIGHT, 3));
  accum4 := ITERATE(accum3, accumObs(LEFT, RIGHT, 4));
  accum5 := ITERATE(accum4, accumObs(LEFT, RIGHT, 5));
  rslt := UNGROUP(PROJECT(accum5, ModelObs)); // data is skewed from grouping
  RETURN rslt;
END;
