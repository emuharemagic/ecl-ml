IMPORT ML;
IMPORT ML.Types AS Types;
IMPORT ML.Lazy AS Lazy;

EXPORT KNN_DotProd(CONST Types.t_count NN_count=5):= MODULE(Lazy.KNN(NN_count))
  SHARED SearchC(DATASET(Types.NumericField) indepData , DATASET(Types.NumericField) qpData):=FUNCTION
    D := Types.ToMatrix(indepData);
    Q := Types.ToMatrix(qpData);
    DQ := ML.LSA.Similarity.DotProductSim(D, Q);
    NN := ML.NearestNeighborsSearch.NN;
    DQC := UNGROUP(TOPN(GROUP(SORT(DQ, y), y), NN_count, -value));
    RETURN PROJECT(DQC, TRANSFORM(NN, SELF.qp_id := LEFT.y; SELF.id := LEFT.x; SELF.distance := LEFT.value));
  END;
  EXPORT ClassifyC(DATASET(Types.NumericField) indepData , DATASET(Types.DiscreteField) depData ,DATASET(Types.NumericField) queryPointsData):= FUNCTION
    Neighbors:= SearchC(indepData , queryPointsData);
    RETURN  MajorityVote(Neighbors, depData);
  END;
END;