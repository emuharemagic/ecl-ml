//
IMPORT PBblas;
EXPORT Types := MODULE
  EXPORT t_time_ord := UNSIGNED4;
  EXPORT t_value := PBblas.Types.value_t;
  EXPORT t_value_set := PBblas.Types.matrix_t;
  EXPORT t_model_id := UNSIGNED2;
  EXPORT UniObservation := RECORD
    t_time_ord period;
    t_value dependent;
  END;
  EXPORT ModelObs := RECORD(UniObservation)
    t_model_id model_id;
  END;
  EXPORT DecoratedObs := RECORD(ModelObs)
    UNSIGNED2 lags;
  END;
  EXPORT CorrRec := RECORD
    t_model_id model_id;
    UNSIGNED2 lag;
    REAL8 corr;     // auto correlation or partial auto correlation
    REAL8 t_like;   // Similar to t statistic, Box-Jenkins
  END;
  EXPORT PACF_ACF := RECORD
    t_model_id model_id;
    UNSIGNED2 lags;
    UNSIGNED2 lag;      // k
    REAL8 av;           // Auto covariance, kth
    REAL8 ac;           // Auto corr, k-th
    REAL8 ac_t_like;    // t like Box Jenkins statistic
    REAL8 pac;          // partial auto corr, kk-th
    REAL8 pac_t_like;   // t-like Box-Jenkins statistic
  END;
  EXPORT Co_efficient := RECORD
    t_value cv;
    UNSIGNED2 lag;
  END;
  EXPORT Ident_Spec := RECORD
    UNSIGNED2 model_id;
    UNSIGNED2 degree;
  END;
  EXPORT Model_Spec := RECORD(Ident_Spec)
    UNSIGNED2 ar_terms;
    UNSIGNED2 ma_terms;
    BOOLEAN constant_term;
  END;
  EXPORT Model_Parameters := RECORD(Model_Spec)
    DATASET(Co_efficient) ar;
    DATASET(Co_efficient) ma;
    t_value c;
  END;
  EXPORT Model_Score := RECORD(Model_Parameters)
    REAL8 s_measure; // square root of SSE/(N - number of parameters)
    REAL8 Q_measure; // Box-Pierce Chi Square
  END;
  EXPORT Parameter_Extension := RECORD
    t_model_id model_id;
    UNSIGNED2 degree;
    UNSIGNED2 terms;
    t_value c;
    t_value mu;
    DATASET(Co_efficient) theta_phi;
    DATASET(Co_efficient) phi;
  END;
  EXPORT Obs_Estimated := RECORD(ModelObs)
    t_value estimate := 0.0;
  END;
END;
