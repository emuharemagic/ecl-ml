// Entend model parameters into co-efficients for application and extract mu
IMPORT TS;
IMPORT TS.Types;
Model_Parameters := Types.Model_Parameters;
Co_efficient := Types.Co_efficient;
Parm_Extend := Types.Parameter_Extension;

EXPORT DATASET(Types.Parameter_Extension)
       ExtendedParameters(DATASET(Model_Parameters) models) := FUNCTION
  Co_efficient mrgF(Co_efficient theta, Co_efficient phi) := TRANSFORM
    SELF.lag := IF(theta.lag<>0, theta.lag, phi.lag);
    SELF.cv := theta.cv + phi.cv;
  END;
  Parm_Extend calcExtend(Model_Parameters prm) := TRANSFORM
    SELF.terms := MAX(prm.ar_terms, prm.ma_terms) + prm.degree + 1;
    SELF.mu := IF(prm.ar_terms>0, prm.c*(1.0-SUM(prm.ar,cv)), prm.c);
    SELF.theta_phi := JOIN(prm.ar, prm.ma, LEFT.lag=RIGHT.lag,
                           mrgF(LEFT,RIGHT), FULL OUTER);
    SELF.phi := prm.ma;
    SELF := prm;
  END;
  extend_specs := PROJECT(models, calcExtend(LEFT));
  RETURN extend_specs;
END;
