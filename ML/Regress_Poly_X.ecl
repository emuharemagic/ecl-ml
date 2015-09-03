IMPORT ML;
IMPORT ML.Types AS Types;
IMPORT ML.Utils AS Utils;
IMPORT Std.Str;
// OLS2Use := ML.Regression.Sparse.OLS_LU;
OLS2Use := ML.Regression.Dense.OLS_LU;

EXPORT Regress_Poly_X(DATASET(Types.NumericField) X,
                      DATASET(Types.NumericField) Y,
                      UNSIGNED1 maxN=6) := MODULE
  SHARED  newX := ML.Generate.ToPoly(X,maxN);
	
  // Transform fieldNumber of Y to max(field number of new X) + 1
  // Needed for FUNCTION Extrapolated to work correctly
  SHARED Types.NumericField transformY_Number(Types.NumericField Dt) := TRANSFORM
     SELF.number := maxN + 1;
     SELF := Dt;
  END;	
  SHARED newY := PROJECT(Y, transformY_Number(LEFT));

  SHARED B := OLS2Use(newX, newY);

  SHARED Pretty_Out := RECORD
    Types.t_RecordID id;
    STRING10 name;
    Types.t_FieldReal value;
  END;
  SHARED Pretty_Out  makePretty(Types.NumericField dt) := TRANSFORM
    SELF.name := ML.Generate.MethodName(dt.number);
    SELF := dt;
  END;
  EXPORT Beta := PROJECT(B.Betas, makePretty(LEFT));

  EXPORT RSquared := B.RSquared;
	
  // Predict Y values given new Data (in format of X) in Dt
  EXPORT DATASET(Types.NumericField) Extrapolated(DATASET(Types.NumericField) Dt) := FUNCTION
     newDt := ML.Generate.ToPoly(Dt, maxN);
     rslt := B.Extrapolated(newDt);
     RETURN rslt;
  END;

  // use K out of N polynomial components, and find the best model
  EXPORT SubBeta(UNSIGNED1 K, UNSIGNED1 N) := FUNCTION

    nk := Utils.NchooseK(N, K);
    R := RECORD
      REAL r2 := 0;
      nk.Kperm;
    END;
    // permutations
    perms := TABLE(nk, R);

    // evaluate permutations for the model fit based on RSquared
    R T(R le) := TRANSFORM
      x_subset := newX(number IN (SET OF INTEGER1)Str.SplitWords(le.Kperm, ' '));
      reg := OLS2Use(x_subset, Y);
      SELF.r2 := (reg.RSquared)[1].rsquared;
      SELF := le;
    END;

    fitDS := PROJECT(perms, T(LEFT));

    //winning permutation
    wperm := fitDS((r2=MAX(fitDS,r2)))[1].Kperm;
    x_winner := newX(number IN (SET OF INTEGER1)Str.SplitWords(wperm, ' '));
    wB := OLS2Use(x_winner, Y).Betas;

    prittyB := PROJECT(wB, ^.makePretty(LEFT));
    RETURN prittyB;
  END;
END;
