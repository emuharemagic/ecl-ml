IMPORT ML;
IMPORT ML.Types AS Types;
IMPORT Std.Str ;
IMPORT ML.mat as Mat;
NumericField := Types.NumericField;

/*
  The object of the regression module is to generate a regression model.
  A regression model relates the dependent variable Y to a function of
  the independent variables X, and a vector of unknown parameters Beta.
    Y = f(X,Beta)
  A regression model is an algorithm that estimates the unknown parameters
  Beta so that a regression function Y = f(X,Beta) can be constructed
*/

// OrdinaryLeastSquares, aka LinearLeastSquares, the simplest and most common estimato
// Beta = (Inv(X'*X)*X')*Y
EXPORT OLS(DATASET(NumericField) X,DATASET(NumericField) Y)
:= MODULE(ML.IRegression)
  SHARED DATASET(NumericField) Independents := X;
  SHARED DATASET(NumericField) Dependents := Y;
  mX_0 := Types.ToMatrix(X);
  SHARED mX := Mat.InsertColumn(mX_0, 1, 1.0); // Insert X1=1 column
  SHARED mXt := Mat.Trans(mX);
  SHARED mY := Types.ToMatrix(Y);
  // Calculate Betas for model
  SHARED DATASET(Mat.Types.Element) mBetas;
  // We want to return the data so that the ID field reflects the 'column number' of
  //the variable we were targeting
  rBetas := Types.FromMatrix( Mat.Trans(mBetas) );
  // We also need to move the 'constant' term into column 0
  NumericField remapCol(NumericField lr) := TRANSFORM
    SELF.number := lr.number - 1;
    SELF := lr;
  END;
  sBetas := PROJECT(rBetas,remapCol(LEFT));
  EXPORT DATASET(NumericField) Betas := sBetas;

  // use calculated estimator to predict Y values
  Y_estM := Mat.Trans(Mat.Mul(Mat.Trans(mBetas) , mXt));
  EXPORT DATASET(NumericField) modelY := Types.FromMatrix(Y_estM);

  EXPORT DATASET(NumericField) Extrapolated(DATASET(NumericField) newX) := FUNCTION
    mX_0 := Types.ToMatrix(newX);
    mXloc := Mat.InsertColumn(mX_0, 1, 1.0); // Insert X1=1 column
    RETURN Types.FromMatrix( Mat.Mul(mXloc, mBetas) );
  END;

  // Y.number = number*2; Y_est.number = number*2+1;
  Y_est1 := PROJECT(modelY, TRANSFORM(NumericField, SELF.number := 2*LEFT.number+1; SELF := LEFT));
  Y1 := PROJECT(Y, TRANSFORM(NumericField, SELF.number := 2*LEFT.number; SELF := LEFT));

  SHARED corr_ds := ML.Correlate(Y1+Y_est1).Simple;

  CoRec getResult(corr_ds le) :=TRANSFORM
    SELF.number := le.left_number/2;
    SELF.RSquared := le.pearson*le.pearson ;
  END;
  // R^2 : square of the correlation coefficient between the observed and modeled (predicted) data values
  // Statistic that gives information about the goodness of fit of a model
  // It estimates the fraction of the variance in Y that is explained by X
  rslt := PROJECT(corr_ds(left_number&1=0,left_number+1=right_number), getResult(LEFT));
  EXPORT DATASET(CoRec) RSquared := rslt;
END;