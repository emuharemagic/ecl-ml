//    Ordinary least squares regression using dense matrix structures.

//The object of the regression module is to generate a regression model.
//A regression model relates the dependent variable Y to a function of
//the independent variables X, and a vector of unknown parameters Beta
//    Y = f(X,Beta)
//A regression model is an algorithm that estimates the unknown parameters
//Beta sothat a regression function Y = f(X,Beta) can be constructed

IMPORT ML;
IMPORT ML.Types AS Types;
IMPORT Std.Str ;
IMPORT ML.mat as Mat;
IMPORT PBblas as PBblas;
IMPORT ML.DMat as DMat;
NotCompat := PBblas.Constants.Dimension_Incompat;
Matrix_Map:= PBblas.Matrix_Map;
Part      := PBblas.Types.Layout_Part;
NumericField := Types.NumericField;

EXPORT OLS(DATASET(NumericField) X,DATASET(NumericField) Y)
:= MODULE(ML.IRegression)
  SHARED DATASET(NumericField) Independents := X;
  SHARED DATASET(NumericField) Dependents := Y;
  // Describe the matrices involved
  SHARED x_rows:= MAX(X, id);
  SHARED x_cols:= MAX(X, number) + 1;    // add constant for intercept
  SHARED y_rows:= MAX(Y, id);
  SHARED y_cols:= MAX(Y, number);
  SHARED block_rows := MIN(PBblas.Constants.Block_Vec_Rows, x_rows);
  SHARED x_map := Matrix_Map(x_rows, x_cols, block_rows, x_cols);
  SHARED y_map := Matrix_Map(y_rows, y_cols, block_rows, y_cols);
  SHARED b_map := Matrix_Map(x_cols, y_cols, x_cols, y_cols);
  SHARED z_map := Matrix_Map(x_cols, x_cols, x_cols, x_cols);
  x_OK  := ASSERT(X, x_rows=y_rows, NotCompat, FAIL);
  SHARED x_part:= DMat.Converted.FromNumericFieldDS(x_OK, x_map, 1, 1.0);
  y_OK  := ASSERT(Y, x_rows=y_rows, NotCompat, FAIL);
  SHARED y_part:= DMat.Converted.FromNumericFieldDS(y_OK, y_map);

  // The beta values for the model
  EXPORT DATASET(Part) BetasAsPartition;
  b_elem:= DMat.Converted.FromPart2Elm(BetasAsPartition);
  EXPORT DATASET(Mat.Types.Element) BetasAsElements := b_elem;
  b_nf  := DMat.Converted.FromPart2DS(BetasAsPartition);
  // We want to return the data so that the ID field reflects the 'column number' of
  //the variable we were targeting
  // We also need to move the 'constant' term into column 0
  NumericField remapCol(NumericField lr) := TRANSFORM
    SELF.id     := lr.number;
    SELF.number := lr.id - 1;
    SELF := lr;
  END;
  sBetas := PROJECT(b_nf,remapCol(LEFT));
  EXPORT DATASET(Types.NumericField) betas := sBetas;

  // the model Y values.
  y_est := PBblas.PB_dgemm(FALSE, FALSE, 1.0, x_map, x_part, b_map, BetasAsPartition, y_map);
  EXPORT DATASET(Part) modelY_part := y_est;
  Y_numbers := SET(dedup(sort(Y,number),number),number);
  y_est_nf := DMat.Converted.FromPart2DS(modelY_part)(number IN Y_numbers);
  EXPORT DATASET(NumericField) modelY := y_est_nf;

  // Extrapolated values
  EXPORT DATASET(Part) Extrapolated_part(DATASET(NumericField) newX) := FUNCTION
    nx_rows := MAX(newX, id);
    new_block := MIN(PBblas.Constants.Block_Vec_Rows, nx_rows);
    nx_map  := Matrix_Map(nx_rows, x_cols, new_block, x_cols);
    ny_map  := Matrix_Map(nx_rows, y_cols, new_block, y_cols);
    nx_part := DMat.Converted.FromNumericFieldDS(newX, nx_map, 1, 1.0);
    ny_ex := PBblas.PB_dgemm(FALSE, FALSE, 1.0, nx_map, nx_part, b_map, BetasAsPartition, ny_map);
    RETURN ny_ex;
  END;
  EXPORT DATASET(NumericField) Extrapolated(DATASET(NumericField) newX) := FUNCTION
    newX_numbers := SET(dedup(sort(newX,number),number),number);
    yex_p := Extrapolated_part(newX);
    rslt := DMat.Converted.FromPart2DS(yex_p)(number NOT IN newX_numbers);
    RETURN rslt;
  END;
  // R-Squared
  // remap columns to even odd pairs for actual and model Y values
  NumericField remapColumn(NumericField lr, INTEGER d) := TRANSFORM
    SELF.number := lr.number*2 + d;
    SELF := lr;
  END;
  y_even := PROJECT(y, remapColumn(LEFT, 0));    // make even
  y_odd  := PROJECT(modelY, remapColumn(LEFT, 1));  // make odd
  corr_ds := ML.Correlate(y_even+y_odd).Simple;
  y_yhat := corr_ds(left_number&1=0,left_number+1=right_number);
  // remap correlations of he pairs back to columns
  CoRec makeRSQ(corr_ds cov_cor) := TRANSFORM
    SELF.number := cov_cor.left_number DIV 2;
    SELF.RSquared := cov_cor.pearson * cov_cor.pearson;
  END;
  EXPORT DATASET(CoRec)  RSquared := PROJECT(y_yhat, makeRSQ(LEFT));
END;