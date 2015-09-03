//
//Estimate the parameters by iteratively re-weighting the least squares.
// Calculates the initial estimates for the AR and MA parameters
// using OLS for the AR terms, and then solves the Yule-Walker
// equations on the residules (or original data if no AR terms)
// for the MA terms.  The estimtes are then adjusted using an
// iterative reweighted least squares method unitl either the
// maximum iterations occur or the change in the estimated
// parameters (measured by the 1-norm) is less than the
// epsilon parameter.
//
//In this version, all data for a model must fit on a single node
// in memory.
IMPORT TS;
IMPORT PBblas;
//Aliases
value_t := PBblas.Types.value_t;
dim_t := PBblas.Types.dimension_t;
matrix_t := PBblas.Types.matrix_t;
// Intermediate types
Model_cell := RECORD(PBblas.Types.Layout_Cell)
  TS.Types.t_model_id model_id;
  UNSIGNED2 lags;
END;
Extend_Spec := RECORD(TS.Types.Model_Spec)
  UNSIGNED2 obs_cnt;
  UNSIGNED2 lag := 0;
  value_t mean;
  value_t total;
  value_t var;
END;
Init_Work := RECORD(TS.Types.Model_Spec)
  UNSIGNED2 obs_cnt;
  value_t mean;
  value_t total;
  value_t var;
  matrix_t dep_set;
  matrix_t lag_set;
  matrix_t betas;
  matrix_t residual;
  matrix_t rlag_set;
  matrix_t yw_set;
  matrix_t cov_set;
  matrix_t ma_coef;
END;
Irls_Work := RECORD(TS.Types.Model_Spec)
  UNSIGNED2 row_cnt;
  value_t mean;
  value_t total;
  value_t var;
  value_t delta;
  value_t resid_mean;
  matrix_t dep_set;
  matrix_t betas;
  matrix_t full_x;
  matrix_t w;
END;
//
EXPORT DATASET(TS.Types.Model_Parameters)
       Estimation(DATASET(TS.Types.UniObservation) obs,
                  DATASET(TS.Types.Model_Spec) spec,
                  UNSIGNED2 maxIter=100, REAL4 epsilon=0.001) := FUNCTION
  model_obs := TS.SeriesByModel(obs, spec);
  post_difference := TS.DifferenceSeries(model_obs, spec);
  // Observations are now present for each model requested and differenced as
  //specified.  Now we need to turn a stream of observations into records
  //where each of the lags is a column.  First, figure out the number of columns
  //and rows and propagate into the detail
  obs_count := TABLE(post_difference,
                        {model_id, obs_cnt:=COUNT(GROUP),
                         mean:=AVE(GROUP,dependent), total:=SUM(GROUP,dependent),
                         var:=VARIANCE(GROUP,dependent)},
                        model_id, FEW, UNSORTED);
  spec_count := JOIN(obs_count, spec, LEFT.model_id=RIGHT.model_id, LOOKUP);
  Extend_Spec explodeLags(RECORDOF(spec_count) spec, UNSIGNED2 c) := TRANSFORM
    SELF.lag := c - 1;
    SELF := spec;
  END;
  spec_exploded := NORMALIZE(spec_count, LEFT.ar_terms+1, explodeLags(LEFT,COUNTER));
  // now ready to explode observations so that we can group up the lags
  Model_cell make_lag_cell(TS.Types.ModelObs obs, Extend_Spec spec) := TRANSFORM
    SELF.model_id := obs.model_id;
    SELF.lags := spec.ar_terms;
    SELF.x := obs.period + spec.lag;
    SELF.y := spec.lag + 1;
    SELF.v := obs.dependent;
  END;
  ar_cells:= JOIN(post_difference, spec_exploded,
                 LEFT.model_id=RIGHT.model_id
                 AND LEFT.period+RIGHT.lag
                     BETWEEN RIGHT.degree+RIGHT.ar_terms+1
                         AND RIGHT.obs_cnt+RIGHT.degree,
                 make_lag_cell(LEFT, RIGHT), MANY LOOKUP);
  ar_sorted_cells := SORT(ar_cells, model_id);
  indy_cells := SORT(ar_sorted_cells(y > 1), model_id);
  dep_cells := SORT(ar_sorted_cells(y=1), model_id);
  // we have our dependent cells, and the independent cells.  Ready to roll to matrix.
  Init_Work makeBase(RECORDOF(spec_count) base) := TRANSFORM
    SELF := base;
    SELF := [];
  END;
  base_ar := SORT(PROJECT(spec_count, makeBase(LEFT)), model_id);
  Target := ENUM(dep_set, lag_set, cov_set, yw_set, rlag_set);
  Init_Work rollCells(Init_Work base, DATASET(Model_cell) cells, Target t):=TRANSFORM
    row_count := base.obs_cnt-base.ar_terms - IF(t=Target.rlag_set,base.ma_terms,0);
    first_row := base.degree + IF(t=Target.rlag_set,base.ma_terms,base.ar_terms+1);
    first_col := IF(base.constant_term, 2, 1);
    lag_insert:= IF(base.constant_term, 1, 0);
    lag_cols  := lag_insert + IF(t=Target.rlag_set, base.ma_terms, base.ar_terms);
    SELF.dep_set := IF(t=Target.dep_set,
                        PBblas.MakeR8Set(row_count, 1, first_row, 1,
                                         PROJECT(cells, PBblas.Types.Layout_Cell),
                                         0, 0.0),
                        base.dep_set);;
    SELF.lag_set := IF(t=Target.lag_set,
                       PBblas.MakeR8Set(row_count, lag_cols, first_row, first_col,
                                        PROJECT(cells, PBblas.Types.Layout_Cell),
                                        lag_insert, 1.0),
                       base.lag_set);
    SELF.cov_set := IF(t=Target.cov_set,
                       PBblas.MakeR8Set(base.ma_terms,1, 1, 1,
                                        PROJECT(cells, PBblas.Types.Layout_Cell),
                                        0, 0.0),
                       base.cov_set);
    SELF.yw_set  := IF(t=Target.yw_set,
                       PBblas.MakeR8Set(base.ma_terms,base.ma_terms, 1, 1,
                                        PROJECT(cells, PBblas.Types.Layout_Cell),
                                        0, 0.0),
                       base.yw_set);
    SELF.rlag_set:= IF(t=Target.rlag_set, // row count and first row adjusted
                       PBblas.MakeR8Set(row_count, base.ma_terms, first_row, 1,
                                        PROJECT(cells, PBblas.Types.Layout_Cell),
                                        0, 0.0),
                        base.rlag_set);
    SELF := base;
  END;
  with_lag  := DENORMALIZE(base_ar, indy_cells, LEFT.model_id=RIGHT.model_id, GROUP,
                           rollCells(LEFT, ROWS(RIGHT), Target.lag_set),
                           NOSORT);
  with_dep  := DENORMALIZE(with_lag, dep_cells, LEFT.model_id=RIGHT.model_id, GROUP,
                           rollCells(LEFT, ROWS(RIGHT), Target.dep_set),
                           NOSORT);
  // Calculate AR parameters and residuals
  Init_Work runOLS(Init_Work mat) := TRANSFORM
    lag_cols := mat.ar_terms + IF(mat.constant_term, 1, 0);
    XtX := PBblas.BLAS.dgemm(TRUE,FALSE, lag_cols, lag_cols,
                             mat.obs_cnt-mat.ar_terms, 1.0,
                             mat.lag_set, mat.lag_set, 0.0);
    XtY := PBblas.BLAS.dgemm(TRUE,FALSE, lag_cols, 1,
                             mat.obs_cnt-mat.ar_terms, 1.0,
                             mat.lag_set, mat.dep_set, 0.0);
    L := PBblas.LAPACK.dpotf2(PBblas.Types.Triangle.Lower, lag_cols, XtX);
    s1:= PBblas.BLAS.dtrsm(PBblas.Types.Side.Ax, PBblas.Types.Triangle.Lower,
                           FALSE, PBblas.Types.Diagonal.NotUnitTri,
                           lag_cols, 1, lag_cols, 1.0, L, XtY);
    bt:= PBblas.BLAS.dtrsm(PBblas.Types.Side.Ax, PBblas.Types.Triangle.Lower,
                           TRUE, PBblas.Types.Diagonal.NotUnitTri,
                           lag_cols, 1, lag_cols, 1.0, L, s1);
    p0:= PBblas.BLAS.dgemm(FALSE, FALSE, mat.obs_cnt-mat.ar_terms, 1, lag_cols,
                           1.0, mat.lag_set, bt, 0.0);
    r0 := PBblas.BLAS.daxpy(mat.obs_cnt-mat.ar_terms, -1.0, p0, 1, mat.dep_set, 1);
    SELF.betas := IF(mat.ar_terms>0, bt, []);
    SELF.residual:= IF(mat.ar_terms>0, r0, mat.dep_set);
    SELF := mat;
  END;
  ar_matrix := PROJECT(with_dep, runOLS(LEFT));
  // Extract residuals
  TS.Types.DecoratedObs extractResidual(Init_Work base, UNSIGNED c) := TRANSFORM
    SELF.period := base.degree + c;
    SELF.dependent := base.residual[c];
    SELF.model_id := base.model_id;
    SELF.lags := base.ma_terms;
  END;
  resid0 := NORMALIZE(ar_matrix(ma_terms>0), LEFT.obs_cnt-LEFT.ar_terms,
                      extractResidual(LEFT, COUNTER));
  corr_tab := TS.LagCorrelations(resid0);
  Model_cell cvtCov2Cell(TS.Types.PACF_ACF corr) := TRANSFORM
    SELF.model_id := corr.model_id;
    SELF.lags := corr.lags;
    SELF.x := corr.lag;
    SELF.y := 1;
    SELF.v := corr.av;
  END;
  av_vals  := PROJECT(corr_tab, cvtCov2Cell(LEFT));
  y_array  := SORT(av_vals, model_id);
  Model_Cell makeDiag(Model_Cell cell) := TRANSFORM
    SELF.v := 1.0;
    SELF.x := cell.x;
    SELF.y := cell.x; // make diag, cell.y is x
    SELF := cell;
  END;
  x_diag := PROJECT(av_vals, makeDiag(LEFT));
  Model_Cell makeMatrix(Model_Cell cell, UNSIGNED2 c) := TRANSFORM
    diag := (c+1) DIV 2 + cell.x;
    thisRow := IF(c%2 <> 0, diag-cell.x, diag);
    thisCol := IF(c%2 <> 0, diag, diag-cell.x);
    SELF.x := thisRow;
    SELF.y := thisCol;
    SELF := cell;
  END;
  x_work := NORMALIZE(av_vals, 2*(LEFT.lags-LEFT.x), makeMatrix(LEFT, COUNTER));
  x_mat := SORT(x_diag+x_work, model_id);
  with_cov := DENORMALIZE(ar_matrix, y_array, LEFT.model_id=RIGHT.model_id, GROUP,
                          rollCells(LEFT, ROWS(RIGHT), Target.cov_set),
                          NOSORT);
  with_yw  := DENORMALIZE(with_cov, x_mat, LEFT.model_id=RIGHT.model_id, GROUP,
                          rollCells(LEFT, ROWS(RIGHT), Target.yw_set),
                          NOSORT);
  Init_Work solveYW(Init_Work iw) := TRANSFORM
    // Should use Durbin&apos;s algorithm to exploit persymetric yw matrix
    // Want to solve cov = yw * theta
    LU:= PBblas.Block.dgetf2(iw.ma_terms, iw.ma_terms, iw.yw_set);
    s1:= PBblas.BLAS.dtrsm(PBblas.Types.Side.Ax, PBblas.Types.Triangle.Lower,
                           FALSE, PBblas.Types.Diagonal.UnitTri,
                           iw.ma_terms, 1, iw.ma_terms, 1.0, LU, iw.cov_set);
    th:= PBblas.BLAS.dtrsm(PBblas.Types.Side.Ax, PBblas.Types.Triangle.Upper,
                           FALSE, PBblas.Types.Diagonal.NotUnitTri,
                           iw.ma_terms, 1, iw.ma_terms, 1.0, LU, s1);
    SELF.ma_coef := IF(iw.ma_terms>0, th, []);
    SELF := iw;
  END;
  init_sol := PROJECT(with_yw, solveYW(LEFT));
  // Refine parameters
  Model_Cell lagResidual(TS.Types.DecoratedObs do, UNSIGNED c) := TRANSFORM
    SELF.x := do.period - 1 + c;
    SELF.y := c;
    SELF.v := do.dependent;
    SELF.model_id := do.model_id;
    SELF.lags := do.lags;
  END;
  laggedResidual := NORMALIZE(resid0, LEFT.lags, lagResidual(LEFT, COUNTER));
  lrGrp := SORT(laggedResidual, model_id);
  with_lagr := DENORMALIZE(init_sol, lrGrp, LEFT.model_id=RIGHT.model_id, GROUP,
                          rollCells(LEFT, ROWS(RIGHT), Target.rlag_set),
                          NOSORT);
  Work_Val := RECORD
    value_t cv;
  END;
  Trim_dat := RECORD
    dim_t m_rows;
    dim_t m_cols;
    dim_t first_row;
    dim_t first_col;
    dim_t last_row;
    dim_t last_col;
  END;
  Work_Val scrnVals(Work_Val wv, Trim_dat td, UNSIGNED cnt) := TRANSFORM
    this_row := ((cnt-1)  %  td.m_rows) + 1;
    this_col := ((cnt-1) DIV td.m_rows) + 1;
    in_row := this_row BETWEEN td.first_row AND td.last_row;
    in_col := this_col BETWEEN td.first_col AND td.last_col;
    SELF.cv := IF(in_row AND in_col, wv.cv, SKIP);
  END;
  Irls_Work cvt2Irls(Init_Work iw) := TRANSFORM
    target_row_cnt := iw.obs_cnt - iw.ma_terms - iw.ar_terms;
    parm_cnt := iw.ar_terms + iw.ma_terms + IF(iw.constant_term, 1, 0);
    initBetas := iw.betas + iw.ma_coef;
    lag_trim := ROW({iw.obs_cnt-iw.ar_terms, iw.ar_terms + IF(iw.constant_term, 1, 0),
                     1 + iw.ma_terms, 1, iw.obs_cnt - iw.ar_terms,
                     iw.ar_terms + IF(iw.constant_term, 1, 0)}, Trim_dat);
    lag_inp := DATASET(iw.lag_set,Work_Val);
    lag_set := SET(PROJECT(lag_inp, scrnVals(LEFT,lag_trim, COUNTER)),cv);
    dep_trim:= ROW({iw.obs_cnt-iw.ar_terms, 1,
                     1 + iw.ma_terms, 1, iw.obs_cnt - iw.ar_terms, 1}, Trim_dat);
    dep_inp := DATASET(iw.dep_set,Work_Val);
    dep_set := SET(PROJECT(dep_inp, scrnVals(LEFT,dep_trim, COUNTER)), cv);
    SELF.row_cnt := target_row_cnt;
    SELF.delta := PBblas.BLAS.dasum(parm_cnt, initBetas, 1);
    SELF.full_x := lag_set + iw.rlag_set;;
    SELF.dep_set := dep_set;
    SELF.betas := initBetas;
    SELF.w := PBblas.Block.make_vector(target_row_cnt);   // initial weights are 1.0
    SELF.resid_mean := AVE(iw.residual);
    SELF := iw;
  END;
  init_irls := PROJECT(with_lagr, cvt2Irls(LEFT));
  // iteratively adjust parameters
  // Solve X' W X B = X' Y
  // Adjust W, W(i) = 1/MAX(0.00001,ABS(Y(i) - X(i)*B(i)))
  value_t newWi(value_t v, dim_t r, dim_t c) := 1.0/MAX(0.00001, ABS(v));
  Irls_Work reweight(Irls_Work iw) := TRANSFORM
    w_in := PBblas.Block.make_Diag(iw.row_cnt, 1.0, iw.w);
    columns := iw.ar_terms + iw.ma_terms
             + IF(iw.ar_terms>0 AND iw.constant_term, 1, 0);
    XtW  := PBblas.BLAS.dgemm(TRUE, FALSE, columns, iw.row_cnt, iw.row_cnt,
                              1.0, iw.full_x, w_in, 0.0);
    XtWY := PBblas.BLAS.dgemm(FALSE, FALSE, columns, 1, iw.row_cnt,
                              1.0, XtW, iw.dep_set, 0.0);
    XtWX := PBblas.BLAS.dgemm(FALSE, FALSE, columns, columns, iw.row_cnt,
                              1.0, XtW, iw.full_x, 0.0);
    LU_W := PBblas.Block.dgetf2(columns, columns, XtWX);
    s1_w := PBblas.BLAS.dtrsm(PBblas.Types.Side.Ax, PBblas.Types.Triangle.Lower,
                              FALSE, PBblas.Types.Diagonal.UnitTri,
                              columns, 1, columns, 1.0, LU_W, XtWY);
    NewB := PBblas.BLAS.dtrsm(PBblas.Types.Side.Ax, PBblas.Types.Triangle.Upper,
                              FALSE, PBblas.Types.Diagonal.NotUnitTri,
                              columns, 1, columns, 1.0, LU_W, s1_w);
    bdif := PBblas.BLAS.daxpy(columns, -1.0, NewB, 1, iw.betas, 1);
    estm := PBblas.BLAS.dgemm(FALSE, FALSE, iw.row_cnt, 1, columns,
                              1.0, iw.full_x, NewB, 0.0);
    rerr := PBblas.BLAS.daxpy(iw.row_cnt, -1.0, estm, 1, iw.dep_set, 1);
    rsum := PBblas.BLAS.dasum(iw.row_cnt, rerr, 1);
    wraw := PBblas.Block.Apply2Cells(iw.row_cnt, 1, rerr, newWi);
    SELF.w := PBblas.BLAS.dscal(iw.row_cnt, rsum, wraw, 1);
    SELF.delta := PBblas.BLAS.dasum(columns, bdif, 1);
    SELF.betas := NewB;
    SELF := iw;
  END;
  Irls_Body(DATASET(Irls_Work) ds) := PROJECT(ds, reweight(LEFT));
  final_irls := LOOP(init_irls, maxIter, LEFT.delta>epsilon, Irls_Body(ROWS(LEFT)));
  // Convert to final form
  TS.Types.Co_efficient cvt2Coef({REAL8 cv} coeff, UNSIGNED2 c) := TRANSFORM
    SELF.cv := coeff.cv;
    SELF.lag := c;
  END;
  TS.Types.Model_Parameters cvt2Parms(Irls_Work iw) := TRANSFORM
    ar_cnt := iw.ar_terms + IF(iw.ar_terms>0 AND iw.constant_term, 1, 0);
    first_ar := IF(iw.constant_term, 2, 1);
    ar_const:= IF(iw.ar_terms>0, iw.betas[1], 0.0);
    ar_coef:= CHOOSEN(DATASET(iw.betas, {REAL8 cv}), iw.ar_terms, first_ar);
    ma_coef:= CHOOSEN(DATASET(iw.betas, {REAL8 cv}), iw.ma_terms, 1 + ar_cnt);
    SELF.c := IF(iw.constant_term, iw.resid_mean + ar_const, 0.0);
    SELF.ar:= IF(iw.ar_terms>0,PROJECT(ar_coef, cvt2Coef(LEFT,COUNTER)));
    SELF.ma:= IF(iw.ma_terms>0,PROJECT(ma_coef, cvt2Coef(LEFT,COUNTER)));
    SELF := iw;
  END;
  rslt := PROJECT(final_irls, cvt2Parms(LEFT));
  RETURN rslt;
END;
