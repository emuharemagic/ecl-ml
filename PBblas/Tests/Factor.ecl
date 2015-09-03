// Test for the factorization definitions.  Cholesky and LU.
IMPORT PBblas;
IMPORT PBblas.Tests;
IMPORT PBblas.Types;
IMPORT ML.DMAT;
Layout_Cell := Types.Layout_Cell;
matrix_t := Types.matrix_t;
Triangle := Types.Triangle;
Upper  := Triangle.Upper;
Lower  := Triangle.Lower;
Diagonal := Types.Diagonal;
UnitTri := Types.Diagonal.UnitTri;
NotUnitTri := Types.Diagonal.NotUnitTri;

// Generate test data
W_Rec := RECORD
  STRING1 x:= '';
END;
W0 := DATASET([{' '}], W_Rec);
Layout_Cell gen(UNSIGNED4 c, UNSIGNED4 NumRows, REAL8 v):=TRANSFORM
  SELF.x := ((c-1)  %  NumRows) + 1;
  SELF.y := ((c-1) DIV NumRows) + 1;
  SELF.v := v;
END;
// Pick some random cells.  Usually will be a full rank matrix.
b_cells := NORMALIZE(W0, 64, gen(COUNTER, 8, RANDOM()/(RANDOM()+1)));
map_8x8_1  := PBblas.Matrix_Map(8, 8, 8, 8);
map_8x8_9  := PBblas.Matrix_Map(8, 8, 3, 3);
b1 := DMAT.Converted.FromCells(map_8x8_1, b_cells);
// Make a symteric positive definite matrix for Cholesky
a1 := PBblas.PB_dgemm(TRUE, FALSE, 1.0, map_8x8_1, b1, map_8x8_1, b1, map_8x8_1);
a_cells := DMAT.Converted.FromPart2Cell(a1);
a9 := DMAT.Converted.FromCells(map_8x8_9, a_cells);
b9 := DMAT.Converted.FromCells(map_8x8_9, b_cells);

C_U1 := PBblas.PB_dpotrf(Upper, map_8x8_1, a1);
C_U9 := PBblas.PB_dpotrf(Upper, map_8x8_9, a9);
C_L1 := PBblas.PB_dpotrf(Lower, map_8x8_1, a1);
C_L9 := PBblas.PB_dpotrf(Lower, map_8x8_9, a9);

// Single partition Choleky
C_U1tU1 := PBblas.PB_dgemm(TRUE, FALSE,
                           1.0, map_8x8_1, C_U1, map_8x8_1, C_U1,
                           map_8x8_1);
test_c1 := Tests.DiffReport.Compare_Parts('Cholesky U1tU1', a1, C_U1tU1);

C_L1U1  := PBblas.PB_dgemm(FALSE, FALSE,
                           1.0, map_8x8_1, C_L1, map_8x8_1, C_U1,
                           map_8x8_1);
test_c2 := Tests.DiffReport.Compare_Parts('Cholesky L1U1', a1, C_L1U1);

C_L1L1t := PBblas.PB_dgemm(FALSE, TRUE,
                           1.0, map_8x8_1, C_L1, map_8x8_1, C_L1,
                           map_8x8_1);
test_c3 := Tests.DiffReport.Compare_Parts('Cholesky L1L1t', a1, C_l1L1t);

// Multiple partition Cholesky
C_U9tU9 := PBblas.PB_dgemm(TRUE, FALSE,
                           1.0, map_8x8_9, C_U9, map_8x8_9, C_U9,
                           map_8x8_9);
test_c4 := Tests.DiffReport.Compare_Parts('Cholesky U9tU9', a1, C_U9tU9);

C_L9U9  := PBblas.PB_dgemm(FALSE, FALSE,
                           1.0, map_8x8_9, C_L9, map_8x8_9, C_U9,
                           map_8x8_9);
test_c5 := Tests.DiffReport.Compare_Parts('Cholesky L9U9', a1, C_L9U9);

C_L9L9t := PBblas.PB_dgemm(FALSE, TRUE,
                           1.0, map_8x8_9, C_L9, map_8x8_9, C_L9,
                           map_8x8_9);
test_c6 := Tests.DiffReport.Compare_Parts('Cholesky L9L9t', a1, C_L9L9t);


// Single partition LU
T_1  := PBblas.PB_dgetrf(map_8x8_1, b1);
T_U1 := PBblas.PB_Extract_Tri(Upper, NotUnitTri, map_8x8_1, T_1);
T_L1 := PBblas.PB_Extract_Tri(Lower, UnitTri, map_8x8_1, T_1);
T_L1U1 := PBblas.PB_dgemm(FALSE, FALSE,
                          1.0, map_8x8_1, T_L1, map_8x8_1, T_U1,
                          map_8x8_1);
test_t1 := Tests.DiffReport.Compare_Parts('LU L1U1', b1, T_L1U1);

// Multiple partition LU
T_9  := PBblas.PB_dgetrf(map_8x8_9, b9);
T_U9 := PBblas.PB_Extract_Tri(Upper, NotUnitTri, map_8x8_9, T_9);
T_L9 := PBblas.PB_Extract_Tri(Lower, UnitTri, map_8x8_9, T_9);
T_L9U9 := PBblas.PB_dgemm(FALSE, FALSE,
                          1.0, map_8x8_9, T_L9, map_8x8_9, T_U9,
                          map_8x8_9);
test_t2 := Tests.DiffReport.Compare_Parts('LU L9U9', b1, T_L9U9);

// report results
rslt := test_c1 + test_c2 + test_c3 + test_c4 + test_c5 + test_c6
      + test_t1 + test_t2;
EXPORT Factor := rslt;
