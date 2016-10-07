IMPORT $ AS DenseSVD;
IMPORT Config FROM ML;
IMPORT ML;
IMPORT ML.DMat AS DMat;
IMPORT PBblas;
IMPORT PBblas.Types AS Types;
IMPORT PBblas.MU AS MU;

Part := Types.Layout_Part;
IMatrix_Map := PBblas.IMatrix_Map;
value_t := PBblas.Types.value_t;
dimension_t := PBblas.Types.dimension_t;
matrix_t := PBblas.Types.matrix_t;

EXPORT eigC(IMatrix_Map a_map, DATASET(Part) A, UNSIGNED4 iter=200) := MODULE
  SHARED eig_comp := ENUM ( T = 1, Q = 2, T0 = 3 );
  
  SHARED Part calc(Part rec) := TRANSFORM
    SELF.mat_part := DenseSVD.eigen_dsyev(rec.part_rows, rec.mat_part);
    SELF := rec;
  END;
  
  SHARED Part getValues(Part rec) := TRANSFORM
    SELF.mat_part := PBblas.Block.make_diag(rec.part_rows, 1.0, rec.mat_part);
    SELF := rec;
  END;
  
  SHARED Part getVectors(Part rec) := TRANSFORM
    SELF.mat_part := rec.mat_part[(rec.part_rows+1)..];
    SELF := rec;
  END;
  
  SHARED eig_calc := PROJECT(A, calc(LEFT));
  EXPORT valuesM := PROJECT(eig_calc, getValues(LEFT));
  EXPORT vectors := PROJECT(eig_calc, getVectors(LEFT));

END;