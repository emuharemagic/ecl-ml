// Absolute sum, the 1-norm
IMPORT PBblas;
IMPORT PBblas.IMatrix_Map;
IMPORT PBblas.Types;
IMPORT PBblas.Constants;
IMPORT PBblas.BLAS;
Part := Types.Layout_Part;
value_t := Types.value_t;
matrix_t := Types.matrix_t;

EXPORT value_t PB_dasum(PBblas.IMatrix_Map x_map, DATASET(Part) X) := FUNCTION
  Work := RECORD
    value_t part_asum;
  END;
  Work asum(Part lr) := TRANSFORM
    SELF.part_asum := BLAS.dasum(lr.part_rows, lr.mat_part, 1);
  END;
  w0 := PROJECT(x, asum(LEFT));
  RETURN SUM(w0, part_asum);
END;
