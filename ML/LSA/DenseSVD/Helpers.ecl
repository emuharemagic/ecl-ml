IMPORT PBblas;
IMPORT PBblas.Types AS Types;
IMPORT PBblas.MU AS MU;
IMPORT PBblas.Block;

Part := Types.Layout_Part;
IMatrix_Map := PBblas.IMatrix_Map;
matrix_t := PBblas.Types.matrix_t;
dimension_t := PBblas.Types.dimension_t;
value_t := PBblas.Types.value_t;

EXPORT Helpers := MODULE
  SHARED value_t onenorm(dimension_t m, dimension_t n, matrix_t x) := FUNCTION
    cell := {value_t v};
    cell ext(cell v, UNSIGNED pos) := TRANSFORM
      r := ((pos-1) % m) + 1;
      c := ((pos-1) DIV m) + 1;
      SELF.v := IF(r=c AND r<=m AND c<=n, ABS(v.v), SKIP);
    END;
    diag := SUM(PROJECT(DATASET(x, cell), ext(LEFT, COUNTER)), v);
    RETURN diag;
  END;
  
  EXPORT NormDiag(IMatrix_map a_map, DATASET(Part) A) := FUNCTION
    A0 := A(block_row = block_col);
    v := PROJECT(A0, TRANSFORM({value_t v}, SELF.v := onenorm(LEFT.part_rows, LEFT.part_cols, LEFT.mat_part)));
    RETURN SUM(v, v);
  END;

END;