//iterate matrix and apply function to each cell
IMPORT PBblas;
matrix_t := PBblas.Types.matrix_t;
dim_t := PBblas.Types.dimension_t;
value_t := PBblas.Types.value_t;
func_t:= PBblas.Block.ICellFunc;

EXPORT matrix_t Apply2Cells(dim_t m, dim_t n, matrix_t x, func_t f) := FUNCTION
  Cell := {value_t v};
  Cell applyFunc(Cell v, UNSIGNED pos) := TRANSFORM
    r := ((pos-1)  %  m) + 1;
    c := ((pos-1) DIV m) + 1;
    SELF.v := f(v.v, r, c);
  END;
  dIn := DATASET(x, Cell);
  dOut := PROJECT(dIn, applyFunc(LEFT, COUNTER));
  RETURN SET(dOut, v);
END;
