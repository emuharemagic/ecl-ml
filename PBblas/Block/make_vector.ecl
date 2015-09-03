//Make a vector
IMPORT PBblas;
matrix_t := PBblas.Types.matrix_t;
dimension_t := PBblas.Types.dimension_t;
value_t := PBblas.Types.value_t;

Cell := RECORD
  value_t v;
END;
Cell makeCell(value_t v) := TRANSFORM
  SELF.v := v;
END;
vec_dataset(dimension_t m, value_t v) := DATASET(m, makeCell(v));

EXPORT matrix_t make_vector(dimension_t m, value_t v=1.0) := SET(vec_dataset(m, v), v);
