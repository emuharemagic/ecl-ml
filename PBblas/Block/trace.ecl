// The trace of a square matrix
IMPORT PBblas;
matrix_t := PBblas.Types.matrix_t;
dimension_t := PBblas.Types.dimension_t;
value_t := PBblas.Types.value_t;
diag := PBblas.Block.extract_diag;

EXPORT value_t trace(dimension_t m, matrix_t x) := SUM(diag(m,m,x));
