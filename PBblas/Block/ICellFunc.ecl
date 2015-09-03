//Function prototype for a function to apply to each cell
IMPORT PBblas;
value_t := PBblas.Types.value_t;
dimension_t := PBblas.Types.dimension_t;

EXPORT value_t ICellFunc(value_t v, dimension_t r, dimension_t c) := v;
