//Function prototype for a function to apply to each element of the distributed matrix
IMPORT PBblas;
value_t := PBblas.Types.value_t;
dimension_t := PBblas.Types.dimension_t;
partition_t := PBblas.Types.partition_t;

EXPORT value_t IElementFunc(value_t v, dimension_t r, dimension_t c) := v;
