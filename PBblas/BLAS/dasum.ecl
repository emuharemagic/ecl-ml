// Absolute sum.  the 1-norm of a vector
IMPORT PBblas;
value_t := PBblas.Types.value_t;
dimension_t := PBblas.Types.dimension_t;
matrix_t := PBblas.Types.matrix_t;

EXPORT value_t dasum(dimension_t m, matrix_t x, dimension_t incx) := BEGINC++
extern "C" {
#include <cblas.h>
}
#option library cblas
#body
  const double* X = (const double*)x;
  double rslt = cblas_dasum(m, X, incx);
ENDC++;