IMPORT PBblas.Types;
dimension_t := Types.dimension_t;
value_t     := Types.value_t;
matrix_t    := Types.matrix_t;

EXPORT matrix_t eigen_dsyev(dimension_t N, matrix_t A) := BEGINC++
#include <eigen3/Eigen/Dense>
#include <eigen3/Eigen/Eigenvalues>
using namespace Eigen;
#body
  __isAllResult = false;
  __lenResult = (n + 1) * n * sizeof(double);
  __result = rtlMalloc(__lenResult);
  double* w = (double*) __result;
  double* new_a = (double*) (w + n);
  MatrixXd work(n, n);
  for(int j = 0; j < n; j++){
    for(int i = 0; i < n; i++) {
        work(i, j) = ((double*) a)[j * n + i];
     }
   }
   SelfAdjointEigenSolver<MatrixXd> eigsolve(work);
   for(int i = (n-1); i >= 0; i--) {
      w[i] = eigsolve.eigenvalues().data()[(n-1)-i];
   }
   for(int i = (n-1); i >= 0; i--) {  // col
      for(int j = 0; j < n; j++) {  // row
        new_a[i*n+j] = eigsolve.eigenvectors().data()[((n-1)-i) * n + j];
      }
   }
   work.resize(0,0);   
ENDC++;
