IMPORT PBblas.Types;
dimension_t := Types.dimension_t;
value_t     := Types.value_t;
matrix_t    := Types.matrix_t;

EXPORT matrix_t lapack_dsyev(dimension_t N, matrix_t A) := BEGINC++
extern "C" int dsyev_(char *jobz, char *uplo, int *n, double *a, 
	 int *lda, double *w, double *work, int *lwork, 
	int *info);
#option library lapack
#body
  int lda = n;
  int new_n = n;
  __isAllResult = false;
  __lenResult = (n + 1) * n * sizeof(double);
  __result = rtlMalloc(__lenResult);
  double* w = (double*) __result;
  double* new_a = (double*) (w + n);
  memcpy(new_a, a, n * n * sizeof(double));
  int info = 0;
  int lwork = 10000;
  double* work = new double[lwork];
  dsyev_("V", "L", &new_n, new_a, &lda, w, work, &lwork, &info);
  for(int i = 0, j = n-1; i < int(n/2); i++, j--) {
      int temp = w[i];
      w[i] = w[j];
      w[j] = temp;
  }
  for( int i = 0, j = (n-1)*n; i < int(n/2) * n; i += n, j -= n ) {
     for(int k = 0; k < n; k++) {
        int temp = new_a[i+k];
        new_a[i+k] = new_a[j+k];
        new_a[j+k] = temp;
     }
  }
  delete[] work;
ENDC++;
