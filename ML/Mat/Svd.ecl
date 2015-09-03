IMPORT * FROM $;

/*
	http://en.wikipedia.org/wiki/Singular_value_decomposition
  Singular value decomposition (SVD) is a factorization of a real or complex matrix, with many 
	useful applications in signal processing and statistics.

	Formally, the singular value decomposition of an m×n real or complex matrix 
	A is a factorization of the form A=USV' where U is an m×m real or complex 
	unitary matrix, S is an m×n rectangular diagonal matrix with nonnegative 
	real numbers on the diagonal, and V' (the conjugate transpose of V) is 
	an n×n real or complex unitary matrix. The diagonal entries Si,i of S 
	are known as the singular values of A. The m columns of U and the n 
	columns of V are called the left singular vectors and right singular 
	vectors of A, respectively. The singular value decomposition and the 
	eigendecomposition are closely related. Namely:
			The left singular vectors of A are eigenvectors of AA'.
			The right singular vectors of A are eigenvectors of A'A.
			The non-zero singular values of A (found on the diagonal entries of S) 
				are the square roots of the non-zero eigenvalues of A'A or AA'.

*/
EXPORT Svd(DATASET(Types.Element) A) := MODULE

		SHARED AAt := Mul(A,Trans(A),2);
		/*
		SHARED Lanczos_AAt := Lanczos(AAt, Has(AAt).Dimension);
		SHARED AAt_T := Lanczos_AAt.TComp;
		AAt_V := Lanczos_AAt.VComp;
		eigAAt_T_vec := Eig(AAt_T).vectors;
		eigAAt_vec := Mul(AAt_V, eigAAt_T_vec);		
		EXPORT UComp := eigAAt_vec;		
		
		eigAAt_T_val := Eig(AAt_T).valuesM;		
		EXPORT S2Comp := eigAAt_T_val;
    */
		EXPORT UComp := Eig(AAt).vectors;		
		EXPORT S2Comp := Eig(AAt).valuesM;
		EXPORT SComp := Each.SQRT(S2Comp);
		
		SHARED AtA := Mul(Trans(A),A,2);
		/*
		SHARED Lanczos_AtA := Lanczos(AtA, Has(AtA).Dimension);
		SHARED AtA_T := Lanczos_AtA.TComp;
		AtA_V := Lanczos_AtA.VComp;
		eigAtA_T_vec:=Eig(AtA_T).vectors;		
		eigAtA_vec := Mul(AtA_V, eigAtA_T_vec);	
		EXPORT VComp := eigAtA_vec;

		eigAtA_T_val:=Eig(AtA_T).valuesM;
		EXPORT SComp_Test := Each.SQRT(eigAtA_T_val);
    */
		EXPORT VComp := Eig(AtA).vectors;
		EXPORT SComp_Test := Each.SQRT(Eig(AtA).valuesM);
END;