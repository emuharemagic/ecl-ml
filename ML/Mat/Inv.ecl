IMPORT * FROM $;
 
Inverse(DATASET(Types.Element) matrix) := FUNCTION
	dim := Has(matrix).Dimension;
	mLU := Decomp.LU(matrix);
	L := Decomp.LComp(mLU);
	U := Decomp.UComp(mLU);
	mI := Identity(dim);
	fsub := Decomp.f_sub(L, mI);
	matrix_inverse := Decomp.b_sub(u, fsub);
	RETURN matrix_inverse;
END;
EXPORT Inv(DATASET(Types.Element) matrix) := IF(Det(matrix)=0, DATASET([],Types.Element), Inverse(matrix) );
