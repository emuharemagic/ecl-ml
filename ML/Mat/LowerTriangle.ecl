IMPORT * FROM $;
// the lower triangular portion of the matrix
EXPORT LowerTriangle(DATASET(Types.Element) matrix) := matrix(x>=y);
