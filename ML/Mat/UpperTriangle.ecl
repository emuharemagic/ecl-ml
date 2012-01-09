IMPORT * FROM $;
// the lower triangular portion of the matrix
EXPORT UpperTriangle(DATASET(Types.Element) matrix) := matrix(x<=y);
