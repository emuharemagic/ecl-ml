// Various tests to see if a matrix is a particular type
IMPORT * FROM $;
EXPORT Is(DATASET(Types.Element) d) := MODULE

SHARED dn := Thin(d);

// Matrix with only zeroes
EXPORT Zero := ~EXISTS(dn);

// Matrix only has entries along diagonal - note - zero matrix is diagonal by this definition
EXPORT Diagonal := ~EXISTS(dn(x<>y));

// Scalar definition - all leading entries must exist - and must be equal
EXPORT Scalar := Diagonal AND COUNT(dn)=MAX(dn,x) AND MAX(dn,value)=MIN(dn,value);

// Matrix is an identity matrix
EXPORT Identity := Scalar AND MAX(dn,value)=1;

// Is the matrix symmetric
EXPORT Symmetric := Eq(dn,Trans(dn));

// Upper triangular if no elements exist in the bottom, left corner
EXPORT UpperTriangular := ~EXISTS(dn(x>y));

// Lower triangular if no elements exist in the top right
EXPORT LowerTriangular := ~EXISTS(dn(x<y));

EXPORT Triangular := UpperTriangular OR LowerTriangular;

  END;