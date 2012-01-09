IMPORT * FROM $;
// This module exists to handle a special sub-case of a matrix; the Vector
// The vector is really just a matrix with only one dimension
EXPORT Vec := MODULE

// Create a vector from 'thin air' - with N entries each of value def
EXPORT From(Types.t_Index N,Types.t_value def = 1.0) := FUNCTION
	seed := DATASET([{0,0}], Types.VecElement);
  PerCluster := ROUNDUP(N/CLUSTERSIZE);
	Types.VecElement addNodeNum(Types.VecElement L, UNSIGNED4 c) := transform
    SELF.i := (c-1)*PerCluster;
		SELF.value := DEF;
  END;
	// Create eventual vector across all nodes (in case it is huge)
	one_per_node := DISTRIBUTE(NORMALIZE(seed, CLUSTERSIZE, addNodeNum(LEFT, COUNTER)), i DIV PerCluster);

	Types.VecElement fillRow(Types.VecElement L, UNSIGNED4 c) := TRANSFORM
		SELF.i := l.i+c;
		SELF.value := def;
	END;
	// Now generate on each node; filter 'clips' of the possible extra '1' generated on some nodes
	m := NORMALIZE(one_per_node, PerCluster, fillRow(LEFT,COUNTER))(i <= N);
	RETURN m;

END;

// The 'To' routines - used to create a matrix from a vector
// Fill in the leading diagonal of a matrix
EXPORT ToDiag(DATASET(Types.VecElement) v) := PROJECT( v, TRANSFORM(Types.Element,SELF.x:=LEFT.i,SELF.y:=LEFT.i,SELF := LEFT));

// Fill in a column
EXPORT ToCol(DATASET(Types.VecElement) v,Types.t_index N) := PROJECT( v, TRANSFORM(Types.Element,SELF.x:=LEFT.i,SELF.y:=N,SELF := LEFT));

// Fill in a row
EXPORT ToRow(DATASET(Types.VecElement) v,Types.t_index N) := PROJECT( v, TRANSFORM(Types.Element,SELF.y:=LEFT.i,SELF.x:=N,SELF := LEFT));

// The 'Rep' routines - used to replace part of a matrix with a vector

EXPORT RepDiag(DATASET(Types.Element) M, DATASET(Types.VecElement) v) := M(X<>Y)+PROJECT( v, TRANSFORM(Types.Element,SELF.x:=LEFT.i,SELF.y:=LEFT.i,SELF := LEFT));

EXPORT RepCol(DATASET(Types.Element) M,DATASET(Types.VecElement) v,Types.t_index N) := M(Y<>N)+PROJECT( v, TRANSFORM(Types.Element,SELF.x:=LEFT.i,SELF.y:=N,SELF := LEFT));

EXPORT RepRow(DATASET(Types.Element) M,DATASET(Types.VecElement) v,Types.t_index N) := M(X<>N)+PROJECT( v, TRANSFORM(Types.Element,SELF.y:=LEFT.i,SELF.x:=N,SELF := LEFT));

// The 'From' routines - extract a vector from part of a matrix
EXPORT FromDiag(DATASET(Types.Element) M) := PROJECT( M(x=y), TRANSFORM(Types.VecElement,SELF.i:=LEFT.x,SELF := LEFT));

EXPORT FromCol(DATASET(Types.Element) M,Types.t_index N) := PROJECT( M(Y=N), TRANSFORM(Types.VecElement,SELF.i:=LEFT.x,SELF := LEFT));

EXPORT FromRow(DATASET(Types.Element) M,Types.t_index N) := PROJECT( M(X=N), TRANSFORM(Types.VecElement,SELF.i:=LEFT.y,SELF := LEFT));

// Vector math
// Compute the dot product of two vectors
EXPORT Dot(DATASET(Types.VecElement) X,DATASET(Types.VecElement) Y) := FUNCTION
  J := JOIN(x,y,LEFT.i=RIGHT.i,TRANSFORM(Types.VecElement,SELF.i := LEFT.i, SELF.value := LEFT.value*RIGHT.value));
	RETURN SUM(J,value);
END; 

END;