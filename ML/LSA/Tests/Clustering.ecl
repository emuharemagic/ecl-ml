IMPORT ML;
IMPORT ML.Mat AS Mat;
IMPORT ML.DMat AS DMat;
IMPORT PBblas;

a := DATASET('~lsa::bbc_train.mtx', Mat.Types.Element, CSV);
a_rows := MAX(a, x);
a_cols := MAX(a, y);
block_rows := a_rows DIV 10;
block_cols := a_cols DIV 10;
a_map := PBblas.Matrix_Map(a_rows, a_cols, block_rows, block_cols); 
Da := DMat.Converted.FromElement(a, a_map);
decomp := ML.LSA.RandomisedSVD.RandomisedSVD(a_map, Da, 100);

V1 := Mat.MU.From(decomp, 3);
V2 := Mat.Sub(V1, Mat.Repmat(Mat.Has(V1).MeanCol, Mat.Has(V1).Stats.XMax, 1));
V3 := Mat.Each.Mul(V2, Mat.Repmat(Mat.Each.Reciprocal(Mat.Has(V1).SDCol), Mat.Has(V1).Stats.XMax, 1));
V := ML.Types.FromMatrix(V3);

labels := DATASET('~lsa::bbc_train.classes', {UNSIGNED4 value}, CSV);
L := PROJECT(labels, TRANSFORM(ML.Types.DiscreteField, SELF.id := COUNTER; SELF.number := 1; SELF.value := LEFT.value + 1));

test := DATASET('~lsa::bbc_test.mtx', Mat.Types.Element, CSV);
test_rows := a_rows;
test_cols := MAX(test, y);
test_map := PBblas.Matrix_Map(test_rows, test_cols, block_rows, test_cols); 
Dtest := DMat.Converted.FromElement(test, test_map);
test_Q := ML.LSA.lsa.ComputeQueryVectors(decomp, test_map, Dtest);

test_V2 := Mat.Sub(test_Q, Mat.Repmat(Mat.Has(test_Q).MeanCol, Mat.Has(test_Q).Stats.XMax, 1));
test_V3 := Mat.Each.Mul(test_V2, Mat.Repmat(Mat.Each.Reciprocal(Mat.Has(test_Q).SDCol), Mat.Has(test_Q).Stats.XMax, 1));
test_V := ML.Types.FromMatrix(test_V3);

test_labels := DATASET('~lsa::bbc_test.classes', {UNSIGNED4 value}, CSV);
test_L := PROJECT(test_labels, TRANSFORM(ML.Types.DiscreteField, SELF.id := COUNTER; SELF.number := 1; SELF.value := LEFT.value + 1));

CL := ML.Cluster.AggloN(V,10,ML.Cluster.DF.Cosine);  
CL.Dendrogram;
CL.Clusters;
CL.Distances

