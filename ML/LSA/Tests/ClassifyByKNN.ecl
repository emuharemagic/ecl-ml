IMPORT ML;
IMPORT ML.Mat AS Mat;
IMPORT ML.DMat AS DMat;
IMPORT PBblas;
IMPORT ML.Classify AS Classify;
IMPORT ML.SVM AS SVM;

a := DATASET('~lsa::bbc_train.mtx', Mat.Types.Element, CSV);
a_rows := MAX(a, x);
a_cols := MAX(a, y);
block_rows := a_rows DIV 10;
block_cols := a_cols DIV 10;
a_map := PBblas.Matrix_Map(a_rows, a_cols, block_rows, block_cols); 
Da := DMat.Converted.FromElement(a, a_map);
decomp := ML.LSA.RandomisedSVD.RandomisedSVD(a_map, Da, 100);

V := Mat.MU.From(decomp, 3);

labels := DATASET('~lsa::bbc_train.classes', {UNSIGNED4 value}, CSV);
L := PROJECT(labels, TRANSFORM(ML.Types.DiscreteField, SELF.id := COUNTER; SELF.number := 1; SELF.value := LEFT.value));

test := DATASET('~lsa::bbc_test.mtx', Mat.Types.Element, CSV);
test_rows := a_rows;
test_cols := MAX(test, y);
test_map := PBblas.Matrix_Map(test_rows, test_cols, block_rows, test_cols); 
Dtest := DMat.Converted.FromElement(test, test_map);
test_V := ML.LSA.lsa.ComputeQueryVectors(decomp, test_map, Dtest);

test_labels := DATASET('~lsa::bbc_test.classes', {UNSIGNED4 value}, CSV);
test_L := PROJECT(test_labels, TRANSFORM(ML.Types.DiscreteField, SELF.id := COUNTER; SELF.number := 1; SELF.value := LEFT.value));

iknn:= ML.LSA.Tests.KNN_DotProd(20);

computed:=  iknn.ClassifyC(ML.Types.FromMatrix(V), L, ML.Types.FromMatrix(test_V));
Comparison:=  ML.Classify.Compare(test_L, computed);
OUTPUT(computed, ALL, NAMED('Predictions'));
OUTPUT(Comparison.Accuracy, ALL, NAMED('Accuracy'));

/* Accuracy : 0.88 */