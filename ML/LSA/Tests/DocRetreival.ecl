IMPORT ML;
IMPORT ML.Mat AS Mat;
IMPORT ML.DMat AS DMat;
IMPORT PBblas;

a := DATASET('~lsa::bbc_train.mtx', Mat.Types.Element, CSV);
a_rows := MAX(a, x);
a_cols := MAX(a, y);
a_map := PBblas.Matrix_Map(a_rows, a_cols, MIN(PBblas.Constants.Block_Vec_Rows, a_rows), a_cols);
Da := DMat.Converted.FromElement(a, a_map);
decomp := ML.LSA.lsa.StandardSVD(a_map, Da, 50);

V := Mat.MU.From(decomp, 3);

labels := DATASET('~lsa::bbc_train.classes', {UNSIGNED4 value}, CSV);
L := PROJECT(labels, TRANSFORM(Mat.Types.Element, SELF.x := COUNTER; SELF.y := 1; SELF.value := LEFT.value + 1));

test := DATASET('~lsa::bbc_test.mtx', Mat.Types.Element, CSV);
test_rows := a_rows;
test_cols := MAX(test, y);
test_map := PBblas.Matrix_Map(test_rows, test_cols, MIN(PBblas.Constants.Block_Vec_Rows, test_rows), test_cols);
Dtest := DMat.Converted.FromElement(test, test_map);
test_V := ML.LSA.lsa.ComputeQueryVectors(decomp, test_map, Dtest);

test_labels := DATASET('~lsa::bbc_test.classes', {UNSIGNED4 value}, CSV);
test_L := PROJECT(test_labels, TRANSFORM(Mat.Types.Element, SELF.x := COUNTER; SELF.y := 1; SELF.value := LEFT.value + 1));

predict := ML.LSA.Similarity.DotProductSim(V, test_V);
predict4 := TOPN(GROUP(SORT(predict, y), y), 5, -value);
OUTPUT(predict4, ALL, NAMED('Top5Docs'));

