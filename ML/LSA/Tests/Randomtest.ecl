IMPORT ML;
IMPORT ML.Mat AS Mat;
IMPORT ML.Docs AS Docs;
IMPORT ML.LSA as LSA;
IMPORT PBblas;
IMPORT ML.DMat AS DMat;

d := DATASET([{'Shipment of Gold damaged in a fire'},
 {'Delivery of silver arrived in a silver truck'},
 {'Shipment of Gold arrived in a truck'}],{string r});
 
//Pre processing
d1 := PROJECT(d,TRANSFORM(Docs.Types.Raw,SELF.Txt := LEFT.r));
d2 := Docs.Tokenize.Enumerate(d1);
d3 := Docs.Tokenize.Clean(d2);
d4 := Docs.Tokenize.Split(d3);
lex := Docs.Tokenize.Lexicon(d4);
w1 := Docs.Tokenize.ToO(d4,lex);
w2 := Docs.Trans(w1).WordBag;

//Convert to Term-Document Matrix
doc_mat := PROJECT(w2, TRANSFORM(Mat.Types.Element, SELF.x := LEFT.word; SELF.y := LEFT.id; SELF.value := LEFT.words_in_doc));
a_rows := MAX(doc_mat, x);
a_cols := MAX(doc_mat, y);
a_map := PBblas.Matrix_Map(a_rows, a_cols, MIN(PBblas.Constants.Block_Vec_Rows, a_rows), a_cols);
Da := DMat.Converted.FromElement(doc_mat, a_map);
decomp := ML.LSA.RandomisedSVD.RandomisedSVD(a_map, Da, 2);
decomp;

//Testing Query
q := DATASET([{'gold silver truck'}], {string r});
q1 := PROJECT(q,TRANSFORM(Docs.Types.Raw,SELF.Txt := LEFT.r));
q2 := Docs.Tokenize.Enumerate(q1);
q3 := Docs.Tokenize.Clean(q2);
q4 := Docs.Tokenize.Split(q3);
qw := Docs.Trans(Docs.Tokenize.ToO(q4, lex)).WordBag;
//Initial Query Vector 
q_mat := PROJECT(qw, TRANSFORM(Mat.Types.Element, SELF.x := LEFT.word; SELF.y := LEFT.id; SELF.value := LEFT.words_in_doc));
q_rows := a_rows;
q_cols := MAX(q_mat, y);
q_map := PBblas.Matrix_Map(q_rows, q_cols, MIN(PBblas.Constants.Block_Vec_Rows, q_rows), q_cols);
Dq := DMat.Converted.FromElement(q_mat, q_map);

//Reduced Query Vector
qvec := LSA.lsa.ComputeQueryVectors(decomp, q_map, Dq);
OUTPUT(qvec, NAMED('QueryReducedVector'));

//Calc Similarity
sim := LSA.Similarity.CosineSim(LSA.lsa.GetDocVectors(decomp), qvec);
OUTPUT(sim, NAMED('Similarities'));

