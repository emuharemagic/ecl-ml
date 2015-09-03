// Logistic Regression EXAMPLE
//
// Demonstrates logistic regression on large, spare dataset.
//
// Dataset is randomly generated.
//
// For this example, the training set is tested against itself for accuracy.
//
// Currently set to 5% density! --> Line 16
//
//---------------------------------------------------------------------------

IMPORT ML;

TestSize := 100000;
density := .05;
a1 := ML.Distribution.Uniform(0,1,10000);

col01 := ML.Distribution.GenData(TestSize,a1,1);
col02 := ML.Distribution.GenData(TestSize,a1,2);
col03 := ML.Distribution.GenData(TestSize,a1,3);
col04 := ML.Distribution.GenData(TestSize,a1,4);
col05 := ML.Distribution.GenData(TestSize,a1,5);
col06 := ML.Distribution.GenData(TestSize,a1,6);
col07 := ML.Distribution.GenData(TestSize,a1,7);
col08 := ML.Distribution.GenData(TestSize,a1,8);
col09 := ML.Distribution.GenData(TestSize,a1,9);
col10 := ML.Distribution.GenData(TestSize,a1,10);
col11 := ML.Distribution.GenData(TestSize,a1,11);
col12 := ML.Distribution.GenData(TestSize,a1,12);
col13 := ML.Distribution.GenData(TestSize,a1,13);
col14 := ML.Distribution.GenData(TestSize,a1,14);
col15 := ML.Distribution.GenData(TestSize,a1,15);
col16 := ML.Distribution.GenData(TestSize,a1,16);
col17 := ML.Distribution.GenData(TestSize,a1,17);
col18 := ML.Distribution.GenData(TestSize,a1,18);
col19 := ML.Distribution.GenData(TestSize,a1,19);
col20 := ML.Distribution.GenData(TestSize,a1,20);
col21 := ML.Distribution.GenData(TestSize,a1,21);
col22 := ML.Distribution.GenData(TestSize,a1,22);
col23 := ML.Distribution.GenData(TestSize,a1,23);
col24 := ML.Distribution.GenData(TestSize,a1,24);
col25 := ML.Distribution.GenData(TestSize,a1,25);
col26 := ML.Distribution.GenData(TestSize,a1,26);
col27 := ML.Distribution.GenData(TestSize,a1,27);
col28 := ML.Distribution.GenData(TestSize,a1,28);
col29 := ML.Distribution.GenData(TestSize,a1,29);
col30 := ML.Distribution.GenData(TestSize,a1,30);


preindep := col01+col02+col03+col04+col05+
           col06+col07+col08+col09+col10+
					 col11+col12+col13+col14+col15+
					 col16+col17+col18+col19+col20+
					 col21+col22+col23+col24+col25+
					 col26+col27+col28+col29+col30;
pretargets := ML.Distribution.GenData(TestSize,a1,31);

//Creates sparse matrix
ML.Types.NumericField remove(preindep L) := TRANSFORM

SELF.value := IF(L.value < density, L.value, 0);
SELF := L;

END;

//Create Sparse representation of matrix 
Indep := PROJECT(preindep,remove(LEFT))(value<>0);
Dep := ML.Discretize.ByRounding(pretargets);

//Set Classification Method
MyLogisticRegression:=ML.Classify.Logistic();

//Learn model
Model3 := MyLogisticRegression.LearnC(Indep,Dep);
OUTPUT(Model3,NAMED('Model3'));

//Test model
predict:=MyLogisticRegression.ClassifyC(Indep,Model3);
OUTPUT(predict,NAMED('predict'));

//View predictions
predicted_v_actual :=
  JOIN(predict,Dep
       ,(LEFT.id=RIGHT.id)
       ,TRANSFORM({UNSIGNED id,UNSIGNED4 number,REAL8 pvalue,REAL8 avalue}
                  ,SELF.pvalue:=LEFT.value
                  ,SELF.avalue:=RIGHT.value
                  ,SELF := LEFT
        )
       ,SMART
  );
OUTPUT(SORT(predicted_v_actual,id,number),NAMED('predicted_v_actual'));

//Calculate Accuracy
Accuracy_proportion := COUNT(predicted_v_actual(pvalue=avalue))/COUNT(predicted_v_actual);
Output(Accuracy_proportion, NAMED('Accuracy'));