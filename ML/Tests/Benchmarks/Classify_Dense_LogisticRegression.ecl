//Logistic Regression EXAMPLE
//
// Demonstrates logistic regression on large, dense dataset.
//
// Dataset download location: https://archive.ics.uci.edu/ml/datasets/HIGGS


//Insert your path and filename
pathToFile := 'simulations::higgs';


//
// 		Runtime on 20-node Thor 4GB RAM
//        Sample Size                       Thor time
//				1,000 rows     x 29 columns       59s
//				10,000 rows    x 29 columns       1m 5s
//				100,000 rows   x 29 columns       2m 21s
//				1,000,000 rows x 29 columns       19m 16s
//
// The sample code shows how to import, transform, and run 
// logistic regression after data has been sprayed to the cluster.
//
// For this example, the training set is tested against itself for accuracy.
//
//---------------------------------------------------------------------------

IMPORT ML;

//Number of rows set at 1,000,000
Training := 1000000;

//Record Structure for initial data
Layout := RECORD
	
		REAL Label;
		REAL kProp1;
		REAL kProp2;
		REAL kProp3;
		REAL kProp4;
		REAL kProp5;
		REAL kProp6;
		REAL kProp7;
		REAL kProp8;
		REAL kProp9;
		REAL kProp10;
		REAL kProp11;
		REAL kProp12;
		REAL kProp13;
		REAL kProp14;
		REAL kProp15;
		REAL kProp16;
		REAL kProp17;
		REAL kProp18;
		REAL kProp19;
		REAL kProp20;
		REAL kProp21;
		REAL hLevel1;
		REAL hLevel2;
		REAL hLevel3;
		REAL hLevel4;
		REAL hLevel5;
		REAL hLevel6;
		REAL hLevel7;
	END;


//Import data set
File := DATASET(pathToFile, Layout, CSV);

//Record Structure for dataset with Record ID
HiggsIDRec := RECORD
	UNSIGNED rid;
	REAL kProp1;
	REAL kProp2;
	REAL kProp3;
	REAL kProp4;
	REAL kProp5;
	REAL kProp6;
	REAL kProp7;
	REAL kProp8;
	REAL kProp9;
	REAL kProp10;
	REAL kProp11;
	REAL kProp12;
	REAL kProp13;
	REAL kProp14;
	REAL kProp15;
	REAL kProp16;
	REAL kProp17;
	REAL kProp18;
	REAL kProp19;
	REAL kProp20;
	REAL kProp21;
	REAL hLevel1;
	REAL hLevel2;
	REAL hLevel3;
	REAL hLevel4;
	REAL hLevel5;
	REAL hLevel6;
	REAL hLevel7;
	REAL Label;
END;

HiggsIDRec IDRecs(File L, INTEGER C) := TRANSFORM
	SELF.rid := C;
	SELF := L;
END; 


//Assign IDs
All_Higgs := PROJECT(File, IDRecs(LEFT,COUNTER));

//Partition Dataset
Higgs := All_Higgs(rid<=Training);

//Convert to numeric field
ML.ToField(Higgs,Higgsflds0);

//Set dependent and independent variables
Dep := ML.Discretize.ByRounding(Higgsflds0(number=29));
Indep := Higgsflds0(number<29);

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