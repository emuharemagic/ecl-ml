IMPORT * FROM ML;
IMPORT ML.Mat;
IMPORT ML.Tests.Explanatory as TE;
/*
//This is the tennis-weather dataset transformed to discrete number values.
weatherRecord := RECORD
	Types.t_RecordID id;
	Types.t_FieldNumber outlook;
	Types.t_FieldNumber temperature;
	Types.t_FieldNumber humidity;
	Types.t_FieldNumber windy;
	Types.t_FieldNumber play;
END;

weather_Data := DATASET([
{1,0,0,1,0,0},
{2,0,0,1,1,0},
{3,1,0,1,0,1},
{4,2,1,1,0,1},
{5,2,2,0,0,1},
{6,2,2,0,1,0},
{7,1,2,0,1,1},
{8,0,1,1,0,0},
{9,0,2,0,0,1},
{10,2,1,0,0,1},
{11,0,1,0,1,1},
{12,1,1,1,1,1},
{13,1,0,0,0,1},
{14,2,1,1,1,0}],
weatherRecord);
indep_data:= TABLE(weather_Data,{id, outlook, temperature, humidity, windy});
dep_data:= TABLE(weather_Data,{id, play});
indep_test:= indep_data
dep_test:= dep_data
*/
//You can use the tennis-weather dataset (above) instead and You will find same models for both decision tree clasiffiers.
//Using MONKS dataset you will find different results between the classifiers.
//TE.MonkDS.Train_Data;
indep_data:= TABLE(TE.MonkDS.Train_Data,{id, a1, a2, a3, a4, a5, a6});
dep_data:= TABLE(TE.MonkDS.Train_Data,{id, class});
ToField(indep_data, pr_indep);
indep := ML.Discretize.ByRounding(pr_indep);
ToField(dep_data, pr_dep);
dep := ML.Discretize.ByRounding(pr_dep);

//TE.MonkDS.Test_Data;;
indep_test:= TABLE(TE.MonkDS.Test_Data,{id, a1, a2, a3, a4, a5, a6});
dep_test:= TABLE(TE.MonkDS.Test_Data,{id, class});
ToField(indep_test, t_indep);
indep_t := ML.Discretize.ByRounding(t_indep);
ToField(dep_test, t_dep);
dep_t := ML.Discretize.ByRounding(t_dep);

trainer1:= Classify.DecisionTree.GiniImpurityBased(5, 1); 
model1:= trainer1.LearnD(Indep, Dep);
trainer2:= Classify.DecisionTree.C45(FALSE); // Unpruned
model2:= trainer2.LearnD(Indep, Dep);
//Pruning using test dataset
nodes2b:=Trees.C45PruneTree(trainer2.Model(model2), indep_t, dep_t);
model2b:=Trees.ToDiscreteTree(nodes2b);

trainer3:= Classify.DecisionTree.C45(TRUE, 3, 0.67449); // Pruned using 3 folds of training dataset and 25% confidence factor (z= 0.67449)
model3:= trainer3.LearnD(Indep, Dep);

OUTPUT(model1, NAMED('Model1'));
OUTPUT(SORT(trainer1.Model(model1), level, node_id), NAMED('DecTree_1'), ALL);
OUTPUT(model2, NAMED('Model2'));
OUTPUT(SORT(trainer2.Model(model2), level, node_id), NAMED('DecTree_2'), ALL);
OUTPUT(model2b, NAMED('Model2b'));
OUTPUT(SORT(trainer2.Model(model2b), level, node_id), NAMED('DecTree_2b'), ALL);
OUTPUT(model3, NAMED('Model3'));
OUTPUT(SORT(trainer2.Model(model3), level, node_id), NAMED('DecTree_3'), ALL);

//Classifying independent test data and comparing with dependent test data 
ClassDist1:= trainer1.ClassProbDistribD(indep_t, model1);
AUC1_0:= Classify.AUC_ROC(ClassDist1, 0, dep_t); //Area under ROC Curve for class "0"
AUC1_1:= Classify.AUC_ROC(ClassDist1, 1, dep_t); //Area under ROC Curve for class "1"
results1:= trainer1.ClassifyD(indep_t, model1);
results11:= Classify.Compare(dep_t, results1);

ClassDist2:= trainer2.ClassProbDistribD(indep_t, model2);
AUC2_0:= Classify.AUC_ROC(ClassDist2, 0, dep_t); //Area under ROC Curve for class "0"
AUC2_1:= Classify.AUC_ROC(ClassDist2, 1, dep_t); //Area under ROC Curve for class "1"
results2:= trainer2.ClassifyD(indep_t, model2);
results21:= Classify.Compare(dep_t, results2);
results2b:= trainer2.ClassifyD(indep_t, model2b);
results21b:= Classify.Compare(dep_t, results2b);
results3:= trainer2.ClassifyD(indep_t, model3);
results31:= Classify.Compare(dep_t, results3);

//Showing Results
OUTPUT(results11.CrossAssignments, NAMED('CrossAssig1'));
OUTPUT(ClassDist1, NAMED('ClassProb1'),ALL);
OUTPUT(AUC1_0, ALL, NAMED('AUC1_0'));
OUTPUT(AUC1_1, ALL, NAMED('AUC1_1'));
OUTPUT(results11.RecallByClass, NAMED('RecallByClass1'));
//OUTPUT(results11.PrecisionByClass, NAMED('PrecByClass1'));
//OUTPUT(SORT(results11.FP_Rate_ByClass, classifier, class), NAMED('FPR_ByClass1'));
OUTPUT(results11.Accuracy, NAMED('Accur1'));
OUTPUT(results21.CrossAssignments, NAMED('CrossAssig2'));
OUTPUT(ClassDist2, NAMED('ClassProb2'),ALL);
OUTPUT(AUC2_0, ALL, NAMED('AUC2_0'));
OUTPUT(AUC2_1, ALL, NAMED('AUC2_1'));
OUTPUT(results21.RecallByClass, NAMED('RecallByClass2'));
//OUTPUT(results21.PrecisionByClass, NAMED('PrecByClass2'));
//OUTPUT(SORT(results21.FP_Rate_ByClass, classifier, class), NAMED('FPR_ByClass2'));
OUTPUT(results21.Accuracy, NAMED('Accur2'));
OUTPUT(results21b.CrossAssignments, NAMED('CrossAssig2b'));
OUTPUT(results21b.RecallByClass, NAMED('RecallByClass2b'));
//OUTPUT(results21b.PrecisionByClass, NAMED('PrecByClass2b'));
//OUTPUT(SORT(results21b.FP_Rate_ByClass, classifier, class), NAMED('FPR_ByClass2b'));
OUTPUT(results21b.Accuracy, NAMED('Accur2b'));
OUTPUT(results31.CrossAssignments, NAMED('CrossAssig3'));
OUTPUT(results31.RecallByClass, NAMED('RecallByClass3'));
//OUTPUT(results31.PrecisionByClass, NAMED('PrecByClass3'));
//OUTPUT(SORT(results31.FP_Rate_ByClass, classifier, class), NAMED('FPR_ByClass3'));
OUTPUT(results31.Accuracy, NAMED('Accur3'));