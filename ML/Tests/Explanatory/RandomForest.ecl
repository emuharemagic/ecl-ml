IMPORT * FROM ML;
IMPORT ML.Tests.Explanatory as TE;
/*
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
OUTPUT(weather_Data, NAMED('weather_Data'));
indep_Data:= TABLE(weather_Data,{id, outlook, temperature, humidity, windy});
dep_Data:= TABLE(weather_Data,{id, play});
*/
indep_data:= TABLE(TE.MonkDS.Train_Data,{id, a1, a2, a3, a4, a5, a6});
dep_data:= TABLE(TE.MonkDS.Train_Data,{id, class});

ToField(indep_data, pr_indep);
indepData := ML.Discretize.ByRounding(pr_indep);
ToField(dep_data, pr_dep);
depData := ML.Discretize.ByRounding(pr_dep);


// Using a small dataset to facilitate understanding of algorithm
OUTPUT(indepData, NAMED('indepData'), ALL);
OUTPUT(depData, NAMED('depData'), ALL);
//Generating a random forest of 25 trees selecting 4 features for splits using impurity:=1.0 and max depth:= 10
learner := Classify.RandomForest(25, 4, 1.0, 10);
result := learner.learnd(IndepData, DepData); // model to use when classifying
OUTPUT(result,NAMED('learnd_output'), ALL); // group_id represent number of tree
model:= learner.model(result);  // transforming model to a easier way to read it
OUTPUT(SORT(model, group_id, node_id),NAMED('model_ouput'), ALL); // group_id represent number of tree

//Class distribution for each Instance
ClassDist:= learner.ClassProbDistribD(IndepData, result);
OUTPUT(ClassDist, NAMED('ClassDist'), ALL);
class:= learner.classifyD(IndepData, result); // classifying
OUTPUT(class, NAMED('class_result'), ALL); // conf show voting percentage

//Measuring Performance of Classifier
performance:= Classify.Compare(depData, class);
OUTPUT(performance.CrossAssignments, NAMED('CrossAssig'));
//AUC_ROC returns all the ROC points and the value of the Area under the curve in the LAST_RECORD(AUC FIELD)
AUC0:= Classify.AUC_ROC(ClassDist, 0, depData); //Area under ROC Curve for class "0"
OUTPUT(AUC0, ALL, NAMED('AUC_0'));
AUC1:= Classify.AUC_ROC(ClassDist, 1, depData); //Area under ROC Curve for class "1"
OUTPUT(AUC1, ALL, NAMED('AUC_1'));
