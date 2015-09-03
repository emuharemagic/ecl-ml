IMPORT * FROM ML;
IMPORT * FROM $;
lymphomaData:= ML.Tests.Explanatory.lymphomaDS.DS;
OUTPUT(lymphomaData, NAMED('lymphomaData'), ALL);
ML.ToField(lymphomaData, full_ds);
OUTPUT(full_ds_Map,ALL, NAMED('DatasetFieldMap'));
indepData:= full_ds(number<4027);

depData:= PROJECT(full_ds(number=4027),TRANSFORM(Types.DiscreteField, SELF.number:=1, SELF:=LEFT));
maxLevel := 10;
learner := Classify.RandomForest(25, 100, 1.0, maxLevel);
result := learner.learnc(IndepData, DepData); // model to use when classifying
OUTPUT(result,NAMED('learnc_output'), ALL); // group_id represent number of tree
model:= learner.modelC(result);  // transforming model to a easier way to read it
OUTPUT(SORT(model, group_id, node_id),NAMED('modelC_ouput')); // group_id represent number of tree

//Class distribution for each Instance
ClassDist:= learner.ClassProbDistribC(IndepData, result);
OUTPUT(ClassDist, NAMED('ClassDist'), ALL);
class:= learner.classifyC(IndepData, result); // classifying
OUTPUT(class, NAMED('class_result'), ALL); // conf show voting percentage
//Measuring Performance of Classifier
performance:= Classify.Compare(depData, class);
OUTPUT(performance.CrossAssignments, NAMED('CrossAssig'));
OUTPUT(performance.RecallByClass, NAMED('RecallByClass'));
OUTPUT(performance.PrecisionByClass, NAMED('PrecisionByClass'));
OUTPUT(performance.FP_Rate_ByClass, NAMED('FP_Rate_ByClass'));