IMPORT * from ML;
IMPORT ML.Mat;
BayesModule := ML.Classify.NaiveBayes;

ds :=  ML.Tests.Explanatory.samplingDS;
ML.AppendID(ds, id, dOrig);
OUTPUT(dOrig, NAMED('OrigSeq'));
ML.ToField(dOrig,dMatrix);
D2 := ML.Discretize.ByRounding(dMatrix);
OUTPUT(D2, NAMED('OrigMatrix'));
indep	:= D2(Number<=3);
dep		:= D2(Number=4);
OUTPUT(indep, NAMED('Indep'));
OUTPUT(dep, NAMED('Dep'));

// Example 1:
// Folding a dependent dataset into 3 partitions
// Modeling and testing 3 parts (3 diff models )
newDependent:= Sampling.DiscreteDepNFolds(dep,3);
results1 := BayesModule.TestD(indep, newDependent);
OUTPUT(newDependent, NAMED('Dep3Folds'),ALL);
OUTPUT(results1.CrossAssignments, NAMED('CA_1'));
OUTPUT(results1.RecallByClass, NAMED('Rec1'));
OUTPUT(results1.PrecisionByClass, NAMED('Pre1'));
OUTPUT(SORT(results1.FP_Rate_ByClass, classifier, c_modeled), NAMED('FPR1'));
OUTPUT(results1.Accuracy, NAMED('Acc1'));

// Example 2:
// Generate one NaiveBayes model with complete dataset
// Test the model with partitions obtained from NFold
model:=	BayesModule.LearnD(Indep, Dep);
// Folding the dataset into 4 partitions
parts:= Sampling.NFold(dMatrix, 4);
OUTPUT(parts.NFoldList, NAMED('FoldList'),ALL);
part1:=  parts.FoldNDS(1, 1000);
part2:=  parts.FoldNDS(2, 2000);
part3:=  parts.FoldNDS(3, 3000);
part4:=  parts.FoldNDS(4, 4000);
AllDiscrete:= Discretize.ByRounding(part1 + part2 + part3 + part4);
indepAll:= AllDiscrete(number<4);
// Getting model classification in one step
classifAll := BayesModule.ClassifyD(indepAll, model);
// Changing number to identify as a different classifier
results:= PROJECT(classifAll, TRANSFORM(RECORDOF(LEFT), SELF.number:= LEFT.id DIV 1000, SELF:= LEFT));
dep1:= PROJECT(part1(number=4),TRANSFORM(Types.DiscreteField, SELF.number:=1, SELF:= LEFT));
dep2:= PROJECT(part2(number=4),TRANSFORM(Types.DiscreteField, SELF.number:=2, SELF:= LEFT));
dep3:= PROJECT(part3(number=4),TRANSFORM(Types.DiscreteField, SELF.number:=3, SELF:= LEFT));
dep4:= PROJECT(part4(number=4),TRANSFORM(Types.DiscreteField, SELF.number:=4, SELF:= LEFT));
depAll:= dep1 + dep2 + dep3 + dep4;
OUTPUT(depAll, NAMED('depAll'),ALL);
// Compare all in one step
compareAll:= ML.Classify.Compare(depAll, results);
// Performance results
OUTPUT(compareAll.CrossAssignments, NAMED('CA_2'));
OUTPUT(compareAll.RecallByClass, NAMED('Rec2'));
OUTPUT(compareAll.PrecisionByClass, NAMED('Pre2'));
OUTPUT(SORT(compareAll.FP_Rate_ByClass, classifier, c_modeled), NAMED('FPR2'));
OUTPUT(compareAll.Accuracy, NAMED('Acc2'));

// Example 3:
// 4-Fold Cross Validation with Naive Bayes classifier
// Folding dataset
CV:= Sampling.NFoldCross(dMatrix, 4);
OUTPUT(CV.NFoldList, NAMED('CVFoldList'),ALL);
// Getting training datasets
train1 := CV.FoldNTrainDS(1, 1000);
train2 := CV.FoldNTrainDS(2, 2000);
train3 := CV.FoldNTrainDS(3, 3000);
train4 := CV.FoldNTrainDS(4, 4000);
trainAll := Discretize.ByRounding(train1 + train2 + train3 + train4);
// changing number to identify each fold as a different classifier
depTrainAll := PROJECT(trainAll(number=4), TRANSFORM(RECORDOF(LEFT), SELF.number:= LEFT.id DIV 1000, SELF:= LEFT));
OUTPUT(depTrainAll, NAMED('depTrainAll'),ALL);
// Build 4 different models, one per training fold
CVModels:=	BayesModule.LearnD(trainAll(number<4), depTrainAll);
// Getting test datasets
test1 := CV.FoldNTestDS(1, 1000);
test2 := CV.FoldNTestDS(2, 2000);
test3 := CV.FoldNTestDS(3, 3000);
test4 := CV.FoldNTestDS(4, 4000);
testAll := Discretize.ByRounding(test1 + test2 + test3 + test4);
// changing number to identify each fold as a different classifier
depTestAll := PROJECT(testAll(number=4), TRANSFORM(RECORDOF(LEFT), SELF.number:= LEFT.id DIV 1000, SELF:= LEFT));
OUTPUT(depTestAll, NAMED('depTestAll'),ALL);
// Getting classification results with test datasets from correspondent model
classCV := BayesModule.ClassifyD(testAll(number<4), CVModels);
// Performance results
compareCV := ML.Classify.Compare(depTestAll, classCV);
ca_3 := compareCV.CrossAssignments;
OUTPUT(ca_3, NAMED('CA_3'));
rec_3:= compareCV.RecallByClass;
OUTPUT(rec_3, NAMED('Rec3'));
pre_3:= compareCV.PrecisionByClass;
OUTPUT(pre_3, NAMED('Pre3'));
fpr_3:= compareCV.FP_Rate_ByClass;
OUTPUT(SORT(fpr_3, classifier, c_modeled), NAMED('FPR3'));
acc_3:=compareCV.Accuracy;
OUTPUT(SORT(acc_3, classifier), NAMED('Acc3'));

// Performance results aggregated
OUTPUT(TABLE(ca_3, {c_actual, c_modeled, tot:= SUM(GROUP,cnt)}, c_actual, c_modeled), NAMED('CA_3_sum'));
OUTPUT(TABLE(rec_3, {c_actual, tp_avg:= AVE(GROUP,tp_rate)}, c_actual), NAMED('Rec_3_avg'));
OUTPUT(TABLE(pre_3, {c_modeled, precision_avg:= AVE(GROUP,precision)}, c_modeled), NAMED('pre_3_avg'));
OUTPUT(TABLE(fpr_3, {c_modeled, fpr_avg:= AVE(GROUP, fp_rate)}, c_modeled), NAMED('fpr_3_avg'));
OUTPUT(AVE(acc_3, accuracy), NAMED('accuracy_3_avg'));

