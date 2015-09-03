IMPORT * FROM ML;
IMPORT ML.Tests.Explanatory as TE;

Depth:= 10;
MedianDepth:= 15;

indep_data:= TABLE(TE.MonkDS.Train_Data,{id, a1, a2, a3, a4, a5, a6});
indep_test:= TABLE(TE.MonkDS.Test_Data,{id, a1, a2, a3, a4, a5, a6});
dep_data:= TABLE(TE.MonkDS.Train_Data,{id, class});
dep_test:= TABLE(TE.MonkDS.Test_Data,{id, class});

ToField(indep_data, indepData);
ToField(indep_test, IndepTest);

ToField(dep_data, pr_dep);
ToField(dep_test, pr_depT);

depData := ML.Discretize.ByRounding(pr_dep);
depTest := ML.Discretize.ByRounding(pr_depT);


iknn:= Lazy.KNN_KDTree(5);

TestModule:=  iknn.TestC(IndepTest, depTest);
TestModule.CrossAssignments;
TestModule.PrecisionByClass;
TestModule.Accuracy;

computed:=  iknn.ClassifyC(IndepData, depData, IndepTest);
Comparison:=  ML.Classify.Compare(depTest, computed);
computed;
Comparison.CrossAssignments;
Comparison.PrecisionByClass;
Comparison.Accuracy;
