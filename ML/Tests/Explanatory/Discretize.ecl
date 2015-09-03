import ml;
value_record := RECORD
unsigned rid;
real height;
real weight;
real age;
integer1 species;
integer1 gender; // 0 = unknown, 1 = male, 2 = female
END;
d := dataset([{1,5*12+7,156*16,43,1,1},
{2,5*12+7,128*16,31,1,2},
{3,5*12+9,135*16,15,1,1},
{4,5*12+7,145*16,14,1,1},
{5,5*12-2,80*16,9,1,1},
{6,4*12+8,72*16,8,1,1},
{7,8,32,2.5,2,2},
{8,6.5,28,2,2,2},
{9,6.5,28,2,2,2},
{10,6.5,21,2,2,1},
{11,4,15,1,2,0},
{12,3,10.5,1,2,0},
{13,2.5,3,0.8,2,0},
{14,1,1,0.4,2,0}
]
,value_record);
// Turn into regular NumericField file (with continuous variables)
ml.ToField(d,o);
// Hand-code the discretization of some of the variables
disc := ML.Discretize.ByBucketing(o(Number IN [2,3]),4)+ML.Discretize.ByTiling(o(Number IN
[1]),6)+ML.Discretize.ByRounding(o(Number=4));
// Create instructions to be executed
inst :=
ML.Discretize.i_ByBucketing([2,3],4)+ML.Discretize.i_ByTiling([1],6)+
ML.Discretize.i_ByRounding([4,5]);
// Execute the instructions
done := ML.Discretize.Do(o,inst);

BayesModule := ML.Classify.NaiveBayes;

TestModule := BayesModule.TestD(done(Number<=3),done(Number=4));
TestModule.CrossAssignments;
TestModule.PrecisionByClass;
TestModule.Accuracy;

Model := BayesModule.LearnD(done(Number<=3),done(Number=4));
Results := BayesModule.ClassifyD(done(Number<=3),Model);
Results;
