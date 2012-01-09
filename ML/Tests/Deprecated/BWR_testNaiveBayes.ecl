IMPORT ML;

TestSize := 10000000;

a1 := ML.Distribution.Poisson(5,100); 
b1 := ML.Distribution.GenData(TestSize,a1,1); // Field 1 Uniform
a2 := ML.Distribution.Poisson(3,100);
b2 := ML.Distribution.GenData(TestSize,a2,2);
a3 := ML.Distribution.Poisson(3,100);
b3 := ML.Distribution.GenData(TestSize,a3,3);

D := b1+b2+b3; // This is the test data
// Now construct a fourth column which is the sum of them all
B4 := PROJECT(TABLE(D,{Id,Val := SUM(GROUP,Value)},Id),TRANSFORM(ML.Types.NumericField,
                                                           SELF.Number:=4,
																													 SELF.Value:=MAP(LEFT.Val < 6 => 0,  // Small
																																					 LEFT.Val < 10 => 1, // Normal
																																					 2 ); // Big
																													 SELF := LEFT));

D1 := D+B4 : PERSIST('TEMP::FourColumnRandom');
D2 := ML.Discretize.ByRounding(D1);

Model := ML.Classify.BuildNaiveBayes(D2(Number<=3),D2(Number=4));
Model;
Results := ML.Classify.NaiveBayes(D2(Number<=3),Model);

Results
