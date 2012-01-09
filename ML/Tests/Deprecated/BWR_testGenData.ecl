IMPORT ML;
TestSize := 25000000;
// Check normal
a1 := ML.Distribution.Normal(4,4,10000);
b1 := ML.Distribution.GenData(TestSize,a1,1);

a2 := ML.Distribution.Normal2(4,4,10000);
b2 := ML.Distribution.GenData(TestSize,a2,2);
// Check Poisson
a3 := ML.Distribution.Poisson(4,100);
b3 := ML.Distribution.GenData(TestSize,a3,3);

D := b1+b2+b3;

ML.FieldAggregates(D).Simple
