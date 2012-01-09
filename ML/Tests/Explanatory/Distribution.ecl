IMPORT ML;

TestSize := 1000000;

a1 := ML.Distribution.Uniform(0,100,10000); 
b1 := ML.Distribution.GenData(TestSize,a1,1); // Field 1 Uniform
// Field 2 Normally Distributed
a2 := ML.Distribution.Normal2(0,10,10000);
b2 := ML.Distribution.GenData(TestSize,a2,2);
// Field 3 - Poisson Distribution
a3 := ML.Distribution.Poisson(4,100);
b3 := ML.Distribution.GenData(TestSize,a3,3);

D := b1+b2+b3; // This is the test data

ML.FieldAggregates(D).Simple;  // Perform some statistics on the test data to ensure it worked