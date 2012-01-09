IMPORT ML;
TestSize := 1000;
a1 := ML.Distribution.Uniform(0,100,10000);
b1 := ML.Distribution.GenData(TestSize,a1,1); // Field 1 Uniform
a2 := ML.Distribution.Poisson(3,100);
b2 := ML.Distribution.GenData(TestSize,a2,2);
D := b1+b2; // This is the test data
Agg := ML.FieldAggregates(D);
Agg.Simple;
Agg.SimpleRanked;
Agg.Ranked;
Agg.Modes;
Agg.Medians;
Agg.NTiles(4);
Agg.NTileRanges(4);
Agg.Buckets(4);
Agg.BucketRanges(4)