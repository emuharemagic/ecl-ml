IMPORT * FROM ML;
//a := ML.Distribution.Normal(4,5,10000);
a := ML.Distribution.Poisson(40,100);
//a := ML.Distribution.Uniform(0,100,10000);
a.Cumulative(5);
choosen(a.DensityV(),1000);
choosen(a.CumulativeV(),1000);
b := ML.Distribution.GenData(200000,a);
ave(b,value);
variance(b,value)