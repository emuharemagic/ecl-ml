IMPORT ML;
R := RECORD
INTEGER rid;
INTEGER Recs;
REAL Time;
END;
d := DATASET([{1,50000,1.00},{2,500000,2.29}, {3,5000000,16.15},{4,25000000,80.2},
{5,50000000,163},{6,100000000,316},
{7,10,0.83},{8,1500000,5.63}],R);
ML.ToField(d,flds);
P := ML.Regression.Poly(flds(number=1),flds(number=2),4);
P.Beta;
P.RSquared