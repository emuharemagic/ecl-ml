IMPORT ML;
value_record := RECORD
unsigned rid;
unsigned age;
real height;
END;
d := DATASET([{1,18,76.1}, {2,19,77}, {3,20,78.1},
{4,21,78.2}, {5,22,78.8}, {6,23,79.7},
{7,24,79.9}, {8,25,81.1}, {9,26,81.2},
{10,27,81.8},{11,28,82.8}, {12,29,83.5}]
,value_record);
ML.ToField(d,o);
X := O(Number IN [1]); // Pull out the age
Y := O(Number IN [2]); // Pull out the height
Reg := ML.Regression.Dense.OLS_LU(X,Y);
B := Reg.Betas();
B;
Reg.ModelY;
Reg.Extrapolated(X);

Reg.RSquared;
Reg.Anova;


