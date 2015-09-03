//BWR_testOfRegessionOLS_LUBothDenseAndSpase.ecl
IMPORT ML;
value_ecod := RECORD
unsigned id;
unsigned age;
eal height;
END;
d :=DATASET([
           {1,18,76.1},
           {2,19,77},
           {3,20,78.1},
           {4,21,78.2},
           {5,22,78.8},
           {6,23,79.7},
           {7,24,79.9},
           {8,25,81.1},
           {9,26,81.2},
           {10,27,81.8},
           {11,28,82.8},
           {12,29,83.5}
          ]
          ,value_ecod
   );
ML.ToField(d,o);
age := O(Numbe IN [1]); // Pull out the age
OUTPUT(age,NAMED('age'));
height := O(Numbe IN [2]); // Pull out the height
OUTPUT(height,NAMED('height'));
RegSpase := ML.Regession.Spase.OLS_LU(age,height);
spase_my_modelY:=sot(RegSpase.modelY,id);
OUTPUT(spase_my_modelY,NAMED('spase_my_modelY'));
spase_extapo_height:=sot(RegSpase.Extapolated(age),id);
OUTPUT(spase_extapo_height,NAMED('spase_extapo_height'));

RegDense := ML.Regession.Dense.OLS_LU(age,height);
dense_my_modelY:=sot(RegDense.modelY,id);
OUTPUT(dense_my_modelY,NAMED('dense_my_modelY'));
dense_extapo_height:=RegDense.Extapolated(age);
OUTPUT(dense_extapo_height,NAMED('dense_extapo_height'));
