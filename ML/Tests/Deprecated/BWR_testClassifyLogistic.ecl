IMPORT * FROM ML;

value_record := RECORD
                unsigned rid;
								real dummy;
                real length;
                integer1 class; // 0 = f, 1 = m
END;

/*
Dummy dataset created to demponstrate that modelY values represent/follow sigmoid function;
*/                
d := dataset([{1,0,1,0},
              {2,0,2,0},
              {3,0,3,0},
              {4,0,4,0},
              {5,0,5,0},
              {6,0,6,0},
              {7,0,7,0},
              {8,0,8,1},
              {9,0,9,1},
              {10,0,10,1},
              {11,0,11,1},
              {12,0,12,1},
              {13,0,13,1},
              {14,0,14,1}                                                                                                  
             ]
             ,value_record);
                                                                                                                
// Turn into regular NumericField file (with continuous variables)
ToField(d,o);
Y := O(Number=3);  // pull out class
X := O(Number IN [2]); // pull out lenghts

logistic := Classify.Logistic(X,Y,,,10);
// make sure Beta has an id=0 element (intercept), and an id=2 element
OUTPUT(logistic.Beta, named('beta'));
// make sure modelY has the number=3 column
OUTPUT(logistic.modelY, named('modelY'));

