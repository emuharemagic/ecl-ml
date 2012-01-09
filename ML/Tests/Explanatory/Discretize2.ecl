import ml;
value_record := RECORD
unsigned rid;
real height;
real weight;
real age;
integer1 species;
END;
d := dataset([{1,5*12+7,156*16,43,1},
{2,5*12+7,128*16,31,1},
{3,5*12+9,135*16,15,1},
{4,5*12+7,145*16,14,1},
{5,5*12-2,80*16,9,1},
{6,4*12+8,72*16,8,1},
{7,8,32,2.5,2},
{8,6.5,28,2,2},
{9,6.5,28,2,2},
{10,6.5,21,2,2},
{11,4,15,1,2},
{12,3,10.5,1,2},
{13,2.5,3,0.8,2},
{14,1,1,0.4,2}
]
value_record);
// Turn into regular NumericField file (with continuous variables)
ml.macPivot(d,o);
// Hand-code the discretization of some of the variables
disc := ML.Discretize.ByBucketing(o(Number = 3),4)+ML.Discretize.ByTiling(o
(Number IN [1,2]),4)+ML.Discretize.ByRounding(o(Number=4));
disc;
// Create instructions to be executed
inst := ML.Discretize.i_ByBucketing([3],4)+ML.Discretize.i_ByTiling([1,2],4)+ML.Discretize.i_ByRounding([4]);
// Execute the instructions
done := ML.Discretize.Do(o,inst);
done;