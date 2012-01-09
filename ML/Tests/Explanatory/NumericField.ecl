IMPORT ML;
value_record := RECORD
UNSIGNED rid;
REAL height;
REAL weight;
REAL age;
INTEGER1 species; // 1 = human, 2 = tortoise
INTEGER1 gender; // 0 = unknown, 1 = male, 2 = female
END;
d := dataset([{1,5*12+7,156*16,43,1,1},
{2,5*12+7,128*16,31,1,2},
{3,5*12+9,135*16,15,1,1},
{4,5*12+7,145*16,14,1,1},
{5,5*12-2,80*16,9,1,1},
{6,4*12+8,72*16,8,1,1},
{7,8,32,2.5,2,2},
{8,6.5,28,2,2,2},
{9,6.5,28,2,2,2},
{10,6.5,21,2,2,1},
{11,4,15,1,2,0},
{12,3,10.5,1,2,0},
{13,2.5,3,0.8,2,0},
{14,1,1,0.4,2,0}
]
,value_record);
d;
ML.ToField(d,o);
d;
o;
ml.FromField(o,value_record,d1);
d1; 
