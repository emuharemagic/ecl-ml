IMPORT * FROM ML;
IMPORT * FROM $;

//Set Parameters
LoopNum := 2; // Number of iterations in softmax algortihm
LAMBDA := 0.0001; // weight decay parameter in  claculation of SoftMax Cost fucntion
ALPHA := 0.1; // //Learning Rate for updating SoftMax parameters

//input data
value_record := RECORD
real	f1	;
real	f2	;
real	f3	;
real	f4	;
real	f5	;
real	f6	;
real	f7	;
real	f8	;
real	f9	;
real	f10	;
real	f11	;
real	f12	;
real	f13	;
real	f14	;
real	f15	;
real	f16	;
real	f17	;
real	f18	;
real	f19	;
real	f20	;
INTEGER Label  ;
END;
input_data_tmp := DATASET('~online::maryam::mytest::cancer_20at_96sa_data', value_record, CSV);
ML.AppendID(input_data_tmp, id, input_data);
OUTPUT  (input_data, NAMED ('input_data'));
//convert input data to two datset: samples dataset and labels dataset
Sampledata_Format := RECORD
input_data.id;
input_data.f1	;
input_data.f2	;
input_data.f3	;
input_data.f4	;
input_data.f5	;
input_data.f6	;
input_data.f7	;
input_data.f8	;
input_data.f9	;
input_data.f10	;
input_data.f11	;
input_data.f12	;
input_data.f13	;
input_data.f14	;
input_data.f15	;
input_data.f16	;
input_data.f17	;
input_data.f18	;
input_data.f19	;
input_data.f20	;
END;

sample_table := TABLE(input_data,Sampledata_Format);
OUTPUT  (sample_table, NAMED ('sample_table'));

labeldata_Format := RECORD
  input_data.id;
  input_data.label;
END;

label_table := TABLE(input_data,labeldata_Format);
OUTPUT  (label_table, NAMED ('label_table'));

ML.ToField(sample_table, indepDataC);
OUTPUT  (indepDataC, NAMED ('indepDataC'));
ML.ToField(label_table, depDataC);
OUTPUT  (depDataC, NAMED ('depDataC'));
label := PROJECT(depDataC,Types.DiscreteField);
OUTPUT  (label, NAMED ('label'));

//initialize THETA
Numclass := MAX (label, label.value);
OUTPUT  (Numclass, NAMED ('Numclass'));
InputSize := MAX (indepDataC,indepDataC.number);
OUTPUT  (InputSize, NAMED ('InputSize'));
T1 := Mat.RandMat (Numclass,InputSize+1);
OUTPUT  (T1, NAMED ('T1'));
IntTHETA := Mat.Scale (T1,0.005);
OUTPUT  (IntTHETA, NAMED ('IntTHETA'));
//SoftMax_Sparse Classfier
UNSIGNED4 prows:=0;
UNSIGNED4 pcols:=0;
UNSIGNED4 Maxrows:=0;
UNSIGNED4 Maxcols:=0;
trainer:= ML.Classify.SoftMax(IntTHETA, LAMBDA, ALPHA, LoopNum);

//Learning Phase
Parameters := trainer.LearnC(indepDataC, label);
OUTPUT  (Parameters, NAMED ('Parameters'));
mod := trainer.Model(Parameters);
mXstats := ML.Mat.Has(Mod).Stats;
mX_n := mXstats.XMax;
mX_m := mXstats.YMax;
OUTPUT  (mX_n, ALL, NAMED ('mX_n'));
OUTPUT  (mX_m, ALL, NAMED ('mX_m'));
//test phase
Model := Parameters;
dist := trainer.ClassProbDistribC(indepDataC,Model );
classified := trainer.ClassifyC(indepDataC,Model);
OUTPUT  (dist, NAMED ('dist'));
OUTPUT  (classified, NAMED ('classified'));