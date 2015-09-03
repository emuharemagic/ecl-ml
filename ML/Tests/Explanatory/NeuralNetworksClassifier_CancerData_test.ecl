IMPORT * FROM ML;
IMPORT * FROM $;
IMPORT PBblas;
Layout_Cell := PBblas.Types.Layout_Cell;
//number of neurons in the first layer = number of features
//number of neurons in the last layer = number of classes
net := DATASET([
{1, 1, 20},
{2,1,3},
{3,1,2}],
Types.DiscreteField);

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

//define the parameters for the back propagation algorithm
//ALPHA is learning rate
//LAMBDA is weight decay rate
REAL8 ALPHA := 0.1;
REAL8 LAMBDA :=0.1;
UNSIGNED2 MaxIter :=2;
UNSIGNED4 prows:=0;
UNSIGNED4 pcols:=0;
UNSIGNED4 Maxrows:=0;
UNSIGNED4 Maxcols:=0;
//initialize weight and bias values for the Back Propagation algorithm
IntW := NeuralNetworks(net).IntWeights;
Intb := NeuralNetworks(net).IntBias;
output(IntW,ALL, named ('IntW'));
output(IntB,ALL, named ('IntB'));
//define the Neural Network Module
NNClassifier := ML.Classify.NeuralNetworksClassifier(net, IntW, Intb,  LAMBDA, ALPHA, MaxIter, prows, pcols, Maxrows, Maxcols);

// Dep := label;
// Y := PROJECT(Dep,Types.NumericField);
// groundTruth:= Utils.ToGroundTruth (Y);
// groundTruth_t := Mat.trans(groundTruth);
// groundTruth_NumericField := Types.FromMatrix (groundTruth_t);
// output(groundTruth_NumericField,ALL, named ('groundTruth_NumericField'));
//training phase
Learntmodel := NNClassifier.LearnC(indepDataC, label);
OUTPUT  (Learntmodel, ALL, NAMED ('Learntmodel'));
NNModel := NNClassifier.Model(Learntmodel);
OUTPUT  (NNModel, ALL, NAMED ('NNModel'));
//testing phase
AEnd := NNClassifier.ClassProbDistribC(indepDataC, Learntmodel);
OUTPUT  (AEnd, ALL, NAMED ('AEnd'));
Class := NNClassifier.ClassifyC(indepDataC, Learntmodel);
OUTPUT  (Class, ALL, NAMED ('Class'));