IMPORT * FROM ML;
IMPORT * FROM $;
IMPORT PBblas;
Layout_Cell := PBblas.Types.Layout_Cell;
//net is the structure of the Back Propagation Network that shows number of neurons in each layer
//net is in NumericFiled format {id, number, value}, "value" is the number of nodes in the "id"th layer
//basically in the first layer number of neurons is : number of features
//Number of neurons in the last layer is number of output assigned to each sample
net := DATASET([
{1, 1, 3},
{2,1,3},
{3,1,4},
{4,1,2}],
Types.DiscreteField);

//input data
value_record := RECORD
  unsigned  id;
  real  f1;
  real  f2;
  real  f3;
  integer1  label;
END;
input_data := DATASET([
{1, 0.1, 0.2, 0.2,1},
{2, 0.8, 0.9,0.4, 2},
{3, 0.5, 0.9,0.5, 2},
{4, 0.8, 0.7, 0.8, 1},
{5, 0.9,0.1,0.1, 2},
{6, 0.1, 0.3,0.7, 1}],
 value_record);
OUTPUT  (input_data, ALL, NAMED ('input_data'));

//convert input data to two datset: samples dataset and labels dataset
Sampledata_Format := RECORD
  input_data.id;
  input_data.f1;
  input_data.f2;
  input_data.f3;
END;

sample_table := TABLE(input_data,Sampledata_Format);
OUTPUT  (sample_table, ALL, NAMED ('sample_table'));

labeldata_Format := RECORD
  input_data.id;
  input_data.label;
END;

label_table := TABLE(input_data,labeldata_Format);
OUTPUT  (label_table, ALL, NAMED ('label_table'));


ML.ToField(sample_table, indepDataC);
OUTPUT  (indepDataC, ALL, NAMED ('indepDataC'));
ML.ToField(label_table, depDataC);
OUTPUT  (depDataC, ALL, NAMED ('depDataC'));
label := PROJECT(depDataC,Types.DiscreteField);
OUTPUT  (label, ALL, NAMED ('label'));
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


Dep := label;
Y := PROJECT(Dep,Types.NumericField);
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