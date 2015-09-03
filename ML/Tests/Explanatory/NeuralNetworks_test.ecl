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
label_record := RECORD
unsigned  id;
  real  f1;
  real  f2;
END;
value_record := RECORD
  unsigned  id;
  real  f1;
  real  f2;
  real  f3;
END;
input_data := DATASET([
{1, 0.1, 0.2, 0.2},
{2, 0.8, 0.9,0.4},
{3, 0.5, 0.9,0.5},
{4, 0.8, 0.7, 0.8},
{5, 0.9,0.1,0.1},
{6, 0.1, 0.3,0.7}],
 value_record);
label := DATASET([
{1, 0.1, 0.2},
{2, 0.8,0.4},
{3, 0.5, 0.9},
{4,  0.7, 0.8},
{5, 0.9,0.1},
{6, 0.1, 0.3}],
label_record);
OUTPUT  (input_data, ALL, NAMED ('input_data'));

ML.ToField(input_data, indepDataC);
OUTPUT  (indepDataC, ALL, NAMED ('indepDataC'));

ML.ToField(label, depDataC);
OUTPUT  (depDataC, ALL, NAMED ('depDataC'));
//define the parameters for the back propagation algorithm
//ALPHA is learning rate
//LAMBDA is weight decay rate
REAL8 ALPHA := 0.1;
REAL8 LAMBDA :=0.1;
UNSIGNED2 MaxIter :=3;
UNSIGNED4 prows:=0;
UNSIGNED4 pcols:=0;
UNSIGNED4 Maxrows:=0;
UNSIGNED4 Maxcols:=0;
//define the Neural Network Module
NN := NeuralNetworks(net,prows, pcols, Maxrows,  Maxcols);
//initialize weight and bias values for the Back Propagation algorithm
IntW := NN.IntWeights;
Intb := NN.IntBias;
output(IntW,ALL, named ('IntW'));
output(IntB,ALL, named ('IntB'));
//trainer module

Learntmodel := NN.NNLearn(indepDataC, depDataC,IntW, Intb,  LAMBDA, ALPHA, MaxIter);
OUTPUT  (Learntmodel, ALL, NAMED ('Learntmodel'));
NNModel := NN.Model(Learntmodel);
OUTPUT  (NNModel, ALL, NAMED ('NNModel'));

FW := NN.ExtractWeights(Learntmodel);
OUTPUT  (FW, ALL, NAMED ('FW'));

FB := NN.ExtractBias(Learntmodel);
OUTPUT  (FB, ALL, NAMED ('FB'));


AEnd :=NN.NNOutput(indepDataC,Learntmodel);//output of the Neural Network
OUTPUT  (AEnd, ALL, NAMED ('AEnd'));

Class := NN.NNClassify(indepDataC,Learntmodel);
OUTPUT  (Class, ALL, NAMED ('Class'));