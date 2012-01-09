//Perceptron Example
//
import ML;
d := dataset([{1,0, 0, 1, 1}, {2,0, 1, 1, 1}, {3,1, 0, 1, 1}, {4,1, 1, 0, 0}],
{ unsigned id,unsigned a, unsigned b, unsigned c, unsigned d });
ML.ToField(d,o);
o1 := ML.Discretize.ByRounding(o)(id<5);
ML.Classify.BuildPerceptron(o1(Number<=2),o1(number>=3),1);
ML.Classify.BuildPerceptron(o1(Number<=2),o1(number>=3),2);
ML.Classify.BuildPerceptron(o1(Number<=2),o1(number>=3),3);
ML.Classify.BuildPerceptron(o1(Number<=2),o1(number>=3),4);
ML.Classify.BuildPerceptron(o1(Number<=2),o1(number>=3),5);
ML.Classify.BuildPerceptron(o1(Number<=2),o1(number>=3),6);
ML.Classify.BuildPerceptron(o1(Number<=2),o1(number>=3),7);
ML.Classify.BuildPerceptron(o1(Number<=2),o1(number>=3),8);
ML.Classify.BuildPerceptron(o1(Number<=2),o1(number>=3),9);
