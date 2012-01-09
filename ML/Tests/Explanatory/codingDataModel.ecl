ml.ToField(d,o);
o1 := ML.Discretize.ByBucketing(o,5);
Independents := o1(Number <= 3);
Dependents := o1(Number >= 4);
Bayes := ML.Classify.BuildNaiveBayes(Independents,Dependents);
Bayes
Better := o(Number<>2 OR Value<>0);
ml.ToField(d,o);
BelowW := o(Number <= 2);
// Those columns whose numbers are not changed
// Shuffle the other columns up - this is not needed if appending a column
AboveW := PROJECT(o(Number>2),TRANSFORM(ML.Types.NumericField,SELF.Number :=
LEFT.Number+1, SELF := LEFT));
NewCol := PROJECT(o(Number=2),TRANSFORM(ML.Types.NumericField,
SELF.Number := 3,
SELF.Value := LEFT.Value*LEFT.Value,
SELF := LEFT) );
NewO := BelowW+AboveW+NewCol;
NewO;