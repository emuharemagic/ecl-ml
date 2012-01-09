IMPORT * FROM $;

EXPORT Det(DATASET(Types.Element) matrix) := 
        AGGREGATE(Decomp.LU(matrix)(x=y), Types.Element, TRANSFORM(Types.Element, SELF.value := IF(RIGHT.x<>0,LEFT.Value*RIGHT.Value,LEFT.Value), SELF := LEFT),
				TRANSFORM(Types.Element, SELF.value := RIGHT1.Value*RIGHT2.Value, SELF := RIGHT2))[1].value;