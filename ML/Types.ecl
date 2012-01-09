IMPORT ML.mat as Mat;
EXPORT Types := MODULE

EXPORT t_RecordID := UNSIGNED;
EXPORT t_FieldNumber := UNSIGNED2;
EXPORT t_FieldReal := REAL8;
EXPORT t_FieldSign := INTEGER1;
EXPORT t_Discrete := UNSIGNED2; // The number of 'groups' a population may be divided into
EXPORT t_Item := UNSIGNED4; // Currently allows up to 9B different elements
EXPORT t_Count := t_RecordID; // Possible to count every record

EXPORT NumericField := RECORD
  t_RecordID id;
	t_FieldNumber number;
	t_FieldReal value;
  END;

EXPORT DiscreteField := RECORD
  t_RecordID id;
	t_FieldNumber number;
	t_Discrete value;
  END;

EXPORT ItemElement := RECORD
  t_Item value;
	t_RecordId id;
  END;
	
EXPORT ToMatrix(DATASET(NumericField) d):=FUNCTION
  RETURN PROJECT(d,TRANSFORM(Mat.Types.Element,SELF.x:=(TYPEOF(Mat.Types.Element.x))LEFT.id;SELF.y:=(TYPEOF(Mat.Types.Element.y))LEFT.number;SELF.value:=(TYPEOF(Mat.Types.Element.value))LEFT.value;));
END;

EXPORT FromMatrix(DATASET(Mat.Types.Element) d):=FUNCTION
  RETURN PROJECT(d,TRANSFORM(NumericField,SELF.id:=(TYPEOF(NumericField.id))LEFT.x;SELF.number:=(TYPEOF(NumericField.number))LEFT.y;SELF.value:=(TYPEOF(NumericField.value))LEFT.value;));
END;

END;