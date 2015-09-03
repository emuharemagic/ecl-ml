IMPORT ML.mat as Mat;
EXPORT Types := MODULE

// The t_RecordID and t_FieldNumber are native datatypes of the ML libraries and they currently
// allow for a quadrillion rows with 9B features. If your needs are lower, then making these two smaller
// will improve performance. In particular an unsigned4 for recordid allows 9B rows and an unsigned2 
// for number allows 64K features.
//
// Some ML modules such as Classify.Logistic, Cluster, Distribution, IRegression and Sampling, also require 
// use of the matrix libraries; When these ML modules are used, and the number of rows exceeds 9B, 
// the ML.Mat.Types.t_Index type needs to be compatible with the t_RecordID type.
EXPORT t_RecordID := UNSIGNED;
EXPORT t_FieldNumber := UNSIGNED4;
EXPORT t_FieldReal := REAL8;
EXPORT t_FieldSign := INTEGER1;
EXPORT t_Discrete := INTEGER4; // The number of 'groups' a population may be divided into - negative to allow for classificaiton to 'undershoot'
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

EXPORT l_result := RECORD(DiscreteField)
  REAL8 conf;  // Confidence - high is good
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