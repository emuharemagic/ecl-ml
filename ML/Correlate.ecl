﻿IMPORT * FROM $;
EXPORT Correlate(DATASET(Types.NumericField) d) := MODULE
  Singles := FieldAggregates(d).Simple;

PairRec := RECORD
  Types.t_FieldNumber left_number;
	Types.t_FieldNumber right_number;
	Types.t_fieldreal   xy;
	END;

PairRec note_prod(d le, d ri) := TRANSFORM
  SELF.left_number := le.number;
	SELF.right_number := ri.number;
	SELF.xy := le.value*ri.value;
  END;
	
pairs := JOIN(d,d,LEFT.id=RIGHT.id AND LEFT.number<RIGHT.number,note_prod(LEFT,RIGHT));

PairAccum := RECORD
  pairs.left_number;
	pairs.right_number;
	e_xy := AVE(GROUP,pairs.xy);
  END;
	
exys := TABLE(pairs,PairAccum,left_number,right_number,FEW); // Few will die for VERY large numbers of variables ...	

with_x := JOIN(exys,singles,LEFT.left_number = RIGHT.number,LOOKUP);

CoRec := RECORD
  Types.t_fieldnumber left_number;
	Types.t_fieldnumber right_number;
	Types.t_fieldreal   covariance;
	Types.t_fieldreal   pearson;
  END;

CoRec MakeCo(with_x le, singles ri) := TRANSFORM
  SELF.covariance := (le.e_xy - le.mean*ri.mean);
  SELF.pearson := (le.e_xy - le.mean*ri.mean)/SQRT(le.var)*SQRT(ri.var);
  SELF := le;
  END;

  EXPORT Simple := JOIN(with_x,singles,left.right_number=right.number,MakeCo(left,right),LOOKUP);
	
END;