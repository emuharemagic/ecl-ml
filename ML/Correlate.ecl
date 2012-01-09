IMPORT * FROM $;
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
  SELF.pearson := (le.e_xy - le.mean*ri.mean)/(le.sd*ri.sd);
  SELF := le;
  END;

  EXPORT Simple := JOIN(with_x,singles,LEFT.right_number=RIGHT.number,MakeCo(LEFT,RIGHT),LOOKUP);

OrderedPairRec := RECORD
	Types.t_RecordID  rid;
	Types.t_FieldNumber number;
  Types.t_FieldReal left_value;
	Types.t_FieldReal right_value;
	Types.t_FieldSign sign;
	END;

OrderedPairRec calculate_rank_direction(d le, d ri) := TRANSFORM
  SELF.rid := [];
  SELF.number := le.number;
  SELF.left_value := le.value;
	SELF.right_value := ri.value;
	SELF.sign := IF(le.value<ri.value, 1, IF(le.value=ri.value, 0, -1));
  END;
	
ordered_pairs := JOIN(d,d,LEFT.number=RIGHT.number AND LEFT.id<RIGHT.id,calculate_rank_direction(LEFT,RIGHT));

Utils.mac_SequenceInField(ordered_pairs, number, rid, sequenced_pairs);
//sequenced_pairs; 

PairRec := RECORD
	Types.t_RecordID  rid;
  Types.t_FieldNumber left_number;
	Types.t_FieldNumber right_number;
	Types.t_FieldSign   sign;
	END;

PairRec calculate_cordance(OrderedPairRec le, OrderedPairRec ri) := TRANSFORM
  SELF.rid := le.rid;
  SELF.left_number := le.number;
	SELF.right_number := ri.number;
	SELF.sign := le.sign*ri.sign;
  END;

pairs := JOIN(sequenced_pairs,sequenced_pairs, LEFT.rid=RIGHT.rid AND LEFT.number<RIGHT.number,calculate_cordance(LEFT,RIGHT));
//pairs;

PairAccum := RECORD
  pairs.left_number;
	pairs.right_number;
	kendall_tau := AVE(GROUP,pairs.sign);
  END;
	
EXPORT Kendall := TABLE(pairs,PairAccum,left_number,right_number,FEW);	
	
END;