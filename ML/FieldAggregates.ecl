IMPORT * FROM $;
EXPORT FieldAggregates(DATASET(Types.NumericField) d) := MODULE

SingleField := RECORD
  d.number;
	Types.t_fieldreal minval  :=MIN(GROUP,d.Value);
	Types.t_fieldreal maxval  :=MAX(GROUP,d.Value);
	Types.t_fieldreal sumval  :=SUM(GROUP,d.Value);
  Types.t_fieldreal countval:=COUNT(GROUP);
	Types.t_fieldreal mean    :=AVE(GROUP,d.Value);
	Types.t_fieldreal var     :=VARIANCE(GROUP,d.Value);
	Types.t_fieldreal sd      :=SQRT(VARIANCE(GROUP,d.Value));
END;

EXPORT Simple:=TABLE(d,SingleField,Number,FEW);

RankableField := RECORD
  d;
	UNSIGNED Pos := 0;
  END;

T := TABLE(SORT(D,Number,Value),RankableField);

Utils.mac_SequenceInField(T,Number,Pos,P)

EXPORT SimpleRanked := P;

dMedianPos:=TABLE(SimpleRanked,{number;SET OF UNSIGNED pos:=IF(MAX(GROUP,pos)%2=0,[MAX(GROUP,pos)/2],[(MAX(GROUP,pos)-1)/2,(MAX(GROUP,pos)-1)/2+1]);},number,FEW);
dMedianValues:=JOIN(SimpleRanked,dMedianPos,LEFT.number=RIGHT.number AND LEFT.pos IN RIGHT.pos,TRANSFORM({RECORDOF(SimpleRanked) AND NOT [id,pos];},SELF:=LEFT;),LOOKUP);
EXPORT Medians:=TABLE(dMedianValues,{number;TYPEOF(dMedianValues.value) median:=IF(COUNT(GROUP)=1,MIN(GROUP,value),SUM(GROUP,value)/2);},number,FEW);

{RECORDOF(SimpleRanked);Types.t_Discrete bucket;} tAssign(SimpleRanked L,Simple R,Types.t_Discrete n):=TRANSFORM
  SELF.bucket:=IF(L.value=R.maxval,n,(Types.t_Discrete)(n*((L.value-R.minval)/(R.maxval-R.minval)))+1);
  SELF:=L;
END;

EXPORT Buckets(Types.t_Discrete n):=JOIN(SimpleRanked,Simple,LEFT.number=RIGHT.number,tAssign(LEFT,RIGHT,n),LOOKUP);
EXPORT BucketRanges(Types.t_Discrete n):=TABLE(Buckets(n),{number;bucket;Types.t_fieldreal Min:=MIN(GROUP,value);Types.t_fieldreal Max:=MAX(GROUP,value);UNSIGNED cnt:=COUNT(GROUP);},number,bucket);

MR := RECORD
  SimpleRanked.Number;
	SimpleRanked.Value;
	Types.t_FieldReal Pos := AVE(GROUP,SimpleRanked.Pos);
  UNSIGNED valcount:=COUNT(GROUP);
END;

SHARED T := TABLE(SimpleRanked,MR,Number,Value);

dModeVals:=TABLE(T,{number;UNSIGNED modeval:=MAX(GROUP,valcount);},number,FEW);
EXPORT Modes:=JOIN(T,dModeVals,LEFT.number=RIGHT.number AND LEFT.valcount=RIGHT.modeval,TRANSFORM({TYPEOF(T.number) number;TYPEOF(T.value) mode; UNSIGNED cnt},SELF.cnt := LEFT.valcount;SELF.mode:=LEFT.value;SELF:=LEFT;),LOOKUP);

EXPORT Cardinality:=TABLE(T,{number;UNSIGNED cardinality:=COUNT(GROUP);},number);

AveRanked := 	RECORD
  d;
	Types.t_FieldReal Pos;
  END;
	
AveRanked Into(D le,T ri) := 	TRANSFORM
  SELF.Pos := ri.pos;
  SELF := le;
  END;
	
EXPORT Ranked := JOIN(D,T,LEFT.Number=RIGHT.Number AND LEFT.Value = RIGHT.Value,Into(LEFT,RIGHT));	

{RECORDOF(Ranked);Types.t_Discrete ntile;} tNTile(Ranked L,Simple R,Types.t_Discrete n):=TRANSFORM
  SELF.ntile:=IF(L.pos=R.countval,n,(Types.t_Discrete)(n*(L.pos/R.countval))+1);
  SELF:=L;
END;
EXPORT NTiles(Types.t_Discrete n):=JOIN(Ranked,Simple,LEFT.number=RIGHT.number,tNTile(LEFT,RIGHT,n),LOOKUP);
EXPORT NTileRanges(Types.t_Discrete n):=TABLE(NTiles(n),{number;ntile;Types.t_fieldreal Min:=MIN(GROUP,value);Types.t_fieldreal Max:=MAX(GROUP,value);UNSIGNED cnt:=COUNT(GROUP);},number,ntile);

  END;