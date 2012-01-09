IMPORT * FROM $;
IMPORT ML,ML.TestML, ML.Types;

output(TestML.ChickWeight,named('CheckWeight'));
/* ChickWeight dataset RECORD
ChickWeightRec := RECORD
  unsigned weight;
  unsigned Time;
  unsigned Chick;
  unsigned Diet;
END;
*/

NumericField := ML.Types.NumericField;
/* THE ABOVE RECORD LAYOUT LOOKS LIKE THE FOLLOWING:
EXPORT NumericField := RECORD
  t_RecordID id;
  t_FieldNumber number;
  t_FieldReal value;
  END;
*/

applyFieldAggregatesFunctionAndOutput(inDS, func, func_name,outDS ) := MACRO
  outDS := ML.FieldAggregates(inDS).func;
  output(outDS,named('Applied_'+func_name));
ENDMACRO;

ml.ToField(TestML.ChickWeight,BothWeightAndTimeMLNumericField);
output(sort(BothWeightAndTimeMLNumericField,id,number),named('BothWeightAndTimeMLNumericField'));

applyFieldAggregatesFunctionAndOutput(BothWeightAndTimeMLNumericField,Simple,'Simple',SimpleBothWeightAndTimeMLNumericField);
applyFieldAggregatesFunctionAndOutput(BothWeightAndTimeMLNumericField,SimpleRanked,'SimpleRanked',SimpleRankedChickWeights);

MR := RECORD
  SimpleRankedChickWeights.Number;
  SimpleRankedChickWeights.Value;
  Types.t_FieldReal Pos := AVE(GROUP,SimpleRankedChickWeights.Pos);
  UNSIGNED valcount:=COUNT(GROUP);
END;

T := TABLE(SimpleRankedChickWeights,MR,Number,Value);
output(T,named('T'));

dModeVals:=TABLE(T,{number;UNSIGNED modeval:=MAX(GROUP,valcount);},number,FEW);
output(dModeVals,named('dModeVals'));

Modes:=
        JOIN(
             T
             , dModeVals
             , LEFT.number=RIGHT.number 
               AND LEFT.valcount=RIGHT.modeval
             , TRANSFORM(
                         {TYPEOF(T.number) number;TYPEOF(T.value) mode;TYPEOF(T.valcount) valcount;}
                         ,SELF.mode:=LEFT.value
												 ,SELF.valcount := LEFT.valcount
                         ,SELF:=LEFT
               )
             , LOOKUP
        );
output(Modes,named('Modes'));

applyFieldAggregatesFunctionAndOutput(BothWeightAndTimeMLNumericField,Modes,'Modes',ModesBothWeightAndTimeMLNumericField);

/*
applyFieldAggregatesFunctionAndOutput(BothWeightAndTimeMLNumericField,Medians,'Medians',MediansBothWeightAndTimeMLNumericField);
applyFieldAggregatesFunctionAndOutput(BothWeightAndTimeMLNumericField,Cardinality,'Cardinality',CardinalityBothWeightAndTimeMLNumericField);
applyFieldAggregatesFunctionAndOutput(BothWeightAndTimeMLNumericField,Ranked,'Ranked',RankedBothWeightAndTimeMLNumericField);
*/
//x := ML.FieldAggregates.
/*
applyFieldAggregatesFunctionAndOutput(BothChickAndDietMLDiscreteField,Buckets,'Buckets',BucketsBothChickAndDietMLDiscreteField);
applyFieldAggregatesFunctionAndOutput(BothChickAndDietMLDiscreteField,BucketRanges,'BucketRanges',BucketRangesBothChickAndDietMLDiscreteField);
applyFieldAggregatesFunctionAndOutput(BothChickAndDietMLDiscreteField,NTiles,'NTiles',NTilesBothChickAndDietMLDiscreteField);
applyFieldAggregatesFunctionAndOutput(BothChickAndDietMLDiscreteField,NTileRanges,'NTileRanges',NTileRangesBothChickAndDietMLDiscreteField);
*/
