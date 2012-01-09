import ml;

value_record := RECORD
  unsigned rid;
  real height;
  real weight;
  real age;
  integer1 species;
  END;
                
d := dataset([
              {1,5*12+7,156*16,43,1},
              {2,5*12+7,128*16,31,1},
              {3,5*12+9,135*16,15,1},
              {4,5*12+7,145*16,14,1},
              {5,5*12-2,80*16,9,1},
              {6,4*12+8,72*16,8,1},
              {7,8,32,2.5,2},
              {8,6.5,28,2,2},
              {9,6.5,28,2,2},
              {10,6.5,21,2,2},
              {11,4,15,1,2},
              {12,3,10.5,1,2},
              {13,2.5,3,0.8,2},
              {14,1,1,0.4,2}                                                                                                      
             ]
             ,value_record);
output(d,named('d'));
                                                                                                                
// Turn into regular NumericField file (with continuous variables)
ml.ToField(d,o);
output(o,named('o'));

Simple := ML.FieldAggregates(o).Simple;
SimpleRanked := ML.FieldAggregates(o).SimpleRanked;

F1 := ML.FieldAggregates(o);
//output(F1,named('F1'));

SpeciesFeatureBuckets := sort(ML.FieldAggregates(o).Buckets(4),number,bucket);
output(SpeciesFeatureBuckets,named('SpeciesFeatureBuckets'));
ChickWeightBucketRanges := ML.FieldAggregates(o).BucketRanges(4);
output(sort(ChickWeightBucketRanges,number,bucket),named('ChickWeightBucketRanges'));

BucketRec := RECORD
  RECORDOF(SimpleRanked);
  ML.Types.t_Discrete bucket;
END;


BucketRec tAssign(SimpleRanked L,Simple R,ML.Types.t_Discrete n):=TRANSFORM
  SELF.bucket:=IF(L.value=R.maxval,n,(ML.Types.t_Discrete)(n*((L.value-R.minval)/(R.maxval-R.minval)))+1);
  SELF:=L;
END;

SpeciesFeatureBuckets2:=JOIN(SimpleRanked,Simple,LEFT.number=RIGHT.number,tAssign(LEFT,RIGHT,4),LOOKUP);

diff_rec := RECORD
  string1 LorR;
  BucketRec;
END;

diff_rec loadDiff( BucketRec brec, string1 LorR ) := TRANSFORM
   self.LorR := LorR;
   self := brec;
END;

diff_left_only := join(sort(SpeciesFeatureBuckets,id,number,value),sort(SpeciesFeatureBuckets2,id,number,value),left=right,loadDiff(left,'L'),LEFT ONLY, NOSORT);
diff_right_only := join(sort(SpeciesFeatureBuckets,id,number,value),sort(SpeciesFeatureBuckets2,id,number,value),left=right,loadDiff(right,'R'),RIGHT ONLY, NOSORT);
diff := diff_left_only + diff_right_only;
output(diff,named('diff'));
