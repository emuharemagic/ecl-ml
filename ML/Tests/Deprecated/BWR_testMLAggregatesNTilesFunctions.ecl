// 
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

SpeciesFeatureSimple := ML.FieldAggregates(o).Simple;
output(SpeciesFeatureSimple,named('SpeciesFeatureSimple'));

SpeciesFeatureRanked := ML.FieldAggregates(o).Ranked;
output(SpeciesFeatureRanked,named('SpeciesFeatureRanked'));

F1 := ML.FieldAggregates(o);
//output(F1,named('F1'));

SpeciesFeatureNTiles := ML.FieldAggregates(o).NTiles(4);
output(SpeciesFeatureNTiles,named('SpeciesFeatureNTiles'));
ChickWeightNTileRanges := ML.FieldAggregates(o).NTileRanges(4);
output(ChickWeightNTileRanges,named('ChickWeightNTileRanges'));
