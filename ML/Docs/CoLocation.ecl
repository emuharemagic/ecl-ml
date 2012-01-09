IMPORT $ AS Docs;
IMPORT Std.Str AS str;
EXPORT CoLocation:=MODULE

  EXPORT Words(DATASET(Docs.Types.Raw) d):=Docs.Tokenize.Split(Docs.Tokenize.Clean(d));
  EXPORT Lexicon(DATASET(Docs.Types.WordElement) Words):=Docs.Tokenize.Lexicon(Words);

  EXPORT AllNGramsLayout:={Docs.Types.t_DocId id;Docs.Types.t_Word ngram;};

  //-------------------------------------------------------------------------
  // Dataset containing all unique id/ngram combinations. User passes in the
  // following parameters:
  //   Words  : The output of a Tokenize.Split call
  //   Lexicon: [OPTIONAL] If the user desires integer substitution, this is 
  //            the output of a Tokenize.Lexicon call using the Words dataset
  //            as input.
  //   n      : [OPTIONAL] The Maximum wordcount for the N-Grams.  Default is 3
  //-------------------------------------------------------------------------
  EXPORT AllNGramsLayout AllNGrams(DATASET(Docs.Types.WordElement) Words,DATASET(Docs.Types.LexiconElement) Lexicon=DATASET([],Docs.Types.LexiconElement),UNSIGNED n=3):=FUNCTION
    IntegerSubstitution:=COUNT(Lexicon)>0;
    WithIDs:=JOIN(Words,Lexicon,LEFT.word=RIGHT.word,TRANSFORM({Docs.Types.WordElement;Docs.Types.t_WordId word_id;},SELF:=RIGHT;SELF:=LEFT;));
    WordData:=IF(IntegerSubstitution,WithIDs,TABLE(Words,{Words;Docs.Types.t_WordId word_id:=0;}));
    IDSorted:=SORT(DISTRIBUTE(WordData,id),id,pos,LOCAL);

    RECORDOF(IDSorted) tDeNorm(IDSorted L,IDSorted R,UNSIGNED C):=TRANSFORM
      SELF.word:=IF(C=1,IF(IntegerSubstitution,(STRING)L.word_id,L.word),L.word+' '+IF(IntegerSubstitution,(STRING)R.word_id,R.word));
      SELF:=L;
    END;
    LongPhrases:=DENORMALIZE(IDSorted,IDSorted,LEFT.id=RIGHT.id AND LEFT.pos>RIGHT.pos-n AND LEFT.pos<=RIGHT.pos,tDeNorm(LEFT,RIGHT,COUNTER),LOCAL);
    LongDedup:=TABLE(LongPhrases,{id;STRING ngram:=word;},id;word,LOCAL);

    AllNGramsLayout tNorm(LongDedup L,UNSIGNED C):=TRANSFORM
      SELF.ngram:=IF(C=n,L.ngram,L.ngram[..str.Find(L.ngram,' ',C)-1]);
      SELF:=L;
    END;
    EveryNGram:=NORMALIZE(LongDedup,n,tNorm(LEFT,COUNTER));
    RETURN PROJECT(TABLE(EveryNGram,{id;ngram},id,ngram,LOCAL)(ngram!=''),AllNGramsLayout);
  END;
  
  //-------------------------------------------------------------------------
  // Support for a set of ngrams is the ratio of the number of documents that
  // contain all the items in the set compared to the total document count.
  //-------------------------------------------------------------------------
  EXPORT Docs.Types.t_value Support(SET OF STRING ssNGrams,DATASET(AllNGramsLayout) d):=FUNCTION
    iDocumentCount:=COUNT(DEDUP(d,id));
    iFilteredCount:=COUNT(TABLE(d(ngram IN ssNGrams),{id;UNSIGNED c:=COUNT(GROUP);},id,MERGE)(c=COUNT(ssNGrams)));
    RETURN (Docs.Types.t_value)(iFilteredCount)/(Docs.Types.t_value)(iDocumentCount);
  END;

  //-------------------------------------------------------------------------
  // Confidence is the ratio of the support of two sets of ngrams compared
  // to the support of the first set only.
  //-------------------------------------------------------------------------
  EXPORT Docs.Types.t_value Confidence(SET OF STRING ss01,SET OF STRING ss02,DATASET(AllNGramsLayout) d):=FUNCTION
    nSupport01:=Support(ss01,d);
    nSupportTotal:=Support(ss01+ss02,d);
    RETURN nSupportTotal/nSupport01;
  END;
  
  //-------------------------------------------------------------------------
  // Lift is similar to Confidence, but the denominator is the product of
  // the supports for each of the two subsets
  //-------------------------------------------------------------------------
  EXPORT Docs.Types.t_value Lift(SET OF STRING ss01,SET OF STRING ss02,DATASET(AllNGramsLayout) d):=FUNCTION
    nSupport01:=Support(ss01,d);
    nSupport02:=Support(ss02,d);
    nSupportTotal:=Support(ss01+ss02,d);
    RETURN nSupportTotal/(nSupport01*nSupport02);
  END;

  //-------------------------------------------------------------------------
  // Conviction is the ratio of (1 - the support for the second set) compared
  // to (1 - the confidence of the two sets)
  //-------------------------------------------------------------------------
  EXPORT Docs.Types.t_value Conviction(SET OF STRING ss01,SET OF STRING ss02,DATASET(AllNGramsLayout) d):=FUNCTION
    nConfidence:=Confidence(ss01,ss02,d);
    nSupport02:=Support(ss02,d);
    RETURN (1-nSupport02)/(1-nConfidence);
  END;

  EXPORT NGramsLayout:={Docs.Types.t_Word ngram;Docs.Types.t_Count doccount;Docs.Types.t_Value pct;Docs.Types.t_Value idf;};

  //-------------------------------------------------------------------------
  // All unique ngrams in the corpus, with aggregate information
  //-------------------------------------------------------------------------
  EXPORT NGramsLayout NGrams(DATASET(AllNGramsLayout) d01):=FUNCTION
    DocumentCount:=COUNT(TABLE(d01,{id},id));
    NGramsAggregated:=TABLE(d01,{ngram;UNSIGNED doccount:=COUNT(GROUP);},ngram,MERGE);
    RETURN PROJECT(NGramsAggregated,TRANSFORM(NGramsLayout,SELF.pct:=(REAL)LEFT.doccount/(REAL)DocumentCount;SELF.idf:=LOG((REAL)DocumentCount/(REAL)LEFT.doccount);SELF:=LEFT;));
  END;

  //-------------------------------------------------------------------------
  // Comparison between the ngram pct and the product of the pcts of each
  // unigram that comprises it
  //-------------------------------------------------------------------------
  EXPORT SubGrams(DATASET(NGramsLayout) d):=FUNCTION
    MaxN:=MAX(TABLE(d,{UNSIGNED c:=str.WordCount(ngram);}),c);
    Unigrams:=d(Str.Find(ngram,' ')=0);
    WithComponent:=TABLE(d,{d;Docs.Types.t_Value components:=0;});

    RECORDOF(WithComponent) tJoin(WithComponent L,Unigrams R,UNSIGNED C):=TRANSFORM
      SELF.components:=IF(L.components=0,1,L.components)*IF(R.pct=0,1,R.pct);
      SELF:=L;
    END;
    RETURN LOOP(WithComponent,MaxN,JOIN(ROWS(LEFT),Unigrams,Str.GetNthWord(LEFT.ngram,COUNTER)=RIGHT.ngram,tJoin(LEFT,RIGHT,COUNTER),LOOKUP));
  END;

  //-------------------------------------------------------------------------
  // Comparison of each ngram to the pcts of any one unigram and the remainder
  //-------------------------------------------------------------------------
  EXPORT SplitCompare(DATASET(NGramsLayout) d):=FUNCTION
    SplitLayout:={RECORDOF(Ngrams);STRING ngram1;STRING ngram2;REAL pct1;REAL pct2;};
    SplitLayout tSplit(NGramsLayout L,UNSIGNED C):=TRANSFORM
      SELF.ngram1:=IF(C=0,'',L.ngram[..str.Find(L.ngram,' ',C-1)])+IF(C=str.WordCount(L.ngram),'',L.ngram[str.Find(L.ngram,' ',C)+1..]);
      SELF.ngram2:=L.ngram[str.Find(L.ngram,' ',C-1)+1..IF(C=str.WordCount(L.ngram),LENGTH(L.ngram),str.Find(L.ngram,' ',C)-1)];
      SELF:=L;
      SELF:=[];
    END;
    Split:=NORMALIZE(d(str.Find(ngram,' ')>0),str.wordcount(LEFT.ngram),tSplit(LEFT,COUNTER));
    SplitJoin01:=JOIN(Split,d,LEFT.ngram1=RIGHT.ngram,TRANSFORM(SplitLayout,SELF.pct1:=RIGHT.pct;SELF:=LEFT;));
    RETURN JOIN(SplitJoin01,d,LEFT.ngram2=RIGHT.ngram,TRANSFORM(SplitLayout,SELF.pct2:=RIGHT.pct;SELF:=LEFT;));
  END;

  //-------------------------------------------------------------------------
  // If integer substitution was used, this will revert a string of space-
  // separated integers back to their word-based ngrams.
  //-------------------------------------------------------------------------
  EXPORT STRING ShowPhrase(DATASET(Docs.Types.LexiconElement) Lexicon,STRING s):=FUNCTION
    d01:=DATASET([{0,s}],{UNSIGNED pos;STRING s;});
    d02:=NORMALIZE(d01,Str.WordCount(s),TRANSFORM(RECORDOF(d01),SELF.pos:=COUNTER;SELF.s:=Str.GetNThWord(LEFT.s,COUNTER);));
    d03:=SORT(JOIN(d02,Lexicon,(UNSIGNED)LEFT.s=RIGHT.word_id,TRANSFORM(RECORDOF(d02),SELF.s:=RIGHT.word;SELF:=LEFT;)),pos);
    d04:=ROLLUP(d03,LEFT.pos!=RIGHT.pos,TRANSFORM(RECORDOF(d03),SELF.s:=LEFT.s+' '+RIGHT.s;SELF:=RIGHT;));
    RETURN d04[1].s;
  END;

END;