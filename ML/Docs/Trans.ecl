// This module exists to perform internal transforms upon an OWordElement stream
// Internal means 'only from information already there' - more sophisticated work is done elsewhere
IMPORT Types FROM $;

EXPORT Trans(DATASET(Types.OWordElement) j) := MODULE


// The word-bag representation loses the primary meaning of the position element (although it is retained as 'first')
// The document gains the count of 
EXPORT WordBag := FUNCTION

	R := RECORD
		j.id;
		Types.t_Position pos := MIN(GROUP,j.pos);
		j.word;
		Types.t_Count total_words := MAX(GROUP,j.total_words);
		Types.t_Count total_docs := MAX(GROUP,j.total_docs);
		Types.t_Count words_in_doc := COUNT(GROUP);
	END;
	RETURN PROJECT( TABLE(J,R,id,word,MERGE), TRANSFORM(Types.OWordElement,SELF := LEFT));

END;
	
EXPORT WordsCounted := FUNCTION
	
	T := WordBag(words_in_doc>1); // Simple device to reduce size of RHS in join (combined with left outer)
	
	Types.OWordElement take_wid(j le, t ri) := TRANSFORM
		SELF.words_in_doc := IF ( ri.words_in_doc <> 0, ri.words_in_doc, 1 );
	  SELF := le;
	END;

	RETURN JOIN(J,T,LEFT.word=RIGHT.word AND LEFT.id=RIGHT.id,take_wid(LEFT,RIGHT),LEFT OUTER);

END;

// Simple keyword harvester: Takes a word stream, determine the TF-IDF value
// for every id/word combination and then returns those that are above the
// thresholds passed in as parameters.
// nLowThreshold is the TF-IDF value a word must be above to be kept.
// iLowDocCount is the least number of documents a word must appear in to
// qualify as a keyword candidate
EXPORT TfIdf(REAL nLowThreshold=.05,UNSIGNED iLowDocCount=200):=FUNCTION
  dWordsRestricted:=WordsCounted(total_docs<iLowDocCount);
  iDocCount:=COUNT(TABLE(dWordsRestricted,{id},id,MERGE));
  dDocWordCount:=TABLE(j,{id;UNSIGNED doc_word_count:=COUNT(GROUP);},id,MERGE);
  dWithoutPos:=TABLE(dWordsRestricted,{id;word;total_docs;words_in_doc;},id,word,total_docs,words_in_doc);
  {Types.t_DocId id;Types.t_WordID word_id;Types.t_Value tf_idf;} tGetTfIdf(dWithoutPos L,dDocWordCount R):=TRANSFORM
    SELF.id:=L.id;
    SELF.word_id:=L.word;
    SELF.tf_idf:=((REAL)L.words_in_doc/(REAL)R.doc_word_count)*(LOG((REAL)iDocCount/(REAL)L.total_docs)/LOG(2));
  END;
  RETURN JOIN(dWithoutPos,dDocWordCount,LEFT.id=RIGHT.id,tGetTfIdf(LEFT,RIGHT))(tf_idf>nLowThreshold);
END;

END;