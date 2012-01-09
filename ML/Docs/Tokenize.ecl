// A collection of routines dedicated to helping turn a stream of text into a set of working word elements
// Broken up into lots of steps; true ingest is messy - people will often need some hand-code
// But we will take them as far as we can
IMPORT Types FROM $;
IMPORT Std.Str AS Str;
EXPORT Tokenize := MODULE

// Just in case your documents do not have record ids; this will append them
EXPORT Enumerate(DATASET(Types.Raw) r) := PROJECT(r,TRANSFORM(Types.Raw,SELF.Id := COUNTER, SELF := LEFT));

// Properly cleaning a text stream for tokenization is a career in and of itself
// The below is adequate to prepare most text to create reasonable tokens

EXPORT Clean(DATASET(Types.Raw) r) := FUNCTION
	CleanForTokens(STRING s):=FUNCTION
		sRestrictChars:=' '+REGEXREPLACE('[^- A-Z0-9\']',Str.ToUpperCase(s),' ')+' ';
		sStripPunctEnds:=REGEXREPLACE('( -)|(- )|( \')|(\' )',sRestrictChars,' ');
		sRemoveNumberOnly:=REGEXREPLACE(' [-0-9\']+(?=[ ])',sStripPunctEnds,'');
		sRemoveSingleChars:=REGEXREPLACE(' [B-H,J-Z](?=[ ])',sRemoveNumberOnly,'');
		sCompressSpaces:=REGEXREPLACE('[ ]+',sRemoveSingleChars,' ');
		sNormalizePosessives:=REGEXREPLACE('\'S ',sCompressSpaces,' ');
		sSplitContraction01:=REGEXREPLACE('\'RE ',sNormalizePosessives,' ARE ');
		sSplitContraction02:=REGEXREPLACE('\'LL ',sSplitContraction01,' WILL ');
		sSplitContraction03:=REGEXREPLACE(' I\'M ',sSplitContraction02,' I AM ');
		RETURN sSplitContraction03;
	END;
	RETURN PROJECT(r,TRANSFORM(Types.Raw,SELF.Txt := CleanForTokens(LEFT.Txt), SELF := LEFT));
  END;
	
// This assumes that the incoming text is a set of nice space-separated strings
// Note : whilst not DISTRIBUTED - the data for one id WILL all be on the same node
EXPORT Split(DATASET(Types.Raw) r) := FUNCTION

	Types.WordElement Take(r le,Types.t_Position c) := 	TRANSFORM
		SELF.id := le.id;
		SELF.pos := c;
		SELF.word := Str.GetNthWord(le.Txt,c);
	END;

  RETURN NORMALIZE(r,Str.WordCount(LEFT.txt),Take(LEFT,COUNTER));

END;

// Construct a lexicon from a WordElement stream
// Note - this code is allowing for lexicon's bigger than main memory
// We could make this AND the ToO go a lot faster if we were prepared to assert small lexica (using ,FEW and ,LOOKUP)
EXPORT Lexicon(DATASET(Types.WordElement) r) := FUNCTION
	rec := RECORD
		Types.t_Count  total_words := COUNT(GROUP);
		r.Word;
		r.id;
	END;
	T := TABLE(r,rec,word,id,MERGE);
	rec2 := RECORD
		total_words := SUM(GROUP,T.total_words);
		total_docs  := COUNT(GROUP);
		t.Word;
	END;
	T1 := TABLE(T,rec2,Word,MERGE);
	S := SORT(T1,-total_docs,-total_words);
	RETURN PROJECT(S,TRANSFORM(Types.LexiconElement,SELF.word_id := COUNTER, SELF := LEFT));
END;

// This converts a regular variable-length record WordElement stream into a fixed length 'optimized' working stream
// If the lexicon has been 'pruned' then this could be lossy (removing stop words or low frequency words)
// Equally if two lexicon words have the SAME id; then multiple words could be mapped onto one
EXPORT ToO(DATASET(Types.WordElement) r, DATASET(Types.LexiconElement) l) := FUNCTION
  IMPORT ML; // A little ugly - should not really import down here
	Types.OWordElement take(r le, l ri) := TRANSFORM
		SELF.word := ri.word_id;
		SELF.total_words := ri.total_words;
		SELF.total_docs := ri.total_docs;
		SELF.words_in_doc := 0; // Flag as uncomputed
	  SELF := le;
	END;
	
	JWide := JOIN(r,l,LEFT.word = RIGHT.word,take(LEFT,RIGHT));
	JThin := JOIN(r,l,LEFT.word = RIGHT.word,take(LEFT,RIGHT),LOOKUP);
	// Have to guess at actual lexicon size ...
	RETURN IF ( COUNT(l)*100 < ML.Config.MaxLookup, JThin, JWide );
	
END;

// A macro to append the text back; not just a pure function - may want to append to different data streams
EXPORT macFromO(infile,idfield,textfield,lexicon,outfile) := MACRO
	#UNIQUENAME(app)
	TYPEOF(InFile) %app%(Infile le,lexicon ri) := TRANSFORM
		SELF.textfield := ri.word;
		SELF := le;
	END;

	outfile := JOIN(infile,lexicon,LEFT.idfield=RIGHT.word_id,%app%(LEFT,RIGHT));
	
  ENDMACRO;

EXPORT FromO(DATASET(Types.OWordElement) d,DATASET(Types.LexiconElement) l) := FUNCTION

	R := RECORD
		d;
		STRING word_text := '';
	END;
	T := TABLE(d,r);

	macFromO(t,word,word_text,l,o)

	RETURN PROJECT(o,TRANSFORM(Types.WordElement,SELF.word := LEFT.word_text,SELF := LEFT));
	
  END;

END;