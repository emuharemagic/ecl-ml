/*****************************************************
* Text Clasifying using NAIVE BAYES CLASSIFIER
******************************************************/
IMPORT ML;

TrainingSetRec := RECORD
  ML.Docs.Types.Raw;
  unsigned class := 0;
END;

StopWords_src := DATASET([{0,'A THE'}], ML.Docs.Types.Raw);

DATASET(ML.Docs.Types.LexiconElement) Vocabulary(DATASET(ML.Docs.Types.Raw) vocab_src) := FUNCTION

	ML.Docs.Types.LexiconElement AddOne(ML.Docs.Types.LexiconElement L) := TRANSFORM
	//Increases word_id value by 1
	//This is so the number "1" can be used for the dependent sentiment variable
		SELF.word_id := L.word_id + 1;
		SELF := L;
	END;

	vWords0 := ML.Docs.Tokenize.Split(ML.Docs.Tokenize.Clean(vocab_src));
	stopWords := ML.Docs.Tokenize.Split(ML.Docs.Tokenize.Clean(StopWords_src));
	vWords := JOIN(vWords0,stopWords,LEFT.word = RIGHT.word,LEFT ONLY);
	//Create Vocabulary
	Lexicon := PROJECT(ML.Docs.Tokenize.Lexicon(vWords),AddOne(LEFT));
	RETURN Lexicon;
END;

DATASET(ML.Types.NumericField) Learn(DATASET(TrainingSetRec) ts, DATASET(ML.Docs.Types.LexiconElement) vocab) := FUNCTION	
	ML.Types.NumericField ToIndep(ML.Docs.Types.OWordElement L) := TRANSFORM
	//Takes relevant data from ML.Docs.Trans.Wordsbag
	//and converts to numericfield
		SELF.id := L.id;
		SELF.number := L.word;
		//Depending on NB Model value is either words_in_doc (term frequency) or 1 (term presence)
		//SELF.value := L.words_in_doc;
		SELF.value := 1;		
	END;

	ML.Types.NumericField ToDep(TrainingSetRec L) := TRANSFORM
	// to extract document ids and sentiment values to a numericfield
		SELF.id := L.id;
		SELF.number := 1;
		SELF.value := L.class;
	END;

	tsRaw := PROJECT(ts,TRANSFORM(ML.Docs.Types.Raw,SELF.id := LEFT.id;SELF := LEFT));
	tsWords := ML.Docs.Tokenize.Split(ML.Docs.Tokenize.Clean(tsRaw));

	//Create Wordbags
	tsO := ML.Docs.Tokenize.ToO(tsWords,Vocab);
	tsBag := SORT(ML.Docs.Trans(tsO).WordBag,id,word);
	//Train Classifier
	tsIndep := PROJECT(tsBag,ToIndep(LEFT));
  dtsIndep := ML.Discretize.ByRounding(tsIndep);

	tsDep := PROJECT(ts,ToDep(LEFT));
  dtsDep := ML.Discretize.ByRounding(tsDep);

	RETURN ML.Classify.NaiveBayes.LearnD(dtsIndep,dtsDep);
END;

Test(DATASET(TrainingSetRec) ts, DATASET(ML.Docs.Types.LexiconElement) vocab) := FUNCTION	
	ML.Types.NumericField ToIndep(ML.Docs.Types.OWordElement L) := TRANSFORM
	//Takes relevant data from ML.Docs.Trans.Wordsbag
	//and converts to numericfield
		SELF.id := L.id;
		SELF.number := L.word;
		//Depending on NB Model value is either words_in_doc (term frequency) or 1 (term presence)
		//SELF.value := L.words_in_doc;
		SELF.value := 1;		
	END;

	ML.Types.NumericField ToDep(TrainingSetRec L) := TRANSFORM
	// to extract document ids and sentiment values to a numericfield
		SELF.id := L.id;
		SELF.number := 1;
		SELF.value := L.class;
	END;

	tsRaw := PROJECT(ts,TRANSFORM(ML.Docs.Types.Raw,SELF.id := LEFT.id;SELF := LEFT));
	tsWords := ML.Docs.Tokenize.Split(ML.Docs.Tokenize.Clean(tsRaw));

	//Create Wordbags
	tsO := ML.Docs.Tokenize.ToO(tsWords,Vocab);
	tsBag := SORT(ML.Docs.Trans(tsO).WordBag,id,word);
	//Train Classifier
	tsIndep := PROJECT(tsBag,ToIndep(LEFT));
  dtsIndep := ML.Discretize.ByRounding(tsIndep);

	tsDep := PROJECT(ts,ToDep(LEFT));
  dtsDep := ML.Discretize.ByRounding(tsDep);

	RETURN ML.Classify.NaiveBayes.TestD(dtsIndep,dtsDep);
END;

Classify(DATASET(ML.Docs.Types.Raw) T, DATASET(ML.Types.NumericField) model, DATASET(ML.Docs.Types.LexiconElement) vocab) := FUNCTION
	
	ML.Types.NumericField ToIndep(ML.Docs.Types.OWordElement L) := TRANSFORM
	//Takes relevant data from ML.Docs.Trans.Wordbag
	//and converts to numericfield
		SELF.id := L.id;
		SELF.number := L.word;
		//Depending on NB Model value is either words_in_doc (term frequency) or 1 (term presence)
		//SELF.value := L.words_in_doc;
		SELF.value := 1;
	END;

	//Pre-Process text
	dTokens	:= ML.Docs.Tokenize.Split(ML.Docs.Tokenize.Clean(T));

	//Create Wordbag with Vocabulary
	t0	:= ML.Docs.Tokenize.ToO(dTokens,Vocab);
	tBag := SORT(ML.Docs.Trans(t0).WordBag,id,word);

	//Classify text with model
	nfIndep := PROJECT(tBag,ToIndep(LEFT));
	dfIndep := ML.Discretize.ByRounding(nfIndep);
	Result := ML.Classify.NaiveBayes.ClassifyD(dfIndep,Model);
	RETURN Result;
END;

TrainingSet := DATASET([{1,'CHINESE BEIJING CHINESE',1},
												{2,'CHINESE CHINESE SHANGHAI',1},
												{3,'CHINESE MACAO',1},
												{4,'TOKYO JAPAN CHINESE',0}],TrainingSetRec);
										
TrainingSet;

TestSet := DATASET([{1,'CHINESE CHINESE TOKYO JAPAN'}],ML.Docs.Types.Raw);

// Build Vocabulary based on the training set
VocabSrc:=TrainingSet;
VocabSrcAnnotated := PROJECT(VocabSrc,TRANSFORM(ML.Docs.Types.Raw,SELF.id := COUNTER;SELF.txt := LEFT.txt));
Vocab := Vocabulary(VocabSrcAnnotated);
OUTPUT(Vocab, named('Vocab'));
TextModule := Test(TrainingSet, Vocab);

OUTPUT(TextModule.CrossAssignments, named('CrossAssignments'));
//RecallByClass, it returns the percentage of instances belonging to a class that was correctly classified,
//               also know as True positive rate and sensivity, TP/(TP+FN).
OUTPUT(TextModule.RecallByClass, named('RecallByClass'));
//PrecisionByClass, returns the percentage of instances classified as a class that really belong to this class: TP /(TP + FP).
OUTPUT(TextModule.PrecisionByClass, named('ArsonPrecisionByClass'));
//FP_Rate_ByClass, it returns the percentage of instances not belonging to a class that were incorrectly classified as this class,
//                 also known as False Positive rate FP / (FP + TN).
OUTPUT(TextModule.FP_Rate_ByClass, named('FP_Rate_ByClass'));
// Accuracy, it returns the percentage of instances correctly classified (total, without class distinction)
OUTPUT(TextModule.Accuracy, named('Accuracy'));


mText0 := Learn(TrainingSet, Vocab);
mText := ML.Classify.NaiveBayes.Model(mText0);
OUTPUT(mText,named('TextModel'));

Classification_result := Classify(TestSet, mText0, Vocab);
Classification_result;

