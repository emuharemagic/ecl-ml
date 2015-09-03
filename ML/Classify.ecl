IMPORT ML;
IMPORT * FROM $;
IMPORT $.Mat;
IMPORT * FROM ML.Types;
IMPORT PBblas;
IMPORT ML.SVM;
Layout_Cell := PBblas.Types.Layout_Cell;

/*
		The object of the classify module is to generate a classifier.
    A classifier is an 'equation' or 'algorithm' that allows the 'class' of an object to be imputed based upon other properties
    of an object.
*/

EXPORT Classify := MODULE

SHARED l_result := Types.l_result;

SHARED l_model := RECORD
  Types.t_RecordId    id := 0; 			// A record-id - allows a model to have an ordered sequence of results
	Types.t_FieldNumber number;				// A reference to a feature (or field) in the independants
	Types.t_Discrete    class_number; // The field number of the dependant variable
	REAL8 w;
END;

// Function to compute the efficacy of a given classification process
// Expects the dependents (classification tags deemed to be true)
// Computeds - classification tags created by the classifier
EXPORT Compare(DATASET(Types.DiscreteField) Dep,DATASET(l_result) Computed) := MODULE
	DiffRec := RECORD
		Types.t_FieldNumber classifier;  // The classifier in question (value of 'number' on outcome data)
		Types.t_Discrete  c_actual;      // The value of c provided
		Types.t_Discrete  c_modeled;		 // The value produced by the classifier
		Types.t_FieldReal score;         // Score allocated by classifier
	END;
	DiffRec  notediff(Computed le,Dep ri) := TRANSFORM
	  SELF.c_actual := ri.value;
		SELF.c_modeled := le.value;
		SELF.score := le.conf;
		SELF.classifier := ri.number;
	END;
	SHARED J := JOIN(Computed,Dep,LEFT.id=RIGHT.id AND LEFT.number=RIGHT.number,notediff(LEFT,RIGHT));
	// Building the Confusion Matrix
	SHARED ConfMatrix_Rec := RECORD
		Types.t_FieldNumber classifier;	// The classifier in question (value of 'number' on outcome data)
		Types.t_Discrete c_actual;			// The value of c provided
    Types.t_Discrete c_modeled;			// The value produced by the classifier
		Types.t_FieldNumber Cnt:=0;			// Number of occurences
	END;
	SHARED class_cnt := TABLE(Dep,{classifier:= number, c_actual:= value, Cnt:= COUNT(GROUP)},number, value, FEW); // Looking for class values
	ConfMatrix_Rec form_cfmx(class_cnt le, class_cnt ri) := TRANSFORM
	  SELF.classifier := le.classifier;
		SELF.c_actual:= le.c_actual;
		SELF.c_modeled:= ri.c_actual;
	END;
	SHARED cfmx := JOIN(class_cnt, class_cnt, LEFT.classifier = RIGHT.classifier, form_cfmx(LEFT, RIGHT)); // Initialzing the Confusion Matrix with 0 counts
	SHARED cross_raw := TABLE(J,{classifier,c_actual,c_modeled,Cnt := COUNT(GROUP)},classifier,c_actual,c_modeled,FEW); // Counting ocurrences
	ConfMatrix_Rec form_confmatrix(ConfMatrix_Rec le, cross_raw ri) := TRANSFORM
		SELF.Cnt	:= ri.Cnt;
		SELF 			:= le;
	END;
//CrossAssignments, it returns information about actual and predicted classifications done by a classifier
//                  also known as Confusion Matrix
  EXPORT CrossAssignments := JOIN(cfmx, cross_raw,
                              LEFT.classifier = RIGHT.classifier AND LEFT.c_actual = RIGHT.c_actual AND LEFT.c_modeled = RIGHT.c_modeled,
                              form_confmatrix(LEFT,RIGHT), LEFT OUTER, LOOKUP);
//RecallByClass, it returns the proportion of instances belonging to a class that was correctly classified,
//               also know as True positive rate and sensivity, TP/(TP+FN).
  EXPORT RecallByClass := SORT(TABLE(CrossAssignments, {classifier, c_actual, tp_rate := SUM(GROUP,IF(c_actual=c_modeled,cnt,0))/SUM(GROUP,cnt)}, classifier, c_actual, FEW), classifier, c_actual);
//PrecisionByClass, returns the proportion of instances classified as a class that really belong to this class: TP /(TP + FP).
  EXPORT PrecisionByClass := SORT(TABLE(CrossAssignments,{classifier,c_modeled, Precision := SUM(GROUP,IF(c_actual=c_modeled,cnt,0))/SUM(GROUP,cnt)},classifier,c_modeled,FEW), classifier, c_modeled);
//FP_Rate_ByClass, it returns the proportion of instances not belonging to a class that were incorrectly classified as this class,
//                 also known as False Positive rate FP / (FP + TN).
  FalseRate_rec := RECORD
    Types.t_FieldNumber classifier; // The classifier in question (value of 'number' on outcome data)
    Types.t_Discrete c_modeled;     // The value produced by the classifier
    Types.t_FieldReal fp_rate;      // False Positive Rate
  END;
  wrong_modeled:= TABLE(CrossAssignments(c_modeled<>c_actual), {classifier, c_modeled, wcnt:= SUM(GROUP, cnt)}, classifier, c_modeled);
  j2:= JOIN(wrong_modeled, class_cnt, LEFT.classifier=RIGHT.classifier AND LEFT.c_modeled<>RIGHT.c_actual);
  allfalse:= TABLE(j2, {classifier, c_modeled, not_actual:= SUM(GROUP, cnt)}, classifier, c_modeled);
  EXPORT FP_Rate_ByClass := JOIN(wrong_modeled, allfalse, LEFT.classifier=RIGHT.classifier AND LEFT.c_modeled=RIGHT.c_modeled,
                          TRANSFORM(FalseRate_rec, SELF.fp_rate:= LEFT.wcnt/RIGHT.not_actual, SELF:= LEFT));
// Accuracy, it returns the proportion of instances correctly classified (total, without class distinction)
  EXPORT Accuracy := TABLE(CrossAssignments, {classifier, Accuracy:= SUM(GROUP,IF(c_actual=c_modeled,cnt,0))/SUM(GROUP, cnt)}, classifier);
END;
/*
	The purpose of this module is to provide a default interface to provide access to any of the 
  classifiers
*/
	EXPORT Default := MODULE,VIRTUAL
		EXPORT Base := 1000; // ID Base - all ids should be higher than this
		// Premise - two models can be combined by concatenating (in terms of ID number) the under-base and over-base parts
		SHARED CombineModels(DATASET(Types.NumericField) sofar,DATASET(Types.NumericField) new) := FUNCTION
			UBaseHigh := MAX(sofar(id<Base),id);
			High := IF(EXISTS(sofar),MAX(sofar,id),Base);
			UB := PROJECT(new(id<Base),TRANSFORM(Types.NumericField,SELF.id := LEFT.id+UBaseHigh,SELF := LEFT));
			UO := PROJECT(new(id>=Base),TRANSFORM(Types.NumericField,SELF.id := LEFT.id+High-Base,SELF := LEFT));
			RETURN sofar+UB+UO;
		END;
	  // Learn from continuous data
	  EXPORT LearnC(DATASET(Types.NumericField) Indep,DATASET(Types.DiscreteField) Dep) := DATASET([],Types.NumericField); // All classifiers serialized to numeric field format
	  // Learn from discrete data - worst case - convert to continuous
	  EXPORT LearnD(DATASET(Types.DiscreteField) Indep,DATASET(Types.DiscreteField) Dep) := LearnC(PROJECT(Indep,Types.NumericField),Dep);
	  // Learn from continuous data - using a prebuilt model
	  EXPORT ClassifyC(DATASET(Types.NumericField) Indep,DATASET(Types.NumericField) mod) := DATASET([],l_result);
	  // Classify discrete data - using a prebuilt model
	  EXPORT ClassifyD(DATASET(Types.DiscreteField) Indep,DATASET(Types.NumericField) mod) := ClassifyC(PROJECT(Indep,Types.NumericField),mod);
		EXPORT TestD(DATASET(Types.DiscreteField) Indep,DATASET(Types.DiscreteField) Dep) := FUNCTION
		  a := LearnD(Indep,Dep);
			res := ClassifyD(Indep,a);
			RETURN Compare(Dep,res);
		END;
		EXPORT TestC(DATASET(Types.NumericField) Indep,DATASET(Types.DiscreteField) Dep) := FUNCTION
		  a := LearnC(Indep,Dep);
			res := ClassifyC(Indep,a);
			RETURN Compare(Dep,res);
		END;
		EXPORT LearnDConcat(DATASET(Types.DiscreteField) Indep,DATASET(Types.DiscreteField) Dep, LearnD fnc) := FUNCTION
		  // Call fnc once for each dependency; concatenate the results
			// First get all the dependant numbers
			dn := DEDUP(Dep,number,ALL);
			Types.NumericField loopBody(DATASET(Types.NumericField) sf,UNSIGNED c) := FUNCTION
			  RETURN CombineModels(sf,fnc(Indep,Dep(number=dn[c].number)));
			END;
			RETURN LOOP(DATASET([],Types.NumericField),COUNT(dn),loopBody(ROWS(LEFT),COUNTER));
		END;
		EXPORT LearnCConcat(DATASET(Types.NumericField) Indep,DATASET(Types.DiscreteField) Dep, LearnC fnc) := FUNCTION
		  // Call fnc once for each dependency; concatenate the results
			// First get all the dependant numbers
			dn := DEDUP(Dep,number,ALL);
			Types.NumericField loopBody(DATASET(Types.NumericField) sf,UNSIGNED c) := FUNCTION
			  RETURN CombineModels(sf,fnc(Indep,Dep(number=dn[c].number)));
			END;
			RETURN LOOP(DATASET([],Types.NumericField),COUNT(dn),loopBody(ROWS(LEFT),COUNTER));
		END;
	END;

  EXPORT NaiveBayes := MODULE(DEFAULT)
		SHARED SampleCorrection := 1;
		SHARED LogScale(REAL P) := -LOG(P)/LOG(2);

/* Naive Bayes Classification 
	 This method can support producing classification results for multiple classifiers at once
	 Note the presumption that the result (a weight for each value of each field) can fit in memory at once
*/
    SHARED BayesResult := RECORD
      Types.t_RecordId    id := 0;        // A record-id - allows a model to have an ordered sequence of results
      Types.t_Discrete    class_number;   // Dependent "number" value - Classifier ID
      Types.t_discrete    c;              // Dependent "value" value - Class value
      Types.t_FieldNumber number;         // A reference to a feature (or field) in the independants
      Types.t_Count       Support;        // Number of cases
    END;
    SHARED BayesResultD := RECORD (BayesResult)
      Types.t_discrete  f := 0;           // Independant value - Attribute value
      Types.t_FieldReal PC;                // Either P(F|C) or P(C) if number = 0. Stored in -Log2(P) - so small is good :)
    END;
    SHARED BayesResultC := RECORD (BayesResult)
      Types.t_FieldReal  mu:= 0;          // Independent attribute mean (mu)
      Types.t_FieldReal  var:= 0;         // Independent attribute sample standard deviation (sigma squared)
    END;
/*
  The inputs to the BuildNaiveBayes are:
  a) A dataset of discretized independant variables
  b) A dataset of class results (these must match in ID the discretized independant variables).
     Some routines can produce multiple classifiers at once; if so these are distinguished using the NUMBER field of cl
*/
	  EXPORT LearnD(DATASET(Types.DiscreteField) Indep,DATASET(Types.DiscreteField) Dep) := FUNCTION
			dd := Indep;
			cl := Dep;
			Triple := RECORD
				Types.t_Discrete c;
				Types.t_Discrete f;
				Types.t_FieldNumber number;
				Types.t_FieldNumber class_number;
			END;
			Triple form(dd le,cl ri) := TRANSFORM
				SELF.c := ri.value;
				SELF.f := le.value;
				SELF.number := le.number;
				SELF.class_number := ri.number;
			END;
			Vals := JOIN(dd,cl,LEFT.id=RIGHT.id,form(LEFT,RIGHT));
			AggregatedTriple := RECORD
				Vals.c;
				Vals.f;
				Vals.number;
				Vals.class_number;
				Types.t_Count support := COUNT(GROUP);
			END;
			// This is the raw table - how many of each value 'f' for each field 'number' appear for each value 'c' of each classifier 'class_number'
			Cnts := TABLE(Vals,AggregatedTriple,c,f,number,class_number,FEW);
			// Compute P(C)
			CTots := TABLE(cl,{value,number,Support := COUNT(GROUP)},value,number,FEW);
			CLTots := TABLE(CTots,{number,TSupport := SUM(GROUP,Support), GC := COUNT(GROUP)},number,FEW);
			P_C_Rec := RECORD
				Types.t_Discrete c;            // The value within the class
				Types.t_Discrete f;            // The number of features within the class
				Types.t_Discrete class_number; // Used when multiple classifiers being produced at once
				Types.t_FieldReal support;     // Used to store total number of C
				REAL8 w;                       // P(C)
			END;
			// Apply Laplace Estimator to P(C) in order to be consistent with attributes' probability
			P_C_Rec pct(CTots le,CLTots ri) := TRANSFORM
				SELF.c := le.value;
				SELF.f := 0; // to be claculated later on
				SELF.class_number := ri.number;
				SELF.support := le.Support + SampleCorrection;
				SELF.w := (le.Support + SampleCorrection) / (ri.TSupport + ri.GC*SampleCorrection);
			END;
			PC_0 := JOIN(CTots,CLTots,LEFT.number=RIGHT.number,pct(LEFT,RIGHT),FEW);
			// Computing Attributes' probability
			AttribValue_Rec := RECORD
				Cnts.class_number; 	// Used when multiple classifiers being produced at once
				Cnts.number;				// A reference to a feature (or field) in the independants
				Cnts.f;				 			// Independant value
				Types.t_Count support := 0;
			END;
			// Generating feature domain per feature (class_number only used when multiple classifiers being produced at once)
			AttValues	:= TABLE(Cnts, AttribValue_Rec, class_number, number, f, FEW);
			AttCnts 	:= TABLE(AttValues, {class_number, number, ocurrence:= COUNT(GROUP)},class_number, number, FEW); // Summarize	
			AttrValue_per_Class_Rec := RECORD
				Types.t_Discrete c;
				AttValues.f;
				AttValues.number;
				AttValues.class_number;
				AttValues.support;
			END;
			// Generating class x feature domain, initial support = 0
			AttrValue_per_Class_Rec form_cl_attr(AttValues le, CTots ri):= TRANSFORM
				SELF.c:= ri.value;
				SELF:= le;
			END;
			ATots:= JOIN(AttValues, CTots, LEFT.class_number = RIGHT.number, form_cl_attr(LEFT, RIGHT), MANY LOOKUP, FEW);
			// Counting feature value ocurrence per class x feature, remains 0 if combination not present in dataset
			ATots form_ACnts(ATots le, Cnts ri) := TRANSFORM
				SELF.support	:= ri.support;
				SELF 			:= le;
			END;
			ACnts := JOIN(ATots, Cnts, LEFT.c = RIGHT.c AND LEFT.f = RIGHT.f AND LEFT.number = RIGHT.number AND LEFT.class_number = RIGHT.class_number, 
														form_ACnts(LEFT,RIGHT),
														LEFT OUTER, LOOKUP);
			// Summarizing and getting value 'GC' to apply in Laplace Estimator
			TotalFs0 := TABLE(ACnts,{c,number,class_number,Types.t_Count Support := SUM(GROUP,Support),GC := COUNT(GROUP)},c,number,class_number,FEW);
			TotalFs := TABLE(TotalFs0,{c,class_number,ML.Types.t_Count Support := SUM(GROUP,Support),Types.t_Count GC := SUM(GROUP,GC)},c,class_number,FEW);
			// Merge and Laplace Estimator
			F_Given_C_Rec := RECORD
				ACnts.c;
				ACnts.f;
				ACnts.number;
				ACnts.class_number;
				ACnts.support;
				REAL8 w;
			END;
			F_Given_C_Rec mp(ACnts le,TotalFs ri) := TRANSFORM
				SELF.support := le.Support+SampleCorrection;
				SELF.w := (le.Support+SampleCorrection) / (ri.Support+ri.GC*SampleCorrection);
				SELF := le;
			END;
			// Calculating final probabilties
			FC := JOIN(ACnts,TotalFs,LEFT.C = RIGHT.C AND LEFT.class_number=RIGHT.class_number,mp(LEFT,RIGHT),LOOKUP);
			PC_0 form_TotalFs(PC_0 le, TotalFs ri) := TRANSFORM
				SELF.f	:= ri.Support+ri.GC*SampleCorrection;
				SELF		:= le;
			END;
			PC := JOIN(PC_0, TotalFs, LEFT.C = RIGHT.C AND LEFT.class_number=RIGHT.class_number,form_TotalFs(LEFT,RIGHT),LOOKUP);		
			Pret := PROJECT(FC,TRANSFORM(BayesResultD, SELF.PC:=LEFT.w, SELF := LEFT))+PROJECT(PC,TRANSFORM(BayesResultD, SELF.PC:=LEFT.w, SELF.number:= 0,SELF:=LEFT));
			Pret1 := PROJECT(Pret,TRANSFORM(BayesResultD, SELF.PC := LogScale(LEFT.PC),SELF.id := Base+COUNTER,SELF := LEFT));
			ToField(Pret1,o);
			RETURN o;
		END;
    // Transform NumericFiled "mod" to discrete Naive Bayes format model "BayesResultD"
    EXPORT Model(DATASET(Types.NumericField) mod) := FUNCTION
      ML.FromField(mod,BayesResultD,o);
      RETURN o;
    END;
		// This function will take a pre-existing NaiveBayes model (mo) and score every row of a discretized dataset
		// The output will have a row for every row of dd and a column for every class in the original training set
    EXPORT ClassProbDistribD(DATASET(Types.DiscreteField) Indep,DATASET(Types.NumericField) mod) := FUNCTION
      d := Indep;
      mo := Model(mod);
      // Firstly we can just compute the support for each class from the bayes result
			dd := DISTRIBUTE(d,HASH(id)); // One of those rather nice embarassingly parallel activities
			Inter := RECORD
				Types.t_discrete c;
				Types.t_discrete class_number;
				Types.t_RecordId Id;
				REAL8  w;
			END;
			Inter note(dd le,mo ri) := TRANSFORM
				SELF.c := ri.c;
				SELF.class_number := ri.class_number;
				SELF.id := le.id;
				SELF.w := ri.PC;
			END;
	// RHS is small so ,ALL join should work ok
	// Ignore the "explicitly distributed" compiler warning - the many lookup is preserving the distribution
			J := JOIN(dd,mo,LEFT.number=RIGHT.number AND LEFT.value=RIGHT.f,note(LEFT,RIGHT),MANY LOOKUP);
			InterCounted := RECORD
				J.c;
				J.class_number;
				J.id;
				REAL8 P := SUM(GROUP,J.W);
				Types.t_FieldNumber Missing := COUNT(GROUP); // not really missing just yet :)
			END;
			TSum := TABLE(J,InterCounted,c,class_number,id,LOCAL);
	// Now we have the sums for all the F present for each class we need to
	// a) Add in the P(C)
	// b) Suitably penalize any 'f' which simply were not present in the model
	// We start by counting how many not present ...
			FTots := TABLE(DD,{id,c := COUNT(GROUP)},id,LOCAL);
			InterCounted NoteMissing(TSum le,FTots ri) := TRANSFORM
				SELF.Missing := ri.c - le.Missing;
				SELF := le;
			END;
			MissingNoted := JOIN(Tsum,FTots,LEFT.id=RIGHT.id,NoteMissing(LEFT,RIGHT),LOOKUP);
			InterCounted NoteC(MissingNoted le,mo ri) := TRANSFORM
				SELF.P := le.P+ri.PC+le.Missing*LogScale(SampleCorrection/ri.f);
				SELF := le;
			END;
			CNoted := JOIN(MissingNoted,mo(number=0),LEFT.c=RIGHT.c,NoteC(LEFT,RIGHT),LOOKUP);
      l_result toResult(CNoted le) := TRANSFORM
        SELF.id := le.id;               // Instance ID
        SELF.number := le.class_number; // Classifier ID
        SELF.value := le.c;             // Class value
        SELF.conf := POWER(2.0, -le.p); // Convert likehood to decimal value
      END;
      // Normalizing Likehood to deliver Class Probability per instance
      InstResults := PROJECT(CNoted, toResult(LEFT), LOCAL);
      gInst := TABLE(InstResults, {number, id, tot:=SUM(GROUP,conf)}, number, id, LOCAL);
      clDist:= JOIN(InstResults, gInst,LEFT.number=RIGHT.number AND LEFT.id=RIGHT.id, TRANSFORM(Types.l_result, SELF.conf:=LEFT.conf/RIGHT.tot, SELF:=LEFT), LOCAL);
      RETURN clDist;
    END;
    // Classification function for discrete independent values and model
    EXPORT ClassifyD(DATASET(Types.DiscreteField) Indep,DATASET(Types.NumericField) mod) := FUNCTION
      // get class probabilities for each instance
      dClass:= ClassProbDistribD(Indep, mod);
      // select the class with greatest probability for each instance
      sClass := SORT(dClass, id, -conf, LOCAL);
      finalClass:=DEDUP(sClass, id, LOCAL);
      RETURN finalClass;
    END;
    /*From Wikipedia
    " ...When dealing with continuous data, a typical assumption is that the continuous values associated with each class are distributed according to a Gaussian distribution.
    For example, suppose the training data contain a continuous attribute, x. We first segment the data by the class, and then compute the mean and variance of x in each class.
    Let mu_c be the mean of the values in x associated with class c, and let sigma^2_c be the variance of the values in x associated with class c.
    Then, the probability density of some value given a class, P(x=v|c), can be computed by plugging v into the equation for a Normal distribution parameterized by mu_c and sigma^2_c..."
    */
    EXPORT LearnC(DATASET(NumericField) Indep, DATASET(DiscreteField) Dep) := FUNCTION
      Triple := RECORD
        Types.t_FieldNumber class_number;
        Types.t_FieldNumber number;
        Types.t_FieldReal value;
        Types.t_Discrete c;
      END;
      Triple form(Indep le, Dep ri) := TRANSFORM
        SELF.class_number := ri.number;
        SELF.number := le.number;
        SELF.value := le.value;
        SELF.c := ri.value;
      END;
      Vals := JOIN(Indep, Dep, LEFT.id=RIGHT.id, form(LEFT,RIGHT));
      // Compute P(C)
      ClassCnts := TABLE(Dep, {number, value, support := COUNT(GROUP)}, number, value, FEW);
      ClassTots := TABLE(ClassCnts,{number, TSupport := SUM(GROUP,Support)}, number, FEW);
      P_C_Rec := RECORD
        Types.t_Discrete class_number; // Used when multiple classifiers being produced at once
        Types.t_Discrete c;             // The class value "C"
        Types.t_FieldReal support;          // Cases count
        Types.t_FieldReal  mu:= 0;          // P(C)
      END;
      // Computing prior probability P(C)
      P_C_Rec pct(ClassCnts le, ClassTots ri) := TRANSFORM
        SELF.class_number := ri.number;
        SELF.c := le.value;
        SELF.support := le.Support;
        SELF.mu := le.Support/ri.TSupport;
      END;
      PC := JOIN(ClassCnts, ClassTots, LEFT.number=RIGHT.number, pct(LEFT,RIGHT), FEW);
      PC_cnt := COUNT(PC);
      // Computing Attributes' mean and variance. mu_c and sigma^2_c.
      AggregatedTriple := RECORD
        Vals.class_number;
        Vals.c;
        Vals.number;
        Types.t_Count support := COUNT(GROUP);
        Types.t_FieldReal mu:=AVE(GROUP, Vals.value);
        Types.t_FieldReal var:= VARIANCE(GROUP, Vals.value);
      END;
      AC:= TABLE(Vals, AggregatedTriple, class_number, c, number);
      Pret := PROJECT(PC, TRANSFORM(BayesResultC, SELF.id := Base + COUNTER, SELF.number := 0, SELF:=LEFT)) +
              PROJECT(AC, TRANSFORM(BayesResultC, SELF.id := Base + COUNTER + PC_cnt, SELF.var:= LEFT.var*LEFT.support/(LEFT.support -1), SELF := LEFT));
      ToField(Pret,o);
      RETURN o;
    END;
    // Transform NumericFiled "mod" to continuos Naive Bayes format model "BayesResultC"
    EXPORT ModelC(DATASET(Types.NumericField) mod) := FUNCTION
      ML.FromField(mod,BayesResultC,o);
      RETURN o;
    END;
    EXPORT ClassProbDistribC(DATASET(Types.NumericField) Indep, DATASET(Types.NumericField) mod) := FUNCTION
      dd := DISTRIBUTE(Indep, HASH(id));
      mo := ModelC(mod);
      Inter := RECORD
        Types.t_FieldNumber class_number;
        Types.t_FieldNumber number;
        Types.t_FieldReal value;
        Types.t_Discrete c;
        Types.t_RecordId Id;
        Types.t_FieldReal  likehood:=0; // Probability density P(x=v|c)
      END;
      Inter ProbDensity(dd le, mo ri) := TRANSFORM
        SELF.id := le.id;
        SELF.value:= le.value;
        SELF.likehood := LogScale(exp(-(le.value-ri.mu)*(le.value-ri.mu)/(2*ri.var))/SQRT(2*ML.Utils.Pi*ri.var));
        SELF:= ri;
      END;
      // Likehood or probability density P(x=v|c) is calculated assuming Gaussian distribution of the class based on new instance attribute value and atribute's mean and variance from model
      LogPall := JOIN(dd,mo,LEFT.number=RIGHT.number , ProbDensity(LEFT,RIGHT),MANY LOOKUP);
      // Prior probaility PC
      LogPC:= PROJECT(mo(number=0),TRANSFORM(BayesResultC, SELF.mu:=LogScale(LEFT.mu), SELF:=LEFT));
      post_rec:= RECORD
        LogPall.id;
        LogPall.class_number;
        LogPall.c;
        Types.t_FieldReal prod:= SUM(GROUP, LogPall.likehood);
      END;
      // Likehood and Prior are expressed in LogScale, summing really means multiply
      LikehoodProduct:= TABLE(LogPall, post_rec, class_number, c, id, LOCAL);
      // Posterior probability = prior x likehood_product / evidence
      // We use only the numerator of that fraction, because the denominator is effectively constant.
      // See: http://en.wikipedia.org/wiki/Naive_Bayes_classifier#Probabilistic_model
      l_result toResult(LikehoodProduct le, LogPC ri) := TRANSFORM
        SELF.id := le.id;               // Instance ID
        SELF.number := le.class_number; // Classifier ID
        SELF.value := ri.c;             // Class value
        SELF.conf:= le.prod + ri.mu;    // Adding mu
      END;
      AllPosterior:= JOIN(LikehoodProduct, LogPC, LEFT.class_number = RIGHT.class_number AND LEFT.c = RIGHT.c, toResult(LEFT, RIGHT), LOOKUP);
      // Normalizing Likehood to deliver Class Probability per instance
      baseExp:= TABLE(AllPosterior, {id, minConf:= MIN(GROUP, conf)},id, LOCAL); // will use this to divide instance's conf by the smallest per id
      l_result toNorm(AllPosterior le, baseExp ri) := TRANSFORM
        SELF.conf:= POWER(2.0, -MIN( le.conf - ri.minConf, 2048));  // minimum probability set to 1/2^2048 = 0 at the end
        SELF:= le;
      END;
      AllOffset:= JOIN(AllPosterior, baseExp, LEFT.id = RIGHT.id, toNorm(LEFT, RIGHT), LOOKUP); // at least one record per id with 1.0 probability before normalization
      gInst := TABLE(AllOffset, {number, id, tot:=SUM(GROUP,conf)}, number, id, LOCAL);
      clDist:= JOIN(AllOffset, gInst,LEFT.number=RIGHT.number AND LEFT.id=RIGHT.id, TRANSFORM(Types.l_result, SELF.conf:=LEFT.conf/RIGHT.tot, SELF:=LEFT), LOCAL);
      RETURN clDist;
    END;
    // Classification function for continuous independent values and model
    EXPORT ClassifyC(DATASET(Types.NumericField) Indep,DATASET(Types.NumericField) mod) := FUNCTION
      // get class probabilities for each instance
      dClass:= ClassProbDistribC(Indep, mod);
      // select the class with greatest probability for each instance
      sClass := SORT(dClass, id, -conf, LOCAL);
      finalClass:=DEDUP(sClass, id, LOCAL);
      RETURN finalClass;
     END;
  END; // NaiveBayes Module

/*
	See: http://en.wikipedia.org/wiki/Perceptron
  The inputs to the BuildPerceptron are:
  a) A dataset of discretized independant variables
  b) A dataset of class results (these must match in ID the discretized independant variables).
  c) Passes; number of passes over the data to make during the learning process
  d) Alpha is the learning rate - higher numbers may learn quicker - but may not converge
  Note the perceptron presently assumes the class values are ordinal eg 4>3>2>1>0

	Output: A table of weights for each independant variable for each class. 
	Those weights with number=class_number give the error rate on the last pass of the data
*/

  EXPORT Perceptron(UNSIGNED Passes,REAL8 Alpha = 0.1) := MODULE(DEFAULT)
		SHARED Thresh := 0.5; // The threshold to apply for the cut-off function

	  EXPORT LearnD(DATASET(Types.DiscreteField) Indep,DATASET(Types.DiscreteField) Dep) := FUNCTION
			dd := Indep;
			cl := Dep;
			MaxFieldNumber := MAX(dd,number);
			FirstClassNo := MaxFieldNumber+1;
			clb := Utils.RebaseDiscrete(cl,FirstClassNo);
			LastClassNo := MAX(clb,number);
			all_fields := dd+clb;
	// Fields are ordered so that everything for a given input record is on one node
	// And so that records are encountered 'lowest first' and with the class variables coming later
			ready := SORT( DISTRIBUTE( all_fields, HASH(id) ), id, Number, LOCAL );
  // A weight record for our perceptron
			WR := RECORD
				REAL8 W := 0;
				Types.t_FieldNumber number; // The field this weight applies to - note field 0 will be the bias, class_number will be used for cumulative error
				Types.t_Discrete class_number;
			END;
			VR := RECORD
				Types.t_FieldNumber number;
				Types.t_Discrete    value;
			END;
	// This function exists to initialize the weights for the perceptron
			InitWeights := FUNCTION
				Classes := TABLE(clb,{number},number,FEW);
				WR again(Classes le,UNSIGNED C) := TRANSFORM
					SELF.number := IF( C > MaxFieldNumber, le.number, C ); // The > case sets up the cumulative error; rest are the field weights
					SELF.class_number := le.number;
				END;
				RETURN NORMALIZE(Classes,MaxFieldNumber+2,again(LEFT,COUNTER-1));
			END;

			AccumRec := RECORD
				DATASET(WR) Weights;
				DATASET(VR) ThisRecord;
				Types.t_RecordId Processed;
			END;
	// The learn step for a perceptrom
			Learn(DATASET(WR) le,DATASET(VR) ri,Types.t_FieldNumber fn,Types.t_Discrete va) := FUNCTION
				let := le(class_number=fn);
				letn := let(number<>fn);     // all of the regular weights
				lep := le(class_number<>fn); // Pass-thru
	  // Compute the 'predicted' value for this iteration as Sum WiXi
				iv := RECORD
					REAL8 val;
				END;
		// Compute the score components for each class for this record
				iv scor(le l,ri r) := TRANSFORM
					SELF.val := l.w*IF(r.number<>0,r.value,1);
				END;
				sc := JOIN(letn,ri,LEFT.number=RIGHT.number,scor(LEFT,RIGHT),LEFT OUTER);
				res := IF( SUM(sc,val) > Thresh, 1, 0 );
				err := va-res;
				let_e := PROJECT(let(number=fn),TRANSFORM(WR,SELF.w := LEFT.w+ABS(err), SELF:=LEFT)); // Build up the accumulative error
				delta := alpha*err; // The amount of 'learning' to do this step
		// Apply delta to regular weights
				WR add(WR le,VR ri) := TRANSFORM
					SELF.w := le.w+delta*IF(ri.number=0,1,ri.value); // Bias will not have matching RHS - so assume 1
					SELF := le;
				END;
				J := JOIN(letn,ri,LEFT.number=right.number,add(LEFT,RIGHT),LEFT OUTER);
				RETURN let_e+J+lep;
			END;
  // Zero out the error values
			WR Clean(DATASET(WR) w) := FUNCTION
				RETURN w(number<>class_number)+PROJECT(w(number=class_number),TRANSFORM(WR,SELF.w := 0, SELF := LEFT));
			END;
	// This function does one pass of the data learning into the weights
			WR Pass(DATASET(WR) we) := FUNCTION
		// This takes a record one by one and processes it
		// That may mean simply appending it to 'ThisRecord' - or it might mean performing a learning step
				AccumRec TakeRecord(ready le,AccumRec ri) := TRANSFORM
					BOOLEAN lrn := le.number >= FirstClassNo;
					BOOLEAN init := ~EXISTS(ri.Weights);
					SELF.Weights := MAP ( init => Clean(we), 
																~lrn => ri.Weights,
																Learn(ri.Weights,ri.ThisRecord,le.number,le.value) );
		// This is either an independant variable - in which case we append it
		// Or it is the last dependant variable - in which case we can throw the record away
		// Or it is one of the dependant variables - so keep the record for now
					SELF.ThisRecord := MAP ( ~lrn => ri.ThisRecord+ROW({le.number,le.value},VR),
																	le.number = LastClassNo => DATASET([],VR),
																	ri.ThisRecord);
					SELF.Processed := ri.Processed + IF( le.number = LastClassNo, 1, 0 );
				END;
		  // Effectively merges two perceptrons (generally 'learnt' on different nodes)
			// For the errors - simply add them
			// For the weights themselves perform a weighted mean (weighting by the number of records used to train)
				Blend(DATASET(WR) l,UNSIGNED lscale, DATASET(WR) r,UNSIGNED rscale) := FUNCTION
					lscaled := PROJECT(l(number<>class_number),TRANSFORM(WR,SELF.w := LEFT.w*lscale, SELF := LEFT));
					rscaled := PROJECT(r(number<>class_number),TRANSFORM(WR,SELF.w := LEFT.w*rscale, SELF := LEFT));
					unscaled := (l+r)(number=class_number);
					t := TABLE(lscaled+rscaled+unscaled,{number,class_number,w1 := SUM(GROUP,w)},number,class_number,FEW);
					RETURN PROJECT(t,TRANSFORM(WR,SELF.w := IF(LEFT.number=LEFT.class_number,LEFT.w1,LEFT.w1/(lscale+rscale)),SELF := LEFT));
				END;		
				AccumRec MergeP(AccumRec ri1,AccumRec ri2) := TRANSFORM
					SELF.ThisRecord := []; // Merging only valid across perceptrons learnt on complete records
					SELF.Processed := ri1.Processed+ri2.Processed;
					SELF.Weights := Blend(ri1.Weights,ri1.Processed,ri2.Weights,ri2.Processed);
				END;

				A := AGGREGATE(ready,AccumRec,TakeRecord(LEFT,RIGHT),MergeP(RIGHT1,RIGHT2))[1];
		// Now return the weights (and turn the error number into a ratio)
				RETURN A.Weights(class_number<>number)+PROJECT(A.Weights(class_number=number),TRANSFORM(WR,SELF.w := LEFT.w / A.Processed,SELF := LEFT));
			END;
			L := LOOP(InitWeights,Passes,PASS(ROWS(LEFT)));
			L1 := PROJECT(L,TRANSFORM(l_model,SELF.id := COUNTER+Base,SELF := LEFT));
			ML.ToField(L1,o);
			RETURN o;
		END;
    EXPORT Model(DATASET(Types.NumericField) mod) := FUNCTION
	    ML.FromField(mod,l_model,o);
		  RETURN o;
	  END;
	  EXPORT ClassifyD(DATASET(Types.DiscreteField) Indep,DATASET(Types.NumericField) mod) := FUNCTION
		  mo := Model(mod);
			Ind := DISTRIBUTE(Indep,HASH(id));
			l_result note(Ind le,mo ri) := TRANSFORM
			  SELF.conf := le.value*ri.w;
				SELF.number := ri.class_number;
				SELF.value := 0;
				SELF.id := le.id;
			END;
			// Compute the score for each component of the linear equation
			j := JOIN(Ind,mo,LEFT.number=RIGHT.number,note(LEFT,RIGHT),MANY LOOKUP); // MUST be lookup! Or distribution goes
			l_result ac(l_result le, l_result ri) := TRANSFORM
			  SELF.conf := le.conf+ri.conf;
			  SELF := le;
			END;
			// Rollup so there is one score for every id for every 'number' (original class_number)
			t := ROLLUP(SORT(j,id,number,LOCAL),LEFT.id=RIGHT.id AND LEFT.number=RIGHT.number,ac(LEFT,RIGHT),LOCAL);
			// Now we have to add on the 'constant' offset
			l_result add_c(l_result le,mo ri) := TRANSFORM
			  SELF.conf := le.conf+ri.w;
				SELF.value := IF(SELF.Conf>Thresh,1,0);
				SELF := le;
			END;
			t1 := JOIN(t,mo(number=0),LEFT.number=RIGHT.class_number,add_c(LEFT,RIGHT),LEFT OUTER);
			t2 := PROJECT(t1,TRANSFORM(l_Result,SELF.conf := ABS(LEFT.Conf-Thresh), SELF := LEFT));
			RETURN t2;
		END;
	END;
  /*
  Apply classification with Neural Network by using NeuralNetworks.ecl
  */
  EXPORT NeuralNetworksClassifier (DATASET(Types.DiscreteField) net, DATASET(Mat.Types.MUElement) IntW, DATASET(Mat.Types.MUElement) Intb, REAL8 LAMBDA=0.001, REAL8 ALPHA=0.1, UNSIGNED2 MaxIter=100, 
  UNSIGNED4 prows=0, UNSIGNED4 pcols=0, UNSIGNED4 Maxrows=0, UNSIGNED4 Maxcols=0) := MODULE(DEFAULT)
  SHARED NN := NeuralNetworks(net, prows,  pcols, Maxrows,  Maxcols);
  EXPORT LearnC(DATASET(Types.NumericField) Indep, DATASET(Types.DiscreteField) Dep) := FUNCTION
    Y := PROJECT(Dep,Types.NumericField);
    groundTruth:= Utils.ToGroundTruth (Y);//groundTruth is a matrix that each column correspond to one sample
    //to convert the groundTruth matrix to NumericFiled format firt I have to trasnpose it to make each sample to correspond to
    //each row, in that case when we convert it to NumericFiled format the id filed is built up correctly
    groundTruth_t := Mat.trans(groundTruth);
    groundTruth_NumericField := Types.FromMatrix (groundTruth_t);
    Learntmodel := NN.NNLearn(Indep, groundTruth_NumericField,IntW, Intb,  LAMBDA, ALPHA, MaxIter);
    RETURN Learntmodel;
  END;
  EXPORT Model(DATASET(Types.NumericField) Lmod) := FUNCTION
    RETURN NN.Model(Lmod);
  END;
  EXPORT ClassProbDistribC(DATASET(Types.NumericField) Indep,DATASET(Types.NumericField) mod) :=FUNCTION
    AEnd := NN.NNOutput(Indep,mod);
    RETURN AEnd;
  END;
  EXPORT ClassifyC(DATASET(Types.NumericField) Indep,DATASET(Types.NumericField) mod) := FUNCTION
    Classes := NN.NNClassify(Indep,mod);
    RETURN Classes;
  END;
  END;//END NeuralNetworksClassifier
/*
/*
	Logistic Regression implementation base on the iteratively-reweighted least squares (IRLS) algorithm:
  http://www.cs.cmu.edu/~ggordon/IRLS-example

	Logistic Regression module parameters:
	- Ridge: an optional ridge term used to ensure existance of Inv(X'*X) even if 
		some independent variables X are linearly dependent. In other words the Ridge parameter
		ensures that the matrix X'*X+mRidge is non-singular.
	- Epsilon: an optional parameter used to test convergence
	- MaxIter: an optional parameter that defines a maximum number of iterations

	The inputs to the Logis module are:
  a) A training dataset X of discretized independant variables
  b) A dataset of class results Y.

*/

EXPORT Logistic_sparse(REAL8 Ridge=0.00001, REAL8 Epsilon=0.000000001, UNSIGNED2 MaxIter=200) := MODULE(DEFAULT)
	Logis(DATASET(Types.NumericField) X,DATASET(Types.NumericField) Y) := MODULE
		SHARED mu_comp := ENUM ( Beta = 1,  Y = 2 );
		SHARED RebaseY := Utils.RebaseNumericField(Y);
		SHARED Y_Map := RebaseY.Mapping(1);
		Y_0 := RebaseY.ToNew(Y_Map);
		mY := Types.ToMatrix(Y_0);
		mX_0 := Types.ToMatrix(X);
		mX := Mat.InsertColumn(mX_0, 1, 1.0); // Insert X1=1 column 
	
		mXstats := Mat.Has(mX).Stats;
		mX_n := mXstats.XMax;
		mX_m := mXstats.YMax;

		mW := Mat.Vec.ToCol(Mat.Vec.From(mX_n,1.0),1);
		mRidge := Mat.Vec.ToDiag(Mat.Vec.From(mX_m,ridge));
		mBeta0 := Mat.Vec.ToCol(Mat.Vec.From(mX_m,0.0),1);	
		mBeta00 := Mat.MU.To(mBeta0, mu_comp.Beta);
		OldExpY_0 := Mat.Vec.ToCol(Mat.Vec.From(mX_n,-1.0),1); // -ones(size(mY))
		OldExpY_00 := Mat.MU.To(OldExpY_0, mu_comp.Y);

		Step(DATASET(Mat.Types.MUElement) BetaPlusY) := FUNCTION
			OldExpY := Mat.MU.From(BetaPlusY, mu_comp.Y);
			AdjY := Mat.Mul(mX, Mat.MU.From(BetaPlusY, mu_comp.Beta));
		// expy =  1./(1+exp(-adjy))
			ExpY := Mat.Each.Reciprocal(Mat.Each.Add(Mat.Each.Exp(Mat.Scale(AdjY, -1)),1));
		// deriv := expy .* (1-expy)
			Deriv := Mat.Each.Mul(expy,Mat.Each.Add(Mat.Scale(ExpY, -1),1));
		// wadjy := w .* (deriv .* adjy + (y-expy))
			W_AdjY := Mat.Each.Mul(mW,Mat.Add(Mat.Each.Mul(Deriv,AdjY),Mat.Sub(mY, ExpY)));
		// weights := spdiags(deriv .* w, 0, n, n)
			Weights := Mat.Vec.ToDiag(Mat.Vec.FromCol(Mat.Each.Mul(Deriv, mW),1));
		// mBeta := Inv(x' * weights * x + mRidge) * x' * wadjy
			mBeta :=  Mat.Mul(Mat.Mul(Mat.Inv(Mat.Add(Mat.Mul(Mat.Mul(Mat.Trans(mX), weights), mX), mRidge)), Mat.Trans(mX)), W_AdjY);
			err := SUM(Mat.Each.Abs(Mat.Sub(ExpY, OldExpY)),value);	
			RETURN IF(err < mX_n*Epsilon, BetaPlusY, Mat.MU.To(mBeta, mu_comp.Beta)+Mat.MU.To(ExpY, mu_comp.Y));
		END;

		SHARED BetaPair := LOOP(mBeta00+OldExpY_00, MaxIter, Step(ROWS(LEFT)));	
		BetaM := Mat.MU.From(BetaPair, mu_comp.Beta);
		rebasedBetaNF := RebaseY.ToOld(Types.FromMatrix(BetaM), Y_Map);
		BetaNF := Types.FromMatrix(Mat.Trans(Types.ToMatrix(rebasedBetaNF)));
	// convert Beta into NumericField dataset, and shift Number down by one to ensure the intercept Beta0 has id=0
		EXPORT Beta := PROJECT(BetaNF,TRANSFORM(Types.NumericField,SELF.Number := LEFT.Number-1;SELF:=LEFT;));
			Res := PROJECT(Beta,TRANSFORM(l_model,SELF.Id := COUNTER+Base,SELF.number := LEFT.number, SELF.class_number := LEFT.id, SELF.w := LEFT.value));
			ToField(Res,o);
		EXPORT Mod := o;
		modelY_M := Mat.MU.From(BetaPair, mu_comp.Y);
		modelY_NF := Types.FromMatrix(modelY_M);
		EXPORT modelY := RebaseY.ToOld(modelY_NF, Y_Map);
	END;
  EXPORT LearnCS(DATASET(Types.NumericField) Indep,DATASET(Types.DiscreteField) Dep) := Logis(Indep,PROJECT(Dep,Types.NumericField)).mod;
	EXPORT LearnC(DATASET(Types.NumericField) Indep,DATASET(Types.DiscreteField) Dep) := LearnCConcat(Indep,Dep,LearnCS);
	EXPORT Model(DATASET(Types.NumericField) mod) := FUNCTION
	  FromField(mod,l_model,o);
		RETURN o;
	END;
  EXPORT ClassifyC(DATASET(Types.NumericField) Indep,DATASET(Types.NumericField) mod) := FUNCTION
	  mod0 := Model(mod);
		Beta0 := PROJECT(mod0,TRANSFORM(Types.NumericField,SELF.Number := LEFT.Number+1,SELF.id := LEFT.class_number, SELF.value := LEFT.w;SELF:=LEFT;));
	  mBeta := Types.ToMatrix(Beta0);
	  mX_0 := Types.ToMatrix(Indep);
		mXloc := Mat.InsertColumn(mX_0, 1, 1.0); // Insert X1=1 column 
		
		AdjY := $.Mat.Mul(mXloc, $.Mat.Trans(mBeta)) ;
		// expy =  1./(1+exp(-adjy))
		sigmoid := $.Mat.Each.Reciprocal($.Mat.Each.Add($.Mat.Each.Exp($.Mat.Scale(AdjY, -1)),1));
		// Now convert to classify return format
		l_result tr(sigmoid le) := TRANSFORM
		  SELF.value := IF ( le.value > 0.5,1,0);
		  SELF.id := le.x;
			SELF.number := le.y;
			SELF.conf := ABS(le.value-0.5);
		END;
		RETURN PROJECT(sigmoid,tr(LEFT));
	END;
		
	END; // Logistic_sparse Module
	
/*
    Logistic Regression implementation base on the iteratively-reweighted least squares (IRLS) algorithm:
  http://www.cs.cmu.edu/~ggordon/IRLS-example

    Logistic Regression module parameters:
    - Ridge: an optional ridge term used to ensure existance of Inv(X'*X) even if
        some independent variables X are linearly dependent. In other words the Ridge parameter
        ensures that the matrix X'*X+mRidge is non-singular.
    - Epsilon: an optional parameter used to test convergence
    - MaxIter: an optional parameter that defines a maximum number of iterations
    - prows: an optional parameter used to set the number of rows in partition blocks (Should be used in conjuction with pcols)
    - pcols: an optional parameter used to set the number of cols in partition blocks (Should be used in conjuction with prows)
    - Maxrows: an optional parameter used to set maximum rows allowed per block when using AutoBVMap
    - Maxcols: an optional parameter used to set maximum cols allowed per block when using AutoBVMap

    The inputs to the Logis module are:
  a) A training dataset X of discretized independant variables
  b) A dataset of class results Y.

*/
    EXPORT Logistic(REAL8 Ridge=0.00001, REAL8 Epsilon=0.000000001, UNSIGNED2 MaxIter=200, 
               UNSIGNED4 prows=0, UNSIGNED4 pcols=0,UNSIGNED4 Maxrows=0, UNSIGNED4 Maxcols=0) := MODULE(DEFAULT)
                     
    Logis(DATASET(Types.NumericField) X, DATASET(Types.NumericField) Y) := MODULE
        SHARED mu_comp := ENUM ( Beta = 1,  Y = 2, BetaError = 3, BetaMaxError = 4 );
        SHARED RebaseY := Utils.RebaseNumericField(Y);
        SHARED Y_Map := RebaseY.Mapping(1);
        mX_0 := Types.ToMatrix(X);
         SHARED mX := Mat.InsertColumn(mX_0, 1, 1.0); // Insert X1=1 column (Xcols = Xcols+1)
         mXstats := Mat.Has(mX).Stats;
         mX_n := mXstats.XMax;
         mX_m := mXstats.YMax;
         
         //Map for Matrix X. Map will be used to derive all other maps in Logis
         havemaxrow := maxrows > 0;
         havemaxcol := maxcols > 0;
         havemaxrowcol := havemaxrow and havemaxcol;
         
         derivemap := IF(havemaxrowcol, PBblas.AutoBVMap(mX_n, mX_m,prows,pcols,maxrows, maxcols),
                         IF(havemaxrow, PBblas.AutoBVMap(mX_n, mX_m,prows,pcols,maxrows),
                            IF(havemaxcol, PBblas.AutoBVMap(mX_n, mX_m,prows,pcols,,maxcols),
                            PBblas.AutoBVMap(mX_n, mX_m,prows,pcols))));

        sizeRec := RECORD
            PBblas.Types.dimension_t m_rows;
            PBblas.Types.dimension_t m_cols;
            PBblas.Types.dimension_t f_b_rows;
            PBblas.Types.dimension_t f_b_cols;
        END;

        SHARED sizeTable := DATASET([{derivemap.matrix_rows,derivemap.matrix_cols,derivemap.part_rows(1),derivemap.part_cols(1)}], sizeRec);
        
        
        mXmap := PBblas.Matrix_Map(sizeTable[1].m_rows,sizeTable[1].m_cols,sizeTable[1].f_b_rows,sizeTable[1].f_b_cols);
        //Create block matrix X
        mXdist := DMAT.Converted.FromElement(mX,mXmap);
        
        
        //Create block matrix Y
        Y_0 := RebaseY.ToNew(Y_Map);
        mY := Types.ToMatrix(Y_0);
        mYmap := PBblas.Matrix_Map(sizeTable[1].m_rows, 1, sizeTable[1].f_b_rows, 1);
        mYdist := DMAT.Converted.FromElement(mY, mYmap);
        
        //New Matrix Generator
        Layout_Cell gen(UNSIGNED4 c, UNSIGNED4 NumRows, REAL8 v) := TRANSFORM
			SELF.x := ((c-1) % NumRows) + 1;
			SELF.y := ((c-1) DIV NumRows) + 1;
			SELF.v := v;
		END;

        //Create block matrix W
        mW := DATASET(sizeTable[1].m_rows, gen(COUNTER, sizeTable[1].m_rows, 1.0),DISTRIBUTED);
        mWdist := DMAT.Converted.FromCells(mYmap, mW);
        
        
        
        //Create block matrix Ridge
        mRidge := DATASET(sizeTable[1].m_cols, gen(COUNTER, sizeTable[1].m_cols, ridge),DISTRIBUTED);
        RidgeVecMap := PBblas.Matrix_Map(sizeTable[1].m_cols, 1, sizeTable[1].f_b_cols, 1);
        Ridgemap := PBblas.Matrix_Map(sizeTable[1].m_cols, sizeTable[1].m_cols, sizeTable[1].f_b_cols, sizeTable[1].f_b_cols);
        mRidgeVec := DMAT.Converted.FromCells(RidgeVecMap, mRidge);
        mRidgedist := PBblas.Vector2Diag(RidgeVecMap, mRidgeVec, Ridgemap);
        
        //Create block matrix Beta
        mBeta0 := DATASET(sizeTable[1].m_cols, gen(COUNTER, sizeTable[1].m_cols, 0.0),DISTRIBUTED);
        mBeta0map := PBblas.Matrix_Map(sizeTable[1].m_cols, 1, sizeTable[1].f_b_cols, 1);
        mBeta00 := PBblas.MU.To(DMAT.Converted.FromCells(mBeta0map,mBeta0), mu_comp.Beta);
        
        //Create block matrix OldExpY
        OldExpY_0 := DATASET(sizeTable[1].m_rows, gen(COUNTER, sizeTable[1].m_rows, -1),DISTRIBUTED); // -ones(size(mY))
        OldExpY_00 := PBblas.MU.To(DMAT.Converted.FromCells(mYmap,OldExpY_0), mu_comp.Y);
        
        

        //Functions needed to calculate ExpY
            PBblas.Types.value_t e(PBblas.Types.value_t v, 
                                      PBblas.Types.dimension_t r, 
                                      PBblas.Types.dimension_t c) := exp(v);
            
            PBblas.Types.value_t AddOne(PBblas.Types.value_t v, 
                                      PBblas.Types.dimension_t r, 
                                      PBblas.Types.dimension_t c) := 1+v;
            
            PBblas.Types.value_t Reciprocal(PBblas.Types.value_t v, 
                                      PBblas.Types.dimension_t r, 
                                      PBblas.Types.dimension_t c) := 1/v;
            
        //Abs (Absolute Value) function
            PBblas.Types.value_t absv(PBblas.Types.value_t v, 
                                      PBblas.Types.dimension_t r, 
                                      PBblas.Types.dimension_t c) := abs(v);
                                      
        //Maps used in Step function
            weightsMap := PBblas.Matrix_Map(sizeTable[1].m_rows, sizeTable[1].m_rows, sizeTable[1].f_b_rows, sizeTable[1].f_b_rows);
            xWeightMap := PBblas.Matrix_Map(sizeTable[1].m_cols, sizeTable[1].m_rows, sizeTable[1].f_b_cols, sizeTable[1].f_b_rows);
            xtranswadjyMap := PBblas.Matrix_Map(sizeTable[1].m_cols, 1, sizeTable[1].f_b_cols, 1);
            
                                      
        Step(DATASET(PBblas.Types.MUElement) BetaPlusY, INTEGER coun) := FUNCTION
            
            OldExpY := PBblas.MU.From(BetaPlusY, mu_comp.Y);
            
            BetaDist := PBblas.MU.From(BetaPlusY, mu_comp.Beta);
            
        
            AdjY := PBblas.PB_dgemm(FALSE, FALSE, 
                             1.0, mXmap, mXdist, mBeta0map, BetaDist, 
                             mYmap);
            
        
            
        // The -adjy of expy =  1./(1+exp(-adjy))
            negAdjY := PBblas.PB_dscal(-1, AdjY);
        // The exp of expy =  1./(1+exp(-adjy))
            e2negAdjY := PBblas.Apply2Elements(mYmap, negAdjY, e);
        // The (1+exp(-adjy) of expy =  1./(1+exp(-adjy))
            OnePlusE2negAdjY := PBblas.Apply2Elements(mYmap, e2negAdjY, AddOne);
        
        
        // expy =  1./(1+exp(-adjy))
            ExpY := PBblas.Apply2Elements(mYmap, OnePlusE2negAdjY, Reciprocal); 
                    
        // deriv := expy .* (1-expy)
            //prederiv := 
            Deriv := PBblas.HadamardProduct(mYmap, Expy, PBblas.Apply2Elements(mYmap,PBblas.PB_dscal(-1, Expy), AddOne));
        
        // Functions needed to calculate w_AdjY
        // The deriv .* adjy of wadjy := w .* (deriv .* adjy + (y-expy))
            derivXadjy := PBblas.HadamardProduct(mYmap, Deriv, AdjY);
        // The (y-expy) of wadjy := w .* (deriv .* adjy + (y-expy))
            yMINUSexpy := PBblas.PB_daxpy(1.0,mYdist,PBblas.PB_dscal(-1, Expy));
        // The (deriv .* adjy + (y-expy)) of wadjy := w .* (deriv .* adjy + (y-expy))
            forWadjy := PBblas.PB_daxpy(1, derivXadjy, yMINUSexpy);
        
        // wadjy := w .* (deriv .* adjy + (y-expy))
            w_Adjy := PBblas.HadamardProduct(mYmap, mWdist, forWadjy);
            
        // Functions needed to calculate Weights
        // The deriv .* w of weights := spdiags(deriv .* w, 0, n, n)
            derivXw := PBblas.HadamardProduct(mYmap,deriv, mWdist);
        
        // weights := spdiags(deriv .* w, 0, n, n)
            
            
            Weights := PBblas.Vector2Diag(weightsMap,derivXw,weightsMap);
            
        // Functions needed to calculate mBeta
        // x' * weights * x of mBeta := Inv(x' * weights * x + mRidge) * x' * wadjy
            
            xweight := PBblas.PB_dgemm(TRUE, FALSE, 1.0, mXmap, mXdist, weightsMap, weights, xWeightMap);
            xweightsx :=  PBblas.PB_dgemm(FALSE, FALSE, 1.0, xWeightMap, xweight, mXmap, mXdist, Ridgemap, mRidgedist, 1.0);
        
        // mBeta := Inv(x' * weights * x + mRidge) * x' * wadjy
            
            side := PBblas.PB_dgemm(TRUE, FALSE,1.0, mXmap, mXdist, mYmap, w_Adjy,xtranswadjyMap);
           
            LU_xwx  := PBblas.PB_dgetrf(Ridgemap, xweightsx);
           
            lc  := PBblas.PB_dtrsm(PBblas.Types.Side.Ax, PBblas.Types.Triangle.Lower, FALSE,
                                   PBblas.Types.Diagonal.UnitTri, 1.0, Ridgemap, LU_xwx, xtranswadjyMap, side);
 
            mBeta := PBblas.PB_dtrsm(PBblas.Types.Side.Ax, PBblas.Types.Triangle.Upper, FALSE,
                                     PBblas.Types.Diagonal.NotUnitTri, 1.0, Ridgemap, LU_xwx, xtranswadjyMap, lc);
            
            //Caculate error to be checked in loop evaluation
            err := SUM(DMAT.Converted.FromPart2Cell(PBblas.Apply2Elements(mBeta0map,PBblas.PB_daxpy(1.0, mBeta,PBblas.PB_dscal(-1, BetaDist)), absv)), v);
            
            errmap := PBblas.Matrix_Map(1, 1, 1, 1);
            
            BE := DATASET([{1,1,err}],Mat.Types.Element);
            BetaError := DMAT.Converted.FromElement(BE,errmap);
            
            BME := DATASET([{1,1,sizeTable[1].m_cols*Epsilon}],Mat.Types.Element);
            BetaMaxError := DMAT.Converted.FromElement(BME,errmap);         
            
            RETURN PBblas.MU.To(mBeta, mu_comp.Beta)+PBblas.MU.To(ExpY, mu_comp.Y)+PBblas.MU.To(BetaError,mu_comp.BetaError)+PBblas.MU.To(BetaMaxError,mu_comp.BetaMaxError);
            
        END;

        SHARED BetaPair := LOOP(mBeta00+OldExpY_00
                       , (COUNTER<=MaxIter)
                          AND (DMAT.Converted.FromPart2Elm(PBblas.MU.From(ROWS(LEFT),mu_comp.BetaError))[1].value > 
                               DMAT.Converted.FromPart2Elm(PBblas.MU.From(ROWS(LEFT),mu_comp.BetaMaxError))[1].value)
                       , Step(ROWS(LEFT),COUNTER)
                   ); 
    
        mBeta00map := PBblas.Matrix_Map(sizeTable[1].m_cols, 1, sizeTable[1].f_b_cols, 1);  
        
        EXPORT Beta := FUNCTION
            mubeta := DMAT.Converted.FromPart2DS(DMAT.Trans.Matrix(mBeta00map,PBblas.MU.From(BetaPair, mu_comp.Beta)));
            rebaseBeta := RebaseY.ToOldFromElemToPart(mubeta, Y_Map);
            RETURN rebaseBeta;
        END;
        
        Res := FUNCTION
            ret := PROJECT(Beta,TRANSFORM(l_model,SELF.Id := COUNTER+Base,SELF.number := LEFT.number, SELF.class_number := LEFT.id, SELF.w := LEFT.value));
            RETURN ret;
        END;
        ToField(Res,o);
        
        EXPORT Mod := o;
        modelY_M := DMAT.Converted.FromPart2Elm(PBblas.MU.From(BetaPair, mu_comp.Y));
        modelY_NF := RebaseY.ToOld(Types.FromMatrix(modelY_M),Y_Map);
        EXPORT modelY := modelY_NF;
    END;//End Logis
    
  EXPORT LearnCS(DATASET(Types.NumericField) Indep,DATASET(Types.DiscreteField) Dep) := Logis(Indep,PROJECT(Dep,Types.NumericField)).mod;
    EXPORT LearnC(DATASET(Types.NumericField) Indep,DATASET(Types.DiscreteField) Dep) := LearnCConcat(Indep,Dep,LearnCS);
    EXPORT Model(DATASET(Types.NumericField) mod) := FUNCTION
      FromField(mod,l_model,o);
        RETURN o;
    END;
  EXPORT ClassifyC(DATASET(Types.NumericField) Indep,DATASET(Types.NumericField) mod) := FUNCTION
        
        mod0 := Model(mod);
        Beta_0 := PROJECT(mod0,TRANSFORM(Types.NumericField,SELF.Number := LEFT.Number,SELF.id := LEFT.class_number, SELF.value := LEFT.w;SELF:=LEFT;));
        RebaseBeta := Utils.RebaseNumericFieldID(Beta_0);
        Beta0_Map := RebaseBeta.MappingID(1);
        Beta0 := RebaseBeta.ToNew(Beta0_Map);
        
        mX_0 := Types.ToMatrix(Indep);
        mXloc := Mat.InsertColumn(mX_0, 1, 1.0); // Insert X1=1 column 
        
        mXlocstats := Mat.Has(mXloc).Stats;
        mXloc_n := mXlocstats.XMax;
        mXloc_m := mXlocstats.YMax;
        
        havemaxrow := maxrows > 0;
        havemaxcol := maxcols > 0;
        havemaxrowcol := havemaxrow and havemaxcol;
        
        //Map for Matrix X. Map will be used to derive all other maps in ClassifyC
        derivemap := IF(havemaxrowcol, PBblas.AutoBVMap(mXloc_n, mXloc_m,prows,pcols,maxrows, maxcols),
                        IF(havemaxrow, PBblas.AutoBVMap(mXloc_n, mXloc_m,prows,pcols,maxrows),
                           IF(havemaxcol, PBblas.AutoBVMap(mXloc_n, mXloc_m,prows,pcols,,maxcols),
                           PBblas.AutoBVMap(mXloc_n, mXloc_m,prows,pcols))));
                    
        
        sizeRec := RECORD
            PBblas.Types.dimension_t m_rows;
            PBblas.Types.dimension_t m_cols;
            PBblas.Types.dimension_t f_b_rows;
            PBblas.Types.dimension_t f_b_cols;
        END;

        sizeTable := DATASET([{derivemap.matrix_rows,derivemap.matrix_cols,derivemap.part_rows(1),derivemap.part_cols(1)}], sizeRec);
        
        
        mXlocmap := PBblas.Matrix_Map(sizeTable[1].m_rows,sizeTable[1].m_cols,sizeTable[1].f_b_rows,sizeTable[1].f_b_cols);
        
        mXlocdist := DMAT.Converted.FromElement(mXloc,mXlocmap);
      
        mBeta := Types.ToMatrix(Beta0);
        mBetastats := Mat.Has(mBeta).Stats;
        mBeta_n := mBetastats.XMax;
        
        
        mBetamap := PBblas.Matrix_Map(mBeta_n, sizeTable[1].m_cols, 1, sizeTable[1].f_b_cols);
        mBetadist := DMAT.Converted.FromElement(mBeta,mBetamap);
      
        AdjYmap := PBblas.Matrix_Map(mXlocmap.matrix_rows, mBeta_n, mXlocmap.part_rows(1), 1);
        AdjY := PBblas.PB_dgemm(FALSE, TRUE, 
                             1.0, mXlocmap, mXlocdist, mBetamap, mBetaDist, 
                             AdjYmap);
        
        // expy =  1./(1+exp(-adjy))
        
        negAdjY := PBblas.PB_dscal(-1, AdjY);
        
        PBblas.Types.value_t e(PBblas.Types.value_t v, 
                                      PBblas.Types.dimension_t r, 
                                      PBblas.Types.dimension_t c) := exp(v);
                                      
        PBblas.Types.value_t AddOne(PBblas.Types.value_t v, 
                                      PBblas.Types.dimension_t r, 
                                      PBblas.Types.dimension_t c) := 1+v;
                                      
        PBblas.Types.value_t Reciprocal(PBblas.Types.value_t v, 
                                      PBblas.Types.dimension_t r, 
                                      PBblas.Types.dimension_t c) := 1/v;
        
        e2negAdjY := PBblas.Apply2Elements(AdjYmap, negAdjY, e);
        
        OnePlusE2negAdjY := PBblas.Apply2Elements(AdjYmap, e2negAdjY, AddOne);
        
        sig := PBblas.Apply2Elements(AdjYmap, OnePlusE2negAdjY, Reciprocal);
        
        //Rebase IDs so correct classifiers can be used
        sigtran := DMAT.Trans.Matrix(AdjYmap,sig);
        
        sigds :=DMAT.Converted.FromPart2DS(sigtran);
        
        sigconvds := RebaseBeta.ToOld(sigds, Beta0_Map);
        
        tranmap := PBblas.Matrix_Map(((mXloc_m-1)+mBeta_n), mXlocmap.matrix_rows, 1, mXlocmap.part_rows(1));
        
        preptranback := DMAT.Converted.FromNumericFieldDS(sigconvds, tranmap);
        
        sigtranback := DMAT.Trans.Matrix(tranmap, preptranback);
        
        sigmoid := DMAT.Converted.frompart2elm(sigtranback);
        
        // Now convert to classify return format
        l_result tr(sigmoid le) := TRANSFORM
          SELF.value := IF ( le.value > 0.5,1,0);
          SELF.id := le.x;
            SELF.number := le.y;
            SELF.conf := ABS(le.value-0.5);
        END;
        
        RETURN PROJECT(sigmoid,tr(LEFT));
        
    END;
    
    END; // Logistic Module 
 // Implementation of SoftMax classifier using PBblas Library
//SoftMax classifier generalizes logistic regression classifier for cases when we have more than two target classes
//The implemenataion is based on Stanford Deep Learning tutorial availabe at http://ufldl.stanford.edu/wiki/index.php/Softmax_Regression
//this implementation is based on using PBblas library
//parameters:
//LAMBDA : wight decay parameter in calculating SoftMax costfunction
//ALPHA : learning rate for updating softmax parameters
//IntTHETA: Initialized parameters that is a matrix of size (number of classes) * (number of features)
EXPORT SoftMax(DATASET (MAT.Types.Element) IntTHETA, REAL8 LAMBDA=0.001, REAL8 ALPHA=0.1, UNSIGNED2 MaxIter=100,
  UNSIGNED4 prows=0, UNSIGNED4 pcols=0,UNSIGNED4 Maxrows=0, UNSIGNED4 Maxcols=0) := MODULE(DEFAULT)
  Soft(DATASET(Types.NumericField) X,DATASET(Types.NumericField) Y) := MODULE
  //Convert the input data to matrix
  //the reason matrix transform is done after ocnverting the input data to the matrix is that
  //in this implementation it is assumed that the  input matrix shows the samples in column-wise format
  //in other words each sample is shown in one column. that's why after converting the input to matrix we apply
  //matrix tranform to refletc samples in column-wise format
  dt := Types.ToMatrix (X);
  SHARED dTmp := Mat.InsertColumn(dt,1,1.0); // add the intercept column
  SHARED d := Mat.Trans(dTmp); //in the entire of the calculations we work with the d matrix that each sample in presented in one column
  SHARED groundTruth:= Utils.ToGroundTruth (Y);//Instead of working with label matrix we work with groundTruth matrix
  //groundTruth is a Numclass*NumSamples matrix. groundTruth(i,j)=1 if label of the jth sample is i, otherwise groundTruth(i,j)=0
  SHARED NumClass := Mat.Has(groundTruth).Stats.XMax;
  SHARED sizeRec := RECORD
    PBblas.Types.dimension_t m_rows;
    PBblas.Types.dimension_t m_cols;
    PBblas.Types.dimension_t f_b_rows;
    PBblas.Types.dimension_t f_b_cols;
  END;
   //Map for Matrix d.
    SHARED havemaxrow := maxrows > 0;
    SHARED havemaxcol := maxcols > 0;
    SHARED havemaxrowcol := havemaxrow and havemaxcol;
    SHARED dstats := Mat.Has(d).Stats;
    SHARED d_n := dstats.XMax;
    SHARED d_m := dstats.YMax;
    derivemap := IF(havemaxrowcol, PBblas.AutoBVMap(d_n, d_m,prows,pcols,maxrows, maxcols),
                   IF(havemaxrow, PBblas.AutoBVMap(d_n, d_m,prows,pcols,maxrows),
                      IF(havemaxcol, PBblas.AutoBVMap(d_n, d_m,prows,pcols,,maxcols),
                      PBblas.AutoBVMap(d_n, d_m,prows,pcols))));
    SHARED sizeTable := DATASET([{derivemap.matrix_rows,derivemap.matrix_cols,derivemap.part_rows(1),derivemap.part_cols(1)}], sizeRec);
    Ones_VecMap := PBblas.Matrix_Map(1, NumClass, 1, NumClass);
    //New Vector Generator
    Layout_Cell gen(UNSIGNED4 c, UNSIGNED4 NumRows) := TRANSFORM
      SELF.y := ((c-1) % NumRows) + 1;
      SELF.x := ((c-1) DIV NumRows) + 1;
      SELF.v := 1;
    END;
    //Create Ones Vector for the calculations in the step fucntion
    Ones_Vec := DATASET(NumClass, gen(COUNTER, NumClass));
    Ones_Vecdist := DMAT.Converted.FromCells(Ones_VecMap, Ones_Vec);
    //Create block matrix d
    dmap := PBblas.Matrix_Map(sizeTable[1].m_rows,sizeTable[1].m_cols,sizeTable[1].f_b_rows,sizeTable[1].f_b_cols);
    ddist := DMAT.Converted.FromElement(d,dmap);
    //Create block matrix groundTruth
    groundTruthmap := PBblas.Matrix_Map(NumClass, sizeTable[1].m_cols, NumClass, sizeTable[1].f_b_cols);
    groundTruthdist := DMAT.Converted.FromElement(groundTruth, groundTruthmap);
    // creat block matrix for IntTHETA
    IntTHETAmap := PBblas.Matrix_Map(NumClass, sizeTable[1].m_rows, NumClass, sizeTable[1].f_b_rows);
    IntTHETAdist := DMAT.Converted.FromElement(IntTHETA, IntTHETAmap);
    //Maps used in step fucntion
    col_col_map := PBblas.Matrix_Map(sizeTable[1].m_cols, sizeTable[1].m_cols, sizeTable[1].f_b_cols, sizeTable[1].f_b_cols);
    THETAmap := PBblas.Matrix_Map(NumClass, sizeTable[1].m_rows, NumClass, sizeTable[1].f_b_rows);
    txmap := groundTruthmap;
    SumColMap := PBblas.Matrix_Map(1, sizeTable[1].m_cols, 1, sizeTable[1].f_b_cols);
    //functions used in step fucntion
    PBblas.Types.value_t e(PBblas.Types.value_t v,PBblas.Types.dimension_t r,PBblas.Types.dimension_t c) := exp(v);
    PBblas.Types.value_t reci(PBblas.Types.value_t v,PBblas.Types.dimension_t r,PBblas.Types.dimension_t c) := 1/v;
    m := d_m; //number of samples
    m_1 := -1 * (1/m);
    Step(DATASET(PBblas.Types.Layout_Part ) THETA) := FUNCTION
      // tx=(theta*d);
      tx := PBblas.PB_dgemm(FALSE, FALSE, 1.0, THETAmap, THETA, dmap, ddist, txmap);
      // tx_M = bsxfun(@minus, tx, max(tx, [], 1));
      tx_mat := DMat.Converted.FromPart2Elm(tx);
      MaxCol_tx_mat := Mat.Has(tx_mat).MaxCol;
      MaxCol_tx := DMAT.Converted.FromElement(MaxCol_tx_mat, SumColMap);
      tx_M := PBblas.PB_dgemm(TRUE, FALSE, -1.0, Ones_VecMap, Ones_Vecdist, SumColMap, MaxCol_tx, txmap, tx, 1.0);
      // Mat.Types.Element DoMinus(tx_mat le,MaxCol_tx_mat ri) := TRANSFORM
        // SELF.x := le.x;
        // SELF.y := le.y;
        // SELF.value := le.value - ri.value;
      // END;
      // tx_mat_M :=  JOIN(tx_mat, MaxCol_tx_mat, LEFT.y=RIGHT.y, DoMinus(LEFT,RIGHT),LOOKUP);
      // tx_M := DMAT.Converted.FromElement(tx_mat_M, txmap);
      //exp_tx_M=exp(tx_M);
      exp_tx_M := PBblas.Apply2Elements(txmap, tx_M, e);
      //Prob = bsxfun(@rdivide, exp_tx_M, sum(exp_tx_M));
      SumCol_exp_tx_M := PBblas.PB_dgemm(FALSE, FALSE, 1.0, Ones_VecMap, Ones_Vecdist, txmap, exp_tx_M, SumColMap);
      SumCol_exp_tx_M_rcip := PBblas.Apply2Elements(SumColMap, SumCol_exp_tx_M, Reci);
      SumCol_exp_tx_M_rcip_diag := PBblas.Vector2Diag(SumColMap, SumCol_exp_tx_M_rcip, col_col_map);
      Prob := PBblas.PB_dgemm(FALSE, FALSE, 1.0, txmap, exp_tx_M, col_col_map, SumCol_exp_tx_M_rcip_diag, txmap);
      second_term := PBblas.PB_dscal((1-ALPHA*LAMBDA), THETA);
      groundTruth_Prob := PBblas.PB_daxpy(1.0,groundTruthdist,PBblas.PB_dscal(-1, Prob));
      groundTruth_Prob_x := PBblas.PB_dgemm(FALSE, True, 1.0, txmap, groundTruth_Prob, dmap, ddist, THETAmap);
      // first_term := PBblas.PB_dscal((-1*ALPHA*m_1), groundTruth_Prob_x);
      // UpdatedTHETA := PBblas.PB_daxpy(1.0, first_term, second_term);
      UpdatedTHETA := PBblas.PB_daxpy((-1*ALPHA*m_1), groundTruth_Prob_x, second_term);
      RETURN UpdatedTHETA;
    END; // END step
    param := LOOP(IntTHETAdist, COUNTER <= MaxIter, Step(ROWS(LEFT)));
    //param := LOOP(IntTHETAdist, MaxIter, Step(ROWS(LEFT))); // does not work
    EXPORT Mod := ML.DMat.Converted.FromPart2DS (param);
  END; //END Soft
  EXPORT LearnC(DATASET(Types.NumericField) Indep, DATASET(Types.DiscreteField) Dep) := Soft(Indep,PROJECT(Dep,Types.NumericField)).mod;
  EXPORT Model(DATASET(Types.NumericField) mod) := FUNCTION
    o:= Types.ToMatrix (Mod);
    RETURN o;
  END; // END Model
  EXPORT ClassProbDistribC(DATASET(Types.NumericField) Indep,DATASET(Types.NumericField) mod) :=FUNCTION
    // take the same steps take in step function to calculate prob
    X := Indep;
    dt := Types.ToMatrix (X);
    dTmp := Mat.InsertColumn(dt,1,1.0);
    d := Mat.Trans(dTmp);
    sizeRec := RECORD
      PBblas.Types.dimension_t m_rows;
      PBblas.Types.dimension_t m_cols;
      PBblas.Types.dimension_t f_b_rows;
      PBblas.Types.dimension_t f_b_cols;
    END;
   //Map for Matrix d.
    havemaxrow := maxrows > 0;
    havemaxcol := maxcols > 0;
    havemaxrowcol := havemaxrow and havemaxcol;
    dstats := Mat.Has(d).Stats;
    d_n := dstats.XMax;
    d_m := dstats.YMax;
    derivemap := IF(havemaxrowcol, PBblas.AutoBVMap(d_n, d_m,prows,pcols,maxrows, maxcols),
                   IF(havemaxrow, PBblas.AutoBVMap(d_n, d_m,prows,pcols,maxrows),
                      IF(havemaxcol, PBblas.AutoBVMap(d_n, d_m,prows,pcols,,maxcols),
                      PBblas.AutoBVMap(d_n, d_m,prows,pcols))));
    sizeTable := DATASET([{derivemap.matrix_rows,derivemap.matrix_cols,derivemap.part_rows(1),derivemap.part_cols(1)}], sizeRec);
    dmap := PBblas.Matrix_Map(sizeTable[1].m_rows,sizeTable[1].m_cols,sizeTable[1].f_b_rows,sizeTable[1].f_b_cols);
    //Create block matrix d
    ddist := DMAT.Converted.FromElement(d,dmap);
    param := Model (mod);
    NumClass := Mat.Has(param).Stats.XMax;
    Ones_VecMap := PBblas.Matrix_Map(1, NumClass, 1, NumClass);
    //New Vector Generator
    Layout_Cell gen(UNSIGNED4 c, UNSIGNED4 NumRows) := TRANSFORM
      SELF.y := ((c-1) % NumRows) + 1;
      SELF.x := ((c-1) DIV NumRows) + 1;
      SELF.v := 1;
    END;
    //Create Ones Vector for the calculations in the step fucntion
    Ones_Vec := DATASET(NumClass, gen(COUNTER, NumClass),DISTRIBUTED);
    Ones_Vecdist := DMAT.Converted.FromCells(Ones_VecMap, Ones_Vec);
    THETAmap := PBblas.Matrix_Map(NumClass, sizeTable[1].m_rows, NumClass, sizeTable[1].f_b_rows);
    THETA := DMAT.Converted.FromElement(param, THETAmap);
    txmap := PBblas.Matrix_Map(NumClass, sizeTable[1].m_cols, NumClass, sizeTable[1].f_b_cols);
    SumColMap := PBblas.Matrix_Map(1, sizeTable[1].m_cols, 1, sizeTable[1].f_b_cols);
    col_col_map := PBblas.Matrix_Map(sizeTable[1].m_cols, sizeTable[1].m_cols, sizeTable[1].f_b_cols, sizeTable[1].f_b_cols);
    PBblas.Types.value_t reci(PBblas.Types.value_t v,PBblas.Types.dimension_t r,PBblas.Types.dimension_t c) := 1/v;
    tx := PBblas.PB_dgemm(FALSE, FALSE, 1.0, THETAmap, THETA, dmap, ddist, txmap);
    // tx_M = bsxfun(@minus, tx, max(tx, [], 1));
    tx_mat := DMat.Converted.FromPart2Elm(tx);
    MaxCol_tx_mat := Mat.Has(tx_mat).MaxCol;
    MaxCol_tx := DMAT.Converted.FromElement(MaxCol_tx_mat, SumColMap);
    tx_M := PBblas.PB_dgemm(TRUE, FALSE, -1.0, Ones_VecMap, Ones_Vecdist, SumColMap, MaxCol_tx, txmap, tx, 1.0);
    //exp_tx_M=exp(tx_M);
    PBblas.Types.value_t e(PBblas.Types.value_t v,PBblas.Types.dimension_t r,PBblas.Types.dimension_t c) := exp(v);
    //Prob = bsxfun(@rdivide, exp_tx_M, sum(exp_tx_M));
    exp_tx_M := PBblas.Apply2Elements(txmap, tx_M, e);
    SumCol_exp_tx_M := PBblas.PB_dgemm(FALSE, FALSE, 1.0, Ones_VecMap, Ones_Vecdist, txmap, exp_tx_M, SumColMap);
    SumCol_exp_tx_M_rcip := PBblas.Apply2Elements(SumColMap, SumCol_exp_tx_M, Reci);
    SumCol_exp_tx_M_rcip_diag := PBblas.Vector2Diag(SumColMap, SumCol_exp_tx_M_rcip, col_col_map);
    Prob := PBblas.PB_dgemm(FALSE, FALSE, 1.0, txmap, exp_tx_M, col_col_map, SumCol_exp_tx_M_rcip_diag, txmap);
    Prob_mat := DMAT.Converted.FromPart2Elm (Prob);
    Types.l_result tr(Mat.Types.Element le) := TRANSFORM
      SELF.value := le.x;
      SELF.id := le.y;
      SELF.number := 1; //number of class
      SELF.conf := le.value;
    END;
    RETURN PROJECT (Prob_mat, tr(LEFT));
  END; // END ClassProbDistribC Function
  EXPORT ClassifyC(DATASET(Types.NumericField) Indep,DATASET(Types.NumericField) mod) := FUNCTION
    Dist := ClassProbDistribC(Indep, mod);
    numrow := MAX (Dist,Dist.value);
    S:= SORT(Dist,id,conf);
    SeqRec := RECORD
    l_result;
    INTEGER8 Sequence := 0;
    END;
    //add seq field to S
    SeqRec AddS (S l, INTEGER c) := TRANSFORM
    SELF.Sequence := c%numrow;
    SELF := l;
    END;
    Sseq := PROJECT(S, AddS(LEFT,COUNTER));
    classified := Sseq (Sseq.Sequence=0);
    RETURN PROJECT(classified,l_result);
  END; // END ClassifyC Function
END; //END SoftMax
/* From Wikipedia: 
http://en.wikipedia.org/wiki/Decision_tree_learning#General
"... Decision tree learning is a method commonly used in data mining.
The goal is to create a model that predicts the value of a target variable based on several input variables.
... A tree can be "learned" by splitting the source set into subsets based on an attribute value test. 
This process is repeated on each derived subset in a recursive manner called recursive partitioning. 
The recursion is completed when the subset at a node has all the same value of the target variable,
or when splitting no longer adds value to the predictions.
This process of top-down induction of decision trees (TDIDT) [1] is an example of a greedy algorithm,
and it is by far the most common strategy for learning decision trees from data, but it is not the only strategy."
The module can learn using different splitting algorithms, and return a model.
The Decision Tree (model) has the same structure independently of which split algorithm was used.
The model  is used to predict the class from new examples.
*/
	EXPORT DecisionTree := MODULE
/*	
		Decision Tree Learning using Gini Impurity-Based criterion
*/
    EXPORT GiniImpurityBased(INTEGER1 Depth=10, REAL Purity=1.0):= MODULE(DEFAULT)
      EXPORT LearnD(DATASET(Types.DiscreteField) Indep, DATASET(Types.DiscreteField) Dep) := FUNCTION
        nodes := ML.Trees.SplitsGiniImpurBased(Indep, Dep, Depth, Purity);
        RETURN ML.Trees.ToDiscreteTree(nodes);
      END;
      EXPORT ClassProbDistribD(DATASET(Types.DiscreteField) Indep,DATASET(Types.NumericField) mod) :=FUNCTION
        RETURN ML.Trees.ClassProbDistribD(Indep, mod);
      END;
      EXPORT ClassifyD(DATASET(Types.DiscreteField) Indep,DATASET(Types.NumericField) mod) := FUNCTION
        RETURN ML.Trees.ClassifyD(Indep,mod);
      END;
      EXPORT Model(DATASET(Types.NumericField) mod) := FUNCTION
        RETURN ML.Trees.ModelD(mod);
      END;
    END;  // Gini Impurity DT Module
/*
		Decision Tree using C4.5 Algorithm (Quinlan, 1987)
*/
    EXPORT C45(BOOLEAN Pruned= TRUE, INTEGER1 numFolds = 3, REAL z = 0.67449) := MODULE(DEFAULT)
      EXPORT LearnD(DATASET(Types.DiscreteField) Indep, DATASET(Types.DiscreteField) Dep) := FUNCTION
        nodes := IF(Pruned, Trees.SplitsIGR_Pruned(Indep, Dep, numFolds, z), Trees.SplitsInfoGainRatioBased(Indep, Dep));
        RETURN ML.Trees.ToDiscreteTree(nodes);
      END;
      EXPORT ClassProbDistribD(DATASET(Types.DiscreteField) Indep,DATASET(Types.NumericField) mod) :=FUNCTION
        RETURN ML.Trees.ClassProbDistribD(Indep, mod);
      END;
      EXPORT ClassifyD(DATASET(Types.DiscreteField) Indep,DATASET(Types.NumericField) mod) := FUNCTION
        RETURN ML.Trees.ClassifyD(Indep,mod);
      END;
      EXPORT Model(DATASET(Types.NumericField) mod) := FUNCTION
        RETURN ML.Trees.ModelD(mod);
      END;
    END;  // C45 DT Module

/*  C45 Binary Decision Tree
    It learns from continuous data and builds a Binary Decision Tree based on Info Gain Ratio
    Configuration Input
      minNumObj   minimum number of instances in a leaf node, used in splitting process
      maxLevel    stop learning criteria, either tree's level reachs maxLevel depth or no more split can be done.
*/
    EXPORT C45Binary(t_Count minNumObj=2, ML.Trees.t_level maxLevel=32) := MODULE(DEFAULT)
      EXPORT LearnC(DATASET(Types.NumericField) Indep, DATASET(Types.DiscreteField) Dep) := FUNCTION
        nodes := Trees.SplitBinaryCBased(Indep, Dep, minNumObj, maxLevel);
        RETURN ML.Trees.ToNumericTree(nodes);
      END;
      EXPORT ClassifyC(DATASET(Types.NumericField) Indep,DATASET(Types.NumericField) mod) := FUNCTION
        RETURN ML.Trees.ClassifyC(Indep,mod);
      END;
      EXPORT Model(DATASET(Types.NumericField) mod) := FUNCTION
        RETURN ML.Trees.ModelC(mod);
      END;
    END; // C45Binary DT Module
  END; // DecisionTree Module
	
/* From http://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm#overview
   "... Random Forests grows many classification trees.
   To classify a new object from an input vector, put the input vector down each of the trees in the forest.
   Each tree gives a classification, and we say the tree "votes" for that class.
   The forest chooses the classification having the most votes (over all the trees in the forest).

   Each tree is grown as follows:
   - If the number of cases in the training set is N, sample N cases at random - but with replacement, from the original data.
     This sample will be the training set for growing the tree.
   - If there are M input variables, a number m<<M is specified such that at each node, m variables are selected at random out of the M
     and the best split on these m is used to split the node. The value of m is held constant during the forest growing.
   - Each tree is grown to the largest extent possible. There is no pruning. ..."

Configuration Input
   treeNum    number of trees to generate
   fsNum      number of features to sample each iteration
   Purity     p <= 1.0
   Depth      max tree level
*/
  EXPORT RandomForest(t_Count treeNum, t_Count fsNum, REAL Purity=1.0, INTEGER1 Depth=32):= MODULE
    EXPORT LearnD(DATASET(Types.DiscreteField) Indep, DATASET(Types.DiscreteField) Dep) := FUNCTION
      nodes := Ensemble.SplitFeatureSampleGI(Indep, Dep, treeNum, fsNum, Purity, Depth);
      RETURN ML.Ensemble.ToDiscreteForest(nodes);
    END;
    EXPORT LearnC(DATASET(Types.NumericField) Indep, DATASET(Types.DiscreteField) Dep) := FUNCTION
      nodes := Ensemble.SplitFeatureSampleGIBin(Indep, Dep, treeNum, fsNum, Purity, Depth);
      RETURN ML.Ensemble.ToContinuosForest(nodes);
    END;
    // Transform NumericFiled "mod" to Ensemble.gSplitF "discrete tree nodes" model format using field map model_Map
    EXPORT Model(DATASET(Types.NumericField) mod) := FUNCTION
      RETURN ML.Ensemble.FromDiscreteForest(mod);
    END;
    // Transform NumericFiled "mod" to Ensemble.gSplitC "binary tree nodes" model format using field map modelC_Map
    EXPORT ModelC(DATASET(Types.NumericField) mod) := FUNCTION
      RETURN ML.Ensemble.FromContinuosForest(mod);
    END;
    // The functions return instances' class probability distribution for each class value
    // based upon independent values (Indep) and the ensemble model (mod).
    EXPORT ClassProbDistribD(DATASET(Types.DiscreteField) Indep, DATASET(Types.NumericField) mod) :=FUNCTION
      RETURN ML.Ensemble.ClassProbDistribForestD(Indep, mod);
    END;
    EXPORT ClassProbDistribC(DATASET(Types.NumericField) Indep,DATASET(Types.NumericField) mod) :=FUNCTION
      RETURN ML.Ensemble.ClassProbDistribForestC(Indep, mod);
    END;
    // Classification functions based upon independent values (Indep) and the ensemble model (mod).
    EXPORT ClassifyD(DATASET(Types.DiscreteField) Indep,DATASET(Types.NumericField) mod) := FUNCTION
      RETURN ML.Ensemble.ClassifyDForest(Indep, mod);
    END;
    EXPORT ClassifyC(DATASET(Types.NumericField) Indep,DATASET(Types.NumericField) mod) := FUNCTION
      RETURN ML.Ensemble.ClassifyCForest(Indep,mod);
    END;
  END; // RandomTree module

/*
  Area Under the ROC curve
*/
  // The function calculate the Area Under the ROC curve based on:
  // - classProbDistclass : probability distribution for each instance
  // - positiveClass      : the class of interest
  // - Dep                : instance's class value
  // The function returns all points of the ROC curve for graphic purposes:
  // label: threshold, point: (threshold's false negative rate, threshold's true positive rate).
  // The area under the ROC curve is returned in the AUC field of the last record.
  // Note: threshold = 100 means classifying all instances as negative, it is not necessarily part of the curve
  EXPORT AUC_ROC(DATASET(l_result) classProbDist, Types.t_Discrete positiveClass, DATASET(Types.DiscreteField) Dep) := FUNCTION
    SHARED cntREC:= RECORD
      Types.t_FieldNumber classifier;  // The classifier in question (value of 'number' on outcome data)
      Types.t_Discrete  c_actual;      // The value of c provided
      Types.t_FieldReal score :=-1;
      Types.t_count     tp_cnt:=0;
      Types.t_count     fn_cnt:=0;
      Types.t_count     fp_cnt:=0;
      Types.t_count     tn_cnt:=0;
    END;
    SHARED compREC:= RECORD(cntREC)
      Types.t_Discrete  c_modeled;
    END;
    classOfInterest := classProbDist(value = positiveClass);
    compared:= JOIN(classOfInterest, Dep, LEFT.id=RIGHT.id AND LEFT.number=RIGHT.number,
                            TRANSFORM(compREC, SELF.classifier:= LEFT.number, SELF.c_actual:=RIGHT.value,
                            SELF.c_modeled:=LEFT.value, SELF.score:=LEFT.conf), HASH);
    sortComp:= SORT(compared, score);
    coi_acc:= TABLE(sortComp, {classifier, score, cntPos:= COUNT(GROUP, c_actual = c_modeled),
                                  cntNeg:= COUNT(GROUP, c_actual<>c_modeled)}, classifier, score, LOCAL);
    coi_tot:= TABLE(coi_acc, {classifier, totPos:= SUM(GROUP, cntPos), totNeg:= SUM(GROUP, cntNeg)}, classifier, FEW);
    totPos:=EVALUATE(coi_tot[1], totPos);
    totNeg:=EVALUATE(coi_tot[1], totNeg);
    // Count and accumulate number of TP, FP, TN and FN instances for each threshold (score)
    acc_sorted:= PROJECT(coi_acc, TRANSFORM(cntREC, SELF.c_actual:= positiveClass, SELF.fn_cnt:= LEFT.cntPos,
                                  SELF.tn_cnt:= LEFT.cntNeg, SELF:= LEFT), LOCAL);
    cntREC accNegPos(cntREC l, cntREC r) := TRANSFORM
      deltaPos:= l.fn_cnt + r.fn_cnt;
      deltaNeg:= l.tn_cnt + r.tn_cnt;
      SELF.score:= r.score;
      SELF.tp_cnt:=  totPos - deltaPos;
      SELF.fn_cnt:=  deltaPos;
      SELF.fp_cnt:=  totNeg - deltaNeg;
      SELF.tn_cnt:= deltaNeg;
      SELF:= r;
    END;
    cntNegPos:= ITERATE(acc_sorted, accNegPos(LEFT, RIGHT));
    accnew := DATASET([{1,positiveClass,-1,totPos,0,totNeg,0}], cntREC) + cntNegPos;
    curvePoint:= RECORD
      Types.t_Count       id;
      Types.t_FieldNumber classifier;
      Types.t_FieldReal   thresho;
      Types.t_FieldReal   fpr;
      Types.t_FieldReal   tpr;
      Types.t_FieldReal   deltaPos:=0;
      Types.t_FieldReal   deltaNeg:=0;
      Types.t_FieldReal   cumNeg:=0;
      Types.t_FieldReal   AUC:=0;
    END;
    // Transform all into ROC curve points
    rocPoints:= PROJECT(accnew, TRANSFORM(curvePoint, SELF.id:=COUNTER, SELF.thresho:=LEFT.score,
                                SELF.fpr:= LEFT.fp_cnt/totNeg, SELF.tpr:= LEFT.tp_cnt/totPos, SELF.AUC:=IF(totNeg=0,1,0) ,SELF:=LEFT));
    // Calculate the area under the curve (cumulative iteration)
    curvePoint rocArea(curvePoint l, curvePoint r) := TRANSFORM
      deltaPos  := if(l.tpr > r.tpr, l.tpr - r.tpr, 0.0);
      deltaNeg  := if( l.fpr > r.fpr, l.fpr - r.fpr, 0.0);
      SELF.deltaPos := deltaPos;
      SELF.deltaNeg := deltaNeg;
      // A classification without incorrectly classified instances must return AUC = 1
      SELF.AUC      := IF(r.fpr=0 AND l.tpr=0 AND r.tpr=1, 1, l.AUC) + deltaPos * (l.cumNeg + 0.5* deltaNeg);
      SELF.cumNeg   := l.cumNeg + deltaNeg;
      SELF:= r;
    END;
    RETURN ITERATE(rocPoints, rocArea(LEFT, RIGHT));
  END;

  // Support Vector Machine.
  //  see https://en.wikipedia.org/wiki/Support_vector_machine
  // Use the SVM attributes directly for scaling and grid search.  This
  //module acts as a facade to the actual SVM attributes and provides only
  //the interface and capabilities defined for the Classify abstract.
  //
  // The inputs are:
  /*  svm_type : set type of SVM, SVM.Types.SVM_Type enum
              C_SVC    (multi-class classification)
              NU_SVC   (multi-class classification)
              ONE_CLASS SVM
              EPSILON_SVR  (regression)
              NU_SVR   (regression)
      kernel_type : set type of kernel function, SVM.Types.Kernel_Type enum
              LINEAR: u'*v
              POLY:   polynomial,  (gamma*u'*v + coef0)^degree
              RBF:    radial basis function: exp(-gamma*|u-v|^2)
              SIGMOID: tanh(gamma*u'*v + coef0)
              PRECOMPUTED: precomputed kernel (kernel values in training_set_file)
      degree : degree in kernel function for POLY
      gamma  : gamma in kernel function for POLY, RBF, and SIGMOID
      coef0  : coef0 in kernel function for POLY, SIGMOID
      cost   : the parameter C of C-SVC, epsilon-SVR, and nu-SVR
      eps    : the epsilon for stopping
      nu     : the parameter nu of nu-SVC, one-class SVM, and nu-SVR
      p      : the epsilon in loss function of epsilon-SVR
      shrinking : whether to use the shrinking heuristics, default TRUE
  */
  // The LibSVM development package must be installed on your cluster!
  EXPORT SVM(SVM.Types.SVM_Type svm_type, SVM.Types.Kernel_Type kernel_type,
             INTEGER4 degree, REAL8 gamma, REAL8 coef0, REAL8 cost, REAL8 eps,
             REAL8 nu, REAL8 p, BOOLEAN shrinking) := MODULE(DEFAULT)
    SVM.Types.Training_Parameters
    makeParm(UNSIGNED4 dep_field, SVM.Types.SVM_Type svm_type,
             SVM.Types.Kernel_Type kernel_type,
             INTEGER4 degree, REAL8 gamma, REAL8 coef0, REAL8 cost,
             REAL8 eps, REAL8 nu, REAL8 p, BOOLEAN shrinking) := TRANSFORM
      SELF.id := dep_field;
      SELF.svmType := svm_type;
      SELF.kernelType := kernel_type;
      SELF.degree := degree;
      SELF.gamma := gamma;
      SELF.coef0 := coef0;
      SELF.C := cost;
      SELF.eps := eps;
      SELF.nu := nu;
      SELF.p := p;
      SELF.shrinking := shrinking;
      SELF.prob_est := FALSE;
      SELF := [];
    END;
    SHARED Training_Param(UNSIGNED4 df) := ROW(makeParm(df, svm_type,
                                          kernel_type, degree,
                                          gamma, coef0, cost, eps, nu,
                                          p, shrinking));
    // Learn from continuous data
    EXPORT LearnC(DATASET(Types.NumericField) Indep,
                  DATASET(Types.DiscreteField) Dep) := FUNCTION
      depc := PROJECT(Dep, Types.NumericField);
      inst_data := SVM.Converted.ToInstance(Indep, Depc);
      dep_field := dep[1].number;
      tp := DATASET(Training_Param(dep_field));
      mdl := SVM.train(tp, inst_data);
      nf_mdl := SVM.Converted.FromModel(Base, mdl);
      RETURN nf_mdl; // All classifiers serialized to numeric field format
    END;
    // Learn from discrete data uses DEFAULT implementation
    // Classify continuous data - using a prebuilt model
    EXPORT ClassifyC(DATASET(Types.NumericField) Indep,
                     DATASET(Types.NumericField) mod) := FUNCTION
      inst_data := SVM.Converted.ToInstance(Indep);
      mdl := SVM.Converted.ToModel(mod);
      pred := SVM.predict(mdl, inst_data).Prediction;
      // convert to standard form
      l_result cvt(SVM.Types.SVM_Prediction p) := TRANSFORM
        SELF.id := p.rid;
        SELF.number := p.id; // model ID is the dependent var field ID
        SELF.value := p.predict_y;
        SELF.conf := 0.5; // no confidence measures
      END;
      rslt := PROJECT(pred, cvt(LEFT));
      RETURN rslt;
    END;
    // Classify discrete data - uses DEFAULT implementation
  END; // SVM
END;