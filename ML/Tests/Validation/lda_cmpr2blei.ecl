//Compare the ML.LDA.Topic_Estimation results against the results from
//the LDA implementation by David Blei et al.  See
//   https://www.cs.princeton.edu/~blei/lda-c/index.html
//and download the lda-c.tgz file and the sample data (2246 AP documents).
//
//Build and run the LDA program.  Run the program a second time pointing to
//the first run 000 file.  This is necessary to get a stable (repeatable)
//set of initial beta values because the model load drops precision from
//the betq values.  Be certain to use the same number of topics and settings
//values for both runs of the reference program and for this run.
//
//Spray the two 000 files, the three final files, and the lda_ap.dat file.
//The beta file has large records, so make the maximum record size 300K.
//
//You will want to change the file names used to correspond with your file
//naming standards.
//
// You can expect a few differences in the results.  These differences are
//primarily due to the update of the topic Gamma values for each word performed
//by the Blei reference implementation.  This implementation updates the
//topic Gamma values after all of the words are processed as described in
//the paper.
//
//  The Missing and Mystery counts reflect base Beta values that are missing or
//don't correspond to the 25 topics.  These should always be empty.  The most
//likely source of a problem is that the Spray broke the records because the
//maximum length specified was too small.
//
filename_prefix := '~THOR::JDH::LDA_TEST::';
init_other  := filename_prefix + '000.other';
init_beta   := filename_prefix + '000.beta';
final_beta  := filename_prefix + 'final_trial3.beta';
final_gamma := filename_prefix + 'final_trial3.gamma';
final_other := filename_prefix + 'final_trial3.other';
documents   := filename_prefix + 'lda_ap.dat';
vocabulary  := filename_prefix + 'lda_vocab.txt';
//********************* Run parameters ***************************
num_topics := 25;
alpha := 2.0;
var_max_iter := 20;
var_convergence := 0.000001; //1e-6
em_max_iter := 100;
em_convergence := 0.0001; //1e-4
estimate_alpha := TRUE;
//******************************************************************
IMPORT ML.LDA;
IMPORT STD.Str;
// Make the parameter dataset, 1 record
LDA.Types.Model_Parameters makeParm() := TRANSFORM
  SELF.model := 1;
  SELF.num_topics := num_topics;
  SELF.alpha := alpha;
  SELF.max_beta_iterations := em_max_iter;
  SELF.max_doc_iterations := var_max_iter;
  SELF.doc_epsilon := var_convergence;
  SELF.beta_epsilon := em_convergence;
  SELF.estimate_alpha := estimate_alpha;
  SELF.initial_alpha := alpha;
END;
model_parms := DATASET([makeParm()]);
// extract the initial model values, alpha and the betas
StrRec := RECORD
  STRING line;
END;
beta_text := RECORD
  LDA.Types.t_topic topic;
  STRING line;
END;
init_raw_betas := PROJECT(DATASET(init_beta, StrRec, CSV),
                          TRANSFORM(beta_text, SELF.topic:=COUNTER, SELF:=LEFT));
LDA.Types.TermValue extV(STRING s, UNSIGNED n) := TRANSFORM
  SELF.nominal := n;
  SELF.v := (REAL8) s;   // values from lda-dist-c are log(beta)
END;
LDA.Types.Model_Topic getBetas(beta_text bt) := TRANSFORM
  SELF.model := 1;
  SELF.topic := bt.topic;
  SELF.alpha := alpha;
  SELF.logBetas := PROJECT(DATASET(Str.SplitWords(bt.line, ' '), StrRec),
                           extV(LEFT.line, COUNTER-1));
END;
initial_model := PROJECT(init_raw_betas, getBetas(LEFT))
               : PERSIST(filename_prefix+'initial_model', EXPIRE(10), SINGLE);
// get the document term vectors
LDA.Types.TermFreq extTF(STRING s) := TRANSFORM
  pair := Str.SplitWords(s, ':');
  SELF.nominal := (UNSIGNED) pair[1];
  SELF.v := (UNSIGNED) pair[2];
END;
Work_Doc := RECORD(LDA.Types.Doc_Mapped)
  UNSIGNED4 terms;
END;
Work_Doc makeDoc(STRING s, UNSIGNED4 c) := TRANSFORM
  items := Str.SplitWords(s, ' ');
  items_ds := DATASET(items[2..], StrRec);
  SELF.rid := c;
  SELF.word_counts := SORT(PROJECT(items_ds, extTF(LEFT.line)), nominal);
  SELF.models := [1];
  SELF.terms := (UNSIGNED) items[1];  // first item is term count
END;
wd := PROJECT(DATASET(documents, StrRec, CSV), makeDoc(LEFT.line, COUNTER));
docs := PROJECT(wd, LDA.Types.Doc_Mapped)
     : PERSIST(filename_prefix+'doc_input', EXPIRE(10), SINGLE);
//**************************************************************************
// Run LDA
//**************************************************************************
stats := LDA.Collection_Stats(model_parms, docs);
test_topics := LDA.Topic_Estimation(model_parms, initial_model, stats, docs)
             : PERSIST(filename_prefix+'result', EXPIRE(10), SINGLE);
// get baseline beta values
base_raw_betas := PROJECT(DATASET(final_beta, StrRec, CSV),
                         TRANSFORM(beta_text, SELF.topic:=COUNTER, SELF:=LEFT));
Topic_Nominal_Beta := RECORD
  LDA.Types.t_model_id model;
  LDA.Types.t_topic topic;
  LDA.Types.t_nominal nominal;
  REAL8 v;
END;
Topic_Nominal_Beta exBeta(LDA.Types.Model_Topic m,
                           LDA.Types.TermValue tv) := TRANSFORM
  SELF.topic := m.topic;
  SELF.model := m.model;
  SELF.nominal := tv.nominal;
  SELF.v := EXP(tv.v);
END;
base_betas := NORMALIZE(PROJECT(base_raw_betas, getBetas(LEFT)),
                       LEFT.logBetas, exBeta(LEFT, RIGHT));
// get baseline document topic scores
LDA.Types.Topic_Value extTV(STRING s, UNSIGNED t) := TRANSFORM
  SELF.topic := t;
  SELF.v := (REAL8) s;
END;
LDA.Types.Document_Scored extDocGamma(StrRec sr, UNSIGNED r) := TRANSFORM
  SELF.model := 1;
  SELF.rid := r;
  SELF.likelihood := 0;   // log likelihood
  SELF.topics := PROJECT(DATASET(Str.SplitWords(sr.line, ' '), StrRec),
                           extTV(LEFT.line, COUNTER));
END;
base_raw_gammas := PROJECT(DATASET(final_gamma, {STRING line}, CSV),
                           extDocGamma(LEFT, COUNTER));
// get alpha
Alpha_Rec := RECORD
  LDA.Types.t_topic topic;
  REAL8 alpha;
END;
Alpha_Rec extAlpha(STRING l) := TRANSFORM
  wds := Str.SplitWords(l, ' ');
  SELF.alpha := (REAL8) wds[2];
  SELF.topic := 1;
END;
base_raw_alpha := PROJECT(DATASET(final_other, StrRec, CSV)(line[1..5]='alpha'),
                         extAlpha(LEFT.line));
// Get the vocab list
LDA.Types.Term_Dict getVocab(StrRec sr, UNSIGNED n) := TRANSFORM
  SELF.nominal := n;
  SELF.term := sr.line;
END;
vocab := PROJECT(DATASET(vocabulary, StrRec, CSV), getVocab(LEFT, COUNTER-1));
//Get term frequency info
Doc_Term := RECORD
  LDA.Types.t_record_id rid;
  LDA.Types.t_nominal nominal;
  UNSIGNED4 v;
END;
doc_terms := NORMALIZE(docs, LEFT.word_counts,
                       TRANSFORM(Doc_Term, SELF:=LEFT, SELF:=RIGHT));
Freq_Rec := RECORD
  LDA.Types.t_nominal nominal := doc_terms.nominal;
  UNSIGNED4 freq := SUM(GROUP, doc_terms.v);
  UNSIGNED4 docs := COUNT(GROUP);
END;
freqs := TABLE(doc_terms, Freq_Rec, nominal, FEW, UNSORTED);
// Compares
diff_threshold := 0.0001;
// Compare alpha (only 1 needed, use topic 1
Alpha_Compare := RECORD
  REAL8 base_alpha;
  REAL8 test_alpha;
  REAL8 diff;
END;
Alpha_Compare cmpr1(LDA.Types.Model_Topic tm, Alpha_Rec ar) := TRANSFORM
  SELF.base_alpha := ar.alpha;
  SELF.test_alpha := tm.alpha;
  SELF.diff := IF(ABS(tm.alpha-ar.alpha)>diff_threshold, tm.alpha-ar.alpha, 0.0);
END;
alpha_test := JOIN(test_topics, base_raw_alpha, LEFT.topic=RIGHT.topic,
                   cmpr1(LEFT, RIGHT));
// Extract test beta records
Topic_Nominal_Beta extTest(LDA.Types.Model_Topic tm, LDA.Types.TermValue b):=TRANSFORM
  SELF.model := tm.model;
  SELF.topic := tm.topic;
  SELF.nominal := b.nominal;
  SELF.v := EXP(b.v);
END;
test_betas := NORMALIZE(test_topics, LEFT.logBetas, extTest(LEFT,RIGHT));
// Compare beta values
Beta_Compare := RECORD
  LDA.Types.t_topic topic;
  LDA.Types.t_nominal nominal;
  REAL8 base_beta;
  REAL8 test_beta;
  REAL8 diff;
  UNSIGNED4 missing_result;
  UNSIGNED4 mystery_result;
  UNSIGNED4 diff_result;
  UNSIGNED4 same_result;
  BOOLEAN ok;
END;
Beta_Compare cmpr2(Topic_Nominal_Beta base, Topic_Nominal_Beta test):=TRANSFORM
  ok := test.model<>0 AND base.model<>0;
  SELF.ok := ok;
  SELF.missing_result := IF(test.model=0, 1, 0);    //test is empty
  SELF.mystery_result := IF(base.model=0, 1, 0);    //base case is empty
  SELF.topic := IF(test.model=0, base.topic, test.topic);
  SELF.nominal := IF(test.model=0, base.nominal, test.nominal);
  SELF.base_beta := base.v;
  SELF.test_beta := test.v;
  SELF.diff := IF(ABS(test.v-base.v)>diff_threshold, test.v-base.v, 0.0);
  SELF.diff_result := IF(ABS(test.v-base.v)>diff_threshold, 1, 0);
  SELF.same_result := IF(ABS(test.v-base.v)<=diff_threshold, 1, 0);
END;
beta_cmpr := JOIN(base_betas, test_betas,
                  LEFT.model=RIGHT.model AND LEFT.topic=RIGHT.topic
                  AND LEFT.nominal=RIGHT.nominal,
                  cmpr2(LEFT, RIGHT), FULL OUTER);
cmpr_summ := TABLE(beta_cmpr,
                    {missing:=SUM(GROUP,missing_result),
                     mystery:=SUM(GROUP,mystery_result),
                     different:=SUM(GROUP, diff_result),
                     same:=SUM(GROUP, same_result)}, FEW, UNSORTED);
cmpr_topic := TABLE(beta_cmpr,
                    {topic, missing:=SUM(GROUP,missing_result),
                     mystery:=SUM(GROUP,mystery_result),
                     different:=SUM(GROUP, diff_result),
                     same:=SUM(GROUP, same_result)}, topic, FEW, UNSORTED);
diff_summ := TABLE(beta_cmpr(ok AND diff<>0.0),
                   {avg_diff:=AVE(GROUP,diff),
                    var_diff:=VARIANCE(GROUP,diff)}, FEW, UNSORTED);
diff_topic := TABLE(beta_cmpr(ok AND diff<>0.0),
                   {topic, avg_diff:=AVE(GROUP,diff),
                    var_diff:=VARIANCE(GROUP,diff)}, topic, FEW, UNSORTED);
Beta_Compare_Freq := RECORD(Beta_Compare)
  UNSIGNED4 freq;
  UNSIGNED4 docs;
  REAL8 doc_diff_rate;
END;
Beta_Compare_Freq add_freqs(Beta_Compare cmpr, Freq_Rec fr) := TRANSFORM
  SELF.doc_diff_rate := ABS(cmpr.diff) / fr.docs;
  SELF := fr;
  SELF := cmpr;
END;
beta_cmpr_f := JOIN(beta_cmpr, freqs, LEFT.nominal=RIGHT.nominal,
                    add_freqs(LEFT, RIGHT), LOOKUP);
select_mystery := CHOOSEN(beta_cmpr(mystery_result>0), 10);
select_missing := CHOOSEN(beta_cmpr(missing_result>0), 10);
model_rpt := TABLE(test_topics(topic=1), {model,
                                  last_alpha_df, last_alpha_iter,
                                  last_init_alpha,
                                  likelihood, likelihood_change,
                                  beta_epsilon, EM_Iterations,
                                  last_average_change, last_min_likelihood,
                                  last_max_likelihood, last_min_change,
                                  last_max_change, last_doc_max_iter,
                                  last_doc_min_iter,
                                  last_doc_iterations, last_docs_converged});
sorted_test_topics := SORT(test_topics, model, topic);
LDA.Types.Likelihood_Hist ext_likelihood(LDA.Types.Likelihood_Hist h):=TRANSFORM
  SELF := h;
END;
raw_likelihoods := NORMALIZE(test_topics, LEFT.hist, ext_likelihood(RIGHT));
likelihoods := DEDUP(SORT(raw_likelihoods, iteration), RECORD);
// check the topic terms
grpd_test_topic_betas := GROUP(SORT(test_betas, model, topic), model, topic);
grpd_base_topic_betas := GROUP(SORT(base_betas, model, topic), model, topic);
test_top_betas := TOPN(grpd_test_topic_betas, 5, -v);
base_top_betas := TOPN(grpd_base_topic_betas, 5, -v);
Topic_Term := RECORD
  LDA.Types.t_model_id model;
  LDA.Types.t_topic topic;
  LDA.Types.t_nominal nominal;
  UNICODE term;
END;
Topic_Term trm(Topic_Nominal_Beta b, LDA.Types.Term_Dict vr) := TRANSFORM
  SELF.term := vr.term;
  SELF := b;
END;
test_w_terms := JOIN(test_top_betas, vocab, LEFT.nominal=RIGHT.nominal,
                      trm(LEFT,RIGHT), LOOKUP);
base_w_terms := JOIN(base_top_betas, vocab, LEFT.nominal=RIGHT.nominal,
                    trm(LEFT,RIGHT), LOOKUP);
Term_Topic := RECORD
  LDA.Types.t_model_id model;
  LDA.Types.t_nominal nominal;
  LDA.Types.t_topic test_topic;
  LDA.Types.t_topic base_topic;
  UNICODE term;
  UNSIGNED4 src;
  UNSIGNED4 match_count;
END;
Term_Topic xsrc(Topic_Term base, Topic_Term test) := TRANSFORM
  SELF.model := IF(base.term<>'', base.model, test.model);
  SELF.nominal := IF(base.term<>'', base.nominal, test.nominal);
  SELF.term := IF(base.term<>'', base.term, test.term);
  SELF.src := IF(base.term<>'', 2, 0) + IF(test.term<>'', 1, 0);
  SELF.test_topic := test.topic;
  SELF.base_topic := base.topic;
  SELF.match_count := IF(base.term<>'' AND test.term<>'', 1, 0);
END;
xmatch := JOIN(base_w_terms, test_w_terms,
               LEFT.term=RIGHT.term AND LEFT.topic=RIGHT.topic,
               xsrc(LEFT, RIGHT), FULL OUTER);
scored := TABLE(xmatch(src=3),
               {test_topic, base_topic, score:=SUM(GROUP, match_count)},
               test_topic, base_topic, MERGE);
pairs := SORT(scored, test_topic);
match_tab := TABLE(scored, {score, topics:=COUNT(GROUP)}, score, FEW, UNSORTED);
match_rpt := SORT(match_tab, -score);
// Top terms
top_terms_dev := LDA.Top_Terms(test_topics, vocab).outliers(10);
//
doc_tab := TABLE(wd, {num_docs:=COUNT(GROUP), ave_size:=AVE(GROUP,terms),
                      max_size:=MAX(GROUP, terms), min_size:=MIN(GROUP,terms)});
/**
 * Compare LDA topic model estimate to a prior estimate.
 * @return an action composition of the report outputs
 */
EXPORT lda_cmpr2blei := PARALLEL(
                               OUTPUT(alpha_test, NAMED('Alpha_Result'))
                              ,OUTPUT(cmpr_summ, NAMED('Beta_Cmpr'))
                              ,OUTPUT(SORT(cmpr_topic,topic), NAMED('Topic_beta_cmpr'))
                              ,OUTPUT(model_rpt, NAMED('Model_report'))
                              ,OUTPUT(doc_tab, NAMED('Doc_Stats'))
                              ,OUTPUT(pairs, NAMED('Model_base_Topic_Matches'))
                              ,OUTPUT(match_rpt, NAMED('Topic_Match_Counts'))
                              ,OUTPUT(likelihoods, NAMED('likelihood_hist'))
                            );
