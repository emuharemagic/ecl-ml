IMPORT $ AS LDA;
//Alias for convenience
Doc_Assigned := LDA.Types.Doc_Assigned;
Doc_Mapped := LDA.Types.Doc_Mapped;
Doc_Topics := LDA.Types.Doc_Topics;
Model_Parameters := LDA.Types.Model_Parameters;
TermFreq_DataSet := LDA.Types.TermFreq_DataSet;
Document_Scored := LDA.Types.Document_Scored;

/** Document inference to assign topics to documents.  Uses variational
 * inference from Blei.
 * @param patrameters the model parameters, in particular the maximum
 * iterations and threshold for variational inference
 * @param model_estimates the alpha and log Beta values for the model
 * @param stats collection statistics
 * @param docs the documents
 * @return the model-document set with topic values
 */
EXPORT DATASET(LDA.Types.Document_Scored)
       Document_Inference(DATASET(LDA.Types.Model_Parameters) parameters,
                          DATASET(LDA.Types.Model_Topic) model_estimates,
                          DATASET(LDA.Types.Model_Collection_Stats) stats,
                          DATASET(LDA.Types.Doc_Mapped) docs) := FUNCTION
  // Assign docs to models
  Doc_Assigned assign_model(Doc_Mapped doc, Model_Parameters p):=TRANSFORM
    tr := LDA.topic_ranges(p.num_topics, COUNT(doc.word_counts));
    SELF.model := p.model;
    SELF.num_topics := p.num_topics;
    SELF.num_ranges := tr.ranges;
    SELF.per_range := tr.per_range;
    SELF := doc;
  END;
  assigned := JOIN(docs(EXISTS(word_counts)), parameters,
                   RIGHT.model IN LEFT.models,
                   assign_model(LEFT,RIGHT), ALL);  // model is a very small set
  // Working version of the model from the parameters, collections, estimates
  wmod_0 := LDA.initial_model(parameters, model_estimates, stats);
  Work_Mod := RECORD(LDA.Types.Model_Topic_Result)
    UNSIGNED2 node;
  END;
  Work_Mod replicate(LDA.Types.Model_Topic_Result mtr, UNSIGNED c) := TRANSFORM
    SELF.node := c;
    SELF := mtr;
  END;
  wmod_1 := NORMALIZE(wmod_0, CLUSTERSIZE, replicate(LEFT, COUNTER));
  working_model := SORT(DISTRIBUTE(wmod_1, node), model, topic, LOCAL);
  //Generate the base model document topic set to be used in the doc inference
  Doc_Topics topicExpand(Doc_Assigned doc, UNSIGNED c) := TRANSFORM
    SELF.topic_range := c;
    SELF.topic_low := 1 + ((c-1)*doc.per_range);
    SELF.topic_high := MIN(doc.num_topics, c*doc.per_range);
    SELF := doc;
    SELF := [];
  END;
  adt_0 := NORMALIZE(assigned, LEFT.num_ranges, topicExpand(LEFT, COUNTER));
  adt_1 := SORT(adt_0, model, rid);
  adt_2 := GROUP(adt_1, model, rid);
  adt_3 := SORT(adt_2, topic_range);
  adt_4 := adt_3;
  base_doc_topics := UNGROUP(adt_4);
  // run inference
  LDA.Types.Topic_Value init_tg(LDA.Types.t_topic topic, REAL8 v):=TRANSFORM
    SELF.topic := topic;
    SELF.v := v;
  END;
  LDA.Types.Topic_Values init_tp(LDA.Types.t_topic t, Doc_Topics d) := TRANSFORM
    w := COUNT(d.word_counts);
    SELF.topic := t;
    SELF.vs := DATASET(w, TRANSFORM(LDA.Types.OnlyValue, SELF.v:=1/d.num_topics));
  END;
  LDA.Types.Topic_Values cvtTV(Work_Mod mod, TermFreq_DataSet wc) := TRANSFORM
    SELF.topic := mod.topic;
    SELF.vs := LDA.select_betas(mod.logBetas, wc)
  END;
  Doc_Topics applyMTopic(Doc_Topics doc, DATASET(Work_Mod) mts) := TRANSFORM
    base_topic := doc.topic_low-1;
    this_nt := doc.topic_high-doc.topic_low+1;
    init_gamma := MAX(mts,alpha)+SUM(doc.word_counts,v)/MAX(mts,num_topics);
    init_digamma := LDA.digamma(init_gamma);
    SELF.likelihood_change := 2*MAX(mts, doc_epsilon);
    SELF.doc_epsilon := MAX(mts, doc_epsilon);
    SELF.max_doc_iterations := MAX(mts, max_doc_iterations);
    SELF.t_logBetas := PROJECT(mts, cvtTV(LEFT, doc.word_counts));
    SELF.alpha := MAX(mts, alpha);  // all the same
    SELF.t_gammas := DATASET(this_nt, init_tg(COUNTER+base_topic, init_gamma));
    SELF.t_digammas := DATASET(this_nt, init_tg(COUNTER+base_topic,init_digamma));
    SELF.t_phis := DATASET(this_nt, init_tp(COUNTER+base_topic, doc));
    SELF := doc;
  END;
  mtd_u := DENORMALIZE(base_doc_topics, working_model,
                       LEFT.model=RIGHT.model
                       AND RIGHT.topic BETWEEN LEFT.topic_low AND LEFT.topic_high,
                       GROUP, applyMTopic(LEFT,ROWS(RIGHT)), LOCAL, NOSORT);
  mod_topic_docs := SORT(mtd_u, model, rid, topic_range, LOCAL);
  cvg_docs := LOOP(mod_topic_docs,
                   LEFT.max_doc_iterations > LEFT.doc_iterations
                   AND LEFT.likelihood_change > LEFT.doc_epsilon,
                   LDA.lda_inference(ROWS(LEFT), COUNTER));
  // collect the results
  Document_Scored cvt2Scored(Doc_Topics doc) := TRANSFORM
    SELF.topics := doc.t_gammas;
    SELF := doc;
  END;
  ext_doc_tr := PROJECT(cvg_docs, cvt2Scored(LEFT));
  RETURN ext_doc_tr;
END;