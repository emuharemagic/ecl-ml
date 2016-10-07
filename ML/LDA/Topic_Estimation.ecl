//Estimate the the topics for each model.  Roughly based upon
//approach by Blei in LDA-C.
IMPORT * FROM $;  // import the folder symbols
IMPORT $.Types;
IMPORT STD AS STD;

//Alias for convenience
Doc_Assigned := Types.Doc_Assigned;
Doc_Mapped := Types.Doc_Mapped;
Doc_Topics := Types.Doc_Topics;
Model_Parameters := Types.Model_Parameters;
TermFreq_DataSet := Types.TermFreq_DataSet;
// Alpha constraints
MAX_ALPHA_ITER := 1000;
ALPHA_THRESHOLD := 0.00001;
//
/**
 * Topic Estimates for a Latent Dirichlet Allocation model.  Based upon
 * the paper Latent Dirichlet Allocation, Journal of Machine Learning Research,
 * 2003 by Blei, Ng, and Jordan.
 * <p>This version follows the paper by using the same topic Gamma values
 * for all of the words in an iteration.  The published lda-c program updates
 * this value for each word which results in slightly (but significant)
 * different values for Beta and Alpha.</p>
 * @param parameters the modeling parameters such as the number of iterations
 * @param initial_estimates the initial estimated Beta values by topic and
 * word
 * @param stats the collection stats, such as number of documents and uniques
 * @param docs the documents mapped to each model
 * @return model-topic level results and statistics
 */
EXPORT DATASET(Types.Model_Topic_Result)
       Topic_Estimation(DATASET(Types.Model_Parameters) parameters,
                        DATASET(Types.Model_Topic) initial_estimates,
                        DATASET(Types.Model_Collection_Stats) stats,
                        DATASET(Types.Doc_Mapped) docs) := FUNCTION
  // Assign docs to models
  Doc_Assigned assign_model(Doc_Mapped doc, Model_Parameters p):=TRANSFORM
    tr := topic_ranges(p.num_topics, COUNT(doc.word_counts));
    SELF.model := p.model;
    SELF.num_topics := p.num_topics;
    SELF.num_ranges := tr.ranges;
    SELF.per_range := tr.per_range;
    SELF := doc;
  END;
  assigned := JOIN(docs(EXISTS(word_counts)), parameters,
                   RIGHT.model IN LEFT.models,
                   assign_model(LEFT,RIGHT), ALL);  // model is a very small set
  // Initial version of the model from the parameters, collections, estimates
  imr_0 := initial_model(parameters, initial_estimates, stats);
  Work_Rslt := RECORD(Types.Model_Topic_Result)
    UNSIGNED2 node;
    UNSIGNED2 origin_node;
  END;
  Work_Rslt replicate(Types.Model_Topic_Result mtr, UNSIGNED c) := TRANSFORM
    SELF.node := c-1;
    SELF.origin_node := STD.System.ThorLib.Node();
    SELF := mtr;
  END;
  imr_1 := NORMALIZE(imr_0, CLUSTERSIZE, replicate(LEFT, COUNTER));
  initial_model_results := SORT(DISTRIBUTE(imr_1, node), model, topic, LOCAL);
  //Generate the base model document topic set to be used in the doc inference
  Types.Doc_Topics topicExpand(Doc_Assigned doc, UNSIGNED c) := TRANSFORM
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
  //EM loop function
  //Doc_Model (doc replicated for each model) evenly distributed, locally
  //sorted by model and RID.  Initial model_topic records are replicated
  //to every node and locally sorted by model and topic.
  DATASET(Work_Rslt) run_EM(DATASET(Work_Rslt) mods, UNSIGNED cnt) := FUNCTION
    //Prepare documents and run variational inference step to calculate
    //likelihood values by doc (same for all topics) and gamma by doc & topic
    Types.Topic_Value init_tg(Types.t_topic topic, REAL8 v):=TRANSFORM
      SELF.topic := topic;
      SELF.v := v;
    END;
    Types.Topic_Values init_tp(Types.t_topic t, Doc_Topics d) := TRANSFORM
      w := COUNT(d.word_counts);
      SELF.topic := t;
      SELF.vs := DATASET(w, TRANSFORM(Types.OnlyValue, SELF.v:=1/d.num_topics));
    END;
    Types.Topic_Values cvtTV(Work_Rslt mod, TermFreq_DataSet wc) := TRANSFORM
      SELF.topic := mod.topic;
      SELF.vs := select_betas(mod.logBetas, wc)
    END;
    Doc_Topics applyMTopic(Doc_Topics doc, DATASET(Work_Rslt) mts) := TRANSFORM
      base_topic := doc.topic_low-1;
      this_nt := doc.topic_high-doc.topic_low+1;
      init_gamma := MAX(mts,alpha)+(SUM(doc.word_counts,v)/MAX(mts,num_topics));
      init_digamma := digamma(init_gamma);
      SELF.likelihood_change := 2*MAX(mts, doc_epsilon);
      SELF.doc_epsilon := MAX(mts, doc_epsilon);
      SELF.max_doc_iterations := MAX(mts, max_doc_iterations);
      SELF.t_logBetas := PROJECT(mts, cvtTV(LEFT, doc.word_counts));
      SELF.alpha := MAX(mts, alpha);  // all the same
      SELF.t_gammas := DATASET(this_nt, init_tg(COUNTER+base_topic, init_gamma));
      SELF.t_digammas := DATASET(this_nt, init_tg(COUNTER+base_topic,init_digamma));
      SELF.t_phis := DATASET(this_nt, init_tp(COUNTER+base_topic, doc));
      SELF.estimate_alpha := mts[1].estimate_alpha;
      SELF := doc;
    END;
    mtd_u := DENORMALIZE(base_doc_topics, mods,
                         LEFT.model=RIGHT.model
                         AND RIGHT.topic BETWEEN LEFT.topic_low AND LEFT.topic_high,
                         GROUP, applyMTopic(LEFT,ROWS(RIGHT)), LOCAL, NOSORT);
    mod_topic_docs := SORT(mtd_u, model, rid, topic_range, LOCAL);
    cvg_docs := LOOP(mod_topic_docs,
                     LEFT.max_doc_iterations > LEFT.doc_iterations
                     AND LEFT.likelihood_change > LEFT.doc_epsilon,
                     lda_inference(ROWS(LEFT), COUNTER));
    // Docs converged, now sum by topics within each model and calc new betas
    W_Cls_Word := RECORD
      Types.t_model_id model;
      Types.t_topic topic;
      Types.t_nominal nominal;
      REAL8 v;
    END;
    W_Cls_Total := RECORD
      Types.t_model_id model;
      Types.t_topic topic;
      REAL8 v;
    END;
    W_Cls_PhiWords := RECORD
      Types.t_model_id model;
      Types.t_topic topic;
      DATASET(Types.TermFreq) word_counts;
      DATASET(Types.OnlyValue) phis;
    END;
    W_Cls_Word getW(W_Cls_PhiWords pw, UNSIGNED cnt) := TRANSFORM
      SELF.nominal := pw.word_counts[cnt].nominal;
      SELF.v := pw.word_counts[cnt].v * pw.phis[cnt].v;
      SELF := pw;
    END;
    W_Cls_PhiWords getTopic(Doc_Topics dts, Types.Topic_Values tvs) := TRANSFORM
      SELF.phis := tvs.vs;
      SELF.topic := tvs.topic;
      SELF := dts;
    END;
    phi_words := NORMALIZE(cvg_docs, LEFT.t_phis, getTopic(LEFT, RIGHT));
    d_words := NORMALIZE(phi_words, COUNT(LEFT.phis), getW(LEFT, COUNTER));
    cls_words := TABLE(d_words, {model, topic, nominal, REAL8 s:=SUM(GROUP,v)},
                         model, topic, nominal, MERGE);
    cls_total := TABLE(d_words, {model, topic, REAL8 t:=SUM(GROUP,v)},
                         model, topic, FEW, UNSORTED);
    W_Cls_Word logBeta(cls_words cword, cls_total ctotal):=TRANSFORM
      SELF.v := IF(cword.s>0, LN(cword.s) - LN(ctotal.t), -100);
      SELF := cword;
    END;
    new_betas := JOIN(cls_words, cls_total,
                      LEFT.model=RIGHT.model AND LEFT.topic=RIGHT.topic,
                      logBeta(LEFT, RIGHT), LOOKUP);
    // Extract topic level gamma values and calculate new alpha
    W_Alpha_Rid := RECORD
      Types.t_model_id model;
      Types.t_record_id rid;
      REAL8 alpha_sufstat;
      UNSIGNED4 docs;
      UNSIGNED4 num_topics;
    END;
    Types.OnlyValue cvt2Only(Types.Topic_Value tv) := TRANSFORM
      SELF.v := tv.v;
    END;
    W_Alpha_Rid roll_doc_ss(Doc_Topics dt, DATASET(Doc_Topics) dts) := TRANSFORM
      d_g := NORMALIZE(dts, LEFT.t_gammas, cvt2Only(RIGHT));
      n_tops := dt.num_topics;
      SELF.model := dt.model;
      SELF.rid := dt.rid;
      SELF.docs := 1;
      SELF.num_topics := n_tops;
      SELF.alpha_sufstat := SUM(d_g, digamma(v)) - n_tops*digamma(SUM(d_g, v));
    END;
    grp_alpha_docs := GROUP(cvg_docs(estimate_alpha), model, rid, LOCAL);
    doc_level_ss := ROLLUP(grp_alpha_docs, GROUP, roll_doc_ss(LEFT, ROWS(LEFT)));
    W_Alpha := RECORD
      Types.t_model_id model := doc_level_ss.model;
      UNSIGNED4 docs := SUM(GROUP, doc_level_ss.docs);
      UNSIGNED4 num_topics := MAX(GROUP, doc_level_ss.num_topics);
      REAL8 alpha_sufstat := SUM(GROUP, doc_level_ss.alpha_sufstat);
    END;
    model_alpha_ss := TABLE(doc_level_ss, W_Alpha, model, MERGE);
    Types.Alpha_Estimate cvt_2_ae(W_Alpha wrk) := TRANSFORM
      SELF.last_df := 2 * ALPHA_THRESHOLD;
      SELF.iter := 0;
      SELF.init_alpha := 100;
      SELF.alpha := EXP(LN(100));
      SELF.log_alpha := LN(100);
      SELF.suff_stat := wrk.alpha_sufstat;
      SELF := wrk;
    END;
    alpha_ss := PROJECT(model_alpha_ss, cvt_2_ae(LEFT));
    new_alpha := LOOP(alpha_ss,
                      ABS(LEFT.last_df)>ALPHA_THRESHOLD
                      AND LEFT.iter<MAX_ALPHA_ITER,
                      update_alpha(ROWS(LEFT)));
    Work_Rslt upd_alpha(Work_Rslt mod, Types.Alpha_Estimate alf) := TRANSFORM
      SELF.alpha := IF(mod.estimate_alpha, alf.alpha, mod.alpha);
      SELF.last_alpha_df := IF(mod.estimate_alpha, alf.last_df, 0.0);
      SELF.last_alpha_iter := IF(mod.estimate_alpha, alf.iter, 0);
      SELF.last_init_alpha := IF(mod.estimate_alpha, alf.init_alpha, 0.0);
      SELF := mod;
    END;
    mod_a0 := JOIN(mods, new_Alpha, LEFT.model=RIGHT.model,
                   upd_alpha(LEFT,RIGHT), LOOKUP, LEFT OUTER);
    mod_a := SORT(mod_a0, model, topic, LOCAL);
    // gather new betas by model and topic
    MTT := RECORD
      Types.t_model_id model;
      Types.t_topic topic;
      DATASET(Types.TermValue) logBetas;
      UNSIGNED2 node;
    END;
    MTT rollB(RECORDOF(new_betas)b1, DATASET(RECORDOF(new_betas)) b):=TRANSFORM
      SELF.logBetas := PROJECT(b, Types.TermValue);
      SELF.node := 0; // not yet, replicate next
      SELF := b1;
    END;
    MTT repl_betas(MTT base, UNSIGNED c) := TRANSFORM
      SELF.node := c;
      SELF := base;
    END;
    new_betas_grp := SORT(GROUP(new_betas, model, topic, ALL), nominal);
    nbr_0 := ROLLUP(new_betas_grp, GROUP, rollB(LEFT, ROWS(LEFT)));
    nbr_1 := NORMALIZE(nbr_0, CLUSTERSIZE, repl_betas(LEFT, COUNTER));
    new_beta_set := SORT(DISTRIBUTE(nbr_1, node), model, topic, LOCAL);
    Work_Rslt update_betas(Work_Rslt mod, MTT upd) := TRANSFORM
      SELF.logBetas := upd.logBetas;
      SELF := mod;
    END;
    mod_ab := JOIN(mod_a, new_beta_set,
                  LEFT.model=RIGHT.model AND LEFT.topic=RIGHT.topic,
                  update_betas(LEFT, RIGHT), LOCAL);
    // extract document log likelihoods and sum.  Every doc has a topic 1
    Work_Doc_ex := RECORD
      Types.t_model_id model;
      REAL8 likelihood;
      REAL8 likelihood_change;
      REAL8 min_likelihood;
      REAL8 max_likelihood;
      REAL8 min_change;
      REAL8 max_change;
      UNSIGNED4 doc_count;
      UNSIGNED2 doc_iterations;
      UNSIGNED2 doc_converged;
      UNSIGNED2 doc_iter_min;
      UNSIGNED2 doc_iter_max;
    END;
    Work_Doc_ex extractDoc(Types.Doc_Topics dt) := TRANSFORM
      converged := dt.likelihood_change BETWEEN 0.0 AND dt.doc_epsilon;
      SELF.model := dt.model;
      SELF.likelihood := dt.likelihood;
      SELF.doc_iterations := dt.doc_iterations;
      SELF.doc_iter_min := dt.doc_iterations;
      SELF.doc_iter_max := dt.doc_iterations;
      SELF.doc_converged := IF(converged, 1, 0);
      SELF.likelihood_change := dt.likelihood_change;
      SELF.min_change := dt.likelihood_change;
      SELF.max_change := dt.likelihood_change;
      SELF.min_likelihood := dt.likelihood;
      SELF.max_likelihood := dt.likelihood;
      SELF.doc_count := 1;
    END;
    mtd_ext := PROJECT(cvg_docs(topic_range=1), extractDoc(LEFT));
    mtd_ext_srt := SORT(mtd_ext, model, LOCAL);
    Work_Doc_ex sum_doc(Work_Doc_ex cumm, Work_Doc_ex incr) := TRANSFORM
      SELF.model := cumm.model;
      SELF.likelihood := cumm.likelihood + incr.likelihood;
      SELF.doc_iterations := cumm.doc_iterations + incr.doc_iterations;
      SELF.doc_iter_max := MAX(cumm.doc_iter_max, incr.doc_iter_max);
      SELF.doc_iter_min := MIN(cumm.doc_iter_min, incr.doc_iter_min);
      SELF.doc_converged := cumm.doc_converged + incr.doc_converged;
      SELF.doc_count := cumm.doc_count + incr.doc_count;
      SELF.likelihood_change := cumm.likelihood_change + incr.likelihood_change;
      SELF.min_change := MIN(cumm.min_change, incr.min_change);
      SELF.max_change := MAX(cumm.max_change, incr.max_change);
      SELF.max_likelihood := MAX(cumm.max_likelihood, incr.max_likelihood);
      SELF.min_likelihood := MIN(cumm.min_likelihood, incr.min_likelihood);
    END;
    l_total := ROLLUP(mtd_ext_srt, sum_doc(LEFT, RIGHT), model, LOCAL);
    sorted_l_total := SORT(l_total, model);
    g_total := ROLLUP(l_total, sum_doc(LEFT, RIGHT), model);
    Work_Rslt updateModel(Work_Rslt mod, Work_Doc_ex ex) := TRANSFORM
      h_step := DATASET([{cnt, ex.likelihood}], Types.Likelihood_Hist);
      raw_chg := (mod.likelihood-ex.likelihood)/mod.likelihood;
      chg := IF(raw_chg < 0.0, 2*mod.beta_epsilon, raw_chg);
      SELF.max_doc_iterations := IF(mod.likelihood>ex.likelihood,
                                    2*mod.max_doc_iterations,
                                    mod.max_doc_iterations);
      SELF.likelihood := ex.likelihood;
      SELF.likelihood_change := IF(mod.likelihood<>0, chg, 2*mod.beta_epsilon);
      SELF.EM_iterations := cnt;  // LOOP counter
      SELF.last_doc_iterations := ex.doc_iterations / ex.doc_count;
      SELF.last_doc_min_iter := ex.doc_iter_min;
      SELF.last_doc_max_iter := ex.doc_iter_max;
      SELF.last_docs_converged := ex.doc_converged;
      SELF.last_average_change := ex.likelihood_change /ex.doc_count;
      SELF.last_max_likelihood := ex.max_likelihood;
      SELF.last_min_likelihood := ex.min_likelihood;
      SELF.last_min_change := ex.min_change;
      SELF.last_max_change := ex.max_change;
      SELF.hist := mod.hist & h_step;
      SELF := mod;
    END;
    updated_models := JOIN(mod_ab, g_total, LEFT.model=RIGHT.model,
                           updateModel(LEFT,RIGHT), LOOKUP);
    RETURN updated_models;
  END;
  node_models := LOOP(initial_model_results,
                      LEFT.max_beta_iterations>=LEFT.EM_iterations
                      AND LEFT.likelihood_change > LEFT.beta_epsilon,
                      run_EM(ROWS(LEFT), COUNTER));
  // The results are replicated by node
  result_models := SORT(node_models(node=origin_node), model, topic);
  RETURN result_models;
END;
