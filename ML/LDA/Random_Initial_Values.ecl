IMPORT $.Types AS Types;
Model_Collection_Stats := Types.Model_Collection_Stats;
Model_Parameters := Types.Model_Parameters;
Model_Topic := Types.Model_Topic;
/** Initial parameters from random values for log betas.  Use the collection
 * statistics and model parameters to generate an initial set of random
 * beta values.  Note that the nominal values must be dense because the
 * nominal for the random Beta is assigned from a counter.
 * @param parameters the model parameters
 * @param stats the collection stats for each model
 * @return random values for the Beta parameters.  The values are normalized
 * such that the sum of the Betas (not the logBetas!) is about 1.
 * @exception Assert collections with sparse nominal values are not supported.
 */
EXPORT DATASET(Types.Model_Topic)
       Random_Initial_Values(DATASET(Types.Model_Parameters) parameters,
                             DATASET(Types.Model_Collection_Stats) stats) := FUNCTION
  // Extract number of topics and number of terms for each model
  Work := RECORD
    Types.t_model_id model;
    UNSIGNED4 num_topics;
    UNSIGNED4 unique_words;
    Types.t_nominal low_nominal;
    REAL8 alpha;
  END;
  Work parm_stats(Model_Parameters p, Model_Collection_Stats s) := TRANSFORM
    SELF.model := p.model;
    SELF.num_topics := p.num_topics;
    SELF.unique_words := s.unique_words;
    SELF.alpha := p.initial_alpha;
    SELF.low_nominal := s.low_nominal;
  END;
  good_stats := ASSERT(stats, unique_words=high_nominal-low_nominal+1);
  drivers := JOIN(parameters, good_stats, LEFT.model=RIGHT.model,
                  parm_stats(LEFT, RIGHT), LOOKUP);
  Topic_Work := RECORD
    Types.t_model_id model;
    Types.t_topic topic;
    UNSIGNED4 num_topics;
    UNSIGNED4 unique_words;
    Types.t_nominal low_nominal;
    REAL8 alpha;
  END;
  Topic_Work make_mt(Work w, UNSIGNED4 t) := TRANSFORM
    SELF.topic := t;
    SELF := w;
  END;
  mt_0 := NORMALIZE(drivers, LEFT.num_topics, make_mt(LEFT, COUNTER));
  model_topics := SORT(mt_0, model, topic); // distribute and sequence
  Work_Nominal := RECORD
    Types.t_model_id model;
    Types.t_topic topic;
    Types.t_nominal nominal;
    REAL8 alpha;
    REAL8 v;
  END;
  Work_Nominal gen_nom(Topic_Work tw, UNSIGNED8 c) := TRANSFORM
    SELF.nominal := tw.low_nominal + c - 1;
    SELF.v := (1.0/tw.unique_words) + (REAL8)((UNSIGNED4)RANDOM())/4294967296.0;
    SELF := tw;
  END;
  raw := NORMALIZE(model_topics, LEFT.unique_words, gen_nom(LEFT, COUNTER));
  t_sums := TABLE(raw, {model, topic, s:=SUM(GROUP,v)}, model, topic, MERGE);
  Work_Nominal norm_raw(Work_Nominal wn, RECORDOF(t_sums) tot) := TRANSFORM
    SELF.v := LN(wn.v / tot.s);
    SELF := wn;
  END;
  normalized := JOIN(raw, t_sums,
                     LEFT.model=RIGHT.model AND LEFT.topic=RIGHT.topic,
                     norm_raw(LEFT, RIGHT), LOOKUP);
  // normalized still distributed by model and topic, sequenced by nominal
  grp_betas := GROUP(normalized, model, topic, LOCAL);
  Model_Topic rollBetas(Work_Nominal w, DATASET(Work_Nominal) b):=TRANSFORM
    SELF.model := w.model;
    SELF.topic := w.topic;
    SELF.alpha := w.alpha;
    SELF.logBetas := PROJECT(b, Types.TermValue);
  END;
  rslt := ROLLUP(grp_betas, GROUP, rollBetas(LEFT, ROWS(LEFT)));
  RETURN rslt;
END;