IMPORT $.Types AS Types;
/** Initial log beta values derived from seed documents.
 *
 */
EXPORT DATASET(Types.Model_Topic)
       Seeded_Initial_Values(DATASET(Types.Model_Parameters) parameters,
                             DATASET(Types.Term_Dict) terms,
                             DATASET(Types.Seed_Document) seeds) := FUNCTION
  //
  MT_Term := RECORD
    Types.t_model_id model;
    Types.t_topic topic;
    Types.t_nominal nominal;
    UNSIGNED8 freq;
    UNSIGNED8 total_freq;
    REAL8 alpha;
    REAL8 v;        // log of topic freq/total freq or -100
  END;
  M_Term := RECORD
    Types.t_model_id model;
    Types.t_nominal nominal;
    UNSIGNED4 num_topics;
    REAL8 alpha;
  END;
  // get base
  M_Term makeTerm(Types.Term_Dict term, Types.Model_Parameters p) := TRANSFORM
    SELF.model := p.model;
    SELF.num_topics := p.num_topics;
    SELF.nominal := term.nominal;
    SELF.alpha := p.alpha;
  END;
  MT_Term makeBase(M_Term term, UNSIGNED cnt) := TRANSFORM
    SELF.topic := cnt;
    SELF.v := 1;
    SELF.freq := 1;
    SELF.total_freq := 1;
    SELF := term;
  END;
  model_terms := NORMALIZE(terms, parameters, makeTerm(LEFT, RIGHT));
  mt_terms := NORMALIZE(model_terms, LEFT.num_topics, makeBase(LEFT, COUNTER));
  // Get seed freqs
  MT_Term getSeed(Types.Seed_Document doc, Types.TermFreq term) := TRANSFORM
    SELF.model := doc.model;
    SELF.topic := doc.topic;
    SELF.nominal := term.nominal;
    SELF.freq := term.v;
    SELF.total_freq := term.v;
    SELF.alpha := 0;
    SELF.v := 1;
  END;
  seed_terms := NORMALIZE(seeds, LEFT.word_counts, getSeed(LEFT, RIGHT));
  all_terms := SORT(seed_terms+mt_terms, model, topic);
  mt_tab := TABLE(all_terms,
                  {model, nominal, total:=SUM(GROUP,freq)},
                  model, nominal, MERGE);
  MT_Term applyTotal(MT_Term term, RECORDOF(mt_tab) tab) := TRANSFORM
    SELF.total_freq := tab.total;
    SELF.v := LN(term.freq/tab.total);
    SELF := term;
  END;
  nrm_terms := JOIN(all_terms, mt_tab,
                    LEFT.model=RIGHT.model AND LEFT.nominal=RIGHT.nominal,
                    applyTotal(LEFT, RIGHT), PARTITION LEFT, NOSORT(LEFT), SMART);
  srt_terms := UNGROUP(SORT(GROUP(nrm_terms, model, topic), nominal));
  MT_Term rollM(MT_Term frst, MT_Term scnd) := TRANSFORM
    SELF.alpha := IF(frst.alpha<>0.0, frst.alpha, scnd.alpha);
    SELF.v := LN((frst.freq + scnd.freq) / frst.total_freq);
    SELF.freq := frst.freq + scnd.freq;
    SELF := frst;
  END;
  mrg_terms := ROLLUP(srt_terms, rollM(LEFT, RIGHT), model, topic, nominal, LOCAL);
  grp_terms := GROUP(mrg_terms, model, topic, LOCAL);
  // make initial values
  Types.Model_Topic rollBetas(MT_Term f, DATASET(MT_Term) rws) := TRANSFORM
    SELF.model := f.model;
    SELF.topic := f.topic;
    SELF.alpha := f.alpha;
    SELF.logBetas := PROJECT(rws, Types.TermValue);
  END;
  rslt := ROLLUP(grp_terms, GROUP, rollBetas(LEFT, ROWS(LEFT)));
  RETURN rslt;
END;