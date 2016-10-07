IMPORT $ AS LDA;
// Aliases
Model_Topic := LDA.Types.Model_Topic;
Term_Dict := LDA.Types.Term_Dict;
Model_Topic_Top_Terms := LDA.Types.Model_Topic_Top_Terms;
TermValue := LDA.Types.TermValue;
Topic_Term := LDA.Types.Topic_Term;

/** Top terms for the topic.  The topic model and term dictionary are
 *  used to produce a term vector for each topic in the model.  The
 * term vector has the significant terms based upon the function used
 * to generate the vector.
 * @param modl the topic model
 * @param dict the term dictionary used to get the text
 */
EXPORT Top_Terms(DATASET(Model_Topic) modl, DATASET(Term_Dict) dict) := MODULE
  // Common section
  SHARED MTT := RECORD
    LDA.Types.t_model_id model;
    LDA.Types.t_topic topic;
    LDA.Types.t_nominal nominal;
    UNSIGNED4 pos;
    REAL8 v;
    REAL8 average;
    REAL8 std_dev;
    REAL8 near5_average;
    REAL8 diff;
    BOOLEAN sig_one;
    BOOLEAN sig_ave;
    BOOLEAN inflection;
    UNICODE term;
  END;
  SHARED Work_MT := RECORD
    LDA.Types.t_model_id model;
    LDA.Types.t_topic topic;
    DATASET(TermValue) betas;
  END;
  TermValue exp_beta(TermValue tv) := TRANSFORM
    SELF.nominal := tv.nominal;
    SELF.v := EXP(tv.v);
  END;
  Work_MT exp_betas(Model_Topic m) := TRANSFORM
    SELF.betas := SORT(PROJECT(m.logBetas, exp_beta(LEFT)), -v);
    SELF := m;
  END;
  mt0 := PROJECT(modl, exp_betas(LEFT));
  MTT flatten(Work_MT m, TermValue tv, UNSIGNED4 c):=TRANSFORM
    SELF.nominal := tv.nominal;
    SELF.v := tv.v;
    SELF.pos := c;
    SELF.average := AVE(m.betas, v);
    SELF.std_dev := SQRT(VARIANCE(m.betas, v));
    SELF.near5_average := AVE(CHOOSEN(m.betas, 5, MAX(1,c-2)), v);
    SELF := m;
    SELF := [];
  END;
  SHARED Flat_MTB := NORMALIZE(mt0, LEFT.betas, flatten(LEFT, RIGHT, COUNTER));
  SHARED Marked_MTB(UNSIGNED2 min_entries, REAL8 delta) := FUNCTION
    MTT mark(MTT next, MTT curr) := TRANSFORM
      SELF.sig_one := curr.pos > min_entries AND curr.v > delta * next.v;
      SELF.sig_ave := curr.pos > min_entries AND curr.v > curr.near5_average;
      SELF.diff := curr.v - next.v;
      SELF.inflection := curr.pos > min_entries AND curr.v - next.v < next.diff;
      SELF := curr;
    END;
    grp_mtt := GROUP(Flat_MTB, model, topic, ALL);
    marked := SORT(ITERATE(SORT(grp_mtt, -pos), mark(LEFT,RIGHT)), pos);
    RETURN UNGROUP(marked);
  END;
  Model_Topic_Top_Terms rollT(MTT r0, DATASET(MTT) rs) := TRANSFORM
    SELF.model := r0.model;
    SELF.topic := r0.topic;
    SELF.terms := PROJECT(rs, Topic_Term);
  END;
  MTT assign_text(MTT base, Term_Dict dict) := TRANSFORM
    SELF.term := dict.term;
    SELF := base;
  END;
  SHARED Make_Result(DATASET(MTT) selected) := FUNCTION
    with_text := JOIN(selected, dict, LEFT.nominal=RIGHT.nominal,
                      assign_text(LEFT, RIGHT), LOOKUP);
    sorted_flat := SORT(GROUP(with_text, model, topic, ALL), pos);
    RETURN ROLLUP(sorted_flat, GROUP, rollT(LEFT, ROWS(LEFT)));
  END;
  /** Top K terms for the topic based upon the beta value
   * @param k the number of terms to return
   * @return the top k terms for each topic in the model.
   */
  EXPORT DATASET(Model_Topic_Top_Terms) top_k(UNSIGNED2 k) := FUNCTION
    tk0 := Flat_MTB(pos <= k);
    RETURN Make_Result(tk0);
  END;
  /** Statistical outlier terms for the topic based upon the difference
   * between the average beta value and the term beta value compared to
   * the standard deviation.
   * @param k_std_dev multiplier for the standard deviation to determine
   * whether the term is an outlier.  For example, a value of 2 would return
   * terms with beta values that were more than 2 standard deviations from
   * the mean.
   * @return the terms that have a beta value of k_std_dev units above the
   * mean beta value.
   */
  EXPORT DATASET(Model_Topic_Top_Terms) outliers(REAL8 k_std_dev):=FUNCTION
    dev := Flat_MTB(v >= average + (k_std_dev*std_dev));
    RETURN Make_Result(dev);
  END;
  /** Big change in beta.  The beta values up to the point where these is a
  * big change in the beta values.
  * @param min_run the minimum number of beta values before the drop
  * @param delta the ratio of the last significant value to the first
  * insignificant value.  Values must be greater than 1.  A 1.1 means
  * that the last term will have a beta value 10% higher than the first
  * insignificant term.
  * @return min_run or more terms
  */
  EXPORT DATASET(Model_Topic_Top_Terms) big_drop(UNSIGNED2 min_run,
                                              REAL8 delta) := FUNCTION
    marked := Marked_MTB(min_run, delta);
    positions := TABLE(marked,
                       {model, topic, p:=MIN(GROUP, IF(sig_one, pos, 999999999))},
                       model, topic, FEW, UNSORTED);
    selected := JOIN(marked, positions,
                     LEFT.model=RIGHT.model AND LEFT.topic=RIGHT.topic
                     AND LEFT.pos<=RIGHT.p,
                     TRANSFORM(MTT, SELF:=LEFT), LOOKUP);
    RETURN Make_Result(selected);
  END;
  /** High median.  The beta values up to the point in the sequence where the
   * median in a 5 value window is higher than the average.
   * @param min_run the minimum number of terms before applying the cut-off
   * @return min_run or more terms
   */
  EXPORT DATASET(Model_Topic_Top_Terms) high_median(UNSIGNED2 min_run):=FUNCTION
    marked := Marked_MTB(min_run, 0);
    positions := TABLE(marked,
                       {model, topic, p:=MIN(GROUP, IF(sig_ave, pos, 999999999))},
                       model, topic, FEW, UNSORTED);
    selected := JOIN(marked, positions,
                     LEFT.model=RIGHT.model AND LEFT.topic=RIGHT.topic
                     AND LEFT.pos<=RIGHT.p,
                     TRANSFORM(MTT, SELF:=LEFT), LOOKUP);
    RETURN Make_Result(selected);
  END;
  /** Inflection point.  The terms prior to the first inflection point in the
   * beta values after the minimum number of terms.
   * @param min_run the minimum number of terms to return
   * @return min_run or more terms
   */
  EXPORT DATASET(Model_Topic_Top_Terms) inflection(UNSIGNED2 min_run):=FUNCTION
    marked := Marked_MTB(min_run, 0);
    positions := TABLE(marked,
                       {model, topic, p:=MIN(GROUP, IF(inflection, pos, 999999999))},
                       model, topic, FEW, UNSORTED);
    selected := JOIN(marked, positions,
                     LEFT.model=RIGHT.model AND LEFT.topic=RIGHT.topic
                     AND LEFT.pos<=RIGHT.p,
                     TRANSFORM(MTT, SELF:=LEFT), LOOKUP);
    RETURN Make_Result(selected);
  END;
END;