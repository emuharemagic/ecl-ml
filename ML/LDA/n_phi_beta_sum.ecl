//  SUM(SUM(SUM(phi_nk w_n_j log(beta_nk))))
IMPORT * FROM $;
IMPORT Types FROM $;
// aliases for convenience
Topic_Values := Types.Topic_Values;
Topic_Value := Types.Topic_Value;
TermFreq := Types.TermFreq;
OnlyValue := Types.OnlyValue;

//EXPORT REAL8 n_phi_beta_sum(Types.Topic_Values_DataSet t_phis,
//                            Types.TermFreq_DataSet word_counts,
//                            Types.Topic_Values_DataSet t_logBetas) := FUNCTION
//  OnlyValue n_phi(OnlyValue phi, TermFreq term) := TRANSFORM
//    SELF.v := phi.v * term.v;
//  END;
//  OnlyValue n_phi_beta(OnlyValue n_phi, OnlyValue logBeta) := TRANSFORM
//    SELF.v := n_phi.v * logBeta.v;
//  END;
//  OnlyValue sum_prods(Topic_Values phis, Topic_Values betas) := TRANSFORM
//    n_phi_terms := COMBINE(phis.vs, word_counts, n_phi(LEFT, RIGHT));
//    SELF.v := SUM(COMBINE(n_phi_terms, betas.vs, n_phi_beta(LEFT, RIGHT)), v);
//  END;
//  topic_terms := COMBINE(t_phis, t_logBetas, sum_prods(LEFT, RIGHT));
//  RETURN SUM(topic_terms, v);
//END;

EXPORT REAL8 n_phi_beta_sum(Types.Topic_Values_DataSet t_phis,
                            Types.TermFreq_DataSet word_counts,
                            Types.Topic_Values_DataSet t_logBetas) := BEGINC++
  #ifndef ECL_LDA_ONLYVALUE
  #define ECL_LDA_ONLYVALUE
  typedef  struct __attribute__ ((__packed__))  LDAOnlyValue {
    double v;
  };
  #endif
  #ifndef ECL_LDA_TOPIC_VALUES
  #define ECL_LDA_TOPIC_VALUES
  typedef  struct __attribute__ ((__packed__))  LDATopicValues {
    uint32_t topic;
    size32_t sz_vs;   //array of Only Values follows of size sz_vs
  };
  #endif
  #ifndef ECL_LDA_TERMFREQ
  #define ECL_LDA_TERMFREQ
  typedef struct __attribute__ ((__packed__)) LDATermFreq {
    uint64_t nominal;
    uint32_t f;
  };
  #endif
  #include <math.h>
  #body
  const LDATermFreq* in_wc = (LDATermFreq*) word_counts;
  size_t words = lenWord_counts/sizeof(LDATermFreq);
  size32_t vs_size = words * sizeof(LDAOnlyValue);
  size32_t fx_size = sizeof(LDATopicValues);
  double rslt = 0.0;
  size32_t consumed = 0;
  if (lenT_phis != lenT_logbetas) rtlFail(0, "Topic phis and betas not equal");
  while (consumed < lenT_phis) {
    const LDATopicValues* in_t_phis = (LDATopicValues*) (t_phis + consumed);
    const LDATopicValues* in_t_logBetas = (LDATopicValues*) (t_logbetas + consumed);
    const LDAOnlyValue* in_phis = (LDAOnlyValue*) (t_phis + fx_size + consumed);
    const LDAOnlyValue* in_logBetas = (LDAOnlyValue*)(t_logbetas + fx_size + consumed);
    if (in_t_phis->sz_vs!=vs_size) rtlFail(0, "Words and Phis not the same");
    if (in_t_logBetas->sz_vs!=vs_size) rtlFail(0, "Wrong number logBetas");
    for (uint32_t word=0; word<words; word++) {
      if (in_phis[word].v > 0.0) {
        rslt += in_wc[word].f * in_phis[word].v * in_logBetas[word].v;
      }
    }
    consumed += fx_size + vs_size;
  }
  return rslt;
ENDC++;
