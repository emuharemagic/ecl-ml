// Sum the phi log phi for each term occurrence
//        SUM(SUM(phi_nk log(phi_nk)))
//N.B., we use a compressed vector with count instead of a 0/1 vector
IMPORT * FROM $;
IMPORT Types FROM $;
Doc_Topics := Types.Doc_Topics;
Topic_Values := Types.Topic_Values;
Topic_Value := Types.Topic_Value;
TermFreq := Types.TermFreq;
OnlyValue := Types.OnlyValue;
//EXPORT REAL8 n_phi_log_phi_sum(Types.Topic_Values_DataSet t_phis,
//                               Types.TermFreq_DataSet word_counts) := FUNCTION
//  Types.OnlyValue prod(Types.OnlyValue tv, Types.TermFreq tf) := TRANSFORM
//    SELF.v := tf.v * tv.v * LN(tv.v);
//  END;
//  Types.OnlyValue sum_terms(Types.Topic_Values t_phi) := TRANSFORM
//    SELF.v := SUM(COMBINE(t_phi.vs, word_counts, prod(LEFT, RIGHT)), v);
//  END;
//  p0 := PROJECT(t_phis, sum_terms(LEFT));
//  RETURN SUM(p0, v);
//END;
EXPORT REAL8 n_phi_log_phi_sum(Types.Topic_Values_DataSet t_phis,
                               Types.TermFreq_DataSet word_counts) := BEGINC++
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
  size_t vs_size = words * sizeof(LDAOnlyValue);
  size_t fx_size = sizeof(LDATopicValues);
  double rslt = 0.0;
  size_t consumed = 0;
  while (consumed < lenT_phis) {
    const LDATopicValues* in_t_phis = (LDATopicValues*) (t_phis + consumed);
    const LDAOnlyValue* in_phis = (LDAOnlyValue*) (t_phis + fx_size + consumed);
    if (in_t_phis->sz_vs!=vs_size) rtlFail(0, "Words and Phis not the same");
    for (uint32_t word=0; word<words; word++) {
      if (in_phis[word].v > 0.0) {
        rslt += in_wc[word].f * in_phis[word].v * log(in_phis[word].v);
      }
    }
    consumed += fx_size + vs_size;
  }
  return rslt;
ENDC++;
