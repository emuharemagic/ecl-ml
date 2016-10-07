//     SUM(SUM(phi_nk (digamma(gamma_k)-digamma(SUM(gamma_k)))))
//N.B., we use a compressed vector with count instead of a 0/1 vector
IMPORT * FROM $;
IMPORT Types FROM $;
// convenient alias definitions
Topic_Value := Types.Topic_Value;
Topic_Values := Types.Topic_Values;
OnlyValue := Types.OnlyValue;
TermFreq := Types.TermFreq;
//EXPORT REAL8 n_phi_gamma_sum(Types.Topic_Values_DataSet t_phis,
//                             Types.TermFreq_DataSet word_counts,
//                             Types.Topic_Value_DataSet t_gammas,
//                             REAL8 gamma_sum) := FUNCTION
//  OnlyValue n_term(OnlyValue phi, TermFreq trm, REAL8 gamma_k) := TRANSFORM
//    SELF.v := trm.v * phi.v * (digamma(gamma_k) - digamma(gamma_sum));
//  END;
//  OnlyValue byTopic(Topic_Value gamma, Topic_Values phis_k):=TRANSFORM
//    terms := COMBINE(phis_k.vs, word_counts, n_term(LEFT, RIGHT, gamma.v));
//    SELF.v := SUM(terms, v);
//  END;
//  rslt := SUM(COMBINE(t_gammas, t_phis, byTopic(LEFT, RIGHT)), v);
//  RETURN rslt;
//END;
EXPORT REAL8 n_phi_gamma_sum(Types.Topic_Values_DataSet t_phis,
                             Types.TermFreq_DataSet word_counts,
                             Types.Topic_Value_DataSet t_digammas,
                             REAL8 digamma_gamma_sum) := BEGINC++
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
  #ifndef ECL_LDA_TOPIC_VALUE
  #define ECL_LDA_TOPIC_VALUE
  typedef  struct __attribute__ ((__packed__))  LDATopicValue {
    uint32_t topic;
    double v;
  };
  #endif
  #include <math.h>
  #body
  const LDATermFreq* in_wc = (LDATermFreq*) word_counts;
  const LDATopicValue* in_t_digammas = (LDATopicValue*) t_digammas;
  size_t words = lenWord_counts/sizeof(LDATermFreq);
  size_t vs_size = words * sizeof(LDAOnlyValue);
  size_t fx_size = sizeof(LDATopicValues);
  size_t num_topics = lenT_digammas / sizeof(LDATopicValue);
  size_t topicx = 0;
  if(lenT_phis!=num_topics*(fx_size+vs_size)) rtlFail(0,"t_phis size wrong");
  double rslt = 0.0;
  size_t consumed = 0;
  while (consumed < lenT_phis && topicx < num_topics) {
    const LDATopicValues* in_t_phis = (LDATopicValues*) (t_phis + consumed);
    if(in_t_phis->topic!=in_t_digammas[topicx].topic) {
      rtlFail(0, "topic id mismatch");
    }
    const LDAOnlyValue* in_phis = (LDAOnlyValue*) (t_phis + fx_size + consumed);
    if (in_t_phis->sz_vs!=vs_size) rtlFail(0, "Words and Phis not the same");
    double digamma_diff = in_t_digammas[topicx].v - digamma_gamma_sum;
    for (uint32_t word=0; word<words; word++) {
      if (in_phis[word].v > 0.0) {
        rslt += in_wc[word].f * in_phis[word].v * digamma_diff;
      }
    }
    consumed += fx_size + vs_size;
    topicx++;
  }
  return rslt;
ENDC++;
