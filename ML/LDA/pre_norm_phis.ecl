// Calculate pre-normalized phi values from the log Beta, the gamma, and counts
IMPORT $.Types;

EXPORT Types.Topic_Values_DataSet
       pre_norm_phis(UNSIGNED4 num_words,
                     Types.Topic_Values_DataSet t_logBetas,
                     Types.Topic_Value_DataSet t_digammas) := BEGINC++
  #ifndef ECL_LDA_ONLYVALUE
  #define ECL_LDA_ONLYVALUE
  typedef  struct __attribute__ ((__packed__)) LDAOnlyValue {
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
  #ifndef ECL_LDA_TOPIC_VALUE
  #define ECL_LDA_TOPIC_VALUE
  typedef  struct __attribute__ ((__packed__))  LDATopicValue {
    uint32_t topic;
    double v;
  };
  #endif
  #include <math.h>
  #body
  // the number and arrangement of phi values matches the Betas
  //phi and beta are by topic and term
  //word_counts are by word, and Gamma by topic
  const LDATopicValue* in_t_digammas = (LDATopicValue*) t_digammas;
  size_t num_topics = lenT_digammas/sizeof(LDATopicValue);
  size_t vs_size = num_words*sizeof(LDAOnlyValue);
  size_t fx_size = sizeof(LDATopicValues);
  const LDATopicValues* in_t_lb = (LDATopicValues*) t_logbetas;
  const LDAOnlyValue* in_logbetas = (LDAOnlyValue*)(t_logbetas+fx_size);
  __lenResult = lenT_logbetas;
  __result = rtlMalloc(__lenResult);
  LDATopicValues* t_phis = (LDATopicValues*) __result;
  LDAOnlyValue* phis = (LDAOnlyValue*) (__result + fx_size);
  size_t consumed = 0;
  uint32_t curr_topic;
  for (curr_topic=0; curr_topic<num_topics; curr_topic++) {
    if (in_t_lb->topic!=in_t_digammas[curr_topic].topic) rtlFail(0,"Topics mismatch");
    if (in_t_lb->sz_vs!=vs_size) rtlFail(0,"Bad beta size");
    t_phis->topic = in_t_digammas[curr_topic].topic;
    t_phis->sz_vs = vs_size;
    for (uint32_t w=0; w<num_words; w++) {
      phis[w].v = exp(in_logbetas[w].v + in_t_digammas[curr_topic].v);
    }
    consumed += vs_size + fx_size;
    in_t_lb = (LDATopicValues*)(t_logbetas + consumed);
    in_logbetas = (LDAOnlyValue*)(t_logbetas + fx_size + consumed);
    t_phis = (LDATopicValues*)(__result + consumed);
    phis = (LDAOnlyValue*)(__result + fx_size + consumed);
  }
  // these phi arrays are ready
ENDC++;
