// Update the topic gammas from the topic phi values
IMPORT $.Types;
EXPORT Types.Topic_Value_DataSet
       update_gammas(REAL8 alpha, Types.Topic_Values_DataSet t_phis,
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
  #ifndef ECL_LDA_TOPIC_VALUE
  #define ECL_LDA_TOPIC_VALUE
  typedef  struct __attribute__ ((__packed__))  LDATopicValue {
    uint32_t topic;
    double v;
  };
  #endif
  #body
  const LDATermFreq* in_word_counts = (LDATermFreq*) word_counts;
  size_t num_words = lenWord_counts / sizeof(LDATermFreq);
  size_t fx_size = sizeof(LDATopicValues);
  size_t vs_size = sizeof(LDAOnlyValue) * num_words;
  size_t consumed = 0;
  size_t num_topics = lenT_phis / (fx_size + vs_size);
  uint32_t topic = 0;
  __lenResult = num_topics * sizeof(LDATopicValue);
  __result = rtlMalloc(__lenResult);
  LDATopicValue* t_gammas = (LDATopicValue*) __result;
  while (consumed<lenT_phis && topic < num_topics) {
    const LDATopicValues* in_t_phis = (LDATopicValues*) (t_phis + consumed);
    const LDAOnlyValue* in_phis = (LDAOnlyValue*) (t_phis + fx_size + consumed);
    if (in_t_phis->sz_vs!=vs_size) rtlFail(0,"Wrong number words");
    t_gammas[topic].topic = in_t_phis->topic;
    t_gammas[topic].v = alpha;
    for (uint32_t word=0; word<num_words; word++) {
      t_gammas[topic].v += in_word_counts[word].f * in_phis[word].v;
    }
    consumed += fx_size + vs_size;
    topic++;
  }
  if (topic!=num_topics || consumed!=lenT_phis) rtlFail(0, "Bad # topics");
ENDC++;
