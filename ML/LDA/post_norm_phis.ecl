// Normalize the topic phi vectors with the phi sum vector
IMPORT $.Types;

EXPORT Types.Topic_Values_DataSet
       post_norm_phis(Types.Topic_Values_Dataset t_phis,
                      Types.OnlyValue_DataSet sum_phis) := BEGINC++
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
  #body
  __lenResult = lenT_phis;
  __result = rtlMalloc(__lenResult);
  char msg_buffer[100];
  size_t fx_size = sizeof(LDATopicValues);
  const LDAOnlyValue* in_phis_sum = (LDAOnlyValue*) sum_phis;
  size_t vs_size = lenSum_phis;
  size_t num_words = lenSum_phis / sizeof(LDAOnlyValue);
  size_t consumed = 0;
  while (consumed < lenT_phis) {
    const LDATopicValues* in_t_phis = (LDATopicValues*) (t_phis + consumed);
    const LDAOnlyValue* in_phis = (LDAOnlyValue*) (t_phis + consumed + fx_size);
    LDATopicValues* rslt_t_phis = (LDATopicValues*) (__result + consumed);
    LDAOnlyValue* rslt_phis = (LDAOnlyValue*) (__result + fx_size + consumed);
    if (vs_size!=in_t_phis->sz_vs) {
      sprintf(msg_buffer,
              "Unexpected number of phi, %i expected; found %i. ",
              vs_size, in_t_phis->sz_vs);
      rtlFail(0, msg_buffer);
    }
    rslt_t_phis->topic = in_t_phis->topic;
    rslt_t_phis->sz_vs = in_t_phis->sz_vs;
    for (uint32_t word=0; word<num_words; word++) {
      rslt_phis[word].v = in_phis[word].v / in_phis_sum[word].v;
    }
    consumed += fx_size + vs_size;
  }
ENDC++;
