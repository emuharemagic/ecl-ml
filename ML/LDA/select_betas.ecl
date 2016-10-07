// Select the beta values from an array based upon the matching word count array
//
IMPORT $.Types;

EXPORT DATASET(Types.OnlyValue)
select_betas(DATASET(Types.TermValue) betas,
             DATASET(Types.TermFreq) terms) := BEGINC++
  #ifndef ECL_LDA_TERMVALUE
  #define ECL_LDA_TERMVALUE
  typedef  struct __attribute__ ((__packed__))  LDATermValue {
    uint64_t nominal;
    double v;
  };
  #endif
  #ifndef ECL_LDA_TERMFREQ
  #define ECL_LDA_TERMFREQ
  typedef  struct __attribute__ ((__packed__))  LDATermFreq {
    uint64_t nominal;
    uint32_t f;
  };
  #endif
  #ifndef ECL_LDA_ONLYVALUE
  #define ECL_LDA_ONLYVALUE
  typedef struct __attribute__ ((__packed__)) LDAOnlyValue {
    double v;
  };
  #endif
  #body
  size_t num_betas = lenBetas / sizeof(LDATermValue);
  size_t num_terms = lenTerms / sizeof(LDATermFreq);
  __lenResult = num_terms * sizeof(LDAOnlyValue);
  __result = rtlMalloc(__lenResult);
  const LDATermValue* in_betas = (LDATermValue*) betas;
  const LDATermFreq*  in_terms = (LDATermFreq*) terms;
  LDAOnlyValue* sel_betas = (LDAOnlyValue*) __result;
  uint32_t top_term = 0;
  for (uint32_t i=0; i<num_betas && top_term<num_terms; i++) {
    if (in_betas[i].nominal == in_terms[top_term].nominal) {
      sel_betas[top_term].v = in_betas[i].v;
      top_term++;
    }
  }
ENDC++;
