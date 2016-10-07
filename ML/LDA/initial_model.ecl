// Convert the model parameters and document collection information into
//an initial topic model for each model and topic in model
IMPORT $.Types;
Model_Rslt := Types.Model_Topic_Result;
Model_init := Types.Model_Topic;
Model_Parm := Types.Model_Parameters;
Model_Collection_Stats := Types.Model_Collection_Stats;
/**
 * Convert model parameters and collection information into the initial
 * model-topic records.
 * @param models the base model building parameters
 * @param initial_values intial beta values
 * @param stats collection statistics needed for the model
 * @return the initial set of model-topic records
 */
EXPORT DATASET(Types.Model_Topic_Result)
       initial_model(DATASET(Types.Model_Parameters) models,
                     DATASET(Types.Model_Topic) initial_values,
                     DATASET(Types.Model_Collection_Stats) stats) := FUNCTION
  // Convert parameters and apply document stats
  Model_Rslt step1(Model_Parm mod, Model_Collection_Stats stat):=TRANSFORM
    SELF.model := mod.model;
    SELF.docs := stat.docs;
    SELF.unique_words := stat.unique_words;
    SELF.alpha := IF(mod.alpha=0.0, 50/mod.num_topics, mod.alpha);
    SELF.likelihood_change := 2*mod.beta_epsilon;
    SELF.max_beta_iterations := mod.max_beta_iterations;
    SELF.max_doc_iterations := mod.max_doc_iterations;
    SELF.beta_epsilon := mod.beta_epsilon;
    SELF.doc_epsilon := mod.doc_epsilon;
    SELF.num_topics := mod.num_topics;
    SELF.estimate_alpha := mod.estimate_alpha;
    SELF.likelihood := 0;
    SELF.EM_iterations := 0;
    SELF.last_doc_iterations := 0;
    SELF.last_docs_converged := 0;
    SELF.last_average_change := 0;
    SELF.last_min_likelihood := 0;
    SELF.last_max_likelihood := 0;
    SELF.last_alpha_iter := 0;
    SELF.last_min_change := 0;
    SELF.last_max_change := 0;
    SELF.last_doc_min_iter := 0;
    SELF.last_doc_max_iter := 0;
    SELF.last_alpha_df := 0;
    SELF.last_init_alpha := 0;
    SELF.logbetas := [];                  // pick up from initial values
    SELF.topic := 0;                      // prime record
    SELF.hist := [];
  END;
  mod := JOIN(models, stats, LEFT.model=RIGHT.model, step1(LEFT,RIGHT), LOOKUP);
  // expand by topic
  Model_Rslt step2(Model_Init init, Model_Rslt mod) := TRANSFORM
    SELF.topic := init.topic;
    SELF.logBetas := init.logBetas;
    SELF.alpha := IF(init.alpha<>0, init.alpha, mod.alpha);
    SELF := mod;
  END;
  mod_3 := JOIN(initial_values, mod, LEFT.model=RIGHT.model,
                step2(LEFT, RIGHT), LOOKUP);
  RETURN mod_3;
END;