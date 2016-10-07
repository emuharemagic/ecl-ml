IMPORT $ AS LDA;
// Aliases
Document_Scored := LDA.Types.Document_Scored;
Topic_Value := LDA.Types.Topic_Value;

EXPORT Top_Topics(DATASET(Document_Scored) ds, UNSIGNED2 num_dev=3) := FUNCTION
  Document_Scored select_topics(Document_Scored d) := TRANSFORM
    ave_score := AVE(d.topics, v);
    std_score := SQRT(VARIANCE(d.topics, v));
    significant_topics := SORT(d.topics(v>=ave_score+(std_score*num_dev)), -v);
    top_topic := TOPN(d.topics, 1, -v);
    SELF.topics := IF(EXISTS(significant_topics), significant_topics, top_topic);
    SELF := d;
  END;
  rslt := PROJECT(ds, select_topics(LEFT));
  RETURN rslt;
END;