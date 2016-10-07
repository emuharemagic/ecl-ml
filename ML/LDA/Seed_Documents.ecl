IMPORT $.Types AS Types;
IMPORT Std.System.ThorLib;
/** Randomly select the seeds for the model topics.
 * @param docs the pool of documents from which to select
 * @param seed_info the number of topics and docs per topic for seeds
 * @param seed documents
 */
EXPORT DATASET(Types.Seed_Document)
       Seed_Documents(DATASET(Types.Doc_Mapped) docs,
                      DATASET(Types.Model_Seed_Info) seed_info) := FUNCTION
  // Mark the documents and select sample rids
  Work_Entry := RECORD
   Types.t_model_id model;
   Types.t_topic topic;
   Types.t_record_id rid;
   UNSIGNED4 num_topics;
   UNSIGNED8 num_docs;
   UNSIGNED8 rval;
   UNSIGNED8 pos;
   UNSIGNED4 node;
  END;
  Work_Entry addSeed(Types.Doc_Mapped d, Types.MOdel_Seed_Info s) := TRANSFORM
    SELF.model := s.model;
    SELF.topic := 0;
    SELF.rid := d.rid;
    SELF.num_topics := s.num_topics;
    SELF.num_docs := s.num_docs;
    SELF.rval := RANDOM();
    SELF.pos := 0;
    SELF.node := 0;
  END;
  docs_expd := JOIN(docs, seed_info,
                    RIGHT.model IN LEFT.models,
                    addSeed(LEFT, RIGHT), ALL);   // only a small number of seed
  docs_sorted := SORT(docs_expd, model, rval);
  Work_Entry localPos(Work_Entry we, UNSIGNED cnt) := TRANSFORM
    SELF.pos := cnt;
    SELF.node := ThorLib.node();
    SELF := we;
  END;
  docs_marked := PROJECT(docs_sorted, localPos(LEFT, COUNTER), LOCAL);
  Pos_Adjust := RECORD
    docs_marked.model;
    docs_marked.node;
    UNSIGNED8 max_pos:=MAX(GROUP, docs_marked.pos);
    UNSIGNED8 adj := 0;
  END;
  pos_map := TABLE(docs_marked, Pos_Adjust, model, node, FEW, UNSORTED, LOCAL);
  pm_grouped := GROUP(pos_map, model, ALL);
  pm_sorted := SORT(pm_grouped, node);
  Pos_Adjust calc_adj(Pos_Adjust prev, Pos_Adjust curr) := TRANSFORM
    SELF.adj := prev.adj + prev.max_pos;
    SELF := curr;
  END;
  adjustments := UNGROUP(ITERATE(pm_sorted, calc_adj(LEFT, RIGHT)));
  Work_Entry adjust_pos(Work_Entry we, Pos_Adjust adj) := TRANSFORM
    SELF.pos := we.pos + adj.adj;
    SELF := we;
  END;
  adjusted := JOIN(docs_marked, adjustments,
                   LEFT.model=RIGHT.model AND LEFT.node=RIGHT.node,
                   adjust_pos(LEFT,RIGHT), LOOKUP);
  selected_rids := adjusted(pos <= num_topics*num_docs);
  // draw the selected
  Types.Seed_Document draw_doc(Types.Doc_Mapped doc, Work_Entry we) := TRANSFORM
    SELF.model := we.model;
    SELF.topic := ((we.pos-1) DIV we.num_docs) + 1;
    SELF.rid := doc.rid;
    SELF.word_counts := doc.word_counts;
  END;
  rslt := JOIN(docs, selected_rids, LEFT.rid=RIGHT.rid,
               draw_doc(LEFT, RIGHT), LOOKUP, MANY);
  RETURN rslt;
END;