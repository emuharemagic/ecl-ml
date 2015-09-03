IMPORT * FROM $;
IMPORT $.Mat;
IMPORT ML;

EXPORT Ensemble := MODULE
  SHARED t_node := INTEGER4;
  SHARED t_level := UNSIGNED2;
  SHARED t_Count:= Types.t_Count;
  SHARED t_Index:= INTEGER4;
  SHARED l_result:= Types.l_result;
  EXPORT modelD_Map :=	DATASET([{'id','ID'},{'node_id','1'},{'level','2'},{'number','3'},{'value','4'},{'new_node_id','5'},{'group_id',6}], {STRING orig_name; STRING assigned_name;});
  EXPORT STRING modelD_fields := 'node_id,level,number,value,new_node_id,group_id';	// need to use field map to call FromField later
  EXPORT modelC_Map :=	DATASET([{'id','ID'},{'node_id','1'},{'level','2'},{'number','3'},{'value','4'},{'high_fork','5'},{'new_node_id','6'},{'group_id',7}], {STRING orig_name; STRING assigned_name;});
  EXPORT STRING modelC_fields := 'node_id,level,number,value,high_fork,new_node_id,group_id';	// need to use field map to call FromField later
  EXPORT NodeID := RECORD
    t_node node_id;
    t_level level;
  END;
  EXPORT NodeInstDiscrete := RECORD
    NodeID;
    Types.DiscreteField;
    Types.t_Discrete depend; // The dependant value
  END;
  EXPORT NodeInstContinuous := RECORD
    NodeID;
    Types.NumericField;
    Types.t_Discrete depend; // The dependant value
    BOOLEAN high_fork:=FALSE;
  END;
  EXPORT SplitF := RECORD		// data structure for splitting results
    NodeID;
    ML.Types.t_FieldNumber number; // The column used to split
    ML.Types.t_Discrete value; // The value for the column in question
    t_node new_node_id; // The new node that value goes to
  END;
  EXPORT SplitC := RECORD		// data structure for splitting results
    NodeID;
    ML.Types.t_FieldNumber number; // The column used to split
    ML.Types.t_FieldReal value; // The cutpoint value for the column in question
    INTEGER1 high_fork:=0;   // 0 = lower or equal than value, 1 greater than value
    t_node new_node_id; // The new node that value goes to
  END;
  EXPORT gSplitF := RECORD
    SplitF;
    t_count group_id;
  END;
  EXPORT gSplitC := RECORD
    SplitC;
    t_count group_id;
  END;
  SHARED gNodeInstDisc := RECORD
    NodeInstDiscrete;
    t_count group_id;
  END;
  SHARED gNodeInstCont := RECORD
    NodeInstContinuous;
    t_count group_id;
  END;
  SHARED DepGroupedRec := RECORD(Types.DiscreteField)
    UNSIGNED group_id := 0;
    Types.t_RecordID new_id := 0;
  END;
  SHARED DepGroupedRec GroupDepRecords (Types.DiscreteField l, Sampling.idListGroupRec r) := TRANSFORM
    SELF.group_id 	:= r.gNum;
    SELF.new_id			:= r.id;
    SELF						:= l;
  END;
  SHARED NxKoutofM(t_Index N, Types.t_FieldNumber K, Types.t_FieldNumber M) := FUNCTION
    rndFeatRec:= RECORD
      t_count	      gNum   :=0;
      Types.t_FieldNumber number :=0;
      Types.t_FieldReal   rnd    :=0;
    END;
    seed:= DATASET([{0,0,0}], rndFeatRec);
    group_seed:= DISTRIBUTE(NORMALIZE(seed, N,TRANSFORM(rndFeatRec, SELF.gNum:= COUNTER)), gNum);
    allFields:= NORMALIZE(group_seed, M, TRANSFORM(rndFeatRec, SELF.number:= (COUNTER % M) +1, SELF.rnd:=RANDOM(), SELF:=LEFT),LOCAL);
    allSorted:= SORT(allFields, gNum, rnd, LOCAL);
    raw_set:= ENTH(allSorted, K, M, 1);
    RETURN TABLE(raw_set, {gNum, number});
  END;

/* Discrete implementation*/
// Function to split a set of nodes based on Feature Selection and Gini Impurity,
// the nodes received were generated sampling with replacement nTrees times.
// Note: it selects kFeatSel out of mTotFeats features for each sample, features must start at 1 and cannot exist a gap in the numeration.
  EXPORT RndFeatSelPartitionGIBased(DATASET(gNodeInstDisc) nodes, t_Count nTrees, t_Count kFeatSel, t_Count mTotFeats, t_Count p_level, REAL Purity=1.0):= FUNCTION
    this_set_all := DISTRIBUTE(nodes, HASH(group_id, node_id));
    node_base := MAX(this_set_all, node_id);           // Start allocating new node-ids from the highest previous
    featSet:= NxKoutofM(nTrees, kFeatSel, mTotFeats);  // generating list of features selected for each tree
    minFeats := TABLE(featSet, {gNum, minNumber := MIN(GROUP, number)}, gNum, FEW); // chose the min feature number from the sample
    this_minFeats:= JOIN(this_set_all, minFeats, LEFT.group_id = RIGHT.gNum AND LEFT.number= RIGHT.minNumber, LOOKUP);
    Purities := ML.Utils.Gini(this_minFeats, node_id, depend); // Compute the purities for each node
    PureEnough := Purities(1-Purity >= gini);
    // just need one match to create a leaf node, all similar instances will fall into the same leaf nodes
    pass_thru  := JOIN(PureEnough, this_set_all , LEFT.node_id=RIGHT.node_id, TRANSFORM(gNodeInstDisc, SELF.id:=0, SELF.number:=0, SELF.value:=0, SELF:=RIGHT), PARTITION RIGHT, KEEP(1));
    // splitting the instances that did not reach a leaf node
    this_set_out:= JOIN(this_set_all, PureEnough, LEFT.node_id=RIGHT.node_id, TRANSFORM(LEFT), LEFT ONLY, LOOKUP);
    this_set  := JOIN(this_set_out, featSet, LEFT.group_id = RIGHT.gNum AND LEFT.number= RIGHT.number, TRANSFORM(LEFT), LOOKUP);
    agg       := TABLE(this_set, {group_id, node_id, number, value, depend,Cnt := COUNT(GROUP)}, group_id, node_id, number, value, depend, LOCAL);
    aggc      := TABLE(agg, {group_id, node_id, number, value, TCnt := SUM(GROUP, Cnt)}, group_id, node_id, number, value, LOCAL);
    r := RECORD
      agg;
      REAL4 Prop; // Proportion pertaining to this dependant value
    END;
    prop := JOIN(agg, aggc, LEFT.group_id = RIGHT.group_id AND LEFT.node_id = RIGHT.node_id
            AND LEFT.number=RIGHT.number AND LEFT.value = RIGHT.value,
            TRANSFORM(r, SELF.Prop := LEFT.Cnt/RIGHT.Tcnt, SELF := LEFT), HASH);
    gini_per := TABLE(prop, {group_id, node_id, number, value, tcnt := SUM(GROUP,Cnt),val := SUM(GROUP,Prop*Prop)}, group_id, node_id, number, value, LOCAL);
    gini     := TABLE(gini_per, {group_id, node_id, number, gini_t := SUM(GROUP,tcnt*val)/SUM(GROUP,tcnt)}, group_id, node_id, number, LOCAL);
    splt     := DEDUP(SORT(gini, group_id, node_id, -gini_t, LOCAL), group_id, node_id, LOCAL);
    node_cand0 := JOIN(aggc, splt, LEFT.group_id = RIGHT.group_id AND LEFT.node_id = RIGHT.node_id AND LEFT.number = RIGHT.number, TRANSFORM(LEFT), LOOKUP, LOCAL);
    node_cand  := PROJECT(node_cand0, TRANSFORM({node_cand0, t_node new_nodeid}, SELF.new_nodeid := node_base + COUNTER, SELF := LEFT));
    // new split nodes found
    nc0      := PROJECT(node_cand, TRANSFORM(gNodeInstDisc, SELF.value := LEFT.new_nodeid, SELF.depend := LEFT.value, SELF.level := p_level, SELF := LEFT, SELF := []), LOCAL);
    // Assignig instances that didn't reach a leaf node to (new) node-ids (by joining to the sampled data)
    r1 := RECORD
      ML.Types.t_Recordid id;
      t_node nodeid;
    END;
    mapp := JOIN(this_set, node_cand, LEFT.group_id = RIGHT.group_id AND LEFT.node_id=RIGHT.node_id AND LEFT.number=RIGHT.number AND LEFT.value=RIGHT.value,
            TRANSFORM(r1, SELF.id := LEFT.id, SELF.nodeid:=RIGHT.new_nodeid ),LOOKUP, LOCAL);
    // Now use the mapping to actually reset all the points
    J := JOIN(this_set_out, mapp,LEFT.id=RIGHT.id,TRANSFORM(gNodeInstDisc, SELF.node_id:=RIGHT.nodeid, SELF.level:=LEFT.level+1, SELF := LEFT),LOCAL);
    RETURN pass_thru + nc0 + J;   // returning leaf nodes, new splits nodes and reassigned instances
  END;
// Function used in Random Forest Classifier Discrete Learning
// Note: returns treeNum Decision Trees, split based on Gini Impurity
//       selects fsNum out of total number of features, they must start at 1 and cannot exist a gap in the numeration.
//       Gini Impurity's default parameters: Purity = 1.0 and maxLevel (Depth) = 32 (up to 126 max iterations)
// more info http://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm#overview
  EXPORT SplitFeatureSampleGI(DATASET(Types.DiscreteField) Indep, DATASET(Types.DiscreteField) Dep, t_Index treeNum, t_Count fsNum, REAL Purity=1.0, t_level maxLevel=32) := FUNCTION
    N       := MAX(Dep, id);       // Number of Instances
    totFeat := COUNT(Indep(id=N)); // Number of Features
    depth   := MIN(126, maxLevel); // Max number of iterations when building trees (max 126 levels)
    // sampling with replacement the original dataset to generate treeNum Datasets
    grList:= ML.Sampling.GenerateNSampleList(treeNum, N); // the number of records will be N * treeNum
    groupDep:= JOIN(Dep, grList, LEFT.id = RIGHT.oldId, GroupDepRecords(LEFT, RIGHT)); // if grList were not too big we should use lookup
    // groupDep:= JOIN(Dep, grList, LEFT.id = RIGHT.oldId, GroupDepRecords(LEFT, RIGHT), MANY LOOKUP);
    ind0 := ML.Utils.FatD(Indep); // Ensure no sparsity in independents
    gNodeInstDisc init(Types.DiscreteField ind, DepGroupedRec depG) := TRANSFORM
      SELF.group_id := depG.group_id;
      SELF.node_id := depG.group_id;
      SELF.level := 1;
      SELF.depend := depG.value;	// Actually copies the dependant value to EVERY node - paying memory to avoid downstream cycles
      SELF.id := depG.new_id;
      SELF := ind;
    END;
    ind1 := JOIN(ind0, groupDep, LEFT.id = RIGHT.id, init(LEFT,RIGHT)); // If we were prepared to force DEP into memory then ,LOOKUP would go quicker
    // generating best feature_selection-gini_impurity splits, loopfilter level = COUNTER let pass only the nodes to be splitted for any current level
    res := LOOP(ind1, LEFT.level=COUNTER, COUNTER < depth , RndFeatSelPartitionGIBased(ROWS(LEFT), treeNum, fsNum, totFeat, COUNTER, Purity));
    // Turning LOOP results into splits and leaf nodes
    gSplitF toNewNode(gNodeInstDisc NodeInst) := TRANSFORM
      SELF.new_node_id  := IF(NodeInst.number>0, NodeInst.value, 0);
      SELF.number       := IF(NodeInst.number>0, NodeInst.number, 0);
      SELF.value        := NodeInst.depend;
      SELF:= NodeInst;
    END;
    new_nodes:= PROJECT(res(id=0), toNewNode(LEFT));    // node splits and leaf nodes
    // Taking care of instances (id>0) that reached maximum level and did not turn into a leaf yet
    mode_r := RECORD
      res.group_id;
      res.node_id;
      res.level;
      res.depend;
      Cnt := COUNT(GROUP);
    END;
    depCnt      := TABLE(res(id>0, number=1),mode_r, group_id, node_id, level, depend, FEW);
    depCntSort  := SORT(depCnt, group_id, node_id, cnt); // if more than one dependent value for node_id
    depCntDedup := DEDUP(depCntSort, group_id, node_id);     // the class value with more counts is selected
    maxlevel_leafs:= PROJECT(depCntDedup, TRANSFORM(gSplitF, SELF.number:=0, SELF.value:= LEFT.depend,
                                          SELF.new_node_id:=0, SELF:= LEFT));
    RETURN new_nodes + maxlevel_leafs;
  END;
  EXPORT FromDiscreteForest(DATASET(Types.NumericField) mod) := FUNCTION
    ML.FromField(mod, gSplitF,o, modelD_Map);
    RETURN o;
  END;
  EXPORT ToDiscreteForest(DATASET(gSplitF) nodes) := FUNCTION
    AppendID(nodes, id, model);
    ToField(model, out_model, id, modelD_fields);
    RETURN out_model;
  END;
  // Function that locates instances into the deepest branch nodes (split) based on their attribute values
  EXPORT gSplitInstD(DATASET(gSplitf) mod, DATASET(Types.DiscreteField) Indep) := FUNCTION
    splits := mod(new_node_id <> 0);	// separate split or branches
    leafs  := mod(new_node_id = 0);	// from final nodes
    join0  := JOIN(Indep, splits, LEFT.number = RIGHT.number AND LEFT.value = RIGHT.value, LOOKUP, MANY);
    sort0  := SORT(join0, group_id, id, level, number, node_id, new_node_id);
    group0 := GROUP(sort0, group_id, id);
    dedup0 := DEDUP(group0, LEFT.group_id = RIGHT.group_id AND LEFT.id = RIGHT.id AND LEFT.new_node_id != RIGHT.node_id, KEEP 1, LEFT);
    RETURN DEDUP(dedup0, LEFT.group_id = RIGHT.group_id AND LEFT.id = RIGHT.id AND LEFT.new_node_id = RIGHT.node_id, KEEP 1, RIGHT);
  END;
  // Probability function for discrete independent values and model
  EXPORT ClassProbDistribForestD(DATASET(Types.DiscreteField) Indep, DATASET(Types.NumericField) mod) := FUNCTION
    nodes := FromDiscreteForest(mod);
    leafs := nodes(new_node_id = 0);	// from final nodes
    splitData_raw:= gSplitInstD(nodes, Indep);
    splitData:= DISTRIBUTE(splitData_raw, id);
    gClass:= JOIN(splitData, leafs, LEFT.new_node_id = RIGHT.node_id AND LEFT.group_id = RIGHT.group_id,
              TRANSFORM(Types.DiscreteField, SELF.id:= LEFT.id, SELF.number := 1, SELF.value:= RIGHT.value), LOOKUP);
    accClass:= TABLE(gClass, {id, number, value, cnt:= COUNT(GROUP)}, id, number, value, LOCAL);
    tClass := TABLE(accClass, {id, number, tot:= SUM(GROUP, cnt)}, id, number, LOCAL);
    sClass:= JOIN(accClass, tClass, LEFT.number=RIGHT.number AND LEFT.id=RIGHT.id, LOCAL);
    RETURN PROJECT(sClass, TRANSFORM(l_result, SELF.conf:= LEFT.cnt/LEFT.tot, SELF:= LEFT, SELF:=[]), LOCAL);
  END;
  // Classification function for discrete independent values and model
  EXPORT ClassifyDForest(DATASET(Types.DiscreteField) Indep,DATASET(Types.NumericField) mod) := FUNCTION
    // get class probabilities for each instance
    dClass:= ClassProbDistribForestD(Indep, mod);
    // select the class with greatest probability for each instance
    sClass := SORT(dClass, id, -conf, LOCAL);
    finalClass:=DEDUP(sClass, id, LOCAL);
    RETURN PROJECT(finalClass, TRANSFORM(l_result, SELF:= LEFT, SELF:=[]), LOCAL);
  END;

/* Continuos implementation*/
// Function to binary-split a set of nodes based on Feature Selection and Gini Impurity,
// the nodes received were generated sampling with replacement nTrees times.
// Note: it selects kFeatSel out of mTotFeats features for each sample, features must start at 1 and cannot exist a gap in the numeration.  
  EXPORT RndFeatSelBinPartitionGIBased(DATASET(gNodeInstCont) nodes, t_Count nTrees, t_Count kFeatSel, t_Count mTotFeats, t_Count p_level, REAL Purity=1.0):= FUNCTION
    this_set_all := DISTRIBUTE(nodes, HASH(group_id, node_id, number));
    node_base := MAX(this_set_all, node_id);           // Start allocating new node-ids from the highest previous
    featSet:= NxKoutofM(nTrees, kFeatSel, mTotFeats);
    minFeats := TABLE(featSet, {gNum, minNumber := MIN(GROUP, number)}, gNum, FEW); // chose the min feature number from the sample
    this_minFeats:= JOIN(this_set_all, minFeats, LEFT.group_id = RIGHT.gNum AND LEFT.number= RIGHT.minNumber, LOOKUP);
    // Calculating dependent and total count for each node
    node_dep := TABLE(this_minFeats, {group_id, node_id, depend, cnt:= COUNT(GROUP)}, group_id, node_id, depend, FEW);
    node_dep_tot := TABLE(node_dep, {group_id, node_id, tot:= SUM(GROUP, cnt)}, group_id, node_id, FEW);
    r := RECORD
      node_dep;
      node_dep_tot.tot;
      REAL4 Prop; // Proportion pertaining to this dependant value
    END;
    node_prop := JOIN(node_dep, node_dep_tot,  LEFT.group_id = RIGHT.group_id AND LEFT.node_id =RIGHT.node_id,
                  TRANSFORM(r, SELF.Prop := LEFT.cnt/RIGHT.tot, SELF.tot:= RIGHT.tot, SELF := LEFT));
    // Compute 1-gini coefficient for each node for each field for each value
    gini_node:= TABLE(node_prop,{node_id, TotalCnt := SUM(GROUP,Cnt), Gini := 1-SUM(GROUP,Prop*Prop)}, node_id, FEW);
    PureEnough := gini_node(gini >= Purity);
    s_node_prop:= SORT(node_prop, group_id, node_id, -cnt);
    d_node_prop:= DEDUP(s_node_prop, group_id, node_id);
    leafsNodes := JOIN(d_node_prop, PureEnough, LEFT.node_id=RIGHT.node_id, TRANSFORM(gNodeInstCont, SELF.id:=0, SELF.number:=0, SELF.value:=0, SELF.level:= p_level,SELF:=LEFT), FEW);
    // splitting the instances that did not reach a leaf node
    this_set_out:= JOIN(this_set_all, PureEnough, LEFT.node_id=RIGHT.node_id, TRANSFORM(LEFT), LEFT ONLY, LOOKUP);
    this_set  := JOIN(this_set_out, featSet, LEFT.group_id = RIGHT.gNum AND LEFT.number= RIGHT.number, TRANSFORM(LEFT), LOOKUP);  
    ts_acc_dep   := TABLE(this_set, {group_id, node_id, number, value, depend, depcnt := COUNT(GROUP)}, group_id, node_id, number, value, depend, LOCAL);
    rec_dep:= RECORD
         ts_acc_dep;
         INTEGER tot_Low:=0;    // total number of ocurrences of Dependent with attrib-value <= treshold the Bag
         INTEGER tot_High:=0;   // total number of ocurrences of Dependent with attrib-value > treshold the Bag
         INTEGER tot_Dep:=0;    // total number of ocurrences of Dep value at the Bag
         INTEGER tot_Node:=0;   // total number of ocurrences at the Node
    END;
    rec_dep pop_dep(ts_acc_dep le, node_prop ri):= TRANSFORM
      SELF.depend:=   ri.depend;
      SELF.depcnt:=   IF(le.depend= ri.depend, le.depcnt, 0);
      SELF.tot_Dep:=  ri.cnt;
      SELF.tot_Node:= ri.tot;
      SELF:=          le;
      SELF:=          ri;
    END;
    deps:= JOIN(ts_acc_dep, node_prop, LEFT.node_id = RIGHT.node_id, pop_dep(LEFT, RIGHT), MANY LOOKUP);
    sort_deps:= SORT(deps, group_id, node_id, number, value, depend, -depcnt, LOCAL);
    ddup_deps:= DEDUP(sort_deps, group_id, node_id, number, depend, value, LOCAL);
    dist_deps:= DISTRIBUTE(ddup_deps, HASH(node_id, number, depend), MERGE(node_id, number, depend, value));
    rec_dep rold(dist_deps le, dist_deps ri) := TRANSFORM
      SELF.tot_Low:= ri.depCnt + IF(le.node_id=ri.node_id AND le.number=ri.number AND le.depend=ri.depend , le.tot_Low, 0);
      SELF.tot_High:= ri.tot_dep - ri.depCnt - IF(le.node_id=ri.node_id AND le.number=ri.number AND le.depend=ri.depend, le.tot_Low, 0);
      SELF := ri;
    END;
    // Accumulated Counting per Dependent value per Cut threshold
    bag_grouped := ITERATE(dist_deps, rold(LEFT,RIGHT), LOCAL);
    sp_bag:= TABLE(bag_grouped, {group_id, node_id, number, value, acc_low:= SUM(GROUP, tot_low), acc_high:= SUM(GROUP, tot_high)}, group_id, node_id, number, value);
    b_prop:= RECORD
      bag_grouped.group_id;
      bag_grouped.node_id;
      bag_grouped.number;
      bag_grouped.value;
      bag_grouped.depend;
      bag_grouped.tot_low;
      sp_bag.acc_low;
      REAL4 dep_prop_low;
      bag_grouped.tot_high;
      sp_bag.acc_high;
      REAL4 dep_prop_high;
      bag_grouped.tot_node;
    END;
    bag_prop:= JOIN(bag_grouped, sp_bag, LEFT.group_id = RIGHT.group_id AND LEFT.node_id = RIGHT.node_id
              AND LEFT.number = RIGHT.number AND LEFT.value = RIGHT.value,
              TRANSFORM(b_prop, SELF.dep_prop_low := LEFT.tot_low/RIGHT.acc_low,
                                SELF.dep_prop_high := LEFT.tot_high/RIGHT.acc_high,
                                SELF := RIGHT, SELF:=LEFT), HASH);
    zigma:= TABLE(bag_prop, {group_id, node_id, number, value,
                    propL:= acc_low/tot_node, giniL:= 1-SUM(GROUP,dep_prop_low*dep_prop_low),
                    propH:= acc_High/tot_node, giniH:= 1-SUM(GROUP,dep_prop_high*dep_prop_high)},
                    group_id, node_id, number, value, LOCAL);
    gini_rec:= RECORD
      zigma.group_id;
      zigma.node_id;
      zigma.number;
      zigma.value;
      REAL4 gini_t:=0;
    END;
    sp_gini:= PROJECT(zigma, TRANSFORM(gini_rec, SELF.gini_t:= LEFT.propL*LEFT.giniL + LEFT.propH*LEFT.giniH, SELF:=LEFT), LOCAL);
    sort_sp:= SORT(sp_gini, group_id, node_id);
    sort_gini:= SORT(sort_sp, group_id, node_id, gini_t, LOCAL);
    node_splits:= DEDUP(sort_gini, group_id, node_id, LOCAL);
    // Start allocating new node-ids from the highest previous
    new_nodes_low:= PROJECT(node_splits, TRANSFORM(gNodeInstCont, SELF.id:= 0, SELF.value:= LEFT.value, SELF.depend := node_base+ 2*COUNTER -1, SELF.level:= p_level, SELF.high_fork:=FALSE, SELF := LEFT));
    new_nodes_high:= PROJECT(node_splits, TRANSFORM(gNodeInstCont, SELF.id:= 0, SELF.value:= LEFT.value, SELF.depend := node_base+ 2*COUNTER, SELF.level:= p_level, SELF.high_fork:=TRUE, SELF := LEFT));
    new_nodes:= new_nodes_low + new_nodes_high;
    // Assignig instances that didn't reach a leaf node to (new) node-ids (by joining to the sampled data)
    noleaf:= JOIN(this_set_out, leafsNodes, LEFT.group_id = RIGHT.group_id AND LEFT.node_id = RIGHT.node_id, LEFT ONLY, LOOKUP);
    r1 := RECORD
      ML.Types.t_Recordid id;
      t_node nodeid;
      BOOLEAN high_fork:=FALSE;
    END;
    mapp := JOIN(noleaf, new_nodes, LEFT.node_id=RIGHT.node_id AND LEFT.number=RIGHT.number AND (LEFT.value>RIGHT.value)= RIGHT.high_fork,
                TRANSFORM(r1, SELF.id := LEFT.id, SELF.nodeid:=RIGHT.depend, SELF.high_fork:=RIGHT.high_fork ),LOOKUP);
    // Now use the mapping to actually reset all the points
    J := JOIN(this_set_out, mapp, LEFT.id=RIGHT.id, TRANSFORM(gNodeInstCont, SELF.node_id:=RIGHT.nodeid, SELF.level:=LEFT.level+1, SELF := LEFT), LOOKUP);
    RETURN nodes(level < p_level) + leafsNodes + new_nodes + J;
  END;
// Function used in Random Forest Classifier Continuos Learning
// Note: returns treeNum Binary Decision Trees, split based on Gini Impurity
//       it selects fsNum out of total number of features, they must start at 1 and cannot exist a gap in the numeration.
//       Gini Impurity's default parameters: Purity = 1.0 and maxLevel (Depth) = 32 (up to 126 max iterations)
  EXPORT SplitFeatureSampleGIBin(DATASET(Types.NumericField) Indep, DATASET(Types.DiscreteField) Dep, t_Index treeNum, t_Count fsNum, REAL Purity=1.0, t_level maxLevel=32) := FUNCTION
    N       := MAX(Dep, id);       // Number of Instances
    totFeat := COUNT(Indep(id=N)); // Number of Features
    depth   := MIN(126, maxLevel); // Max number of iterations when building trees (max 126 levels)
    // sampling with replacement the original dataset to generate treeNum Datasets
    grList:= ML.Sampling.GenerateNSampleList(treeNum, N); // the number of records will be N * treeNum
    groupDep0:= JOIN(dep, grList, LEFT.id = RIGHT.oldId, GroupDepRecords(LEFT, RIGHT));
    groupDep:=DISTRIBUTE(groupDep0, HASH(id));
    ind0 := ML.Utils.Fat(Indep); // Ensure no sparsity in independents
    gNodeInstCont init(Types.NumericField ind, DepGroupedRec depG) := TRANSFORM
      SELF.group_id := depG.group_id;
      SELF.node_id := depG.group_id;
      SELF.level := 1;
      SELF.depend := depG.value;	// Actually copies the dependant value to EVERY node - paying memory to avoid downstream cycles
      SELF.id := depG.new_id;
      SELF := ind;
    END;
    ind1 := JOIN(ind0, groupDep, LEFT.id = RIGHT.id, init(LEFT,RIGHT), LOCAL); 
    // generating best feature_selection-gini_impurity splits, loopfilter level = COUNTER let pass only the nodes to be splitted for any current level
    res := LOOP(ind1,  LEFT.level=COUNTER AND LEFT.level<= depth, RndFeatSelBinPartitionGIBased(ROWS(LEFT), treeNum, fsNum, totFeat, COUNTER, Purity));
    // Turning LOOP results into splits and leaf nodes
    gSplitC toNewNode(gNodeInstCont NodeInst) := TRANSFORM
      SELF.new_node_id  := IF(NodeInst.number>0, NodeInst.depend, 0);
      SELF.value := IF(NodeInst.number>0, NodeInst.value, NodeInst.depend);
      SELF.high_fork:=(INTEGER1)NodeInst.high_fork;
      SELF:= NodeInst;
    END;
    new_nodes:= PROJECT(res(id=0), toNewNode(LEFT), LOCAL);    // node splits and leaf nodes
    mode_r := RECORD
      res.group_id;
      res.node_id;
      res.level;
      res.depend;
      cnt := COUNT(GROUP);
    END;
    // Taking care instances (id>0) that reached maximum level and did not turn into a leaf yet
    depCnt      := TABLE(res(id>0, number=1), mode_r, group_id, node_id, level, depend, FEW);
    // Assigning class value based on majority voting
    depCntSort  := SORT(depCnt, group_id, node_id, -cnt); // if more than one dependent value for node_id
    depCntDedup := DEDUP(depCntSort, group_id, node_id);     // the class value with more counts is selected
    maxlevel_leafs:= PROJECT(depCntDedup, TRANSFORM(gSplitC, SELF.number:=0, SELF.value:= LEFT.depend, SELF.new_node_id:=0, SELF:= LEFT));
    RETURN new_nodes + maxlevel_leafs;
  END;
  EXPORT ToContinuosForest(DATASET(gSplitC) nodes) := FUNCTION
    AppendID(nodes, id, model);
    ToField(model, out_model, id, modelC_fields);
    RETURN out_model;
  END;
  EXPORT FromContinuosForest(DATASET(Types.NumericField) mod) := FUNCTION
    ML.FromField(mod, gSplitC,o, modelC_Map);
    RETURN o;
  END;
  // Function that locates instances into the deepest branch nodes (split) based on their attribute values
  EXPORT gSplitInstC(DATASET(gSplitC) mod, DATASET(Types.NumericField) Indep) := FUNCTION
    splits:= mod(new_node_id <> 0);	// separate split or branches
    leafs := mod(new_node_id = 0);	// from final nodes
    Ind   := DISTRIBUTE(Indep, HASH(id));
    join0 := JOIN(Ind, splits, LEFT.number = RIGHT.number AND RIGHT.high_fork = IF(LEFT.value > RIGHT.value, 1, 0), LOOKUP, MANY);
    sort0 := SORT(join0, group_id, id, level, node_id, LOCAL);
    dedup0:= DEDUP(sort0, LEFT.group_id = RIGHT.group_id AND LEFT.id = RIGHT.id AND LEFT.new_node_id != RIGHT.node_id, KEEP 1, LEFT, LOCAL);
    RETURN DEDUP(dedup0, LEFT.group_id = RIGHT.group_id AND LEFT.id = RIGHT.id AND LEFT.new_node_id = RIGHT.node_id, KEEP 1, RIGHT, LOCAL);
  END;
  // Probability function for continuous independent values and model
  EXPORT ClassProbDistribForestC(DATASET(Types.NumericField) Indep, DATASET(Types.NumericField) mod) := FUNCTION
    nodes := FromContinuosForest(mod);
    leafs := nodes(new_node_id = 0);	// from final nodes
    splitData_raw:= gSplitInstC(nodes, Indep);
    splitData:= DISTRIBUTE(splitData_raw, id);
    gClass:= JOIN(splitData, leafs, LEFT.new_node_id = RIGHT.node_id AND LEFT.group_id = RIGHT.group_id,
              TRANSFORM(Types.DiscreteField, SELF.id:= LEFT.id, SELF.number := 1, SELF.value:= RIGHT.value), LOOKUP);
    accClass:= TABLE(gClass, {id, number, value, cnt:= COUNT(GROUP)}, id, number, value, LOCAL);
    tClass := TABLE(accClass, {id, number, tot:= SUM(GROUP, cnt)}, id, number, LOCAL);
    sClass:= JOIN(accClass, tClass, LEFT.number=RIGHT.number AND LEFT.id=RIGHT.id, LOCAL);
    RETURN PROJECT(sClass, TRANSFORM(l_result, SELF.conf:= LEFT.cnt/LEFT.tot, SELF:= LEFT, SELF:=[]), LOCAL);
  END;
  // Classification function for continuous independent values and model
  EXPORT ClassifyCForest(DATASET(Types.NumericField) Indep,DATASET(Types.NumericField) mod) := FUNCTION
    // get class probabilities for each instance
    dClass:= ClassProbDistribForestC(Indep, mod);
    // select the class with greatest probability for each instance
    sClass := SORT(dClass, id, -conf, LOCAL);
    finalClass:=DEDUP(sClass, id, LOCAL);
    RETURN PROJECT(finalClass, TRANSFORM(l_result, SELF:= LEFT, SELF:=[]), LOCAL);
  END;
END;