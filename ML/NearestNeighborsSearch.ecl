IMPORT ML;
IMPORT * FROM $;
EXPORT NearestNeighborsSearch:= MODULE
    EXPORT leafData := RECORD
      Types.t_RecordID qp_id;
      Types.NumericField;
    END;
    EXPORT NN := RECORD
    Types.t_RecordID qp_id;        // Query Point Id
    Types.t_RecordID id;           // Nearest Neighbor id
    Types.t_FieldReal distance;    // distance from QP to NN
  END;
  EXPORT DEFAULT := MODULE,VIRTUAL
    EXPORT SearchC(DATASET(Types.NumericField) indepData , DATASET(Types.NumericField) queryPointsData) := DATASET([], NN);
  END;
/*
  Nearest Neighbors Search could be done using different algorithms:
    Linear Search
    KDTree Search   (Implemented here)
    BallTree Search
    CoverTree
  Thus we create NearestNeighborsSearch as Virtual, all of them need to implement the algorithm in SearchC and return K-Nearest Neighbors
*/
  EXPORT KDTreeNNSearch(CONST Types.t_count NN_count=5, Trees.t_level Depth=10,Trees.t_level MedianDepth=0) := MODULE(DEFAULT)
    SHARED query_point := RECORD
      Types.t_RecordID id;                    // id of query point
      Types.t_FieldNumber p_number:=    0;    // qp attribute
      Types.t_FieldReal p_value:=       0;    // qp atribute’s value
      Types.t_RecordID node_id;               // id of the current node in the search
      Types.t_FieldNumber split_number:=0;    // attribute used to split
      Types.t_FieldReal split_value:=  -1;    // split value
      UNSIGNED CloseFarSon:=      0;    // CLOSER/FARTHER flag register
      UNSIGNED DownUpJump:=       0;    // DOWN/UP flag register
      DATASET(NN, CHOOSEN(NN_count)) NearNeighs;  // NN_count Nearest Neighbors
      INTEGER1 NNFound:=          0;
      Types.t_FieldReal BallRadius:=    99999999;
      BOOLEAN BWB:=           FALSE;        // Ball Within Bounds flag
      BOOLEAN BOB:=           FALSE;        // Bound Overlap Ball flag
      BOOLEAN isTerminal:=    FALSE;        // isTerminal flag
    END;
    EXPORT KNNeighbors(DATASET(Types.NumericField) queryPointsData, DATASET(Trees.Node) KDTFullTree, DATASET(Trees.Node) KDTPartitioned, DATASET(Trees.sNode) KDTBoundaries) := FUNCTION
      root:= KDTFullTree(node_id =1);
      queryPoints(DATASET(Types.NumericField) qp_ids):= FUNCTION
        seed:= DATASET([{0,0,99999999}], NN);
        rootnum:= MIN(root, number);
        rootval:= MIN(root, value);
        query_point ParentMove(qp_ids L, Types.t_Count KNN_count) := TRANSFORM
          SELF.node_id:= 1;
          SELF.split_number:= rootnum;
          SELF.split_value:=  rootval;
          SELF.NearNeighs := NORMALIZE(seed, KNN_count, TRANSFORM(NN, SELF.qp_id:= L.id, SELF:=LEFT));
          SELF := L;
        END;
        RETURN PROJECT(qp_ids, ParentMove(LEFT, NN_count));
      END;
      NN DistanceFromLeafNodePtoQueryP(DATASET(leafData) leafNodePoints):= FUNCTION
        attibsDist:= JOIN(queryPointsData, leafNodePoints, LEFT.id = RIGHT.qp_id AND LEFT.number = RIGHT.number, TRANSFORM(leafData, SELF.value:= LEFT.value - RIGHT.value, SELF:= RIGHT));
        pointsDist:= TABLE(attibsDist, {qp_id; id; distance:= SUM(GROUP,value*value)}, qp_id, id);
        RETURN pointsDist;
      END;
      query_point ChildMove(query_point L, NN R, INTEGER C):=TRANSFORM
        SELF.NearNeighs := L.NearNeighs + ROW(R, NN);
        SELF.NNFound := L.NNFound + IF(R.id >0 AND C<=nn_count, 1, 0);
        SELF.BallRadius:= IF(C<=nn_count, R.distance, L.BallRadius);
        SELF := L;
      END;
      c_SonType := ENUM ( UNSIGNED1, Closer = 0, Farther = 1 );
      query_point TxGoCloseFarSon(DATASET(query_point) qPoints, c_SonType CFson):= FUNCTION
        qpInfo:= JOIN(queryPointsData, qPoints, LEFT.id = RIGHT.id AND LEFT.number = RIGHT.split_number, TRANSFORM(query_point, SELF.p_number:=LEFT.number, SELF.p_value:=LEFT.value, SELF:= RIGHT));
        qpNextNode:= PROJECT(qpInfo, TRANSFORM(query_point, SELF.node_id := (LEFT.node_id << 1) + IF(LEFT.p_value < LEFT.split_value, CFson, (CFson+1)%2), SELF.CloseFarSon:=(LEFT.CloseFarSon<<1) + CFson, SELF.DownUpJump:= LEFT.DownUpJump<<1, SELF.BOB:=FALSE, SELF:= LEFT));
        qpSplitInfo:= JOIN(KDTFullTree, qpNextNode, LEFT.node_id = RIGHT.node_id, TRANSFORM(query_point, SELF.split_number:=LEFT.number, SELF.split_value:=LEFT.value, SELF.isTerminal:=(LEFT.number=0), SELF:= RIGHT));
        RETURN qpSplitInfo;
      END;
      query_point UpdateBOB(DATASET(query_point) qPoints):= FUNCTION
        qpUpdated:= PROJECT(qPoints, TRANSFORM(query_point, SELF.BOB:=ABS(LEFT.p_value - LEFT.split_value)< SQRT(LEFT.ballradius), SELF:= LEFT));
        RETURN qpUpdated;
      END;
      query_point UpdateBWB(DATASET(query_point) qPoints):= FUNCTION
        rootNodeBWB:= qPoints(node_id=1 AND NNFound=NN_count);
        directBWB:=   PROJECT(rootNodeBWB, TRANSFORM(query_point, SELF.BWB:=IF(LEFT.CloseFarSon%2=1 AND LEFT.DownUpJump%2=1, TRUE, NOT LEFT.BOB), SELF:=LEFT));
        distBWB:= RECORD
          Types.t_RecordID id;
          Types.t_FieldReal p_value:=0;
          Types.t_FieldReal b_dist:=0;
          Types.t_FieldReal ballradius;
          Types.t_RecordID splitId;
          Types.t_FieldNumber number;
          Types.t_FieldReal value;
          BOOLEAN highBranch;
          Types.t_Discrete p_bwb:=0;
        END;
        needCalc:=qPoints(node_id>1 OR NNFound<NN_count);
        lbounds:= JOIN(KDTBoundaries, needCalc, LEFT.splitid=RIGHT.node_id, TRANSFORM(distBWB, SELF.id:=RIGHT.id, SELF.ballradius:= RIGHT.ballradius, SELF:=LEFT));
        lbdist:=  JOIN(queryPointsData, lbounds, LEFT.id=RIGHT.id AND LEFT.number=RIGHT.number, TRANSFORM(distBWB, SELF.p_value:=LEFT.value, SELF.b_dist:= (LEFT.value-RIGHT.value)*(LEFT.value-RIGHT.value), SELF:=RIGHT));
        bwb_all:= PROJECT(lbdist, TRANSFORM(distBWB, SELF.P_BWB:=IF(LEFT.ballradius<LEFT.b_dist,0,1), SELF:=LEFT));
        bwb:=TABLE(bwb_all, {id, bwb:=SUM(GROUP, p_bwb)},id);
        qpUpdated:= JOIN(qPoints, bwb, LEFT.id=RIGHT.id, TRANSFORM(query_point,SELF.BWB:=IF(RIGHT.id>0, RIGHT.bwb=0,FALSE), SELF:=LEFT));
        RETURN directBWB + qpUpdated;
      END;
      query_point PruneLogCloser(DATASET(query_point) qPoints):= FUNCTION
        query_point pruneLog(query_point qp):= TRANSFORM
          SELF.node_id := qp.node_id>>1;
          cfsBase:=  qp.CloseFarSon>>2;
          cfsLast:=  cfsBase % 2;
          SELF.CloseFarSon:=  (cfsBase<<1) + cfsLast;
          dujBase:=  qp.DownUpJump>>2;
          SELF.DownUpJump:=   (dujBase<<1) + 1;
          SELF:= qp;
        END;
        qpNextNode:= PROJECT(qPoints, pruneLog(LEFT));
        qpSplitInfo:= JOIN(KDTFullTree, qpNextNode, LEFT.node_id = RIGHT.node_id, TRANSFORM(query_point, SELF.split_number:=LEFT.number, SELF.split_value:=LEFT.value,SELF.isTerminal:=FALSE, SELF:= RIGHT));
        qpInfo:=  JOIN(queryPointsData, qpSplitInfo, LEFT.id = RIGHT.id AND LEFT.number = RIGHT.split_number, TRANSFORM(query_point, SELF.p_number:=LEFT.number, SELF.p_value:=LEFT.value, SELF:= RIGHT));
        qpBOB:=   UpdateBOB(qpInfo);
        RETURN UpdateBWB(qpBOB);
      END;
      query_point PruneLogFarther(DATASET(query_point) qPoints):= FUNCTION
        query_point pruneLog(query_point qp):= TRANSFORM
          SELF.node_id := qp.node_id>>1;
          cfsBase :=  qp.CloseFarSon>>4;
          cfsLast :=  cfsBase % 2;
          cfsNew  :=(cfsBase<<1) + cfsLast;
          SELF.CloseFarSon:= cfsNew;
          dujBase:=  qp.DownUpJump>>4;
          SELF.DownUpJump:= (dujBase<<1) +1;
          SELF:= qp;
        END;
        qpNextNode:= PROJECT(qPoints, pruneLog(LEFT)) ;
        qpSplitInfo:= JOIN(KDTFullTree, qpNextNode, LEFT.node_id = RIGHT.node_id, TRANSFORM(query_point, SELF.split_number:=LEFT.number, SELF.split_value:=LEFT.value,SELF.isTerminal:=FALSE, SELF:= RIGHT));
        qpInfo  := JOIN(queryPointsData, qpSplitInfo, LEFT.id = RIGHT.id AND LEFT.number = RIGHT.split_number, TRANSFORM(query_point, SELF.p_number:=LEFT.number, SELF.p_value:=LEFT.value, SELF:= RIGHT));
        qpBOB   := PROJECT(qpInfo, TRANSFORM(query_point, SELF.BOB:=IF(LEFT.BOB, TRUE, ABS(LEFT.p_value - LEFT.split_value)< SQRT(LEFT.ballradius)), SELF:= LEFT));
        onRoot  := qpBOB(node_id=1);
        notRoot := qpBOB(node_id>1);
        needBWBupdate:= onRoot(CloseFarSon%2=0 AND BOB=FALSE) + onRoot(CloseFarSon%2=1) + notRoot(BOB=FALSE);
        RETURN onRoot(CloseFarSon=0 AND BOB=TRUE) + notRoot(BOB=TRUE) + UpdateBWB(needBWBupdate);
      END;
      query_point TxReturnToParent(DATASET(query_point) qPoints, c_SonType CFSon):= FUNCTION
        query_point goUp(query_point qp, c_SonType cfs):= TRANSFORM
          SELF.node_id := (qp.node_id DIV 2);
          SELF.CloseFarSon:=  (qp.CloseFarSon<<1) + cfs;
          SELF.DownUpJump:=   (qp.DownUpJump<<1) + 1;
          SELF:= qp;
        END;
        qpNextNode:= PROJECT(qPoints, goUp(LEFT, CFSon)) ;
        qpSplitInfo:= JOIN(KDTFullTree, qpNextNode, LEFT.node_id = RIGHT.node_id, TRANSFORM(query_point, SELF.split_number:=LEFT.number, SELF.split_value:=LEFT.value,SELF.isTerminal:=FALSE, SELF:= RIGHT));
        RETURN UpdateBOB(qpSplitInfo);
      END;
      query_point TxTerminal(DATASET(query_point) qpoints):= FUNCTION
        leafNodesData:= JOIN(KDTPartitioned, qpoints, LEFT.node_id = RIGHT.node_id, TRANSFORM(leafData, SELF.qp_id:=RIGHT.id, SELF:= LEFT));
        jqlDist:= DistanceFromLeafNodePtoQueryP(leafNodesData);
        lastNN:= NORMALIZE(qpoints, LEFT.NearNeighs, TRANSFORM(RIGHT));
        allNN:= SORT(jqlDist + lastNN, qp_id, distance);
        pre_qpoints:= PROJECT(qpoints, TRANSFORM(query_point, SELF.NearNeighs:= [], SELF.NNFound:=0, SELF.IsTerminal:=FALSE, SELF:=LEFT));
        denorm:= DENORMALIZE(pre_qpoints, allNN, LEFT.id = RIGHT.qp_id, ChildMove(LEFT,RIGHT, COUNTER));
        bwbupdated:= UpdateBWB(denorm);
        closerBack:= TxReturnToParent(bwbupdated(CloseFarSon%2 = 0), 0);
        fartherBack:= TxReturnToParent(bwbupdated(CloseFarSon%2 = 1), 1);
        RETURN closerBack + fartherBack;
      END;
      loopbody(DATASET(query_point) qpoints):= FUNCTION
        // From an upper level
        fromParent:= qpoints(DownUpJump % 2 = 0);
        backToParent:= TxTerminal((fromParent(isTerminal=TRUE)));
        downToCloserSon:= TxGoCloseFarSon(fromParent(isTerminal=FALSE), c_SonType.Closer);
        // From a lower level
        fromCloserSon:= qpoints(DownUpJump % 2 = 1, CloseFarSon % 2 = 0);
        fromFartherSon:= qpoints(DownUpJump % 2 = 1, CloseFarSon % 2 = 1);
        // Ball Overlap Bound state => down to farther son
        downToFartherSon:= TxGoCloseFarSon(fromCloserSon(BOB =TRUE), c_SonType.Farther);
        // Search over on these branches, prune log and go to parent
        returnFromCloser:= PruneLogCloser(fromCloserSon(BOB =FALSE));
        returnFromFarther:= PruneLogFarther(fromFartherSon);
        // Next iteration
        RETURN backToParent + downToCloserSon + downToFartherSon + returnFromCloser + returnFromFarther;
      END;
      qpoints:= queryPoints(queryPointsData(number = 1));
      // Keep processing query points until get Ball within Bounds state
      //res:= LOOP(qpoints, LEFT.bwb=FALSE, EXISTS(ROWS(LEFT)), loopbody(ROWS(LEFT)));
      res:= LOOP(qpoints, LEFT.bwb=FALSE, loopbody(ROWS(LEFT)));
      RETURN NORMALIZE(res, LEFT.NearNeighs, TRANSFORM(RIGHT));
    END;
    EXPORT SearchC(DATASET(Types.NumericField) indepData , DATASET(Types.NumericField) queryPointsData):= FUNCTION
      // Partition the space using KDTree
      KDT:= Trees.KdTree(indepData, Depth, MedianDepth);
      fulltree:= KDT.FullTree;
      Partitioned:= KDT.Partitioned;
      Boundaries:= KDT.Boundaries;
      // Search for the K Nearest Neighbors
      RETURN  KNNeighbors(queryPointsData, fulltree, Partitioned, Boundaries);
    END;
  END;
END;