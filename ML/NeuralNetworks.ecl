IMPORT ML;
IMPORT * FROM $;
IMPORT $.Mat;
IMPORT * FROM ML.Types;
IMPORT PBblas;
Layout_Cell := PBblas.Types.Layout_Cell;
Layout_Part := PBblas.Types.Layout_Part;

EXPORT NeuralNetworks (DATASET(Types.DiscreteField) net,UNSIGNED4 prows=0, UNSIGNED4 pcols=0, UNSIGNED4 Maxrows=0, UNSIGNED4 Maxcols=0) := MODULE

//initialize bias values in the neural network
//each bias matrix is a vector
//bias with no=L means the bias that goes to the layer L+1 so its size is equal to number of nodes in layer L+1
  EXPORT IntBias := FUNCTION
    //New Randome Matrix Generator
    Mat.Types.Element RandGen(UNSIGNED4 c, UNSIGNED4 NumRows) := TRANSFORM
      SELF.x := ((c-1) % NumRows) + 1;
      SELF.y := ((c-1) DIV NumRows) + 1;
      SELF.value := 1;
    END;
    //Creat the first weight matrix with no=1 (weight matrix between layer 1 and layer 2)
    b1rows := net(id=(2))[1].value;
    b1cols := 1;
    b1size := b1rows*b1cols;
    b1 := DATASET(b1size, RandGen(COUNTER, b1rows),DISTRIBUTED);
    b1no := Mat.MU.To(b1, 1);
    //step function for initialize the rest of the weight matrices
    Step(DATASET(Mat.Types.MUElement) InputBias, INTEGER coun) := FUNCTION
      L := coun+1; //creat the weight between layers L and L+1
      brows := net(id=(L+1))[1].value;
      bcols := 1;
      bsize := brows*bcols;
      b := DATASET(bsize, RandGen(COUNTER, brows),DISTRIBUTED);
      bno := Mat.MU.To(b, L);
      RETURN InputBias+bno;
    END;
    LoopNum := MAX(net,id)-2;
    initialized_Bias := LOOP(b1no, COUNTER <= LoopNum, Step(ROWS(LEFT),COUNTER));
  RETURN initialized_Bias;
  END;
//initialize the weights in a neural network
//the output is in ML.Mat.Types.MUElement format that each wight matrix has its own id, "no" value assigned to each wight marix represents the wight matrix belongs to the weight between layer no and layer no+1
//the size of the wight matrix with "no" value is i*j which i is the number of nodes in layer no+1 and j is the number of nodes in layer no
//the structure of the neural network is shown in the net record set
//in the net record set each id shows the layer numebr and the corresponding value shows number of nodes in that layer
//for example net:=DATASET ([{1,1,4},{2,1,2},{3,1,5}],Types.NumericField) shows a network that layer 1 has 4 nodes, layer 2 has 2 nodes and layer 3 has 5
  EXPORT IntWeights := FUNCTION
    //Generate a random number
    Produce_Random () := FUNCTION
      G := 1000000;
      R := (RANDOM()%G) / (REAL8)G;
      RETURN R;
    END;
    //New Randome Matrix Generator
    Mat.Types.Element RandGen(UNSIGNED4 c, UNSIGNED4 NumRows) := TRANSFORM
      SELF.x := ((c-1) % NumRows) + 1;
      SELF.y := ((c-1) DIV NumRows) + 1;
      SELF.value := Produce_Random();
    END;
    //Creat the first weight matrix with no=1 (weight matrix between layer 1 and layer 2)
    w1rows := net(id=2)[1].value;
    w1cols := net(id=1)[1].value;
    w1size := w1rows*w1cols;
    w1 := DATASET(w1size, RandGen(COUNTER, w1rows),DISTRIBUTED);
    w1no := Mat.MU.To(w1, 1);
    //step function for initialize the rest of the weight matrices
    Step(DATASET(Mat.Types.MUElement) InputWeight, INTEGER coun) := FUNCTION
      L := coun+1; //creat the weight between layers L and L+1
      wrows := net(id=(L+1))[1].value;
      wcols := net(id=L)[1].value;
      wsize := wrows*wcols;
      w := DATASET(wsize, RandGen(COUNTER, wrows),DISTRIBUTED);
      wno := Mat.MU.To(w, L);
      RETURN InputWeight+wno;
    END;
    LoopNum := MAX(net,id)-2;
    initialized_weights := LOOP(w1no, COUNTER <= LoopNum, Step(ROWS(LEFT),COUNTER));
    RETURN initialized_weights;
  END;
  //in the built model the no={1,2,..,NL-1} are the weight indexes
  //no={NL+1,NL+2,..,NL+NL} are bias indexes that go to the second, third, ..,NL)'s layer respectively
  EXPORT Model(DATASET(Types.NumericField) mod) := FUNCTION
  modelD_Map :=	DATASET([{'id','ID'},{'x','1'},{'y','2'},{'value','3'},{'no','4'}], {STRING orig_name; STRING assigned_name;});
    FromField(mod,Mat.Types.MUElement,dOut,modelD_Map);
    RETURN dOut;
  END;
  EXPORT ExtractWeights (DATASET(Types.NumericField) mod) := FUNCTION
    NNmod := Model (mod);
    NL := MAX (net, id);
    RETURN NNmod (no<NL);
  END;
  EXPORT ExtractBias (DATASET(Types.NumericField) mod) := FUNCTION
    NNmod := Model (mod);
    NL := MAX (net, id);
    B := NNmod (no>NL);
    Mat.Types.MUElement Sno (Mat.Types.MUElement l) := TRANSFORM
      SELF.no := l.no-NL;
      SELF := l;
    END;
    RETURN PROJECT (B,Sno(LEFT));
  END;
  /*
  implementation based on stanford deep learning toturi al (http://ufldl.stanford.edu/wiki/index.php/Neural_Networks)
  X is input data
  w and b represent the structure of neural network
  w represnts weight matrices : matrix with id=L means thw weight matrix between layer L and layer L+1
  w(i,j) with id=L represents the weight between unit i of layer L+1 and unit j of layer L
  b represent bias matrices
  b with id = L shows the bias value for the layer L+1
  b(i) with id= L show sthe bias value goes to uni i of layer L
  */

  // back propagation algorithm
  BP(DATASET(Types.NumericField) X,DATASET(Types.NumericField) Y,DATASET(Mat.Types.MUElement) IntW, DATASET(Mat.Types.MUElement) Intb, REAL8 LAMBDA=0.001, REAL8 ALPHA=0.1, UNSIGNED2 MaxIter=100) := MODULE
    dt := Types.ToMatrix (X);
    //SHARED dTmp := Mat.InsertColumn(dt,1,1.0); // add the intercept column
    dTmp := dt;
    SHARED d := Mat.Trans(dTmp); //in the entire of the calculations we work with the d matrix that each sample is presented in one column
    SHARED m := MAX (d, d.y); //number of samples
    SHARED m_1 := 1/m;
    yt := Types.ToMatrix (Y);
    SHARED Ytmp := Mat.Trans(yt);
    SHARED sizeRec := RECORD
      PBblas.Types.dimension_t m_rows;
      PBblas.Types.dimension_t m_cols;
      PBblas.Types.dimension_t f_b_rows;
      PBblas.Types.dimension_t f_b_cols;
    END;
   //Map for Matrix d.
    SHARED havemaxrow := maxrows > 0;
    SHARED havemaxcol := maxcols > 0;
    SHARED havemaxrowcol := havemaxrow and havemaxcol;
    SHARED dstats := Mat.Has(d).Stats;
    SHARED d_n := dstats.XMax;
    SHARED d_m := dstats.YMax;
    SHARED Ystats := Mat.Has(Ytmp).Stats;
    SHARED output_num := Ystats.XMax;
    derivemap := IF(havemaxrowcol, PBblas.AutoBVMap(d_n, d_m,prows,pcols,maxrows, maxcols),
                   IF(havemaxrow, PBblas.AutoBVMap(d_n, d_m,prows,pcols,maxrows),
                      IF(havemaxcol, PBblas.AutoBVMap(d_n, d_m,prows,pcols,,maxcols),
                      PBblas.AutoBVMap(d_n, d_m,prows,pcols))));
    SHARED sizeTable := DATASET([{derivemap.matrix_rows,derivemap.matrix_cols,derivemap.part_rows(1),derivemap.part_cols(1)}], sizeRec);
    //Create block matrix d
    dmap := PBblas.Matrix_Map(sizeTable[1].m_rows,sizeTable[1].m_cols,sizeTable[1].f_b_rows,sizeTable[1].f_b_cols);
    ddist := DMAT.Converted.FromElement(d,dmap);
    //Create block matrix Ytmp
    Ymap := PBblas.Matrix_Map(output_num,sizeTable[1].m_cols,sizeTable[1].f_b_rows,sizeTable[1].f_b_cols);
    Ydist := DMAT.Converted.FromElement(Ytmp,Ymap);
    //Creat block matrices for weights
    w1_mat := Mat.MU.From(IntW,1);
    w1_mat_x := Mat.Has(w1_mat).Stats.Xmax;
    w1_mat_y := Mat.Has(w1_mat).Stats.Ymax;
    w1map := PBblas.Matrix_Map(w1_mat_x, w1_mat_y, sizeTable[1].f_b_rows, sizeTable[1].f_b_rows);
    w1dist := DMAT.Converted.FromElement(w1_mat,w1map);
    w1no := PBblas.MU.TO(w1dist,1);
    //loopbody to creat the rest of weight blocks
    CreatWeightBlock(DATASET(PBblas.Types.MUElement) inputWno, INTEGER coun) := FUNCTION
      L := coun+1; //creat the weight block for weight between layers L and L+1
      w_mat := Mat.MU.From(IntW,L);
      w_mat_x := Mat.Has(w_mat).Stats.Xmax;
      w_mat_y := Mat.Has(w_mat).Stats.Ymax;
      wmap := PBblas.Matrix_Map(w_mat_x, w_mat_y, sizeTable[1].f_b_rows , sizeTable[1].f_b_rows);
      wdist := DMAT.Converted.FromElement(w_mat,wmap);
      wno := PBblas.MU.TO(wdist,L);
      RETURN inputWno+wno;
    END;
    iterations := MAX(IntW,no)-1;
    weightsdistno := LOOP(w1no, COUNTER <= iterations, CreatWeightBlock(ROWS(LEFT),COUNTER));
    //two kind of Bias blocks are calculated
    //1- each bias vector is converted to block format
    //2-each Bias vector is repeated first to m columns, then the final repreated bias matrix is converted to block format
    //the second kind of bias is calculated to make the next calculations easier, the first vector bias format is used just when we
    //want to update the bias vectors
    //Creat block vectors for Bias (above case 1)
    b1vec := Mat.MU.From(Intb,1);
    b1vec_x := Mat.Has(b1vec).Stats.Xmax;
    b1vecmap := PBblas.Matrix_Map(b1vec_x, 1, sizeTable[1].f_b_rows, 1);
    b1vecdist := DMAT.Converted.FromElement(b1vec,b1vecmap);
    b1vecno := PBblas.MU.TO(b1vecdist,1);
    //loopbody to creat the rest of bias vector blocks
    CreatBiasVecBlock(DATASET(PBblas.Types.MUElement) inputb, INTEGER coun) := FUNCTION
      L := coun+1; //creat the weight block for weight between layers L and L+1
      b_mat := Mat.MU.From(Intb,L);
      b_mat_x := Mat.Has(b_mat).Stats.Xmax;
      bmap := PBblas.Matrix_Map(b_mat_x, 1, sizeTable[1].f_b_rows, 1);
      bdist := DMAT.Converted.FromElement(b_mat,bmap);
      bno := PBblas.MU.TO(bdist,L);
      RETURN inputb+bno;
    END;
    biasVecdistno := LOOP(b1vecno, COUNTER <= iterations, CreatBiasVecBlock(ROWS(LEFT),COUNTER));
    //Creat block matrices for Bias (repeat each bias vector to a matrix with m columns) (above case 2)
    b1_mat := Mat.MU.From(Intb,1);
    b1_mat_x := Mat.Has(b1_mat).Stats.Xmax;
    b1_mat_rep := Mat.Repmat(b1_mat, 1, m); // Bias vector is repeated in m columns to make the future calculations easier
    b1map := PBblas.Matrix_Map(b1_mat_x, m, sizeTable[1].f_b_rows, sizeTable[1].f_b_cols);
    b1dist := DMAT.Converted.FromElement(b1_mat_rep,b1map);
    b1no := PBblas.MU.TO(b1dist,1);
    //loopbody to creat the rest of bias blocks
    CreatBiasBlock(DATASET(PBblas.Types.MUElement) inputb, INTEGER coun) := FUNCTION
      L := coun+1; //creat the weight block for weight between layers L and L+1
      b_mat := Mat.MU.From(Intb,L);
      b_mat_x := Mat.Has(b_mat).Stats.Xmax;
      b_mat_rep := Mat.Repmat(b_mat, 1, m); // Bias vector is repeated in m columns to make the future calculations easier
      bmap := PBblas.Matrix_Map(b_mat_x, m, sizeTable[1].f_b_rows, sizeTable[1].f_b_cols);
      bdist := DMAT.Converted.FromElement(b_mat_rep,bmap);
      bno := PBblas.MU.TO(bdist,L);
      RETURN inputb+bno;
    END;
    biasMatdistno := LOOP(b1no, COUNTER <= iterations, CreatBiasBlock(ROWS(LEFT),COUNTER));
    // creat ones vector for calculating bias gradients
    Layout_Cell gen(UNSIGNED4 c, UNSIGNED4 NumRows, REAL8 v) := TRANSFORM
      SELF.x := ((c-1) % NumRows) + 1;
      SELF.y := ((c-1) DIV NumRows) + 1;
      SELF.v := v;
     END;
     onesmap := PBblas.Matrix_Map(m, 1, sizeTable[1].f_b_cols, 1);
     ones := DATASET(m, gen(COUNTER, m, 1.0),DISTRIBUTED);
     onesdist := DMAT.Converted.FromCells(onesmap, ones);
    //functions used
    PBblas.Types.value_t sigmoid(PBblas.Types.value_t v, PBblas.Types.dimension_t r, PBblas.Types.dimension_t c) := 1/(1+exp(-1*v));
    //make parameters
    NumLayers := MAX (net, id);
    //define the Trasnfroms to add and decrease the Numlayers
    PBblas.Types.MUElement Addno (PBblas.Types.MUElement l) := TRANSFORM
      SELF.no := l.no+NumLayers;
      SELF := l;
    END;
    PBblas.Types.MUElement Subno (PBblas.Types.MUElement l) := TRANSFORM
      SELF.no := l.no-NumLayers;
      SELF := l;
    END;
    //creat the parameters to be passed to the main gradient descent loop
    biasVecdistno_added := PROJECT (biasVecdistno,Addno(LEFT));
    param_tobe_passed := weightsdistno + biasVecdistno_added;
    FF(DATASET(PBblas.Types.MUElement) w, DATASET(PBblas.Types.MUElement) b ):= FUNCTION
      w1 := PBblas.MU.From(W, 1); // weight matrix between layer 1 and layer 2 of the neural network
      b1 := PBblas.MU.From(b, 1); //bias entered to the layer 2 of the neural network
      //z2 = w1*X+b1;
      z2 := PBblas.PB_dgemm(FALSE, FALSE,1.0,w1map, W1, dmap, ddist, b1map,b1, 1.0  );
      //a2 = sigmoid (z2);
      a2 := PBblas.Apply2Elements(b1map, z2, sigmoid);
      a2no := PBblas.MU.To(a2,2);

      FF_Step(DATASET(PBblas.Types.MUElement) InputA, INTEGER coun) := FUNCTION
        L := coun+1;
        wL := PBblas.MU.From(w, L); // weight matrix between layer L and layer L+1 of the neural network
        wL_x := net(id=(L+1))[1].value;
        wL_y := net(id=(L))[1].value;;
        bL := PBblas.MU.From(b, L); //bias entered to the layer L+1 of the neural network
        bL_x := net(id=(L+1))[1].value;
        aL := PBblas.MU.From(InputA, L); //output of layer L
        aL_x := net(id=(L))[1].value;;
        wLmap := PBblas.Matrix_Map(wL_x, wL_y, sizeTable[1].f_b_rows, sizeTable[1].f_b_rows);
        bLmap := PBblas.Matrix_Map(bL_x, m, sizeTable[1].f_b_rows, sizeTable[1].f_b_cols);
        aLmap := PBblas.Matrix_Map(aL_x,m,sizeTable[1].f_b_rows,sizeTable[1].f_b_cols);
        //z(L+1) = wL*aL+bL;
        zL_1 := PBblas.PB_dgemm(FALSE, FALSE,1.0,wLmap, wL, aLmap, aL, bLmap,bL, 1.0  );
        //aL_1 = sigmoid (zL_1);
        aL_1 := PBblas.Apply2Elements(bLmap, zL_1, sigmoid);
        aL_1no := PBblas.MU.To(aL_1,L+1);
        RETURN InputA+aL_1no;
      END;//end FF_step
      final_A := LOOP(a2no, COUNTER <= iterations, FF_Step(ROWS(LEFT),COUNTER));
      return final_A;
    END;//end FF
    Delta(DATASET(PBblas.Types.MUElement) w, DATASET(PBblas.Types.MUElement) b, DATASET(PBblas.Types.MUElement) A ):= FUNCTION
      PBblas.Types.value_t siggrad(PBblas.Types.value_t v, PBblas.Types.dimension_t r, PBblas.Types.dimension_t c) := v*(1-v);
      A_end := PBblas.MU.From(A,NumLayers);
      siggrad_A_end := PBblas.Apply2Elements(Ymap, A_end, siggrad);
      a_y := PBblas.PB_daxpy(-1, Ydist, A_end);//-1 * (y-a) = a-y
      Delta_End := PBblas.HadamardProduct(Ymap, a_y, siggrad_A_end);
      Delta_End_no := PBblas.MU.To(Delta_End,NumLayers);
      Delta_Step(DATASET(PBblas.Types.MUElement) InputD, INTEGER coun) := FUNCTION
        L := NumLayers - coun ;
        DL_1 := PBblas.MU.From(InputD, L+1);//Delta for layer L+1:DL_1
        DL_1_x := net(id=(L+1))[1].value;
        DL_1_y := m;
        wL := PBblas.MU.From(w, L); // weight matrix between layer L and layer L+1 of the neural network
        wL_x := net(id=(L+1))[1].value;
        wL_y := net(id=(L))[1].value;
        aL := PBblas.MU.From(A, L);//output of layer L
        aL_x := net(id=(L))[1].value;
        aL_y := m;
        DL_1map := PBblas.Matrix_Map(DL_1_x,m,sizeTable[1].f_b_rows,sizeTable[1].f_b_cols);
        wLmap := PBblas.Matrix_Map(wL_x, wL_y, sizeTable[1].f_b_rows, sizeTable[1].f_b_rows);
        aLmap := PBblas.Matrix_Map(aL_x,m,sizeTable[1].f_b_rows,sizeTable[1].f_b_cols);
        siggrad_aL := PBblas.Apply2Elements(aLmap, aL, siggrad);
        //wLtDL_1=wL(transpose)*DL_1
        wLtDL_1 := PBblas.PB_dgemm (TRUE, FALSE, 1.0, wLmap, wL, DL_1map, DL_1, aLmap);
        //calculated delta = delta_L = wLtDL_1 .* siggrad_aL
        Delta_L := PBblas.HadamardProduct(aLmap, wLtDL_1, siggrad_aL);
        Delta_L_no := PBblas.MU.To(Delta_L,L);
        RETURN InputD+Delta_L_no;
      END;//END Delta_Step
      final_Delta := LOOP(Delta_End_no, COUNTER <= iterations, Delta_Step(ROWS(LEFT),COUNTER));
      RETURN final_Delta;
    END;//END Delta
    WeightGrad(DATASET(PBblas.Types.MUElement) w, DATASET(PBblas.Types.MUElement) A, DATASET(PBblas.Types.MUElement) Del ):= FUNCTION
      //calculate update term for wights (1/m*(DELTAw) + LAMBDA*w)
      //w1_g1=d2*a1'
      D2 := PBblas.MU.From(Del, 2);
      D2_x := net(id=(2))[1].value;
      D2_y := m;
      D2_map := PBblas.Matrix_Map(D2_x,m,sizeTable[1].f_b_rows,sizeTable[1].f_b_cols);
      w1_g1_map := PBblas.Matrix_Map(net(id=(2))[1].value,net(id=(1))[1].value,sizeTable[1].f_b_rows,sizeTable[1].f_b_rows);
      w1_g1 := PBblas.PB_dgemm(FALSE, TRUE,1.0,D2_map, D2, dmap, ddist, w1_g1_map );
      //wight decay term :lambda* w1;
      w1 := PBblas.MU.From(w, 1);
      w1_g2 := PBblas.PB_dscal(LAMBDA, w1);
      //w1_g := 1/m*w1_g1 + w1_g2
      w1_g := PBblas.PB_daxpy(m_1, w1_g1, w1_g2);
      w1_g_no := PBblas.MU.To(w1_g,1);
      WeightGrad_Step(DATASET(PBblas.Types.MUElement) InputWG, INTEGER coun) := FUNCTION
        L := coun+1;
        //calculate update term for wights (1/m*(DELTAw) + LAMBDA*w)
        //w1_g1=d2*a1'
        DL_1 := PBblas.MU.From(Del, L+1);
        DL_1_x := net(id=(L+1))[1].value;
        DL_1_y := m;
        DL_1_map := PBblas.Matrix_Map(DL_1_x,m,sizeTable[1].f_b_rows,sizeTable[1].f_b_cols);
        aL := PBblas.MU.From(A, L);//output of layer L
        aL_x := net(id=(L))[1].value;
        aL_y := m;
        aLmap := PBblas.Matrix_Map(aL_x,m,sizeTable[1].f_b_rows,sizeTable[1].f_b_cols);
        wL_g1_map := PBblas.Matrix_Map(net(id=(L+1))[1].value,net(id=(L))[1].value,sizeTable[1].f_b_rows,sizeTable[1].f_b_rows);
        wL_g1 := PBblas.PB_dgemm(FALSE, TRUE,1.0,DL_1_map, DL_1, aLmap, aL, wL_g1_map );
        //wight decay term :lambda* w1;
        wL := PBblas.MU.From(w, L);
        wL_g2 := PBblas.PB_dscal(LAMBDA, wL);
        //w1_g := 1/m*w1_g1 + w1_g2
        wL_g := PBblas.PB_daxpy(m_1, wL_g1, wL_g2);
        wL_g_no := PBblas.MU.To(wL_g,L);
        RETURN InputWG+wL_g_no;
      END;//WeightGrad_Step
      final_WG := LOOP(w1_g_no, COUNTER <= iterations, WeightGrad_Step(ROWS(LEFT),COUNTER));
      RETURN final_WG;
    END;//END WeightGrad
    BiasGrad (DATASET(PBblas.Types.MUElement) Del ):= FUNCTION
      D2 := PBblas.MU.From(Del, 2);
      D2_x := net(id=(2))[1].value;
      D2_y := m;
      D2_map := PBblas.Matrix_Map(D2_x,m,sizeTable[1].f_b_rows,sizeTable[1].f_b_cols);
      b1_g_map := PBblas.Matrix_Map(D2_x,1,sizeTable[1].f_b_rows,1);
      b1_g_tmp := PBblas.PB_dgemm(FALSE, FALSE,1.0,D2_map, D2, onesmap, onesdist, b1_g_map);
      b1_g := PBblas.PB_dscal(m_1, b1_g_tmp);
      b1_g_no := PBblas.MU.To(b1_g,1);
      BiasGrad_Step(DATASET(PBblas.Types.MUElement) InputBG, INTEGER coun) := FUNCTION
        L := coun +1 ;
        DL_1 := PBblas.MU.From(Del, L+1);
        DL_1_x := net(id=(L+1))[1].value;
        DL_1_y := m;
        DL_1_map := PBblas.Matrix_Map(DL_1_x,m,sizeTable[1].f_b_rows,sizeTable[1].f_b_cols);
        bL_g_map := PBblas.Matrix_Map(DL_1_x,1,sizeTable[1].f_b_rows,1);
        bL_g_tmp := PBblas.PB_dgemm(FALSE, FALSE,1.0,DL_1_map, DL_1, onesmap, onesdist, bL_g_map);
        bL_g := PBblas.PB_dscal(m_1, bL_g_tmp);
        bL_g_no := PBblas.MU.To(bL_g,L);
        RETURN InputBG+bL_g_no;
      END;//END BiasGrad_Step
      final_bg := LOOP(b1_g_no, COUNTER <= iterations, BiasGrad_Step(ROWS(LEFT),COUNTER));
      RETURN final_bg;
    END;//End BiasGrad
    GradDesUpdate (DATASET(PBblas.Types.MUElement) tobeUpdated, DATASET(PBblas.Types.MUElement) GradDesTerm ):= FUNCTION
      tmp1 := PBblas.MU.From(tobeUpdated, 1);
      gterm1 := PBblas.MU.From(GradDesTerm, 1);
      tmp1_updated := PBblas.PB_daxpy(-1, PBblas.PB_dscal(ALPHA, gterm1), tmp1);
      tmp1_updated_no := PBblas.MU.To(tmp1_updated,1);
      GradDesUpdate_Step(DATASET(PBblas.Types.MUElement) Inputtmp, INTEGER coun) := FUNCTION
        L := coun + 1;
        tmpL := PBblas.MU.From(tobeUpdated, L);
        gtermL := PBblas.MU.From(GradDesTerm, L);
        tmpL_updated := PBblas.PB_daxpy(-1, PBblas.PB_dscal(ALPHA, gtermL), tmpL);
        tmpL_updated_no := PBblas.MU.To(tmpL_updated,L);
        RETURN Inputtmp+tmpL_updated_no;
      END;//End GradDesUpdate_Step
      final_updated := LOOP(tmp1_updated_no,  iterations, GradDesUpdate_Step(ROWS(LEFT),COUNTER));
      RETURN final_updated;
    END;//End GradDesUpdate
    //main Loop ieteration in back propagation algorithm that does the gradient descent and weight and bias updates
    GradDesLoop (DATASET(PBblas.Types.MUElement) Intparams ):= FUNCTION
      GradDesLoop_Step (DATASET(PBblas.Types.MUElement) Inputparams) := FUNCTION
        w_in := Inputparams (no<NumLayers);//input weight parameter in PBblas.Types.MUElement format
        b_in_tmp := Inputparams (no>NumLayers);
        b_in := PROJECT (b_in_tmp,Subno(LEFT));//input bias parameter in PBblas.Types.MUElement format
        //creat matrix of each bias vector by repeating each bias vector in m columns (to make the following calculations easier)
        b_in1 := PBblas.MU.From(b_in,1);
        b_in1_mat := ML.DMat.Converted.FromPart2Elm (b_in1);
        b_in1_mat_x := Mat.Has(b_in1_mat).Stats.Xmax;
        b_in1_mat_rep := Mat.Repmat(b_in1_mat, 1, m); // Bias vector is repeated in m columns to make the future calculations easier
        b_in1map := PBblas.Matrix_Map(b_in1_mat_x, m, sizeTable[1].f_b_rows, sizeTable[1].f_b_cols);
        b_in1dist := DMAT.Converted.FromElement(b_in1_mat_rep,b_in1map);
        b_in1no := PBblas.MU.TO(b_in1dist,1);//first bias vector is converted to a matrix, now convert the rest of bias vectors into teh matrix
        //loopbody to creat the rest of bias matrix blocks
        Creat_BiasBlock(DATASET(PBblas.Types.MUElement) inputb, INTEGER coun) := FUNCTION
          L := coun+1; //creat the weight block for weight between layers L and L+1
          b_inL := PBblas.MU.From(b_in,L);
          b_inL_mat := ML.DMat.Converted.FromPart2Elm (b_inL);
          b_inL_mat_x := Mat.Has(b_inL_mat).Stats.Xmax;
          b_inL_mat_rep := Mat.Repmat(b_inL_mat, 1, m); // Bias vector is repeated in m columns to make the future calculations easier
          b_inLmap := PBblas.Matrix_Map(b_inL_mat_x, m, sizeTable[1].f_b_rows, sizeTable[1].f_b_cols);
          b_inLdist := DMAT.Converted.FromElement(b_inL_mat_rep, b_inLmap);
          b_inLno := PBblas.MU.TO(b_inLdist,L);
          //RETURN inputb+bno;
          RETURN b_inLno+inputb;
        END;
        //b_in_rep := LOOP(b_in1no, COUNTER <= iterations, Creat_BiasBlock(ROWS(LEFT),COUNTER));//matrices of converted bias vectors
        b_in_rep := LOOP(b_in1no, iterations, Creat_BiasBlock(ROWS(LEFT),COUNTER));//matrices of converted bias vectors
        //w_in , b_in and b_in_repno are three block matrices we are going to work with
        //w_in : weight matrices
        //b_in : bias matrices
        //b_in_rep : each bias vector is repeated m columns to make the calculations easier
        //in all the calculations and defined functions (FF, DELTA) the repeated bias matrices are used, the only time
        //that the bias vector is used is when we update the bias in "GradDesUpdate".
        //1- apply the Feed Forward pass
        A_ffpass :=  FF (w_in,b_in_rep);
        //2-apply the back propagation step to update the parameters
        D_delta := DELTA (w_in, b_in_rep, A_ffpass);
        Weight_GD := WeightGrad(w_in, A_ffpass,  D_delta);
        Bias_GD := BiasGrad (D_delta);
        NewWeight := GradDesUpdate (w_in, Weight_GD);
        NewBias := GradDesUpdate (b_in, Bias_GD);
        NewBias_added := PROJECT (NewBias,Addno(LEFT));
        Updated_Params := NewWeight + NewBias_added;
        RETURN Updated_Params;
      END;//END GradDesLoop_Step
      Final_Updated_Params := LOOP(Intparams, COUNTER <= MaxIter, GradDesLoop_Step(ROWS(LEFT)));
      RETURN Final_Updated_Params;
    END;//END GradDesLoop
    NNparams := GradDesLoop (param_tobe_passed);// NNparams is in PBblas.Types.MUElement format
    //convert NNparams to Numeric Field format
    nnparam1 := PBblas.MU.From(NNparams,1);
    nnparam1_mat := DMat.Converted.FromPart2Elm (nnparam1);
    nnparam1_mat_no := Mat.MU.TO(nnparam1_mat,1);
    NL := MAX (net, id);
    Mu_convert(DATASET(Mat.Types.MUElement) inputMU, INTEGER coun) := FUNCTION
      L := IF(coun < NL-1, coun+1, coun+2);
      nnparamL := PBblas.MU.From(NNparams,L);
      nnparamL_mat := DMat.Converted.FromPart2Elm (nnparamL);
      nnparamL_mat_no := Mat.MU.TO(nnparamL_mat,L);
      RETURN inputMU+nnparamL_mat_no;
    END;
    NNparams_MUE := LOOP(nnparam1_mat_no, 2*NL-3, Mu_convert(ROWS(LEFT),COUNTER));
    AppendID(NNparams_MUE, id, NNparams_MUE_id);
    ToField (NNparams_MUE_id, NNparams_MUE_out, id, 'x,y,value,no');
    EXPORT Mod := NNparams_MUE_out;//mod is in NumericField format
    //EXPORT alaki := biasVecdistno_added;
  END;// END BP
  EXPORT NNLearn(DATASET(Types.NumericField) Indep, DATASET(Types.NumericField) Dep,DATASET(Mat.Types.MUElement) IntW, DATASET(Mat.Types.MUElement) Intb, REAL8 LAMBDA=0.001, REAL8 ALPHA=0.1, UNSIGNED2 MaxIter=100) := BP(Indep,Dep, IntW,  Intb, LAMBDA,  ALPHA,  MaxIter).mod;
  //this function applies the feed forward pass to the input dataset (Indep) based on the input neural network model (Learntmod)
    EXPORT NNOutput(DATASET(Types.NumericField) Indep,DATASET(Types.NumericField) Learntmod) :=FUNCTION
      //used fucntion
      PBblas.Types.value_t sigmoid(PBblas.Types.value_t v, PBblas.Types.dimension_t r, PBblas.Types.dimension_t c) := 1/(1+exp(-1*v));
      dt := Types.ToMatrix (Indep);
      //dTmp := Mat.InsertColumn(dt,1,1.0); // add the intercept column
      dTmp := dt;
      d := Mat.Trans(dTmp); //in the entire of the calculations we work with the d matrix that each sample is presented in one column
      m := MAX (d, d.y); //number of samples
      m_1 := 1/m;
      sizeRec := RECORD
        PBblas.Types.dimension_t m_rows;
        PBblas.Types.dimension_t m_cols;
        PBblas.Types.dimension_t f_b_rows;
        PBblas.Types.dimension_t f_b_cols;
      END;
     //Map for Matrix d.
      havemaxrow := maxrows > 0;
      havemaxcol := maxcols > 0;
      havemaxrowcol := havemaxrow and havemaxcol;
      dstats := Mat.Has(d).Stats;
      d_n := dstats.XMax;
      d_m := dstats.YMax;
      NL := MAX(net,id);
      iterations := NL-2;
      output_num := net(id=NL)[1].value;
      derivemap := IF(havemaxrowcol, PBblas.AutoBVMap(d_n, d_m,prows,pcols,maxrows, maxcols),
                     IF(havemaxrow, PBblas.AutoBVMap(d_n, d_m,prows,pcols,maxrows),
                        IF(havemaxcol, PBblas.AutoBVMap(d_n, d_m,prows,pcols,,maxcols),
                        PBblas.AutoBVMap(d_n, d_m,prows,pcols))));
      SHARED sizeTable := DATASET([{derivemap.matrix_rows,derivemap.matrix_cols,derivemap.part_rows(1),derivemap.part_cols(1)}], sizeRec);
      //Create block matrix d
      dmap := PBblas.Matrix_Map(sizeTable[1].m_rows,sizeTable[1].m_cols,sizeTable[1].f_b_rows,sizeTable[1].f_b_cols);
      ddist := DMAT.Converted.FromElement(d,dmap);
      //Extract Weights and Bias
      W_mat := ExtractWeights (Learntmod);
      B_mat := ExtractBias (Learntmod);
      //creat w1 partion block matrix
      w1_mat := Mat.MU.From(W_mat,1);
      w1_mat_x := Mat.Has(w1_mat).Stats.Xmax;
      w1_mat_y := Mat.Has(w1_mat).Stats.Ymax;
      w1map := PBblas.Matrix_Map(w1_mat_x, w1_mat_y, sizeTable[1].f_b_rows, sizeTable[1].f_b_rows);
      w1dist := DMAT.Converted.FromElement(w1_mat,w1map);
      //repeat b1 vector in m columsn and the creat the partion block matrix
      b1_mat := Mat.MU.From(B_mat,1);
      b1_mat_x := Mat.Has(b1_mat).Stats.Xmax;
      b1_mat_rep := Mat.Repmat(b1_mat, 1, m); // Bias vector is repeated in m columns to make the future calculations easier
      b1map := PBblas.Matrix_Map(b1_mat_x, m, sizeTable[1].f_b_rows, sizeTable[1].f_b_cols);
      b1dist := DMAT.Converted.FromElement(b1_mat_rep,b1map);
      //calculate a2 (output from layer 2)
      //z2 = w1*X+b1;
      z2 := PBblas.PB_dgemm(FALSE, FALSE,1.0,w1map, w1dist, dmap, ddist, b1map, b1dist, 1.0);
      //a2 = sigmoid (z2);
      a2 := PBblas.Apply2Elements(b1map, z2, sigmoid);
      FF_Step(DATASET(Layout_Part) A, INTEGER coun) := FUNCTION
        L := coun + 1;
        aL := A; //output of layer L
        aL_x := net(id=L)[1].value;;
        aLmap := PBblas.Matrix_Map(aL_x,m,sizeTable[1].f_b_rows,sizeTable[1].f_b_cols);
        //creat wL partion block matrix
        wL_mat := Mat.MU.From(W_mat,L);
        wL_mat_x := Mat.Has(wL_mat).Stats.Xmax;
        wL_mat_y := Mat.Has(wL_mat).Stats.Ymax;
        wLmap := PBblas.Matrix_Map(wL_mat_x, wL_mat_y, sizeTable[1].f_b_rows, sizeTable[1].f_b_rows);
        wLdist := DMAT.Converted.FromElement(wL_mat,wLmap);
        //repeat b1 vector in m columsn and the creat the partion block matrix
        bL_mat := Mat.MU.From(B_mat,L);
        bL_mat_x := Mat.Has(bL_mat).Stats.Xmax;
        bL_mat_rep := Mat.Repmat(bL_mat, 1, m); // Bias vector is repeated in m columns to make the future calculations easier
        bLmap := PBblas.Matrix_Map(bL_mat_x, m, sizeTable[1].f_b_rows, sizeTable[1].f_b_cols);
        bLdist := DMAT.Converted.FromElement(bL_mat_rep,bLmap);
        //calculate a(L+1) (output from layer L)
        //z(L+1) = wL*X+bL;
        zL_1 := PBblas.PB_dgemm(FALSE, FALSE,1.0, wLmap, wLdist, aLmap, aL, bLmap, bLdist, 1.0);
        //aL_1 = sigmoid (zL_1);
        aL_1 := PBblas.Apply2Elements(bLmap, zL_1, sigmoid);
        RETURN aL_1;
      END;
      final_A := LOOP(a2, COUNTER <= iterations, FF_Step(ROWS(LEFT),COUNTER));
      final_A_mat := DMat.Converted.FromPart2Elm(final_A);
      Types.l_result tr(Mat.Types.Element le) := TRANSFORM
        SELF.value := le.x;
        SELF.id := le.y;
        SELF.number := 1; //number of class
        SELF.conf := le.value;
      END;
      RETURN PROJECT (Final_A_mat, tr(LEFT));
    END;// END NNOutput
    EXPORT NNClassify(DATASET(Types.NumericField) Indep,DATASET(Types.NumericField) Learntmod) := FUNCTION
      Dist := NNOutput(Indep, Learntmod);
      numrow := MAX (Dist,Dist.value);//number of nodes in the last layer of the neural network
      // d_Dist := DISTRIBUTE (Dist, id);
      // S:= SORT(d_Dist,id,conf,LOCAL);
      d_grpd := GROUP(Dist, id, ALL);
      GS := SORT(d_grpd, conf);
      S := GROUP(GS); // Ungrouped GS
      SeqRec := RECORD
      l_result;
      INTEGER8 Sequence := 0;
      END;
      //add seq field to S
      SeqRec AddS (S l, INTEGER c) := TRANSFORM
        SELF.Sequence := c%numrow;
        SELF := l;
      END;
      Sseq := PROJECT(S, AddS(LEFT,COUNTER),LOCAL);
      classified := Sseq (Sseq.Sequence=0);
      RETURN PROJECT(classified,l_result,LOCAL);
    END; // END NNClassify
  
END;//END NeuralNetworks