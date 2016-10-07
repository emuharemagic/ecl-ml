/* The Implementation is based on the paper 
'FINDING STRUCTURE WITH RANDOMNESS: PROBABILISTIC ALGORITHMS FOR CONSTRUCTING
APPROXIMATE MATRIX DECOMPOSITIONS'
By N. HALKO, P. G. MARTINSSON, AND J. A. TROPP
*/

IMPORT ML.Mat AS Mat;
IMPORT ML.LSA.lsa AS LSA;
IMPORT ML;
IMPORT ML.Distribution AS Dist;
IMPORT ML.DMat AS DMat;
IMPORT PBblas.MU AS MU;
IMPORT PBblas;

Part := PBblas.Types.Layout_Part;
IMatrix_Map := PBblas.IMatrix_Map;
ToElm := DMat.Converted.FromPart2Elm;

EXPORT RandomisedSVD := MODULE
  EXPORT RangeFinder(IMatrix_Map a_map, DATASET(Part) A, UNSIGNED4 size) := FUNCTION
    nd := Dist.normal(0, 1, 10000);
    data_gen := Dist.GenData(a_map.matrix_cols * size, nd);
    dim := a_map.matrix_cols;
    Mat.Types.Element toMat(ML.Types.NumericField l) := TRANSFORM
      SELF.x := ((l.id - 1) % dim) + 1;
      SELF.y := ((l.id - 1) DIV dim) + 1 ;
      SELF.value := l.value;
    END;
    g_map := PBblas.Matrix_map(dim, size, a_map.part_cols(1), size);  // G = n x k
    gauss_mat := DMat.Converted.FromElement(PROJECT(data_gen, toMat(LEFT)), g_map);
    y0_map := PBblas.Matrix_map(a_map.matrix_rows, g_map.matrix_cols, a_map.part_rows(1), g_map.part_cols(1)); // Y0 = m x k
    Y0 := DMat.Mul(a_map, A, g_map, gauss_mat, y0_map);
    Q := ML.LSA.DenseSVD.QR(y0_map, Y0).QComp;
    RETURN Q;
  END;
  
  SHARED mat_type := ENUM( U = 1, S = 2, V = 3 );
  EXPORT RandomisedSVD(IMatrix_Map a_map, DATASET(Part) dA, UNSIGNED4 r) := FUNCTION
    UNSIGNED4 size := IF(r + 10 < a_map.matrix_cols, r + 10, a_map.matrix_cols);
    Q := RangeFinder(a_map, dA, size);  // Q = m x k
    aT_map := DMat.Trans.TranMap(a_map);
    aTa_map := PBblas.Matrix_Map(aT_map.matrix_rows, a_map.matrix_cols, aT_map.part_rows(1), a_map.part_cols(1));
    g_map := PBblas.Matrix_map(a_map.matrix_cols, size, a_map.part_cols(1), size);  // G = n x k
    q_map := PBblas.Matrix_map(a_map.matrix_rows, g_map.matrix_cols, a_map.part_rows(1), g_map.part_cols(1)); // Y0 = m x k
    qT_map := DMat.Trans.TranMap(q_map);
    b_map := PBblas.Matrix_map(qT_map.matrix_rows, a_map.matrix_cols, qT_map.part_rows(1), a_map.part_cols(1)); // B = k x n
    B := PBBlas.PB_dgemm(TRUE, FALSE, 1.0, q_map, Q, a_map, dA, b_map);
    decomp := ML.LSA.DenseSVD.SVD(b_map, B).MLN();
    QU := DMat.Mul(q_map, Q, b_map, MU.From(decomp, mat_type.U), a_map); // U = k x n, QU = m x n
    S := MU.From(decomp, mat_type.S); // S = n x n
    V := MU.From(decomp, mat_type.V); // V = n x n
    RETURN LSA.ReduceRank(MU.To(QU, mat_type.U) + MU.To(S, mat_type.S) + MU.To(V, mat_type.V), r);    
  END;

END;