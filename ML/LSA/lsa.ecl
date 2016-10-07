IMPORT ML;
IMPORT ML.Mat AS Mat;
IMPORT ML.DMat AS DMat;
IMPORT PBblas;

value_t := PBblas.Types.value_t;
dimension_t := PBblas.Types.dimension_t;
partition_t := PBblas.Types.partition_t;
ToElm := DMat.Converted.FromPart2Elm;
FromElm := DMat.Converted.FromElement;
Part := PBblas.Types.Layout_Part;
IMatrix_Map := PBblas.IMatrix_Map;

EXPORT lsa := MODULE

  EXPORT mat_type := ENUM( U = 1, S = 2, V = 3 );
  EXPORT ComputeSVD(IMatrix_Map a_map, DATASET(Part) A) := FUNCTION
    RETURN ML.LSA.DenseSVD.SVD(a_map, A).decomp;
  END;
  
  EXPORT ReduceRank(DATASET(PBblas.Types.MUElement) decomp, UNSIGNED4 r) := FUNCTION
    U := ToElm(PBblas.MU.From(decomp, mat_type.U));
    S := ToElm(PBblas.MU.From(decomp, mat_type.S));
    V := ToElm(PBblas.MU.From(decomp, mat_type.V));
    F := SET(TOPN(S, r, -value), y);
    Ur := U(y in F);
    Sr := S(y in F, x=y);
    Vr := V(y in F);
    RETURN Mat.MU.To(Ur, mat_type.U) + Mat.MU.To(Sr, mat_type.S) + Mat.MU.To(Vr, mat_type.V);
  END;
  
  EXPORT StandardSVD(IMatrix_map a_map, DATASET(Part) A, UNSIGNED4 r) := FUNCTION
    decomp := ComputeSVD(a_map, A);
    RETURN ReduceRank(decomp, r);  
  END;
  
  EXPORT ComputeQueryVectors(DATASET(Mat.Types.MUElement) decomp, IMatrix_map q_map, DATASET(Part) Q) := FUNCTION
    U := Mat.MU.From(decomp, mat_type.U);
    S := Mat.Each.Reciprocal(Mat.MU.From(decomp, mat_type.S));
    dims := [MAX(U, x), MAX(U, y), MAX(S, x), MAX(S, y)];
    qT_map := DMat.Trans.TranMap(q_map);
    u_map := PBblas.Matrix_map(dims[1], dims[2], qT_map.part_cols(1), dims[2]);
    s_map := PBblas.Matrix_map(dims[3], dims[4], u_map.part_cols(1), dims[4]);
    nk_map := PBBlas.Matrix_Map(q_map.matrix_cols, u_map.matrix_cols, q_map.part_cols(1), u_map.part_cols(1));
    dU := FromElm(U, u_map);
    dS := FromElm(S, s_map);
    US := PBBlas.PB_dgemm(FALSE, FALSE, 1.0, u_map, dU, s_map, dS, u_map);
    TQUS := PBBlas.PB_dgemm(TRUE, FALSE, 1.0, q_map, Q, u_map, US, nk_map);
    QVec := ToElm(TQUS);
    RETURN QVec;
  END;
  
  EXPORT GetDocVectors(DATASET(Mat.Types.MUElement) decomp) := FUNCTION
    RETURN Mat.MU.From(decomp, mat_type.V);
  END;
  
  EXPORT FoldIn(DATASET(Mat.Types.MUElement) decomp, IMatrix_map q_map, DATASET(Part) NewDocs) := FUNCTION
    D := ComputeQueryVectors(decomp, q_map, NewDocs);
    CurrentV := Mat.MU.From(decomp, mat_type.V);
    max_rows := Mat.Has(CurrentV).Stats.XMax;
    newD := PROJECT(D, TRANSFORM(Mat.Types.Element, SELF.x := LEFT.x + max_rows; SELF := LEFT));
    NewV := CurrentV + newD;
    RETURN decomp(no=mat_type.U) + decomp(no=mat_type.S) + Mat.MU.To(NewV, mat_type.V);
  END;
  
END;