IMPORT $ AS DenseSVD;
IMPORT ML.DMat AS DMat;
IMPORT PBblas;
IMPORT PBblas.Types AS Types;

Part := Types.Layout_Part;
IMatrix_Map := PBblas.IMatrix_Map;
value_t := PBblas.Types.value_t;
dimension_t := PBblas.Types.dimension_t;
partition_t := PBblas.Types.partition_t;
EigC := DenseSVD.EigC;
Eig := DenseSVD.Eig;

EXPORT SVD(IMatrix_map a_map, DATASET(Part) A) := MODULE
    SHARED mat_type := ENUM( U = 1, S = 2, V = 3 );
    SHARED value_t Sqroot(value_t v, dimension_t r, dimension_t c) := SQRT(v);
    SHARED value_t Reciprocal(value_t v, dimension_t r, dimension_t c) := IF(v <> 0, 1.0/v, 0.0);
    
    EXPORT MGN() := FUNCTION
      aT_map := DMat.Trans.TranMap(a_map);
      aTa_map := PBblas.Matrix_Map(aT_map.matrix_rows, a_map.matrix_cols, aT_map.part_rows(1), a_map.part_cols(1));
      AtA := PBBlas.PB_dgemm(TRUE, FALSE, 1.0, a_map, A, a_map, A, aTa_map);
      V := IF(COUNT(AtA) = 1, EigC(aTa_map, AtA).vectors, Eig(aTa_map, AtA).vectors);
      S1 := IF(COUNT(AtA) = 1, EigC(aTa_map, AtA).valuesM, Eig(aTa_map, AtA).valuesM);
      S := PBblas.Apply2Elements(aTa_map, S1, Sqroot);
      InvS := PBblas.Apply2Elements(aTa_map, S, Reciprocal);
      U := DMat.Mul(a_map, DMat.Mul(a_map, A, aTa_map, V, a_map), aTa_map, InvS, a_map);
      RETURN PBblas.MU.To(U, mat_type.U) + PBblas.MU.To(S, mat_type.S) + PBblas.MU.To(V, mat_type.V);
    END;
    
    EXPORT MLN() := FUNCTION
      aT_map := DMat.Trans.TranMap(a_map);
      aaT_map := PBblas.Matrix_Map(a_map.matrix_rows, aT_map.matrix_cols, a_map.part_rows(1), aT_map.part_cols(1));
      AAt := PBBlas.PB_dgemm(FALSE, TRUE, 1.0, a_map, A, a_map, A, aaT_map);
      U := IF(COUNT(AAt) = 1, EigC(aaT_map, AAt).vectors, Eig(aaT_map, AAt).vectors);
      S1 := IF(COUNT(AAt) = 1, EigC(aaT_map, AAt).valuesM, Eig(aaT_map, AAt).valuesM);
      S := PBblas.Apply2Elements(aaT_map, S1, Sqroot);
      InvS := PBblas.Apply2Elements(aaT_map, S, Reciprocal);
      V := DMat.Mul(aT_map, PBblas.PB_dgemm(TRUE, FALSE, 1.0, a_map, A, aaT_map, U, aT_map), aaT_map, InvS, aT_map);
      RETURN PBblas.MU.To(U, mat_type.U) + PBblas.MU.To(S, mat_type.S) + PBblas.MU.To(V, mat_type.V);    
    END;
    
    EXPORT decomp := IF(a_map.matrix_rows > a_map.matrix_cols, MGN(), MLN());
END;