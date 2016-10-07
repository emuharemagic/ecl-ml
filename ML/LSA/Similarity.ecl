IMPORT ML.Mat.Types AS Types;
IMPORT ML.Mat AS Mat;

EXPORT Similarity := MODULE
  
  EXPORT CosineSim(DATASET(Types.Element) D, DATASET(Types.Element) Q) := FUNCTION
    qd_prod := Mat.Mul(D, Mat.Trans(Q)); // D.Q
    q_norm := Mat.Vec.Norm(Mat.Vec.FromRow(Q, 1)); // SQRT(Q.Q)

    d0_norm := Mat.Each.Mul(D, D);
    NormRec := RECORD
      d0_norm.x;
      Mat.Types.t_Index y := 1;
      Mat.Types.t_value value := SUM(GROUP, d0_norm.value);
    END;
    d_norm := Mat.Each.Sqrt(TABLE(d0_norm, NormRec, x)); //SQRT(D.D)

    // Cosine-Sim(Q, D) = (Q.D) / (SQRT(Q.Q)*SQRT(D.D))
    RETURN Mat.Each.Mul(Mat.Scale(qd_prod, 1.0/q_norm), Mat.Each.Reciprocal(d_norm));
  END;
  
  EXPORT DotProductSim(DATASET(Types.Element) D, DATASET(Types.Element) Q) := FUNCTION
    RETURN Mat.Mul(D, Mat.Trans(Q));
  END;

END;