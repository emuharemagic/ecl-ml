IMPORT $ AS DenseSVD;
IMPORT Config FROM ML;
IMPORT ML.DMat AS DMat;
IMPORT PBblas;
IMPORT PBblas.Types AS Types;
IMPORT PBblas.MU AS MU;

Part := Types.Layout_Part;
IMatrix_Map := PBblas.IMatrix_Map;

EXPORT eig(IMatrix_Map a_map, DATASET(Part) A, UNSIGNED4 iter=200) := MODULE
  SHARED eig_comp := ENUM ( T = 1, Q = 2, T0 = 3 );
  EXPORT DATASET(Part) QRalgorithm() := FUNCTION
      
      QR0 := DenseSVD.QR(a_map, A);
      Q0 := QR0.QComp;  //Q0
      R0 := QR0.RComp;  //R0
      T0 := DMat.Mul(a_map, R0, a_map, Q0, a_map); //T0 = R0*Q0
            
    CheckConv(DATASET(Types.MUElement) ds, UNSIGNED4 k) := FUNCTION
      Tnew := MU.From(ds, eig_comp.T);
      Told := MU.From(ds, eig_comp.T0);
      diff := PBblas.PB_daxpy(-1.0, Tnew, Told);
      bConv := DenseSVD.Helpers.NormDiag(a_map, diff)/DenseSVD.Helpers.NormDiag(a_map, Told);
      RETURN bConv > 0.0001;
    END;
      
    loopBody(DATASET(Types.MUElement) ds, UNSIGNED4 k) := FUNCTION

      T := MU.From(ds, eig_comp.T);	 // Tk-1
      Q := MU.From(ds, eig_comp.Q);  // Qk-1             
            
      QR1 := DenseSVD.QR(a_map, T);
      QComp := QR1.QComp; // Qc * Rc = Tk-1
      Q1 := DMat.Mul(a_map, Q, a_map, QComp, a_map); //Qk = Qk-1 * Qc
      RComp := QR1.RComp;
      T1 := DMat.Mul(a_map, RComp, a_map, QComp, a_map); // Tk = Rc * Qc
      
    RETURN MU.To(T1, eig_comp.T)+MU.To(Q1, eig_comp.Q)+MU.To(T, eig_comp.T0);
    END;
    
    RETURN LOOP(Mu.To(T0, eig_comp.T)+Mu.To(Q0, eig_comp.Q)+MU.To(T0, eig_comp.T0), COUNTER=1 OR ((COUNTER <= iter) AND CheckConv(ROWS(LEFT), COUNTER))
                    , loopBody(ROWS(LEFT), COUNTER));
  END;

  EXPORT valuesM := PBblas.HadamardProduct(a_map, DMat.Identity(a_map), MU.From(QRalgorithm(), eig_comp.T));
  EXPORT vectors := MU.From(QRalgorithm(), eig_comp.Q);

END;