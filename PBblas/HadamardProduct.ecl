//Element-wise multiplication of X .* Y
//PBlas implementation of Mat.Each.Mul
IMPORT PBblas;
IMPORT PBblas.IMatrix_Map;
IMPORT PBblas.Types;
IMPORT PBblas.Constants;
IMPORT PBblas.BLAS;
IMPORT std.system.Thorlib;
Dimension_IncompatZ := Constants.Dimension_IncompatZ;
Dimension_Incompat  := Constants.Dimension_Incompat;
//Alias entries for convenience
Part := Types.Layout_Part;
value_t := Types.value_t;

//x_map - a map that must be the same for both X and Y datasets
//X - dataset in PBblas format
//Y - dataset in PBblas format

EXPORT DATASET(Part)
      HadamardProduct(IMatrix_Map x_map, DATASET(Part) X, DATASET(Part) Y) := FUNCTION
  Product(PBblas.Types.value_t val1, PBblas.Types.value_t val2) := val1 * val2;
  
  Elem := {value_t v};  //short-cut record def
  
  x_check := ASSERT(X, node_id=Thorlib.node(), Constants.Distribution_Error, FAIL);
  y_check := ASSERT(Y, node_id=Thorlib.node(), Constants.Distribution_Error, FAIL);
  
  Part mulPart(Part xrec, Part yrec) := TRANSFORM
    haveX := IF(xrec.part_cols=0, FALSE, TRUE);
    haveY := IF(yrec.part_cols=0, FALSE, TRUE);
    part_cols := IF(haveX, xrec.part_cols, yrec.part_cols);
    part_rows := IF(haveX, xrec.part_rows, yrec.part_rows);
    block_cols:= IF(NOT haveY OR part_cols=yrec.part_cols,
                    part_cols,
                    FAIL(UNSIGNED4, Dimension_IncompatZ, Dimension_Incompat));
    block_rows:= IF(NOT haveY OR part_rows=yrec.part_rows,
                    part_rows,
                    FAIL(UNSIGNED4, Dimension_IncompatZ, Dimension_Incompat));
  
  elemsX := DATASET(xrec.mat_part, Elem);
  elemsY := DATASET(yrec.mat_part, Elem);
    
  new_elems := COMBINE(elemsX, elemsY, TRANSFORM(Elem, SELF.v := Product(LEFT.v,RIGHT.v)));

  SELF.mat_part := MAP(haveX AND haveY =>  SET(new_elems, v),
                       haveX           => PBblas.MakeR8Set(xrec.part_rows, xrec.part_cols, xrec.first_row, xrec.first_col,
                                          DATASET([], PBblas.Types.Layout_Cell), 0, 0.0),
                       PBblas.MakeR8Set(yrec.part_rows, yrec.part_cols, yrec.first_row, yrec.first_col,
                            DATASET([], PBblas.Types.Layout_Cell), 0, 0.0));
    SELF := xrec;
  
  END;
  rs := JOIN(x_check, y_check, LEFT.partition_id=RIGHT.partition_id, mulPart(LEFT,RIGHT), FULL OUTER, LOCAL);
  RETURN rs;
END;