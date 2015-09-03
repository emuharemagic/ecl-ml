//Convert a vector into a diagonal matrix.  The typical
//notation is D = diag(V).
IMPORT PBblas;
IMPORT PBblas.IMatrix_Map;
IMPORT PBblas.Types;
IMPORT PBblas.Constants;
IMPORT PBblas.Block;
//Alias entries for convenience
Part := Types.Layout_Part;
value_t := Types.value_t;
dim_t := PBblas.Types.dimension_t;

EXPORT DATASET(Part)
       Vector2Diag(IMatrix_Map x_map, DATASET(Part) X, IMatrix_Map d_map):= FUNCTION
  //maps must be compatible
  dim_check(UNSIGNED v_dim, UNSIGNED d_dim) := v_dim=1 OR v_dim=d_dim;
  checkedX := ASSERT(X,
                     ASSERT(dim_check(x_map.matrix_cols, d_map.matrix_cols),
                            PBblas.Constants.Dimension_Incompat, FAIL),
                     ASSERT(dim_check(x_map.matrix_rows, d_map.matrix_rows),
                            PBblas.Constants.Dimension_Incompat, FAIL),
                     ASSERT(dim_check(x_map.row_blocks, d_map.row_blocks),
                            PBblas.Constants.Dimension_Incompat, FAIL),
                     ASSERT(dim_check(x_map.col_blocks, d_map.col_blocks),
                            PBblas.Constants.Dimension_Incompat, FAIL)
                     );
  Part makeDiag(Part base) := TRANSFORM
    part_rows := MAX(base.part_rows, base.part_cols);
    first_row := MAX(base.first_row, base.first_col);
    block_row := MAX(base.block_row, base.block_col);
    partition_id := d_map.assigned_part(block_row, block_row);
    SELF.node_id := d_map.assigned_node(partition_id);
    SELF.partition_id := partition_id;
    SELF.block_row := block_row;
    SELF.block_col := block_row;
    SELF.first_row := first_row;
    SELF.part_rows := part_rows;
    SELF.first_col := first_row;
    SELF.part_cols := part_rows;
    SELF.mat_part := Block.make_diag(part_rows, 1.0, base.mat_part);
  END;
  RETURN PROJECT(checkedX, makeDiag(LEFT));
END;
