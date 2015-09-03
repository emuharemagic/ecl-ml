// Implements result <- alpha*op(A)op(B) + beta*C.  op is No Transpose or Transpose.
//Result has same matrix map as C.
IMPORT PBblas;
IMPORT PBblas.Types;
IMPORT PBblas.IMatrix_Map;
IMPORT PBBlas.BLAS;
Layout_Part := Types.Layout_Part;
Layout_Target := Types.Layout_Target;
value_t := Types.value_t;
emptyC := DATASET([], Layout_Part);
SET OF value_t empty_array := [];

EXPORT PB_dgemm(BOOLEAN transposeA, BOOLEAN transposeB, value_t alpha,
                IMatrix_Map map_a, DATASET(Layout_Part) A_in,
                IMatrix_Map map_b, DATASET(Layout_Part) B_in,
                IMatrix_Map map_c, DATASET(Layout_Part) C=emptyC,
                value_t beta=0.0) := FUNCTION
// First check maps for compatability.  Normalize for transpose operations.
  a_matrix_rows := IF(transposeA, map_a.matrix_cols, map_a.matrix_rows);
  a_matrix_cols := IF(transposeA, map_a.matrix_rows, map_a.matrix_cols);
  a_row_blocks  := IF(transposeA, map_a.col_blocks,  map_a.row_blocks);
  a_col_blocks  := IF(transposeA, map_a.row_blocks,  map_a.col_blocks);
  b_matrix_rows := IF(transposeB, map_b.matrix_cols, map_b.matrix_rows);
  b_matrix_cols := IF(transposeB, map_b.matrix_rows, map_b.matrix_cols);
  b_row_blocks  := IF(transposeB, map_b.col_blocks,  map_b.row_blocks);
  b_col_blocks  := IF(transposeB, map_b.row_blocks,  map_b.col_blocks);
  c_matrix_rows := map_c.matrix_rows;
  c_matrix_cols := map_c.matrix_cols;
  c_row_blocks  := map_c.row_blocks;
  c_col_blocks  := map_c.col_blocks;
  A := ASSERT(A_in,
              ASSERT(a_matrix_cols=b_matrix_rows AND a_col_blocks=b_row_blocks,
                    'A-B: ' + 'A is ' + map_a.matrix_rows+'x'+map_a.matrix_cols +
                    ' Trans: '+ IF(transposeA,'Yes','No') + ' and B is ' + map_b.matrix_rows+'x'+map_b.matrix_cols +
                    ' Trans: '+ IF(transposeB,'Yes','No') + ' ' +PBblas.Constants.Dimension_Incompat, FAIL),
              ASSERT(a_matrix_rows=c_matrix_rows AND a_row_blocks=c_row_blocks,
                    'A-C: ' + 'Arows: ' + a_matrix_rows +' Crows '+c_matrix_rows+' '+ 
                              'Ablocks: ' + a_row_blocks +' Cblocks '+c_row_blocks+' '+
                              PBblas.Constants.Dimension_Incompat, FAIL));
  B := ASSERT(B_in,
              ASSERT(a_matrix_cols=b_matrix_rows AND a_col_blocks=b_row_blocks,
                    'A-B: ' + 'A is ' + map_a.matrix_rows+'x'+map_a.matrix_cols +
                    ' Trans: '+ IF(transposeA,'Yes','No') + ' and B is ' + map_b.matrix_rows+'x'+map_b.matrix_cols +
                    ' Trans: '+ IF(transposeB,'Yes','No') +' ' +PBblas.Constants.Dimension_Incompat, FAIL),
              ASSERT(b_matrix_cols=c_matrix_cols AND b_col_blocks=c_col_blocks,
                    'B-C: ' + 'Brows: ' + b_matrix_cols +' Ccols '+c_matrix_cols+' '+ 
                              'Bblocks: ' + b_row_blocks +' Cblocks '+c_col_blocks+' '+
                              PBblas.Constants.Dimension_Incompat, FAIL));
//
  Layout_Target cvt(Layout_Part par, INTEGER c,
                    BOOLEAN transpose, BOOLEAN keepRow) := TRANSFORM
    s_block_row       := IF(transpose, par.block_col, par.block_row);
    s_block_col       := IF(transpose, par.block_row, par.block_col);
    part_id_new_row   := map_c.assigned_part(c, s_block_col);
    part_id_new_col   := map_c.assigned_part(s_block_row, c);
    partition_id      := IF(keepRow, part_id_new_col, part_id_new_row);
    SELF.t_node_id    := map_c.assigned_node(partition_id);
    SELF.t_part_id    := partition_id;
    SELF.t_block_row  := IF(keepRow, s_block_row, c);
    SELF.t_block_col  := IF(keepRow, c, s_block_col);
    SELF.t_term       := IF(keepRow, s_block_col, s_block_row);
    SELF              := par;
  END;

  // A: copy of each cell in a row (column) goes to a column(row) (transpose)
  a_fact := IF(transposeB, map_b.row_blocks, map_b.col_blocks);
  a_work := NORMALIZE(A, a_fact, cvt(LEFT, COUNTER, transposeA, TRUE));
  a_dist := DISTRIBUTE(a_work, t_node_id);
  a_sort := SORT(a_dist, t_part_id, LOCAL);
  // B: copy of each cell in a column goes to a row
  b_fact := IF(transposeA, map_a.col_blocks, map_a.row_blocks);
  b_work := NORMALIZE(B, b_fact, cvt(LEFT, COUNTER, transposeB, FALSE));
  b_dist := DISTRIBUTE(b_work, t_node_id);
  b_sort := SORT(b_dist, t_part_id, LOCAL);
  // Multiply
  Layout_Part mul(Layout_Target a_part, Layout_Target b_part):=TRANSFORM
    part_id     := a_part.t_part_id;    //arbitrary choice
    part_a_cols := a_part.part_cols;
    part_a_rows := a_part.part_rows;
    part_b_rows := b_part.part_rows;
    part_c_rows := map_c.part_rows(part_id);
    part_c_cols := map_c.part_cols(part_id);
    part_c_first_row  := map_c.first_row(part_id);
    part_c_first_col  := map_c.first_col(part_id);
    k := IF(transposeA, part_a_rows, part_a_cols);
    SELF.partition_id := part_id;
    SELF.node_id      := a_part.t_node_id;
    SELF.block_row    := a_part.t_block_row;
    SELF.block_col    := a_part.t_block_col;
    SELF.first_row    := map_c.first_row(part_id);
    SELF.part_rows    := part_c_rows;
    SELF.first_col    := part_c_first_col;
    SELF.part_cols    := part_c_cols;
    SELF.mat_part     := BLAS.dgemm(transposeA, transposeB,
                                    part_c_rows, part_c_cols, k,
                                    alpha, a_part.mat_part, b_part.mat_part,
                                    0.0, empty_array);
  END;
  ab_prod := JOIN(a_sort, b_sort,
                  LEFT.t_part_id=RIGHT.t_part_id AND LEFT.t_term=RIGHT.t_term,
                  mul(LEFT,RIGHT), LOCAL);

  // Apply beta
  Layout_Part applyBeta(Layout_Part part) := TRANSFORM
    SELF.mat_part := BLAS.dscal(map_c.matrix_rows*map_c.matrix_cols,
                                beta, part.mat_part, 1);
    SELF          := part;
  END;
  upd_C := PROJECT(C, applyBeta(LEFT));

  // Sum terms
  Layout_Part sumTerms(Layout_Part cumm, Layout_Part term) := TRANSFORM
    N := map_c.part_rows(cumm.partition_id) * map_c.part_cols(cumm.partition_id);
    SELF.mat_part := BLAS.daxpy(N, 1.0, cumm.mat_part, 1, term.mat_part, 1);
    SELF := cumm;
  END;
  sorted_terms := SORT(upd_c+ab_prod, partition_id, LOCAL);
  rslt := ROLLUP(sorted_terms, sumTerms(LEFT, RIGHT), partition_id, LOCAL);
  RETURN rslt;
END;