// Implements Cholesky factorization of A = U**T * U if Triangular.Upper requested
//or A = L * L**T if Triangualr.Lower is requested.
// The matrix A must be symmetric positive definite.
//  | A11   A12 |      |  L11   0   |    | L11**T     L21**T |
//  | A21   A22 |  ==  |  L21   L22 | *  |  0           L22  |
//
//                     | L11*L11**T          L11*L21**T      |
//                 ==  | L21*L11**T  L21*L21**T + L22*L22**T |
//
// So, use Cholesky on the first block to get L11.
//     L21 = A21*L11**T**-1   which can be found by dtrsm on each column block
//     A22' is A22 - L21*L21**T
// Based upon PB-BLAS: A set of parallel block basic linear algebra subprograms
// by Choi and Dongarra
//
// Iterate through the diagonal blocks
IMPORT PBblas;
IMPORT PBblas.Types;
IMPORT PBblas.IMatrix_Map;
IMPORT PBBlas.BLAS;
IMPORT PBblas.LAPACK;
Layout_Part := Types.Layout_Part;
Layout_Target := Types.Layout_Target;
Triangle := Types.Triangle;
Upper:= Types.Triangle.Upper;
Lower:= Types.Triangle.Lower;
Diagonal := Types.Diagonal;
Types.matrix_t empty_mat := [];
dimension_t := Types.dimension_t;

EXPORT PB_dpotrf(Triangle tri, IMatrix_Map map_a, DATASET(Layout_Part) A) := FUNCTION
  // LOOP body
  loopBody(DATASET(Layout_Part) parts, UNSIGNED4 rc_pos) := FUNCTION
    // Select diagonal block, use dpotf2 in PROJECT to produce L11 or U11
    A_11 := SORTED(parts(block_row=rc_pos AND block_col=rc_pos), partition_id);
    Layout_Part factorBlock(Layout_Part part) := TRANSFORM
      r := part.part_rows;
      // dpotf2 throws error if factoring fails
      SELF.mat_part := LAPACK.dpotf2(tri, r, part.mat_part);
      SELF := part;
    END;
    cornerMatrix := PROJECT(A_11, factorBlock(LEFT));
    // Use PB_DTRSM with L11 get L21 (U12)
    A_21 := parts(block_col=rc_pos AND block_row>rc_pos);
    A_12 := parts(block_row=rc_pos AND block_col>rc_pos);
   Layout_Part updateSub(Layout_Part outPart, Layout_Part corner) := TRANSFORM
      side := IF(tri=Lower, Types.Side.xA, Types.Side.Ax);
      part_rows := map_a.part_rows(outPart.partition_id);
      part_cols := map_a.part_cols(outPart.partition_id);
      lda       := map_a.part_rows(corner.partition_id);
      SELF.mat_part := BLAS.dtrsm(side, tri, TRUE, Diagonal.NotUnitTri,
                                  part_rows, part_cols, lda, 1.0,
                                  corner.mat_part, outPart.mat_part);
      SELF := outPart;
    END;
    L_21 := JOIN(A_21, cornerMatrix, LEFT.block_col=RIGHT.block_col,
                updateSub(LEFT,RIGHT), LOOKUP);
    U_12 := JOIN(A_12, cornerMatrix, LEFT.block_row=RIGHT.block_row,
                updateSub(LEFT, RIGHT), LOOKUP);
    edgeMatrix := IF(tri=Lower, L_21, U_12);
    // Prep for rank update
    Layout_Target stampC(Layout_Part part) := TRANSFORM
      SELF.t_part_id    := part.partition_id;
      SELF.t_node_id    := part.node_id;
      SELF.t_block_row  := part.block_row;
      SELF.t_block_col  := part.block_col;
      SELF.t_term       := 3;   // C
      SELF              := part;
    END;
    Term3_d := PROJECT(parts(block_row>rc_pos AND block_col>rc_pos), stampC(LEFT));
    // Replicate  L21(U12) to get new sub-matrix
    Layout_Target replicate(Layout_Part part, dimension_t tr,
                            dimension_t tc, UNSIGNED term) := TRANSFORM
      target_part       := map_a.assigned_part(tr, tc);
      SELF.t_part_id    := target_part;
      SELF.t_node_id    := map_a.assigned_node(target_part);
      SELF.t_block_row  := tr;
      SELF.t_block_col  := tc;
      SELF.t_term       := term;
      SELF              := part;
    END;
    X_L_21   := NORMALIZE(L_21, map_a.row_blocks-rc_pos,
                          replicate(LEFT, LEFT.block_row, rc_pos+COUNTER, 1));
    X_L_21T  := NORMALIZE(L_21, map_a.row_blocks-rc_pos,
                          replicate(LEFT, rc_pos+COUNTER, LEFT.block_row, 2));
    X_U_12   := NORMALIZE(U_12, map_a.col_blocks-rc_pos,
                          replicate(LEFT, rc_pos+COUNTER, LEFT.block_col, 2));
    X_U_12T  := NORMALIZE(U_12, map_a.col_blocks-rc_pos,
                          replicate(LEFT, LEFT.block_col, rc_pos+COUNTER, 1));
    Term1_d  := SORT(IF(tri=Lower, X_L_21, X_U_12T), t_part_id);
    Term2_d  := SORT(IF(tri=Lower, X_L_21T, X_U_12), t_part_id);
    // Bring together sub-matrix parts and perform rank update
    Layout_Target updMat(Layout_Target lr, DATASET(Layout_Target) rws):=TRANSFORM
      NumRows           := map_a.part_rows(lr.t_part_id);
      NumCols           := map_a.part_cols(lr.t_part_id);
      have_a            := EXISTS(rws(t_term=1));
      idA               := rws(t_term=1)[1].partition_id;
      have_b            := EXISTS(rws(t_term=2));
      multiplyTerms     := have_a AND have_b;
      tranA             := IF(tri=Upper, TRUE, FALSE);
      tranB             := IF(tri=Lower, TRUE, FALSE);
      have_c            := EXISTS(rws(t_term=3));
      matrix_c          := IF(have_c, rws(t_term=3)[1].mat_part, empty_mat);
      matrix_a          := rws(t_term=1)[1].mat_part;
      matrix_b          := rws(t_term=2)[1].mat_part;
      inside            := IF(tri=Upper, map_a.part_rows(idA), map_a.part_cols(idA));
      SELF.partition_id := lr.t_part_id;
      SELF.node_id      := map_a.assigned_node(lr.t_part_id);
      SELF.block_row    := lr.t_block_row;
      SELF.block_col    := lr.t_block_col;
      SELF.first_row    := map_a.first_row(lr.t_part_id);
      SELF.part_rows    := NumRows;
      SELF.first_col    := map_a.first_col(lr.t_part_id);
      SELF.part_cols    := NumCols;
      SELF.mat_part     := IF(multiplyTerms,
                              BLAS.dgemm(tranA, tranB, NumRows, NumCols, inside,
                                        -1.0, matrix_a, matrix_b, 1.0, matrix_c),
                              matrix_c);
      SELF              := lr;
    END;
    recordSets := GROUP(SORT(Term1_d+Term2_d+Term3_d, t_part_id), t_part_id);
    updatedSub := ROLLUP(recordSets, GROUP, updMat(LEFT, ROWS(LEFT)));
    subMatrix  := PROJECT(updatedSub, Layout_Part);
    rslt       := SORT(cornerMatrix+edgeMatrix+subMatrix, partition_id);
    RETURN rslt;
  END;
  // Drop out parts that are not needed
  a_checked := ASSERT(A, map_a.matrix_rows=map_a.matrix_cols AND
                         map_a.row_blocks=map_a.col_blocks,
                      PBblas.Constants.Not_Square, FAIL);
  workParts := a_checked((tri=Upper AND block_row<=block_col) OR
                         (tri=Lower AND block_col<=block_row));
  work_d := SORT(workParts, partition_id);
  triangleParts := LOOP(work_d, map_a.row_blocks,
                       COUNTER<=LEFT.block_row AND COUNTER<=LEFT.block_col,
                       loopBody(ROWS(LEFT), COUNTER));
  rslt := SORT(DISTRIBUTE(triangleParts, node_id), partition_id, LOCAL);
  RETURN rslt;
END;
