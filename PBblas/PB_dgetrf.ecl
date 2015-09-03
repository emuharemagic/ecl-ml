// LU Factorization.  Produces composite LU matrix for the diagonal
//blocks.  Iterates through the matrix a row of blocks and column of blocks at
//a time.  Partition A into M block rows and N block columns.  The A11 cell is a
//single block.  A12 is a single row of blocks with N-1 columns.  A21 is a single
//column of blocks with M-1 rows.  A22 is a sub-matrix of M-1 x N-1 blocks.
//  | A11   A12 |      |  L11   0   |    | U11        U12    |
//  | A21   A22 |  ==  |  L21   L22 | *  |  0         U22    |
//
//                     | L11*U11             L11*U12         |
//                 ==  | L21*U11         L21*U12 + L22*U22   |
//
// Based upon PB-BLAS: A set of parallel block basic linear algebra subprograms
// by Choi and Dongarra
//
IMPORT PBblas;
IMPORT PBblas.Types;
IMPORT PBblas.IMatrix_Map;
IMPORT PBBlas.BLAS;
IMPORT PBblas.Block;
Layout_Part := Types.Layout_Part;
Layout_Target := Types.Layout_Target;
Triangle := Types.Triangle;
Upper:= Types.Triangle.Upper;
Lower:= Types.Triangle.Lower;
Diagonal := Types.Diagonal;
Side := Types.Side;
Types.matrix_t empty_mat := [];
dim_t := Types.dimension_t;
Term := ENUM(UNSIGNED1, LeftTerm=1, RghtTerm=2, BaseTerm=3);

EXPORT PB_dgetrf(IMatrix_Map map_a, DATASET(Layout_Part) A) := FUNCTION
  // Loop body
  loopBody(DATASET(Layout_Part) parts, UNSIGNED4 rc_pos) := FUNCTION
    // Select diagonal block, use dgetf2 in PROJECT to produce L11 and U11
    A_11 := parts(block_row=rc_pos AND block_col=rc_pos);
    Layout_Part factorBlock(Layout_Part part) := TRANSFORM
      m := part.part_rows;
      n := part.part_cols;
      // dgetf2 throws error if factoring fails
      SELF.mat_part := Block.dgetf2(m, n, part.mat_part);
      SELF := part;
    END;
    newCorner := SORTED(PROJECT(A_11, factorBlock(LEFT)), partition_id);
    // The dtrsm routine will work composite matrix, no need to extract
    Layout_Part divide(Layout_Part a_part, Layout_Part f_part) := TRANSFORM
      SELF.mat_part := IF(a_part.block_col=rc_pos,
                  BLAS.dtrsm(Side.xA, Upper, FALSE, Diagonal.NotUnitTri,
                             a_part.part_rows, a_part.part_cols, f_part.part_rows,
                             1.0, f_part.mat_part, a_part.mat_part),
                  BLAS.dtrsm(Side.Ax, Lower, FALSE, Diagonal.UnitTri,
                             a_part.part_rows, a_part.part_cols, f_part.part_rows,
                             1.0, f_part.mat_part, a_part.mat_part));
      SELF := a_part;
    END;
    newRow := JOIN(parts(block_col>rc_pos), newCorner,
                   LEFT.block_row=RIGHT.block_row,
                   divide(LEFT, RIGHT), LOOKUP);
    newCol := JOIN(parts(block_row>rc_pos), newCorner,
                   LEFT.block_col=RIGHT.block_col,
                   divide(LEFT, RIGHT), LOOKUP);
    // Outer row and column updated.  Now update the sub-matrix.
    Layout_Target stamp(Layout_Part p, dim_t tr, dim_t tc, Term trm) := TRANSFORM
      t_part := map_a.assigned_part(tr, tc);
      SELF.t_node_id := map_a.assigned_node(t_part);
      SELF.t_part_id := t_part;
      SELF.t_block_row := tr;
      SELF.t_block_col := tc;
      SELF.t_term := trm;
      SELF := p;
    END;
    base0     := PROJECT(parts(block_row>rc_pos AND block_col>rc_pos),
                         stamp(LEFT, LEFT.block_row, LEFT.block_col, Term.BaseTerm));
    baseParts := SORT(base0, t_part_id);
    col0w     := NORMALIZE(newCol, map_a.row_blocks-rc_pos,
                         stamp(LEFT, LEFT.block_row, rc_pos+COUNTER, Term.LeftTerm));
    colParts  := SORT(col0w, t_part_id);
    row0w     := NORMALIZE(newRow, map_a.col_blocks-rc_pos,
                         stamp(LEFT, rc_pos+COUNTER, LEFT.block_col, Term.RghtTerm));
    rowParts  := SORT(row0w, t_part_id);
    Layout_Target update(DATASET(Layout_Target) p) := TRANSFORM
      haveBase := EXISTS(p(t_term=Term.BaseTerm));
      haveLeft := EXISTS(p(t_term=Term.LeftTerm));
      haveRght := EXISTS(p(t_term=Term.RghtTErm));
      doMultiply := haveLeft AND haveRght;
      node_id  := p[1].t_node_id;
      part_id  := p[1].t_part_id;
      block_row:= p[1].t_block_row;
      block_col:= p[1].t_block_col;
      inside   := p(t_term=Term.RghtTerm)[1].part_rows;
      NumRows  := map_a.part_rows(part_id);
      NumCols  := map_a.part_cols(part_id);
      BaseMat  := p(t_term=Term.BaseTerm)[1].mat_part;
      LeftMat  := p(t_term=Term.LeftTerm)[1].mat_part;
      RghtMat  := p(t_term=Term.RghtTerm)[1].mat_part;
      SELF.node_id      := node_id;
      SELF.partition_id := part_id;
      SELF.block_row    := block_row;
      SELF.block_col    := block_col;
      SELF.first_row    := map_a.first_row(part_id);
      SELF.first_col    := map_a.first_col(part_id);
      SELF.part_rows    := NumRows;
      SELF.part_cols    := NumCols;
      SELF.mat_part     := IF(doMultiply,
                              BLAS.dgemm(FALSE, FALSE, NumRows, NumCols, inside,
                                         -1.0, LeftMat, RghtMat,
                                         IF(haveBase, 1.0, 0.0), BaseMat),
                              BaseMat);
      SELF.t_node_id  := node_id;
      SELF.t_part_id  := part_id;
      SELF.t_block_row:=block_row;
      SELF.t_block_col:= block_col;
      SELF.t_term     := Term.BaseTerm;
    END;
    new0 := GROUP(SORT(rowParts+colParts+baseParts, t_part_id), t_part_id);
    new1 := ROLLUP(new0, GROUP, update(ROWS(LEFT)));
    newSub := SORTED(PROJECT(new1, Layout_Part), partition_id);
    rslt := SORT(newCorner + newRow + newCol + newSub, partition_id);
    RETURN rslt;
  END; // loop Body
  a_sorted := SORT(A, partition_id);
  factorParts := LOOP(a_sorted, MIN(map_a.row_blocks, map_a.col_blocks),
                      COUNTER<=LEFT.block_row AND COUNTER<=LEFT.block_col,
                      loopBody(ROWS(LEFT), COUNTER));
  rslt := SORT(DISTRIBUTE(factorParts, node_id), partition_id, LOCAL);
  RETURN rslt;
END;