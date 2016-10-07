IMPORT ML;
IMPORT ML.Mat AS Mat;
IMPORT ML.DMat AS DMat;
IMPORT PBblas.MU AS MU;
IMPORT PBblas;

a := DATASET('~lsa::6c5.mtx', Mat.Types.Element, CSV);
a_rows := MAX(A, x);
a_cols := MAX(A, y);
block_rows := MIN(a_rows DIV 10, PBblas.Constants.Block_Vec_Rows);
block_cols := a_cols DIV 10;
a_map := PBblas.Matrix_Map(a_rows, a_cols, block_rows, block_cols); 
dA := DMat.Converted.FromElement(A, a_map);
decomp := ML.LSA.RandomisedSVD.RandomisedSVD(a_map, dA, 100);
outputFormat := RECORD
    dA.node_id;
    dA.partition_id;
    dA.block_row;
    dA.block_col;
    dA.first_row;
    dA.part_rows;
    dA.first_col;
    dA.part_cols;
END;
OUTPUT(dA, outputFormat, NAMED('Data'), ALL);
OUTPUT(decomp); 