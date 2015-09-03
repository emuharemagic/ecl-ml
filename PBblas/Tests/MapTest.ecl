//This test can be used to determine a suggested size for matrix partitions
//Only the first two params of AutoBVMap are needed for this. (NumRows,NumCols)
//That last two params can be include if you want to change the maximum 
//number of rows/cols per partition. (NumRows,NumCols,,,MaxRows,MaxCols)
//Can be used to determine inputs for partition sizes for that use the AutoMap function
//To speed results by skipping the Auto phase during code execution. (i.e. ML.Classify.Logistic input)
//Keep in mind that this is only for an individual matrix and for matrix operations
//all matrices and their partitions must be compatible.

IMPORT PBblas;

mXmap := PBblas.AutoBVMap(100000,29);
//mXmap := PBblas.AutoBVMap(100000,29,,,5000,(INTEGER)(CLUSTERSIZE*1.5));

sizeRec := RECORD
			PBblas.Types.dimension_t m_rows;
			PBblas.Types.dimension_t m_cols;
			PBblas.Types.dimension_t f_b_rows;
			PBblas.Types.dimension_t f_b_cols;
		END;

		sizeTable := DATASET([{mXmap.matrix_rows,mXmap.matrix_cols,mXmap.part_rows(1),mXmap.part_cols(1)}], sizeRec);
		
		OUTPUT(sizeTable[1].m_rows,NAMED('Rows'));
		OUTPUT(sizeTable[1].f_b_rows,NAMED('BlockRows'));
		OUTPUT(sizeTable[1].m_cols,NAMED('Columns'));
		OUTPUT(sizeTable[1].f_b_cols,NAMED('BlockColumns'));