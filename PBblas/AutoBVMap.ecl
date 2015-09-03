IMPORT * FROM $;
IMPORT Std.Str;
IMPORT std.system.Thorlib;
IMPORT PBblas;
dimension_t := PBblas.Types.dimension_t;
partition_t := PBblas.Types.partition_t;
IMPORT ML.MAT;

EXPORT AutoBVMap(dimension_t m_rows, dimension_t m_cols,
                  dimension_t f_b_rows=0, dimension_t f_b_cols=0, UNSIGNED maxrows=5000, UNSIGNED maxcols=2500) 
		:= MODULE(PBblas.IMatrix_Map)
	
	SHARED MapMatrix := FUNCTION
			
			//This is the function used to reduce the number of
			//rows or columns per block. This is only called once
			//per AutoMap for either cutting row or columns.
			//NEVER BOTH!
			//x = row/col size of dataset
			//y = maxrow/maxcol
			//z = clustersize
			findsize(UNSIGNED4 x, UNSIGNED4 y, UNSIGNED4 z) := FUNCTION
	  		     
	  		     blocks := MAX((x DIV (y * z)) * z, z); //blocks is a multiple of the cluster size
	  		     blockSize := (x + blocks - 1) DIV blocks; //blockSize is CEILING(x/blocks)
	  		     RETURN blockSize;
	  		     
	  		END;
			
			//Reaching this function mean that the number of rows and cols in dataset
			//Are below the number of maxrows and maxcols.
			//In this case, the entire matrix will be the block.
			smallcols := FUNCTION
			
				RETURN [m_rows, m_cols];
			
			END;
			
			
			//Reduce the size of cols per block so that they are smaller than the maxcols
			//Cluster size is used to get as even a distribution as possible.
			//part rows = m_rows && part cols = result
			largecols := FUNCTION
				cutcols := findsize(m_cols, maxcols, CLUSTERSIZE);
				RETURN [m_rows, cutcols];
			END;
			
			//We have established that we have a small number of rows
			//Now it is time to check and see if we have a large number of columns
			//If larger we need to reduce the number of cols per block
			smallrows := FUNCTION
			    RETURN if(m_cols > maxcols, largecols, smallcols);
			
			END;
			
			
			//Reduce the size of rows per block so that they are smaller than the maxrows
			//Cluster size is used to get as even a distribution as possible.
			//part rows = result && part cols = m_cols
			largerows  := FUNCTION
				cutrows := findsize(m_rows, maxrows, CLUSTERSIZE);
				RETURN [cutrows, m_cols];
			END;
			
			
			//Block sizes have been provided.
			definedsize := FUNCTION
				RETURN [f_b_rows, f_b_cols];
			END;
			
			
			//Check to see of number of rows is larger than the maxrows allowed per block
			//If larger we need to reduce the number of rows per block
			//If smaller we are done with row portion of our partition. (part rows = m_rows)
			nodefinedsize := FUNCTION
				RETURN if(m_rows > maxrows, largerows, smallrows);
			END;
			
			//Check to see if blocks are defined. If not call no defined size.
			SET OF dimension_t blockrowcolset := FUNCTION
			 RETURN IF((f_b_rows > 0 AND f_b_cols > 0), definedsize, nodefinedsize );
			END;
			
			RETURN blockrowcolset;
		
		END;
	
	
	
	SHARED nodes_available := Thorlib.nodes();
	SHARED this_node       := Thorlib.node();
	//
	EXPORT row_blocks   := IF(f_b_rows>0, ((m_rows-1) DIV f_b_rows) + 1, 1);
	EXPORT col_blocks   := IF(f_b_cols>0, ((m_cols-1) DIV f_b_cols) + 1, 1);
	SHARED rowcol := MapMatrix;
	SHARED block_rows   := rowcol[1];
	SHARED block_cols   := rowcol[2];
	//
	EXPORT matrix_rows  := m_rows;
	EXPORT matrix_cols  := m_cols;
	EXPORT partitions_used := row_blocks * col_blocks;
	EXPORT nodes_used   := MIN(nodes_available, partitions_used);
	// Functions.
	EXPORT row_block(dimension_t mat_row) := ((mat_row-1) DIV block_rows) + 1;
	EXPORT col_block(dimension_t mat_col) := ((mat_col-1) DIV block_cols) + 1;
	EXPORT assigned_part(dimension_t rb, dimension_t cb) := ((cb-1) * row_blocks) + rb;
	EXPORT assigned_node(partition_t p) := ((p-1) % nodes_used);
	EXPORT first_row(partition_t p)   := (((p-1)  %  row_blocks) * block_rows) + 1;
	EXPORT first_col(partition_t p)   := (((p-1) DIV row_blocks) * block_cols) + 1;
	EXPORT part_rows(partition_t p)   := MIN(matrix_rows-first_row(p)+1, block_rows);
	EXPORT part_cols(partition_t p)   := MIN(matrix_cols-first_col(p)+1, block_cols);
		
END;