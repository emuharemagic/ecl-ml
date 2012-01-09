IMPORT * FROM $;
EXPORT InsertColumn(DATASET(Types.Element) d, Types.t_Index col_i, Types.t_value filler) := FUNCTION

filler_col := Vec.ToCol( Vec.From( MAX(d,x), filler ), col_i );

Types.Element shiftRight(d le) := TRANSFORM
	  SELF.y := IF(le.y>= col_i, le.y +1, le.y);
	  SELF := le;
	END;
	d1 := PROJECT(d,shiftRight(LEFT));

	RETURN filler_col+d1;

  END;