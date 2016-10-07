// Matrix Properties
IMPORT * FROM $;
EXPORT Has(DATASET(Types.Element) d) := MODULE

r := RECORD
  UNSIGNED NElements := COUNT(GROUP);
	UNSIGNED XMax := MAX(GROUP,d.x);
	UNSIGNED YMax := MAX(GROUP,d.y);
  END;

EXPORT Stats := TABLE(d,r)[1];

// The largest dimension of the matrix
EXPORT Dimension := MAX(Stats.XMax,Stats.YMax);

// The percentage of the sparse matrix that is actually there
EXPORT Density := Stats.NElements / (Stats.XMax*Stats.YMax);

EXPORT Norm := SQRT(SUM(Each.Mul(d,d),value));

r := RECORD
  Types.t_Index x := d.x ;
	Types.t_Index y := 1;
	Types.t_Value value := AVE(GROUP,d.value);
END;

// MeanRow is a column vector containing the mean value of each row.
EXPORT MeanRow := TABLE(d,r,d.x);

r := RECORD
  Types.t_Index x := 1 ;
	Types.t_Index y := d.y;
	Types.t_Value value := AVE(GROUP,d.value);
END;

// MeanCol is a row vector containing the mean value of each column.
EXPORT MeanCol := TABLE(d,r,d.y);
r := RECORD
  Types.t_Index x := d.x ;
  Types.t_Index y := 1;
  Types.t_Value value := MAX(GROUP,d.value);
END;

// MaxRow is a column vector containing the max value of each row.
EXPORT MaxRow := TABLE(d,r,d.x);

r := RECORD
  Types.t_Index x := 1 ;
  Types.t_Index y := d.y;
  Types.t_Value value := MAX(GROUP,d.value);
END;

// MaxCol is a row vector containing the max value of each column.
EXPORT MaxCol := TABLE(d,r,d.y);

r := RECORD
  Types.t_Index x := d.x ;
  Types.t_Index y := 1;
  Types.t_Value value := SUM(GROUP,d.value);
END;

// SumRow is a column vector containing the sum value of each row.
EXPORT SumRow := TABLE(d,r,d.x);

r := RECORD
  Types.t_Index x := 1 ;
  Types.t_Index y := d.y;
  Types.t_Value value := SUM(GROUP,d.value);
END;

// SumCol is a row vector containing the sum value of each column.
EXPORT SumCol := TABLE(d,r,d.y);

r := RECORD
  Types.t_Index x := 1;
  Types.t_Index y := d.y;
  Types.t_Value value := SQRT(VARIANCE(GROUP,d.value));
END;

EXPORT SDCol := TABLE(d,r,d.y);
END;