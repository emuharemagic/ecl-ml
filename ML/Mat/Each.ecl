// Element-wise matrix operations
IMPORT ML.Mat.Types;
EXPORT Each := MODULE

EXPORT Sqrt(DATASET(Types.Element) d) := FUNCTION
  Types.Element fn(d le) := TRANSFORM
	  SELF.value := sqrt(le.value);
	  SELF := le;
	END;
	RETURN PROJECT(d,fn(LEFT));
END;
	
EXPORT Exp(DATASET(Types.Element) d) := FUNCTION
  Types.Element fn(d le) := TRANSFORM
	  SELF.value := exp(le.value);
	  SELF := le;
	END;
	RETURN PROJECT(d,fn(LEFT));
END;

EXPORT Abs(DATASET(Types.Element) d) := FUNCTION
  Types.Element fn(d le) := TRANSFORM
	  SELF.value := Abs(le.value);
	  SELF := le;
	END;
	RETURN PROJECT(d,fn(LEFT));
END;

EXPORT Mul(DATASET(Types.Element) l,DATASET(Types.Element) r) := FUNCTION
// Only slight nastiness is that these matrices may be sparse - so either side could be null
Types.Element Multiply(l le,r ri) := TRANSFORM
    SELF.x := le.x ;
    SELF.y := le.y ;
	  SELF.value := le.value * ri.value; 
  END;
	RETURN JOIN(l,r,LEFT.x=RIGHT.x AND LEFT.y=RIGHT.y,Multiply(LEFT,RIGHT));
END;


// matrix .+ scalar
EXPORT Add(DATASET(Types.Element) d,Types.t_Value scalar) := FUNCTION
  Types.Element add(d le) := TRANSFORM
	  SELF.value := le.value + scalar;
	  SELF := le;
	END;
	RETURN PROJECT(d,add(LEFT));
END;

/*
 factor ./ matrix	; 
*/	
EXPORT Reciprocal(DATASET(Types.Element) d, Types.t_Value factor=1) := FUNCTION
  Types.Element divide(d le) := TRANSFORM
	  SELF.value := factor / le.value;
	  SELF := le;
	END;
	RETURN PROJECT(d,divide(LEFT));
 END;

END;