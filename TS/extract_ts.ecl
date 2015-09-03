/**
 * Extract the univariate time series.  The data is required to be in 
 * time order, and the time perioods are reqiured to be regular.  Missing
 * values are not supported.  The user must prepare the file such that
 * these assumptions are met.
 * @param dsIn the input time series source data set
 * @param fieldName the input field name
 * @return a Univariate time series dataset
 */
EXPORT extract_ts(dsIn, fieldName) := FUNCTIONMACRO
  IMPORT Std.System.ThorLib;
  IMPORT TS.Types;
  Work1 := RECORD(Types.UniObservation)
    UNSIGNED2 nodeid;
  END;
  Work1 ext1(RECORDOF(dsIn) src, UNSIGNED c) := TRANSFORM
    SELF.nodeid := ThorLib.node();
     SELF.period := c;
    SELF.dependent := src.fieldName;
  END;
  w0 := PROJECT(dsIn, ext1(LEFT, COUNTER), LOCAL);
  t0 := TABLE(w0, {nodeid, nc:=COUNT(GROUP), pc:=0}, nodeid, LOCAL);
  RECORDOF(t0) incr_pc(RECORDOF(t0) prev, RECORDOF(t0) curr) := TRANSFORM
    SELF.pc := prev.pc + prev.nc;
    SELF := curr;
  END;
  t1 := ITERATE(t0, incr_pc(LEFT,RIGHT));
  Types.UniObservation set_time(Work1 rec, RECORDOF(t0) t) := TRANSFORM
    SELF.period := rec.period + t.pc;
    SELF := rec;
  END;
  RETURN JOIN(w0, t1, LEFT.nodeid=RIGHT.nodeid, set_time(LEFT,RIGHT), LOOKUP);
ENDMACRO;