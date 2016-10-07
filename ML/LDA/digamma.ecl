//The approximation of the Digamma function via a Taylor series.  Digamma
//is the first derivative of the log gamma function.
EXPORT REAL8 digamma(REAL8 x) := DEFINE FUNCTION
  REAL8 xp6 := x + 6.0;
  REAL8 p0 := 1.0 / (xp6 * xp6);
  REAL8 p1 := ( ( (0.004166666666667*p0
                  -0.003968253986254)*p0
                +0.008333333333333)*p0
              -0.083333333333333)*p0;
  REAL8 lx := LN(xp6) - 0.5/xp6 - 1/(xp6-1) - 1/(xp6-2) - 1/(xp6-3) - 1/(xp6-4)
                  - 1/(xp6-5) - 1/(xp6-6);
  RETURN p1 + lx;
END;
