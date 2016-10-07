//The approximation of the Trigamma function via a Taylor series.  Trigamma
//is the second derivative of the log gamma function.
EXPORT REAL8 trigamma(REAL8 x) := DEFINE FUNCTION
  REAL8 xp6 := x + 6.0;
  REAL8 xp5 := x + 5.0;
  REAL8 xp4 := x + 4.0;
  REAL8 xp3 := x + 3.0;
  REAL8 xp2 := x + 2.0;
  REAL8 xp1 := x + 1.0;
  REAL8 p0 := 1/(xp6*xp6);
  REAL8 p1 := (((((0.075757575757576*p0 - 0.033333333333333)
                  *p0 + 0.0238095238095238)
                 *p0 - 0.033333333333333)
                *p0 + 0.166666666666667)
               *p0 + 1 );
  REAL8 p2 := p1/xp6 + 0.5*p0;
  REAL8 sx := (1/(xp1*xp1)) + (1/(xp2*xp2)) + (1/(xp3*xp3)) + (1/(xp4*xp4))
            + (1/(xp5*xp5)) + (1/(x*x));
  RETURN p2 + sx;
END;
