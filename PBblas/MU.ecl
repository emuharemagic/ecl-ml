IMPORT * FROM $;

EXPORT MU := MODULE

// These fundamental (but trivial) routines move a regular matrix in and out of matrix-universe format
// The matrix universe exists to allow multiple matrices to co-reside inside one dataflow
// This eases passing of them in and out of functions - but also reduces the number of operations required to co-locate elements
EXPORT To(DATASET(Types.Layout_Part) d, Types.t_mu_no num) := PROJECT(d, TRANSFORM(Types.MUElement, SELF.no := num, SELF := LEFT));
EXPORT From(DATASET(Types.MUElement) d, Types.t_mu_no num) := PROJECT(d(no=num), TRANSFORM(Types.Layout_Part, SELF := LEFT));

  END;