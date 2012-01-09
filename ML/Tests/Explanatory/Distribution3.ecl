IMPORT ML;
/*EXPORT t_FieldReal Density(t_FieldReal RH):=0.0
// The probability density function at point RH
EXPORT t_FieldReal Cumulative(t_FieldReal RH):=0.0
// The cumulative probability function from – infinity[1] up to RH
EXPORT DensityV():=0.0
// A vector providing the probability density function at each range point –
this is approximately equal to the ‘distribution tables’ that might be published in various books
EXPORT CumulativeV()
// A vector providing the cumulative probability density function at each range point – again roughly
equal to ‘cumulative distribution tables’ as published in the back of statistics books
EXPORT Ntile(Pcnt
//provides the value from the underlying domain that corresponds to the given percentile.
Thus .Ntile(99) gives the value beneath which 99% of all should observations will fall.
*/
a := ML.Distribution.StudentT(3,10000);
a.NTile(99); // Single tail
a.NTile(99.5); // Double tail
GenData(NRecords,Distribution,FieldNumber);