IMPORT TS;
IMPORT TS.Demo;
IMPORT TS.Types;
// Starting values
starts := DATASET([{2.0, 1}, {4.0, 2}, {1.0, 3}, {2.0, 4}], TS.Types.Co_efficient);
// time series generations
d0 := TS.Demo.Series_Data(starts, 1.0, 2.0, 500);
// persist the data to facilitate re_running
d1 := d0 : PERSIST('TEMP::ARIMA::TEST_1::SOURCE', EXPIRE(20));
// The different degrees of differencing to try for identification
Specs := DATASET([{1,0}, {2,1}, {3,2}], TS.Types.Ident_Spec);

// The identification run for flat data
flat0 := TS.extract_ts(d1, val_flat);
id_flat := TS.Identification(flat0, Specs);
OUTPUT(id_flat.Correlations(20), NAMED('Flat_Corr'));
OUTPUT(id_flat.ACF(20), NAMED('Flat_ACF'));
OUTPUT(id_flat.PACF(20), NAMED('Flat_PACF'));

// The identification run for a flat slope
slope0 := TS.extract_ts(d1, val_slope);
id_slope := TS.Identification(slope0, Specs);
OUTPUT(id_slope.Correlations(20), NAMED('Slope_Corr'));
OUTPUT(id_slope.ACF(20), NAMED('Slope_ACF'));
OUTPUT(id_slope.PACF(20), NAMED('Slope_PACF'));

// The identification run for data that buiulds upon previous values
accum0 := TS.extract_ts(d1, val_accum);
id_accum := TS.Identification(accum0, Specs);
OUTPUT(id_accum.Correlations(20), NAMED('Accum_Corr'));
OUTPUT(id_accum.ACF(20), NAMED('Accum_ACF'));
OUTPUT(id_accum.PACF(20), NAMED('Accum_PACF'));