//EXPORT BWR_Demo_Estimate := 'todo';
IMPORT TS;
IMPORT TS.Demo;
IMPORT TS.Types;

// Starting values
starts := DATASET([{20.0, 1}], TS.Types.Co_efficient);
// time series generations
d0 := TS.Demo.Series_Data(starts, 1.0, 2.0, 20, 0.0, 0.05);
// persist the data to facilitate re_running
d1 := d0 : PERSIST('TEMP::ARIMA::TEST_1::SOURCE', EXPIRE(10));

ident_specs := DATASET([{1,0}, {2,1}, {3,2}], TS.Types.Ident_Spec);
// The identification run for a flat slope
slope0 := TS.extract_ts(d1, val_slope);
OUTPUT(slope0, NAMED('Series'));

id_slope := TS.Identification(slope0, ident_specs);
OUTPUT(id_slope.Correlations(50), ALL, NAMED('Slope_Corr'));
//OUTPUT(id_slope.ACF(20), NAMED('Slope_ACF'));
//OUTPUT(id_slope.PACF(20), NAMED('Slope_PACF'));

model_specs := DATASET([{1, 0, 2, 0, TRUE},
                        {2, 2, 0, 4, TRUE},
                        {3, 0, 2, 1, TRUE}], TS.Types.Model_Spec);
model_parms := TS.Estimation(slope0, model_specs, 100);
OUTPUT(model_parms, NAMED('Model_Rslt'));

x0 := TS.Diagnosis(slope0, model_parms);
OUTPUT(x0, NAMED('Scored'));

slope_mod := TS.SeriesByModel(slope0, model_specs);
estimates := TS.EstimatedObs(slope_mod, model_parms);
OUTPUT(estimates, NAMED('Estimaed_Obs'));
