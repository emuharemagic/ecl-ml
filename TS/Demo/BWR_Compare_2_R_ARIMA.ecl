//EXPORT BWR_Compare_2_R_ARIMA := 'todo';
//EMBEDR_ARIMA.ecl
IMPORT R;
IMPORT TS;

set of real4 ARIMA(
                   set of real4 t,
                   set of unsigned tsparms=[1,1],
                   set of unsigned o=[1,0,1],
                   boolean with_mean=TRUE) := EMBED(R)
   s=tsparms[1];
   f=tsparms[2];
   p=o[1];
   d=o[2];
   q=o[3];
   x = ts(data=t,start=s,frequency=f);
   x.fit = arima(x, order = c(p,d,q),include.mean=with_mean);
   se <- sqrt(diag(vcov(x.fit)));
   c(as.numeric(coef(x.fit)),as.numeric(se))
ENDEMBED;

my_time_series:=[1.92358795,1.84967079,1.93580744,1.84835519,1.96146137,1.84495426,-0.33036203,-0.01028657,-0.73156235,0.17916775,0.52740827,-1.50743360,-2.61802881,-2.48115519,-1.01724669,-1.34597144,-1.80577117,-1.12053765,-0.60467794,-0.68687905,-0.30468543,-0.61576890,0.42311529,-0.56797383,1.03758339,1.93548533,1.33423134,1.39015369,0.31764500,-1.53488404,-0.30884261,-0.29312203,0.37102962,1.32938875,1.86731347,0.23608561,-1.16260618,0.72402403,0.18307090,-1.79193297,-1.80907498,-0.28903723,0.18409793,-0.65108787,-1.36380549,-2.77205591,-1.02934461,-0.11441797,-0.19874956,-0.59553250,-0.36035824,-0.52799855,0.52520666,-1.40390359,-0.34928168,0.08741312,-0.83949241,0.32950502,0.48757366,-0.55871789,-1.34739852,-0.71375306,-0.15797424,-1.05153583,-2.76705558,-2.60354950,-1.14528648,-0.79097041,1.03169239,0.10578378,0.12849934,-0.27736799,0.58776787,1.46739189,3.21016772,2.94407891,4.00650708,2.21715184,3.63347125,3.27092841,2.57312996,2.12654200,2.09344380,0.68978377,0.52599392,2.25491362,0.39349594,-0.59934390,1.13281277,0.80212553,1.12354523,-0.05115319,0.91600660,0.05864814,-0.15665874,0.20384673,1.76286749,0.71522506,0.17521208,-1.54967522];
ts_0 := DATASET(my_time_series, {TS.Types.t_value dependent});
TS.Types.UniObservation enum_recs(RECORDOF(ts_0) lr, UNSIGNED cnt) := TRANSFORM
  SELF.period := cnt;
  SELF.dependent := lr.dependent;
END;
ts_1 := PROJECT(ts_0, enum_recs(LEFT, COUNTER));

pdq_parms := [1,0,1];
want_const := TRUE;
//
// First run an R implementation of ARIMA
//
r_model_set := ARIMA(my_time_series,,pdq_parms,want_const);
r_model_out := DATASET(r_model_set, {REAL4 coeff});
OUTPUT(r_model_out, NAMED('R_version_coeff_plus'));
TS.Types.Co_efficient make_coef({TS.Types.t_value cv} lr, UNSIGNED cnt) := TRANSFORM
  SELF.cv := lr.cv;
  SELF.lag := cnt;
END;
TS.Types.Model_Parameters makeParms(SET OF UNSIGNED inp, SET OF REAL8 coeff, BOOLEAN want_const) := TRANSFORM
  values := DATASET(coeff, {TS.Types.t_value cv});
  SELF.model_id := 1;
  SELF.degree := inp[2];
  SELF.ar_terms := inp[1];
  SELF.ma_terms := inp[3];
  SELF.constant_term := want_const;
  SELF.ar := PROJECT(values[1..inp[1]], make_coef(LEFT, COUNTER));
  SELF.ma := PROJECT(values[1+inp[1]..inp[1]+inp[3]], make_coef(LEFT, COUNTER));
  SELF.c := IF(want_const, coeff[1+inp[1]+inp[3]], 0.0);
END;
r_model := DATASET(1, makeParms(pdq_parms, r_model_set, want_const));
OUTPUT(r_model, NAMED('R_model'));
r_scored := TS.Diagnosis(ts_1, r_model);
OUTPUT(r_scored, NAMED('R_Scored'));

//
// Now run the ECL version of AIRMA
//
model_spec := DATASET([{1, pdq_parms[2], pdq_parms[1], pdq_parms[3], want_const}], TS.Types.Model_Spec);
model_parms := TS.Estimation(ts_1, model_spec, 100);
OUTPUT(model_parms, NAMED('Model_coeff'));
scored := TS.Diagnosis(ts_1, model_parms);
OUTPUT(scored, NAMED('Model_score'));
