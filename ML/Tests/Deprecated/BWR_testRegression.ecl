IMPORT * FROM ML;

/*
Healthy  Breakfast: a subset of data about different brands of cerial
fat : grams of fat
sugars: grams of sugars
rating: a rating of cerial
*/
CerialRec := RECORD
             UNSIGNED rid;
					   REAL fat; 
             REAL sugars;
             REAL rating;
 END;
                
cerial := DATASET([{1,1,6,68.402973},
									{2,5,8,33.983679},
									{3,1,5,59.425505},
									{4,0,0,93.704912},
									{5,2,8,34.384843},
									{6,2,10,29.509541},
									{7,0,14,33.174094},
									{8,2,8,37.038562},
									{9,1,6,49.120253},
									{10,0,5,53.313813},
									{11,2,12,18.042851},
									{12,2,1,50.764999},
									{13,3,9,19.823573},
									{14,2,7,40.400208},
									{15,1,13,22.736446},
									{16,0,3,41.445019},
									{17,0,2,45.863324},
									{18,0,12,35.782791},
									{19,1,13,22.396513},
									{20,3,7,40.448772},
									{21,0,0,64.533816},
									{22,0,3,46.895644},
									{23,1,10,36.176196},
									{24,0,5,44.330856},
									{25,1,13,32.207582},
									{26,0,11,31.435973},
									{27,0,7,58.345141},
									{28,2,10,40.917047},
									{29,0,12,41.015492},
									{30,1,12,28.025765},
									{31,0,15,35.252444},
									{32,1,9,23.804043},
									{33,1,5,52.076897},
									{34,0,3,53.371007},
									{35,3,4,45.811716},
									{36,2,11,21.871292},
									{37,1,10,31.072217},
									{38,0,11,28.742414},
									{39,1,6,36.523683},
									{40,1,9,36.471512},
									{41,1,3,39.241114},
									{42,2,6,45.328074},
									{43,1,12,26.734515},
									{44,1,3,54.850917},
									{45,3,11,37.136863},
									{46,3,11,34.139765},
									{47,2,13,30.313351},
									{48,1,6,40.105965},
									{49,1,9,29.924285},
									{50,2,7,40.69232},
									{51,0,2,59.642837},
									{52,2,10,30.450843},
									{53,1,14,37.840594},
									{54,0,3,41.50354},
									{55,0,0,60.756112},
									{56,0,0,63.005645},
									{57,1,6,49.511874},
									{58,2,-1,50.828392},
									{59,1,12,39.259197},
									{60,2,8,39.7034},
									{61,0,6,55.333142},
									{62,0,2,41.998933},
									{63,0,3,40.560159},
									{64,0,0,68.235885},
									{65,0,0,74.472949},
									{66,0,0,72.801787},
									{67,1,15,31.230054},
									{68,0,3,53.131324},
									{69,0,5,59.363993},
									{70,1,3,38.839746},
									{71,1,14,28.592785},
									{72,1,3,46.658844},
									{73,1,3,39.106174},
									{74,1,12,27.753301},
									{75,1,3,49.787445},
									{76,1,3,51.592193},
									{77,1,8,36.187559}],CerialRec);
                                                                                                                
// Turn into regular NumericField file (with continuous variables)
ToField(cerial,o);
X := O(Number in [1,2]); // Pull out fat and sugars
Y := O(Number IN [3]); // See if you can predict the rating

// Matrix inverse test (implicit LU decomposition test)
mX := Types.ToMatrix(X);
mTst := Mat.Mul(Mat.Trans(mX), mX);
invTest := Mat.Thin(Mat.RoundDelta(Mat.Mul(Mat.Inv(mTst),mTst)));
OUTPUT(invTest, named('InvTest_identityMatrix'));

// Cholesky decomposition test	
mLL := Mat.Decomp.Cholesky(mTst);
choleskyTst := Mat.Thin(Mat.RoundDelta(Mat.Sub(Mat.Mul(mLL, Mat.Trans(mLL)),mTst)));
OUTPUT(choleskyTst, named('CholeskyTest_zeroMatrix'));

// Beta test: LU vs Cholesky decomposition
ols := Regression.OLS(X,Y);
cholesky := Mat.Thin(Mat.RoundDelta(Types.ToMatrix(ols.Beta())));
lu := Mat.Thin(Mat.RoundDelta(Types.ToMatrix(ols.Beta(ols.MDM.LU))));
OUTPUT(choleskyTst, named('RegressionBetaTest_zeroMatrix'));

// http://www.stat.yale.edu/Courses/1997-98/101/anovareg.htm
//Rating = 61.1 - 2.21 Sugars - 3.07 Fat
OUTPUT(cholesky, named('RegressionBetaTest_betaResult'));

// r^2=0.622, indicating that 62.2% of the variability
// in the "Ratings" variable is explained by the "Sugars" and "Fat" variables
rsquared := ols.RSquared;
OUTPUT(rsquared, named('RegressionRsquaredTest'));

/*
The degrees of freedom are provided in the "DF" column, the calculated sum of squares terms are 
provided in the "SS" column, and the mean square terms are provided in the "MS" column.

Analysis of Variance

Source       DF          SS          MS         F        P
Regression    2      9325.3      4662.6     60.84    0.000
Error        74      5671.5        76.6
Total        76     14996.8

*/
anova := ols.Anova;
OUTPUT(anova, named('RegressionAnovaTest'));


