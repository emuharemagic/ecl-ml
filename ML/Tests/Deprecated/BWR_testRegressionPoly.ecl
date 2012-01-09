IMPORT * FROM ML;
/*
 Poly Tests:
		Use different seed column distributions dx (1,2,3..., 10,20,30..., 100, 200, 300..., 1, 10, 100...)
    Create multiple columns Yi from it by applying log, x, xlog,... functions to it
    Create Linear Regression Model using dx as a dependent variable and Y as an independent variable
    Expected result: if Yi = dx(log) then beta result for Yi should have beta1 close to 1 and all other beta
     values close to zero 
*/
d1 := dataset([{1,1,1},{2,1,2},{3,1,3},{4,1,4.0}, {5,1,5.0},{6,1,6.0},{7,1,7.0},{8,1,8.0}, {9,1,9.0}, {10,1,10}], Types.NumericField );                                                
d2 := dataset([{1,1,10},{2,1,20},{3,1,30},{4,1,40.0}, {5,1,50.0},{6,1,60.0},{7,1,70.0},{8,1,80.0}, {9,1,90.0}, {10,1,100}], Types.NumericField );    
d3 := dataset([{1,1,100},{2,1,200},{3,1,300},{4,1,400}, {5,1,500},{6,1,600},{7,1,700},{8,1,800}, {9,1,900}, {10,1,1000}], Types.NumericField ); 		
d4 := dataset([{1,1,1},{2,1,10},{3,1,100},{4,1,1000}, {5,1,10000},{6,1,100000},{7,1,1000000},{8,1,10000000}, {9,1,100000000}, {10,1,1000000000}], Types.NumericField ); 										

poly1 := Generate.toPoly(d1,6);
poly2 := Generate.toPoly(d2,6);
poly3 := Generate.toPoly(d3,6);
poly4 := Generate.toPoly(d4,6);

r1:= Regression.Poly(d1,poly1).Beta;
OUTPUT(r1(id = Generate.tp_Method.LogX), named('LogX_Beta'));
OUTPUT(Regression.Poly(d1,poly1(number=Generate.tp_Method.LogX)).RSquared, named('LogX_RSquared'));

r2:= Regression.Poly(d2,poly2).Beta;
OUTPUT(r2(id = Generate.tp_Method.X), named('X_Beta'));
OUTPUT(Regression.Poly(d2,poly2(number=Generate.tp_Method.X)).RSquared, named('X_RSquared'));

r3:=Regression.Poly(d3,poly3).Beta;
OUTPUT(r3(id = Generate.tp_Method.XLogX), named('XLogX_Beta'));
OUTPUT(Regression.Poly(d3,poly3(number=Generate.tp_Method.XLogX)).RSquared, named('XLogX_RSquared'));

r4:=Regression.Poly(d4,poly4).Beta;
OUTPUT(r4(id = Generate.tp_Method.XX), named('XX_Beta'));
OUTPUT(Regression.Poly(d4,poly4(number=Generate.tp_Method.XX)).RSquared, named('XX_RSquared'));


