options nocenter nonumber nodate ls=80;

data d;
  input trt xu y;
  cards;
1 1 5
1 1 6
1 2 9
1 2 12
2 3 22
2 3 18
2 4 16
2 4 17
;
run;

/*
The following code shows that the default in SAS gets
the denominator degrees of freedom for the trt test wrong.
*/
 
proc mixed;
  class trt xu;
  model y=trt;
  random xu;
run;


/*
All is well if the nesting is explicitly indicated
in the code and/or if ddfm=satterth is used.
*/


proc mixed;
  class trt xu;
  model y=trt;
  random xu(trt);
run;


proc mixed;
  class trt xu;
  model y=trt/ddfm=satterthwaite;
  random xu;
run;

proc mixed;
  class trt xu;
  model y=trt/ddfm=satterthwaite;
  random xu(trt);
run;
