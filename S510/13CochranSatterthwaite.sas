options nocenter nodate ls=80;

data d;
  input trt xu y;
  cards;
1 1 6.4
1 2 4.2
2 1 1.5
2 1 0.9
;
run;

proc mixed method=type1;
  class trt xu;
  model y=trt / ddfm=satterthwaite;
  random xu(trt);
run;



