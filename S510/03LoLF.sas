options nocenter nodate ls=80;

data d;
  input x y;
  cards;
1 11
1 13
1 9
2 18
2 22
2 23
3 19
3 24
3 22
;
run;

proc glm;
  class x;
  model y=x;
  contrast 'Lack of Linear Fit' x 1 -2 1;
run; 
