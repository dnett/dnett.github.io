options nocenter nodate ls=80;

data d;
  input time temp y;
  cards;
3   20  3
3   20  5
3   30 11
3   30 13
3   30 15
6   20  5
6   20  6
6   20  6
6   20  7
6   30 16
;
run;

proc glm;
  class time temp;
  model y=time temp time*temp / ss1 ss2 ss3;
run;
