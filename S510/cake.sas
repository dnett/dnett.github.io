options nocenter nonumber nodate ls=80;

proc import datafile="c:\z\Courses\S510\Homework\cake.txt"
   dbms=TAB replace out=d;
run;

proc print data=d;
run;

proc mixed;
  class cr fr baker judge;
  model y=cr fr cr*fr / ddfm=satterth;
  random baker(cr) fr*baker(cr) judge;
  lsmeans cr*fr / pdiff adjust=tukey;
run;

/*
proc mixed method=type1;
  class cr fr baker judge;
  model y=cr fr cr*fr;
  random baker(cr) fr*baker(cr) judge;
run;
*/

