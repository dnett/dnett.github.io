options nocenter nodate ls=80;

proc import datafile='C:\z\Courses\S510\Data\dietdrug.txt' 
   dbms=TAB replace out=d;
run;

proc print data=d;
run;

proc mixed;
  class diet drug;
  model weightgain=diet drug diet*drug;
  lsmeans diet*drug / slice=diet;
  estimate 'drug 1 - drug 2 for diet 1' drug 1 -1  0 diet*drug 1 -1  0  0  0  0 / cl;
  estimate 'drug 1 - drug 3 for diet 1' drug 1  0 -1 diet*drug 1  0 -1  0  0  0;
  estimate 'drug 2 - drug 3 for diet 1' drug 0  1 -1 diet*drug 0  1 -1  0  0  0;
  estimate 'drug 1 - drug 2 for diet 2' drug 1 -1  0 diet*drug 0  0  0  1 -1  0;
  estimate 'drug 1 - drug 3 for diet 2' drug 1  0 -1 diet*drug 0  0  0  1  0 -1;
  estimate 'drug 2 - drug 3 for diet 2' drug 0  1 -1 diet*drug 0  0  0  0  1 -1;
run; 

