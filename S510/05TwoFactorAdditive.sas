options nocenter nodate ls=80;

proc import datafile='C:\z\Courses\S510\Data\dietdrug.txt' 
   dbms=TAB replace out=d;
run;

proc print data=d;
run;

proc mixed;
  class diet drug;
  model weightgain=diet drug;
  lsmeans diet drug / cl;
  estimate 'diet effect' diet 1 -1 / cl;
  estimate 'drug 1 - drug 2' drug 1 -1  0;
  estimate 'drug 1 - drug 3' drug 1  0 -1;
  estimate 'drug 2 - drug 3' drug 0  1 -1;
  contrast 'drug main effects' drug 1 -1  0,
                               drug 1  0 -1;
run;

