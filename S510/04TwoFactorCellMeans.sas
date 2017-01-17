options nocenter nodate ls=80;

proc import datafile='C:\z\Courses\S510\Data\dietdrug.txt' 
   dbms=TAB replace out=d;
run;

proc print data=d;
run;

proc mixed;
  class diet drug;
  model weightgain=diet*drug / noint;
  lsmeans diet*drug;
  estimate 'lsmean for diet 1'
            diet*drug 1 1 1 0 0 0 /divisor=3;
  estimate 'simple effect of diet for drug 1'
            diet*drug 1 0 0 -1 0 0;
  estimate 'simple effect of drug 2 vs. drug 3 for diet 2'
            diet*drug 0 0 0 0 1 -1;
  estimate 'diet main effect'
            diet*drug 1 1 1 -1 -1 -1 / divisor=3;
  contrast 'simple effect of diet for drug 1'
            diet*drug 1 0 0 -1 0 0;
  contrast 'simple effect of drug 2 vs. drug 3 for diet 2'
            diet*drug 0 0 0 0 1 -1;
  contrast 'diet main effect'
            diet*drug 1 1 1 -1 -1 -1;
  contrast 'drug main effects'
            diet*drug 1 -1  0  1 -1  0,
            diet*drug 1  0 -1  1  0 -1;
  contrast 'drug main effects also'
            diet*drug 1 -1  0  1 -1  0,
            diet*drug 0  1 -1  0  1 -1;
  contrast 'drug main effects also2'
            diet*drug 2 -2  0  2 -2  0,
            diet*drug 1  1 -2  1  1 -2;
  contrast 'diet-by-drug interactions'
            diet*drug 1 -1  0 -1  1  0,
            diet*drug 1  0 -1 -1  0  1;
run; 

proc mixed;
  class diet drug;
  model weightgain=diet drug diet*drug;
  lsmeans diet drug diet*drug;
  estimate 'lsmean for diet 1'
            intercept 3 diet 3 0 drug 1 1 1 diet*drug 1 1 1 0 0 0/divisor=3;
  estimate 'simple effect of diet for drug 1'
            diet 1 -1 diet*drug 1 0 0 -1 0 0;
  estimate 'simple effect of drug 2 vs. drug 3 for diet 2'
            drug 0 1 -1 diet*drug 0 0 0 0 1 -1;
  estimate 'diet main effect'
            diet 3 -3 diet*drug 1 1 1 -1 -1 -1 / divisor=3;
  contrast 'simple effect of diet for drug 1'
            diet 1 -1 diet*drug 1 0 0 -1 0 0;
  contrast 'simple effect of drug 2 vs. drug 3 for diet 2'
            drug 0 1 -1 diet*drug 0 0 0 0 1 -1;
  contrast 'diet main effect'
            diet 3 -3 diet*drug 1 1 1 -1 -1 -1;
  contrast 'drug main effect'
            drug 2 -2  0 diet*drug 1 -1  0  1 -1  0,
            drug 2  0 -2 diet*drug 1  0 -1  1  0 -1;
  contrast 'diet-by-drug interactions'
            diet*drug 1 -1  0 -1  1  0,
            diet*drug 1  0 -1 -1  0  1;
run; 
