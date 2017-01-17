options nocenter nonumber nodate ls=80;

proc import datafile="c:\z\Courses\S510\Data\RepeatedMeasures.txt"
   dbms=TAB replace out=d;
run;

proc print data=d (obs=14);
run;


*ods listing close;
*options orientation=landscape;
*ods pdf file="c:\z\Courses\S510\Notes\21sasoutput.pdf" notoc;

proc mixed;
  class program subj time;
  model strength=program time program*time;
  random subj(program);
run;


proc mixed;
  class program subj time;
  model strength=program time program*time;
  repeated time / subject=subj type=cs;
run;

proc mixed;
  class program subj time;
  model strength=program time program*time;
  repeated time / subject=subj type=ar(1);
run;

proc mixed;
  class program subj time;
  model strength=program time program*time;
  repeated time / subject=subj type=un;
run;

proc mixed;
  class program subj time;
  model strength=program time program*time;
  repeated time / subject=subj type=ar(1);
  lsmeans program*time / slice=time;
  estimate 'program 1 - 2 at 14 days'
    program 1 -1 0
    program*time 0 0 0 0 0 0  1
                 0 0 0 0 0 0 -1
                 0 0 0 0 0 0  0 / cl; 
  estimate 'program 1 - 3 at 14 days'
    program 1 0 -1
    program*time 0 0 0 0 0 0  1
                 0 0 0 0 0 0  0
                 0 0 0 0 0 0 -1/ cl; 
  estimate 'program 2 - 3 at 14 days'
    program 0 1 -1
    program*time 0 0 0 0 0 0  0
                 0 0 0 0 0 0  1
                 0 0 0 0 0 0 -1/ cl; 
run;

*ods pdf close;
*ods listing;


