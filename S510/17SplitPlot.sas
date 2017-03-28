options nocenter nonumber nodate ls=80;

proc import datafile='u:\a\Courses\S510\Code\SimulatedSplitPlotData.txt'
     dbms=TAB replace out=d;
run;

proc print data=d (obs=14);
run;


ods listing close;
options orientation=landscape;
ods pdf
    file='u:\a\Courses\S510\LectureNotes\sasoutput.pdf' notoc;

proc mixed;
  class block geno fert;
  model y=geno fert geno*fert / ddfm=satterthwaite;
  random block block*geno;
  estimate 'geno 1'
      intercept 4 geno 4 0 0 fert 1 1 1 1
      geno*fert 1 1 1 1 0 0 0 0 0 0 0 0 / divisor=4 cl;
  estimate 'geno 1 - geno 2' 
      geno 4 -4 0
      geno*fert 1 1 1 1 -1 -1 -1 -1 0 0 0 0 / divisor=4 cl;
  estimate 'geno 1 - geno 2 with no fertilizer' 
      geno 1 -1 0 geno*fert 1 0 0 0 -1 0 0 0 0 0 0 0 / cl;
run;

proc mixed;
  class block geno fert;
  model y=block geno fert geno*fert / ddfm=satterthwaite;
  random block*geno;
  estimate 'geno 1'
      intercept 4 block 1 1 1 1 geno 4 0 0 fert 1 1 1 1
      geno*fert 1 1 1 1 0 0 0 0 0 0 0 0 / divisor=4 cl;
  estimate 'geno 1 - geno 2' 
      geno 4 -4 0
      geno*fert 1 1 1 1 -1 -1 -1 -1 0 0 0 0 / divisor=4 cl;
  estimate 'geno 1 - geno 2 with no fertilizer' 
      geno 1 -1 0 geno*fert 1 0 0 0 -1 0 0 0 0 0 0 0 / cl;
run;


ods pdf close;
ods listing;
run;

