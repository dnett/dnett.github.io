options nocenter nonumber nodate ls=80;

filename field url "https://dnett.github.io/S510/FieldSplitPlotData.txt";

proc import datafile=field
     dbms=TAB replace out=Field;
run;

proc print data=Field (obs=14);
run;

proc mixed data=Field;
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

proc mixed data=Field;
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

filename dd url "https://dnett.github.io/S510/DietDrugSplitPlotData.txt";

proc import datafile=dd
     dbms=TAB replace out=DietDrug;
run;

proc print data=DietDrug;
run;

proc mixed data=DietDrug;
  class litter diet drug;
  model y=diet drug diet*drug / ddfm=satterthwaite;
  random litter(diet);
  lsmeans diet drug diet*drug;
  estimate 'diet 1 - diet 2' diet 2 -2 diet*drug 1 1 -1 -1 / divisor=2;
  estimate 'drug 1 - drug 2' drug 2 -2 diet*drug 1 -1 1 -1 / divisor=2;
  estimate 'diet 1 - diet 2 for drug 2' diet 1 -1 diet*drug 0 1 0 -1;
  estimate 'drug 1 - drug 2 for diet 2' drug 1 -1 diet*drug 0 0 1 -1;
run;
