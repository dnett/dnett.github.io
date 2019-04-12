#Read the data and create factors.

d=read.delim("http://dnett.github.io/S510/RepeatedMeasures.txt")
head(d)

d$Program = factor(d$Program)
d$Subj = factor(d$Subj)
d$Timef = factor(d$Time)
head(d)

# Compute sample averages

averages = tapply(d$Strength,
    list(d$Time,d$Program),mean)
averages

# Make a profile plot of the averages

x.axis = unique(d$Time)
matplot(c(2,14), c(79,85.7), type="n", 
     xlab="Time (Days)", ylab="Average Strength",
     main= " ")   
matlines(x.axis,averages,type='l',lty=c(1,2,3),lwd=2)
matpoints(x.axis,averages, pch=c(16,17,15),cex=1.5)    
legend(2.1,85.69,legend=c("Program 1 (Rep Increase)",
    'Program 2 (Weight Increase','Program 3 (Control)'),lty=c(1,2,3),
    lwd=2,col=1:3,bty='n')


# Load the nlme package

library(nlme)

# Use the lme function to fit the
# linear mixed-effects model with
# random subject effects.

o.lme = lme(Strength ~ Program*Timef,
  random= ~ 1|Subj, data=d,
  method="REML")

summary(o.lme)

#  Use the gls( ) function to fit a
#  model where the errors have a 
#  compound symmetry covariance structure
#  within subjects. Random effects are
#  not used to induce correlation.

o.cs = gls(Strength ~ Program*Timef,
  data=d,
  correlation = corCompSymm(form=~1|Subj),
  method="REML")
summary(o.cs)

# Try an auto regressive covariance 
# structures across time within 
# subjects

o.ar1 = gls(Strength ~ Program*Timef,
  data=d,
  correlation = corAR1(form=~1|Subj),
  method="REML")
summary(o.ar1)

#  Use an unstructured covariance matrix
#  for observations at different time 
#  points within subjects

o.un = gls(Strength ~ Program*Timef,
  data=d,
  correlation = corSymm(form=~1|Subj),
  weight = varIdent(form = ~ 1|Timef),
  method="REML")
summary(o.un)

#  Compare the fit of various covariance
#  structures.

anova(o.cs, o.un)
anova(o.ar1, o.un)


# Treat time as a continuous variable and
# fit quadratic trends in strength
# over time

o.time = gls(Strength ~ Program+Time+
  Program*Time+I(Time^2)+Program*I(Time^2),
  data=d,
  correlation = corAR1(form=~1|Subj),
  method="REML")
summary(o.time)
anova(o.time)


# To compare the continuous time model to the 
# model where we fit a different mean at each 
# time point, we must compare likelihood values
# instead of REML likelihood values.


o.ar1mle = gls(Strength ~ Program*Timef,
  data=d,
  correlation = corAR1(form=~1|Subj),
  method="ML")


o.timemle = gls(Strength ~ Program+ Time+
     Program*Time+I(Time^2)+Program*I(Time^2),
  data=d,
  correlation = corAR1(form=~1|Subj),
  method="ML")

anova(o.ar1mle, o.timemle)


#  Do not fit different quadratic trends
#  for different programs

o.timemle = gls(Strength ~ Program+Time+
     Program*Time+I(Time^2), data=d,
  correlation = corAR1(form=~1|Subj),
  method="ML")

anova(o.ar1mle, o.timemle)


#  Fit a model with random regression coefficients
#  for individual subjects 

o.timer = lme(Strength ~ Program+Time+
     Program*Time+I(Time^2), 
  random = ~ Time + I(Time^2) | Subj,
  data=d,
  correlation = corAR1(form=~1|Subj),
  control=list(msMaxIter=100),
  method="REML")

o.timer

fixef(o.timer)

ranef(o.timer)

coef(o.timer)

prog=d$Program[seq(7,nrow(d),by=7)]

b=cbind(coef(o.timer)[,1]+
       (prog==2)*coef(o.timer)[,2]+
       (prog==3)*coef(o.timer)[,3],
        coef(o.timer)[,4]+
       (prog==2)*coef(o.timer)[,6]+
       (prog==3)*coef(o.timer)[,7],
        coef(o.timer)[,5])       
x=seq(2,14,by=.1)
plot(c(2,14),c(71,90),ylim=c(71,90),pch=" ",
     ylab="Strength",xlab="Day")
for(i in 1:57){
  lines(x,b[i,1]+b[i,2]*x+b[i,3]*x^2,col=prog[i],lwd=1.9)
}
legend(2.1,75,legend=c("RI program",
    'WI Program','Controls'),lty=1,col=1:3,bty='n',lwd=1.9)

# Do we need the AR(1) structure in the
# random coefficients model?

o.timeru = lme(Strength ~ Program+Time+
     Program*Time+I(Time^2), 
  random = ~ Time + I(Time^2) | Subj,
  data=d,
  method="REML")

anova(o.timer,o.timeru)

# The more complicated model is preferred.
# Keep the AR(1) structure.











