#Example code for simulating data from our
#classic split plot example.

block=factor(rep(1:4,each=12))
geno=factor(rep(rep(1:3,each=4),4))
x=rep(seq(0,150,by=50),12)
fert=factor(x)

X=model.matrix(~geno+x+I(x^2)+geno:x)
beta=c(125,15,-10,.4,-0.0015,0,.2)

Z1=model.matrix(~0+block)
Z2=model.matrix(~0+geno:block)
Z=cbind(Z1,Z2)

#The code below generates the random effects
#and random errors and assembles the response
#vector. The function set.seed is used to
#control the random number generator so that
#the same random effects and errors will be
#generated each time this code is called.

set.seed(532)
u=c(rnorm(4,0,6),rnorm(12,0,7))
e=rnorm(48,0,6)
y=X%*%beta+Z%*%u+e
y=round(y,1)

d=data.frame(block,geno,fert,y)
d

#ANOVA-based analysis

o=lm(y~block+geno+block:geno+fert+geno:fert)
anova(o)
a=as.matrix(anova(o))

#ANOVA estimates of variance components:

#Estimate of sigma^2_e

MSE=a[6,3]
MSE        

#Estimate of sigma^2_w
 
MSBlockGeno=a[4,3]

(MSBlockGeno-MSE)/4   

#Save the square roots of these estimates
#for comparison with alternative estimates
#computed later.

sige=sqrt(MSE)
sigw=sqrt((MSBlockGeno-MSE)/4)

#F test for genotype main effects

MSGeno=a[2,3]
Fstat=MSGeno/MSBlockGeno
Fstat

pval=1-pf(Fstat,a[2,1],a[4,1])
pval

#95% confidence interval for geno 2 - geno 1

gmeans=tapply(y,geno,mean)
gmeans

est=gmeans[2]-gmeans[1]
names(est)=NULL

#We showed previously that the variance of
#the difference between genotype means
#is 2*E(MS_block*geno)/(nblocks*nferts)

#Thus, we compute a standard error as

se=sqrt(2*MSBlockGeno/(4*4))

lower=est-qt(.975,a[4,1])*se
upper=est+qt(.975,a[4,1])*se

c(estimate=est,se=se,lower=lower,upper=upper)

#REML analysis via lme

library(nlme)

#Below I create f and g factors to shorten
#code and the names that R assigns to the
#elements of beta hat.

f=factor((x+50)/50)
f

g=geno

o=lme(y~g*f,random=~1|block/g)
o

#Note that the REML estimates of
#standard deviation match the
#ANOVA estimates computed
#from lm output.

sigw
sige

#The ANOVA table computed from lme output
#automatically gives the correct tests for
#genotype, fertilizer, and
#genotype by fertilizer interaction for
#the balanced data case.

anova(o)

#The GLS estimate of the fixed effect
#parameter beta is obtained as follows.

fixed.effects(o)

#The estimated variance covariance matrix of
#the GLS estimator is obtained as follows.

vcov(o) 

#We can use the estimate of beta and it's
#variance covariance matrix to construct
#test statistics and confidence intervals
#for testable and estimable quantities.
#This will work in the unbalanced case
#as well. However, care must be taken to
#assign the appropriate degrees of freedom
#and inferences will be only approximate
#for the unbalanced case and whenever
#variance estimates depend on more than
#one mean square.

#For example, here is a revised version of
#the confidence interval function that we
#used for The normal theory Gauss-Markov
#linear model.  The test function we
#previously used could be modified in a
#similar way.

ci=function(lmeout,C,df,a=0.05)
{
  b=fixed.effects(lmeout)
  V=vcov(lmeout)
  Cb=C%*%b
  se=sqrt(diag(C%*%V%*%t(C)))
  tval=qt(1-a/2,df)
  low=Cb-tval*se
  up=Cb+tval*se
  m=cbind(C,Cb,se,low,up)
  dimnames(m)[[2]]=c(paste("c",1:ncol(C),sep=""),
             "estimate","se",
             paste(100*(1-a),"% Conf.",sep=""),
             "limits")
  m
}

#Suppose would like a confidence interval
#for the genotype 2 mean minus the
#genotype 1 mean while averaging over the
#levels of fertilizer.

#The following table shows the cell means
#in terms of the R parameterization.

#                           f
############################################################# 
#     1            2                3                4
#############################################################
# g
#
# 1  mu       mu   +f2         mu   +f3         mu   +f4  
#
# 2  mu+g2    mu+g2+f2+g2f2    mu+g2+f3+g2f3    mu+g2+f4+g2f4
#
# 3  mu+g3    mu+g3+f2+g3f2    mu+g3+f3+g3f3    mu+g3+f4+g3f4
#
#############################################################

#The average of row 2 minus the average of row 1 is
#
#  g2 + g2f2/4 + g2f3/4 + g2f4/4
#

C=matrix(c(0,1,0,0,0,0,.25,0,.25,0,.25,0),nrow=1)

#Note that interval produced below matches
#the interval computed from the lm output.

ci(o,C,6)

#We can also come up with the coefficients in
#the balanced case using the following code.

X=model.matrix(o)

apply(X[g==2,],2,mean)-apply(X[g==1,],2,mean)

#
#We can obtain the best linear unbiased predictions
#(BLUPs) for the random effects as follows.

random.effects(o)

#Because we have simulated the data, we can
#compare the predictions with the true values
#of the random effects.

cbind(u,unlist(random.effects(o)))

#The same sorts of analyses could be carried out
#using lmer.

library(lme4)
o=lmer(y~g*f+(1|block)+(1|block:g))
o
