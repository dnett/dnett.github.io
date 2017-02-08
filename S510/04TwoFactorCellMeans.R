########################################
#
#This code shows one way to fit and
#analyze a two-factor experiment in
#R using a cell means model.
#
########################################

d=read.delim("http://dnett.github.io/S510/dietdrug.txt")
d

d$diet=factor(d$diet)
d$drug=factor(d$drug)

o=lm(weightgain~0+drug:diet,data=d)

#The X matrix:

model.matrix(o)

#betahat vector which contains the treatment means:

coef(o)

#Estimated variance of betahat:

vcov(o)

#The degrees of freedom for error:

o$df

#The following function can be used to obtain
#confidence intervals for each element of an
#estimable C*beta.

estimate=function(lmout,C,a=0.05)
{
  b=coef(lmout)
  V=vcov(lmout)
  df=lmout$df
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

C=matrix(c(
1/3,1/3,1/3,0,0,0,
1,0,0,-1,0,0,
0,0,0,0,1,-1,
1/3,1/3,1/3,-1/3,-1/3,-1/3
),nrow=4,byrow=T)

C

#With this choice of C, you get estimates and
#confidence intervals for the following:

#Row 1: lsmean for diet 1
#Row 2: simple effect of diet for drug 1
#Row 3: simple effect of drug 2 vs. drug 3 for diet 2
#Row 4: diet main effect

estimate(o,C)

#The following function takes the
#result of a an lm fit and performs
#a test of C beta = d for specified
#C matrix and d vector.

test=function(lmout,C,d=0){
  b=coef(lmout)
  V=vcov(lmout)
  dfn=nrow(C)
  dfd=lmout$df
  Cb.d=C%*%b-d
  Fstat=drop(t(Cb.d)%*%solve(C%*%V%*%t(C))%*%Cb.d/dfn)
  pvalue=1-pf(Fstat,dfn,dfd)
  list(Fstat=Fstat,pvalue=pvalue)
}

#Test for diet main effect:

C=matrix(c(
1, 1,  1,  -1, -1,  -1
), nrow=1, byrow=T)

C

test(o,C)

#Test for drug main effect:

C=matrix(c(
1, -1,  0,  1, -1,  0,
1,  0, -1,  1,  0, -1
), nrow=2, byrow=T)

C

test(o,C)

#Test for diet-by-drug interactions:

C=matrix(c(
1, -1,  0,  -1, 1,  0,
1,  0, -1,  -1,  0, 1
), nrow=2, byrow=T)

C

test(o,C)

########################################
#
#This code shows another way to fit and
#analyze a two-factor experiment in
#R using a cell means model.
#
########################################

o=lm(weightgain~diet+drug+diet:drug,data=d)

model.matrix(o)

#Estimate treatment means

C=matrix(c(
1,  0,  0,  0,  0,  0,
1,  0,  1,  0,  0,  0,
1,  0,  0,  1,  0,  0,
1,  1,  0,  0,  0,  0,
1,  1,  1,  0,  1,  0,
1,  1,  0,  1,  0,  1
),nrow=6,byrow=T)

estimate(o,C)

#Test for diet main effect:

C=matrix(c(
0, -1,  0,  0, -1/3,  -1/3
), nrow=1, byrow=T)

C

test(o,C)

#Test for drug main effect:

C=matrix(c(
0,  0,  1,  0, 1/2,  0,
0,  0,  0,  1,  0,  1/2
), nrow=2, byrow=T)

C

test(o,C)

#Test for diet-by-drug interactions:

C=matrix(c(
0,0,0,0,1,0,
0,0,0,0,0,1
), nrow=2, byrow=T)

C

test(o,C)
