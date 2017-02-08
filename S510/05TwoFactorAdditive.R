########################################
#
#This code shows one way to fit and
#analyze a two-factor experiment in
#R using an additive model.
#
########################################

d=read.delim("http://dnett.github.io/S510/dietdrug.txt")
d

d$diet=factor(d$diet)
d$drug=factor(d$drug)

o=lm(weightgain~diet+drug,data=d)

#The X matrix:

model.matrix(o)

#betahat vector:

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
1, 0, 1/3, 1/3,
1, 1, 1/3, 1/3,
1, 1/2, 0, 0,
1, 1/2, 1, 0,
1, 1/2, 0, 1,
0, -1, 0, 0,
0, 0, -1,  0,
0, 0,  0, -1,
0, 0,  1, -1
),ncol=4,byrow=T)

C

#With this choice of C, you get estimates and
#confidence intervals for the following:

#Row 1: lsmean for diet 1
#Row 2: lsmean for diet 2
#Row 3: lsmean for drug 1
#Row 4: lsmean for drug 2
#Row 5: lsmean for drug 3
#Row 6: diet 1 - diet 2 effect
#Row 7: drug 1 - drug 2 effect
#Row 8: drug 1 - drug 3 effect
#Row 9: drug 2 - drug 3 effect

estimate(o,C)

estimate(o,C)[,-(1:4)]


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
0, 1, 0, 0
), nrow=1, byrow=T)

C

test(o,C)

#Test for drug main effects:

C=matrix(c(
0, 0, -1,  0,
0, 0,  0, -1
), nrow=2, byrow=T)

C

test(o,C)

#######################
#
# Movie Ratings Example
#
#######################

y=c(4,1,3,5,3,3,1)

X=matrix(c(
1,1,0,0,0,1,0,0,
1,1,0,0,0,0,1,0,
1,0,1,0,0,0,1,0,
1,0,1,0,0,0,0,1,
1,0,0,1,0,0,0,1,
1,0,0,0,1,1,0,0,
1,0,0,0,1,0,1,0
),byrow=T,nrow=7)

XX=t(X)%*%X

library(MASS)

XXgi=ginv(XX)

Px=X%*%XXgi%*%t(X)

Px

round(Px,2)

fractions(Px)

yhat=Px%*%y

yhat

bhat=XXgi%*%t(X)%*%y

bhat

C=matrix(c(
1,1,0,0,0,1,0,0,
1,1,0,0,0,0,1,0,
1,1,0,0,0,0,0,1,
1,0,1,0,0,1,0,0,
1,0,1,0,0,0,1,0,
1,0,1,0,0,0,0,1,
1,0,0,1,0,1,0,0,
1,0,0,1,0,0,1,0,
1,0,0,1,0,0,0,1,
1,0,0,0,1,1,0,0,
1,0,0,0,1,0,1,0,
1,0,0,0,1,0,0,1
),byrow=T,nrow=12)

Cbhat=C%*%bhat
Cbhat

M=matrix(Cbhat,nrow=4,byrow=T)

M

apply(M,2,mean)

C=matrix(c(
0,0,0,0,0,1,-1,0,
0,0,0,0,0,1,0,-1,
0,0,0,0,0,0,1,-1
),byrow=T,nrow=3)


Cbhat=C%*%bhat
Cbhat

round(C%*%XXgi%*%t(X),2)

customer=factor(c(1,1,2,2,3,4,4))
movie=factor(c(1,2,2,3,3,1,2))
d=data.frame(customer,movie,y)

d

o=lm(y~customer+movie,data=d)

model.matrix(o)
coef(o)
fitted(o)
resid(o)

o$coe
o$fit
o$res

-o$coe[5]
-o$coe[6]
o$coe[5]-o$coe[6]

C=matrix(c(
0,0,0,0,-1,0,
0,0,0,0,0,-1,
0,0,0,0,1,-1
),byrow=T,nrow=3)

C%*%o$coe

C=matrix(c(
1,0,0,0,0,0,
1,0,0,0,1,0,
1,0,0,0,0,1,
1,1,0,0,0,0,
1,1,0,0,1,0,
1,1,0,0,0,1,
1,0,1,0,0,0,
1,0,1,0,1,0,
1,0,1,0,0,1,
1,0,0,1,0,0,
1,0,0,1,1,0,
1,0,0,1,0,1
),byrow=T,nrow=12)

matrix(C%*%o$coe,nrow=4,byrow=T)

