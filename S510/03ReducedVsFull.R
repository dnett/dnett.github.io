x=rep(1:3,each=3)
x

y=c(11,13,9,18,22,23,19,24,22)

plot(x,y,pch=16,col=4,xlim=c(.5,3.5),
     xlab="Fertilizer Amount",
     ylab="Yield",axes=F,cex.lab=1.5)
axis(1,labels=1:3,at=1:3)
axis(2)
box()

X0=model.matrix(~x)
X0

X=model.matrix(~0+factor(x))
X


proj=function(x){
  x%*%ginv(t(x)%*%x)%*%t(x)
}

library(MASS)
PX0=proj(X0)
PX=proj(X)

Fstat=(t(y)%*%(PX-PX0)%*%y/1)/
      (t(y)%*%(diag(rep(1,9))-PX)%*%y/(9-3))
Fstat

pvalue=1-pf(Fstat,1,6)
pvalue

reduced=lm(y~x)
full=lm(y~0+factor(x))

rvsf=function(reduced,full)
{
  sser=deviance(reduced)
  ssef=deviance(full)
  dfer=reduced$df
  dfef=full$df
  dfn=dfer-dfef
  Fstat=(sser-ssef)/dfn/
        (ssef/dfef)
  pvalue=1-pf(Fstat,dfn,dfef)
  list(Fstat=Fstat,dfn=dfn,dfd=dfef,pvalue=pvalue)
}

rvsf(reduced,full)

anova(reduced,full)

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

test(full,matrix(c(1,-2,1),nrow=1))