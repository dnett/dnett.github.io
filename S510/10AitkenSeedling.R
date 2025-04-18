d=read.delim("http://dnett.github.io/S510/SeedlingDryWeight.txt")
d

y=d[,3]
geno=factor(d[,1])
count=d[,4]

X=matrix(model.matrix(~geno),nrow=8)
X

V=diag(1/count)

#Compute V^{-.5}

Vn.5=diag(sqrt(count))

#In general, we could compute V^{-.5} as follows:
#
# e=eigen(V)
# P=e$vectors
# lambda=e$values
# Vn.5=P%*%diag(1/sqrt(lambda))%*%t(P)

#Now transform y and X to z and W. 

z=Vn.5%*%y

W=Vn.5%*%X

o=lm(z~0+W)
summary(o)

#Because V is diagonal in this case, we can 
#alternatively analyze using lm and the weights argument.

o2=lm(y~geno,weights=count)
summary(o2)

#The unweighted (OLS) analysis is inferior in this case.
#The OLS estimator of beta is still unbiased, but it's
#variance is larger than that of the GLS estimator.
#OLS inferences regarding beta are not, in general, valid.

o3=lm(y~geno)
summary(o3)
