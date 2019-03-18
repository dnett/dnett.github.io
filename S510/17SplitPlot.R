#install.packages("lme4")
#install.packages("lmerTest")
library(lme4)
library(lmerTest)

fd = read.delim("https://dnett.github.io/S510/FieldSplitPlotData.txt")
head(fd)

#Define factors and shorten names.

y = fd$y
b = factor(fd$block)
g = factor(fd$geno)
f = factor(fd$fert/50+1)

o = lmer(y ~ g + f + g:f + (1 | b) + (1 | b:g))
summary(o)
anova(o)
ls_means(o)

betahat=fixef(o)
betahat

#The following table shows the cell means
#in terms of the R parameterization.
#############################################################
# 
#   f1=0units f2=50units      f3=100units      f4=150units
#
#g1  mu       mu   +f2         mu   +f3         mu   +f4  
#
#g2  mu+g2    mu+g2+f2+g2f2    mu+g2+f3+g2f3    mu+g2+f4+g2f4
#
#g3  mu+g3    mu+g3+f2+g3f2    mu+g3+f3+g3f3    mu+g3+f4+g3f4
#
#############################################################

#Coefficients for geno 1 marginal mean

C1 = matrix(c(1,
              0, 0,
              1/4, 1/4, 1/4,
              0, 0, 0, 0, 0, 0), nrow=1)

#Coefficients for geno 1 marginal mean - geno 2 marginal mean

C2 = matrix(c(0,
             -1, 0,
              0, 0, 0,
             -1/4, 0, -1/4, 0, -1/4, 0), nrow=1)


#Coefficients for geno 1 - geno 2 with no fertilizer

C3 = matrix(c(0,
             -1, 0,
              0, 0, 0,
              0, 0, 0, 0, 0, 0), nrow=1)

C = rbind(C1, C2, C3)
contest(o, L = C, joint = F, confint = T)

#The degrees of freedom, sums of squares,
#and mean squares from a sequential ANOVA
#table could be used to estimate variance
#components and compute test statistics.

a=anova(lm(y~b+g+b:g+f+g:f))
a

#For example, based on expected mean squares
#presented in slide set 15, an unbiased
#estimator of variance for the whole-plot
#random effects is (MSbg - MSe) / 4. 

MSbg = a[4,3]
MSbg
MSe = a[6,3]
MSe
(MSbg - MSe) / 4

#The F tests and p-values in the
#ANOVA table "a" all use MSe as the
#denominator becuase the lm function
#implies a Gauss-Markov model rather
#than a linear mixed-effects model.
#This is fine for f and g:f, but not
#for the other lines of the ANOVA
#table.

#The correct F statistic for testing
#for genotype main effects is
#MSg/MSbg

MSg = a[2,3]
MSg

MSbg = a[4,3]
MSbg

MSg / MSbg

#When the experiment is balanced, whole-plot-factor analysis
#can be accomplished by computing the average for each
#whole-plot experimental unit and then analyzing those
#averages.

d = aggregate(y, by = list(b, g), FUN = mean)
names(d) = c("block", "geno", "wpaverage")
d

anova(lm(wpaverage ~ block + geno, data = d))

#Now analyze the diet drug split-plot experiment.
#The main difference here is that the whole-plot
#part of the experiment is a CRD rather than an
#RCBD.

ddd = read.delim("https://dnett.github.io/S510/DietDrugSplitPlotData.txt")
head(ddd)
ddd$litter = factor(ddd$litter)
ddd$diet = factor(ddd$diet)
ddd$drug = factor(ddd$drug)

o = lmer(y ~ diet + drug + diet:drug + (1 | litter), data = ddd)
summary(o)
anova(o)
ls_means(o)

#The following table shows the cell means
#in terms of the R parameterization.
#########################################################
#      
#          drug 1       drug 2
#
#diet 1    mu            mu + drug2           
#
#diet 2    mu + diet2    mu + diet2 + drug2 + diet2:drug2
# 
#########################################################

#Coefficients for diet 1 - diet 2 marginal mean

C1 = matrix(c(0, -1, 0, -1/2), nrow = 1)

#Coefficients for drug 1 - drug 2 marginal mean

C2 = matrix(c(0, 0, -1, -1/2), nrow = 1)

#Coefficients for diet 1 - diet 2 for drug 2

C3 = matrix(c(0, -1, 0, -1), nrow=1)

#Coefficients for drug 1 - drug 2 for diet 2

C4 = matrix(c(0, 0, -1, -1), nrow=1)

C = rbind(C1, C2, C3, C4)
contest(o, L = C, joint = F, confint = T)

#The F statistic for diet main effects
#is also given by the ratio
#MSdiet / MSlitter from the appropriate
#sequential ANOVA table. This can be
#shown formally, but MSlitter makes
#sense as the denominator mean square
#because litters are the experimental
#units for the factor diet.

a = anova(
    lm(y ~ diet + litter + drug + diet:drug,
       data=ddd))
a

MSdiet = a[1,3]
dfdiet = a[1,1]
MSlitter = a[2,3]
dflitter = a[2,1]
F = MSdiet / MSlitter
p = 1 - pf(F, dfdiet, dflitter)
F
p

#When the experiment is balanced, whole-plot-factor analysis
#can be accomplished by computing the average for each
#whole-plot experimental unit and then analyzing those
#averages.

d = aggregate(ddd$y, by = list(ddd$diet, ddd$litter), FUN = mean)
names(d) = c("diet", "litter", "wpaverage")
d

anova(lm(wpaverage ~ diet, data = d))
