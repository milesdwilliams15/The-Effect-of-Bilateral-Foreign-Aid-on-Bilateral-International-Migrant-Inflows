
## Import Data from CSV File ##

# Create File Path
path<-file.path("C:","Users","Miles","Documents","Thesis","Thesis.csv")

# Create Data Frame
ThesisNoNA<-read.csv(path)


## Make New Variables ##

# Create New DV
ThesisNoNA$logrInflowLead<-log(ThesisNoNA$rInflowLead+1)

# Create New IVs 
ThesisNoNA$logdAid<-log(ThesisNoNA$dAid+1)
ThesisNoNA$logrInflow<-log(ThesisNoNA$rInflow+1)
ThesisNoNA$logdGDPcap<-log(ThesisNoNA$dGDP/ThesisNoNA$dPopulation)
ThesisNoNA$logdrDistance<-log(ThesisNoNA$drDistance)
ThesisNoNA$drColony<-ThesisNoNA$drColonly
ThesisNoNA$logdPopulation<-log(ThesisNoNA$dPopulation)
ThesisNoNA$logdGDP<-log(ThesisNoNA$dGDP)
ThesisNoNA$YearDummy<-as.factor(ThesisNoNA$Year)
ThesisNoNA$DonorMat<-model.matrix(~ThesisNoNA$Donor)
ThesisNoNA$RecipientMat<-model.matrix(~ThesisNoNA$Recipient)
ThesisNoNA$YearMat<-model.matrix(~ThesisNoNA$YearDummy)

## Estimate OLS and Tobit Models ##

# OLS Models
ols1<-lm(logrInflowLead~logdAid+dRestrict+rConflict+logdrDistance+drColony+drLanguage+rDemo+logdGDPcap+dUnemployment+logdPopulation+logrInflow+factor(DonorNum)+factor(RecipientNum),data=meanThesis)
ols2<-lm(logrInflowLead~logdAid+dRestrict+rConflict+logdrDistance+drColony+drLanguage+rDemo+logdGDPcap+dUnemployment+logdPopulation+logrInflow+(logdAid*drColony)+factor(DonorNum)+factor(RecipYear),data=ThesisNoNA)
ols3<-lm(logrInflowLead~logdAid+dRestrict+rConflict+logdrDistance+drColony+drLanguage+rDemo+logdGDPcap+dUnemployment+logdPopulation+logrInflow+(logdAid*drLanguage)+factor(DonorNum)+factor(RecipYear),data=ThesisNoNA)
ols4<-lm(logrInflowLead~logdAid+dRestrict+rConflict+logdrDistance+drColony+drLanguage+rDemo+logdGDPcap+dUnemployment+logdPopulation+logrInflow+(logdAid*rDemo)+factor(DonorNum)+factor(RecipYear),data=ThesisNoNA)
ols5<-lm(logrInflowLead~logdAid+dRestrict+rConflict+logdrDistance+drColony+drLanguage+rDemo+logdGDPcap+dUnemployment+logdPopulation+logrInflow+(logdAid*drColony)+(logdAid*drLanguage)+(logdAid*rDemo)+factor(DonorNum)+factor(RecipYear),data=ThesisNoNA)
ols6<-lm(logrInflowLead~logdAid+dRestrict+rConflict+logdrDistance+drColony+drLanguage+rDemo+logdGDPcap+dUnemployment+logdPopulation+logrInflow+(logdAid*logdrDistance)+factor(DonorNum)+factor(RecipYear),data=ThesisNoNA)
ols7<-lm(logrInflowLead~logdAid+dRestrict+rConflict+logdrDistance+drColony+drLanguage+rDemo+logdGDPcap+dUnemployment+logdPopulation+logrInflow+(logdAid*dRestrict)+factor(DonorNum)+factor(RecipYear),data=ThesisNoNA)
ols8a<-lm(logrInflowLead~logdAid+logdAid2+dRestrict+rConflict+logdrDistance+drColony+drLanguage+rDemo+logdGDPcap+dUnemployment+logdPopulation+logrInflow+factor(DonorNum)+factor(RecipYear),data=ThesisNoNA)
ols8b<-lm(logrInflowLead~logdAid+logdAid2+dRestrict+rConflict+logdrDistance+drColony+drLanguage+rDemo+logdGDPcap+dUnemployment+logdPopulation+logrInflow+Donor+Recipient+year,data=ThesisNoNA)

# Tobit Models
library(VGAM)
vglmTob1<-vglm(logrInflowLead~logdAid+dRestrict+rConflict+logdrDistance+drColony+drLanguage+rDemo+logdGDPcap+dUnemployment+logdPopulation+logrInflow+Donor+Recipient+year,data=ThesisNoNA,family=tobit(Lower=0))
vglmTob2<-vglm(logrInflowLead~logdAid+dRestrict+rConflict+logdrDistance+drColony+drLanguage+rDemo+logdGDPcap+dUnemployment+logdPopulation+logrInflow+(logdAid*drColony)+Donor+Recipient+year,data=ThesisNoNA,family=tobit(Lower=0))
vglmTob3<-vglm(logrInflowLead~logdAid+dRestrict+rConflict+logdrDistance+drColony+drLanguage+rDemo+logdGDPcap+dUnemployment+logdPopulation+logrInflow+(logdAid*drLanguage)+Donor+Recipient+year,data=ThesisNoNA,family=tobit(Lower=0))
vglmTob4<-vglm(logrInflowLead~logdAid+dRestrict+rConflict+logdrDistance+drColony+drLanguage+rDemo+logdGDPcap+dUnemployment+logdPopulation+logrInflow+(logdAid*rDemo)+Donor+Donor+Recipient+year,data=ThesisNoNA,family=tobit(Lower=0))
vglmTob5<-vglm(logrInflowLead~logdAid+dRestrict+rConflict+logdrDistance+drColony+drLanguage+rDemo+logdGDPcap+dUnemployment+logdPopulation+logrInflow+(logdAid*drColony)+(logdAid*drLanguage)+(logdAid*rDemo)+Donor+Recipient+year,data=ThesisNoNA,family=tobit(Lower=0))
vglmTob6<-vglm(logrInflowLead~logdAid+dRestrict+rConflict+logdrDistance+drColony+drLanguage+rDemo+logdGDPcap+dUnemployment+logdPopulation+logrInflow+(logdAid*logdrDistance)+Donor+Recipient+year,data=ThesisNoNA,family=tobit(Lower=0))
vglmTob7<-vglm(logrInflowLead~logdAid+dRestrict+rConflict+logdrDistance+drColony+drLanguage+rDemo+logdGDPcap+dUnemployment+logdPopulation+logrInflow+(logdAid*dRestrict)+Donor+Recipient+year,data=ThesisNoNA,family=tobit(Lower=0))
vglmTob8<-vglm(logrInflowLead~logdAid+logdAid2+dRestrict+rConflict+logdrDistance+drColony+drLanguage+rDemo+logdGDPcap+dUnemployment+logdPopulation+logrInflow+Donor+Recipient+year,data=ThesisNoNA,family=tobit(Lower=0))


## Caclulate Clustered Standard Errors for OLS Models ##

# Open Appropriate Package
library(lmtest)
library(plm)

# Caclulate Clustered SEs for ols1
G <- length(unique(ThesisNoNA$Dyad))
N <- length(ThesisNoNA$Dyad)
dfa1 <- (G/(G - 1)) * ((N - 1)/ols1$df.residual)
dyad_c_vcov1 <- dfa1 * vcovHC(ols1, type = "HC0", cluster = "group", adjust = T)
ols1CSE<-coeftest(ols1, vcov = dyad_c_vcov1)

# Caclulate Clustered SEs for ols2
G <- length(unique(ThesisNoNA$Dyad))
N <- length(ThesisNoNA$Dyad)
dfa2 <- (G/(G - 1)) * (N - 1)/ols2$df.residual
dyad_c_vcov2 <- dfa2 * vcovHC(ols2, type = "HC0", cluster = "group", adjust = T)
ols2CSE<-coeftest(ols2, vcov = dyad_c_vcov2)

# Caclulate Clustered SEs for ols3
G <- length(unique(ThesisNoNA$Dyad))
N <- length(ThesisNoNA$Dyad)
dfa3 <- (G/(G - 1)) * (N - 1)/ols3$df.residual
dyad_c_vcov3 <- dfa3 * vcovHC(ols3, type = "HC0", cluster = "group", adjust = T)
ols3CSE<-coeftest(ols3, vcov = dyad_c_vcov3)

# Caclulate Clustered SEs for ols4
G <- length(unique(ThesisNoNA$Dyad))
N <- length(ThesisNoNA$Dyad)
dfa4 <- (G/(G - 1)) * (N - 1)/ols4$df.residual
dyad_c_vcov4 <- dfa4 * vcovHC(ols4, type = "HC0", cluster = "group", adjust = T)
ols4CSE<-coeftest(ols4, vcov = dyad_c_vcov4)

# Caclulate Clustered SEs for ols5
G <- length(unique(ThesisNoNA$Dyad))
N <- length(ThesisNoNA$Dyad)
dfa5 <- (G/(G - 1)) * (N - 1)/ols5$df.residual
dyad_c_vcov5 <- dfa5 * vcovHC(ols5, type = "HC0", cluster = "group", adjust = T)
ols5CSE<-coeftest(ols5, vcov = dyad_c_vcov5)

# Caclulate Clustered SEs for ols6
G <- length(unique(ThesisNoNA$Dyad))
N <- length(ThesisNoNA$Dyad)
dfa6 <- (G/(G - 1)) * (N - 1)/ols6$df.residual
dyad_c_vcov6 <- dfa6 * vcovHC(ols6, type = "HC0", cluster = "group", adjust = T)
ols6CSE<-coeftest(ols6, vcov = dyad_c_vcov6)

# Caclulate Clustered SEs for ols7
G <- length(unique(ThesisNoNA$Dyad))
N <- length(ThesisNoNA$Dyad)
dfa7 <- (G/(G - 1)) * (N - 1)/ols7$df.residual
dyad_c_vcov7 <- dfa7 * vcovHC(ols7, type = "HC0", cluster = "group", adjust = T)
ols7CSE<-coeftest(ols7, vcov = dyad_c_vcov7)

# Caclulate Clustered SEs for ols8a
G <- length(unique(ThesisNoNA$Dyad))
N <- length(ThesisNoNA$Dyad)
dfa8a <- (G/(G - 1)) * (N - 1)/ols8a$df.residual
dyad_c_vcov8a <- dfa8a * vcovHC(ols8a, type = "HC0", cluster = "group", adjust = T)
ols8aCSE<-coeftest(ols8a, vcov = dyad_c_vcov8a)

# Caclulate Clustered SEs for ols8b
G <- length(unique(ThesisNoNA$Dyad))
N <- length(ThesisNoNA$Dyad)
dfa8b <- (G/(G - 1)) * (N - 1)/ols8b$df.residual
dyad_c_vcov8b <- dfa8b * vcovHC(ols8b, type = "HC0", cluster = "group", adjust = T)
ols8bCSE<-coeftest(ols8b, vcov = dyad_c_vcov8b)

# Caclulate Clustered SEs for ols10
G <- length(unique(ThesisNoNA$Dyad))
N <- length(ThesisNoNA$Dyad)
dfa10 <- (G/(G - 1)) * (N - 1)/ols10$df.residual
dyad_c_vcov10 <- dfa10 * vcovHC(ols10, type = "HC0", cluster = "group", adjust = T)
ols10CSE<-coeftest(ols10, vcov = dyad_c_vcov10)

# Caclulate Clustered SEs for ols11
G <- length(unique(ThesisNoNA$Dyad))
N <- length(ThesisNoNA$Dyad)
dfa11 <- (G/(G - 1)) * (N - 1)/ols11$df.residual
dyad_c_vcov11 <- dfa11 * vcovHC(ols11, type = "HC0", cluster = "group", adjust = T)
ols11CSE<-coeftest(ols11, vcov = dyad_c_vcov11)

# Caclulate Clustered SEs for ols12
G <- length(unique(ThesisNoNA$Dyad))
N <- length(ThesisNoNA$Dyad)
dfa12 <- (G/(G - 1)) * (N - 1)/ols12$df.residual
dyad_c_vcov12 <- dfa12 * vcovHC(ols12, type = "HC0", cluster = "group", adjust = T)
ols12CSE<-coeftest(ols12, vcov = dyad_c_vcov12)

## Model Diagnostics ##

# OLS Model 1
yhat <- fitted(ols1)[,1]
rr <- resid(ols1, type = "response")
rp <- resid(ols1, type = "pearson")

ggQQ <- function(LM) # argument: a linear model
{
  y <- quantile(LM$resid[!is.na(LM$resid)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  p <- ggplot(LM, aes(sample=.resid)) +
    stat_qq(alpha = 0.5) +
    geom_abline(slope = slope, intercept = int, color="blue") +
    labs(title="Normal Q-Q Plot")
  
  return(p)
}
p1 <- ggQQ(ols1)
p2 <- qplot(yhat, rp, main = "Fitted vs Pearson Residuals",ylab="Pearson Residuals",xlab="OLS Fitted Values")
p3 <- qplot(ThesisNoNA$logrInflowLead, rp, main = "Actual vs Pearson Residuals",ylab="Pearson Residuals",xlab="Actual Values")
p4 <- qplot(ThesisNoNA$logrInflowLead, yhat, main = "Actual vs Fitted",ylab="OLS Fitted Values",xlab="Actual Values")

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

multiplot(p1,p2,p3,p4,cols=2)

#Tobit Model 1
yhat <- fitted(vglmTob1)[,1]
rr <- resid(vglmTob1, type = "response")
rp <- resid(vglmTob1, type = "pearson")[,1]

par(mfcol = c(2, 3))


qplot(yhat, rr, main = "Fitted vs Residuals")
qqnorm(rr)
plot(yhat, rp, main = "Fitted vs Pearson Residuals")
qqnorm(rp)
qplot(ThesisNoNA$logrInflowLead, rp, main = "Actual vs Pearson Residuals")
qplot(ThesisNoNA$logrInflowLead, yhat, main = "Actual vs Fitted")

library(compactr)

## Estimate Marginal Effect of Bilateral Aid as Bilateral Distance Varies

# pull out coefficient estimates
beta.hat <- coef(ols6) 

# pull out the covariance matrix
cov <- dyad_c_vcov6

# a set of values of logdrDistance to compute the (instantaneous) 
# effect of logdAid 
z0 <- seq(min(ThesisNoNA$logdrDistance), max(ThesisNoNA$logdrDistance), length.out = 1000)

# calculate the instantaneous effect of logdAid as drDistance varies
dy.dx <- beta.hat["logdAid"] + beta.hat["logdAid:logdrDistance"]*z0

# calculate the standard error of each estimated effect
se.dy.dx <- sqrt(cov["logdAid", "logdAid"] 
                 + z0^2*cov["logdAid:logdrDistance", "logdAid:logdrDistance"] 
                 + 2*z0*cov["logdAid", "logdAid:logdrDistance"])

# calculate upper and lower bounds of a 95% CI
upr <- dy.dx + 1.96*se.dy.dx
lwr <- dy.dx - 1.96*se.dy.dx

Y<-c(upr,lwr,dy.dx)
Z<-c(z0,z0,z0)

newdat4<-data.frame(Y,Z)

b<-matrix(1:3,nrow=length(newdat4$Y),ncol=1)
newdat4$b<-sort(b)
newdat4$Lines<-recode(newdat4$b,"1='1. Upper 95% Confidence Interval';2='3. Lower 95% Confidence Interval';3='2. Marginal Effect'")
library(mgcv)
library(ggplot2)
a <- ggplot(data = newdat4, aes(x = Z, y = Y,col=Lines))
a <- a + coord_cartesian(xlim=NULL,ylim=NULL)
a <- a + theme(legend.position=c(0.2,0.2))
a <- a + theme(legend.background=element_rect(fill="white",size=0.5,
                                              linetype="solid",
                                              color="darkblue"))
a <- a + geom_smooth(size=1)
a <- a + ylab("Marginal Effect of log(Bilateral Aid + 1)") + xlab("log(Bilateral Distance")
b1<-a 


## Estimate Marginal Effect of Bilateral Aid as Migration Policy Restrictiveness Varies

# pull out coefficient estimates
beta.hat <- coef(ols7) 

# pull out the covariance matrix
cov <- dyad_c_vcov7

# a set of values of logdrDistance to compute the (instantaneous) 
# effect of logdAid 
z0 <- seq(min(ThesisNoNA$dRestrict), max(ThesisNoNA$dRestrict), length.out = 1000)

# calculate the instantaneous effect of logdAid as drDistance varies
dy.dx <- beta.hat["logdAid"] + beta.hat["logdAid:dRestrict"]*z0

# calculate the standard error of each estimated effect
se.dy.dx <- sqrt(cov["logdAid", "logdAid"] 
                 + z0^2*cov["logdAid:dRestrict", "logdAid:dRestrict"] 
                 + 2*z0*cov["logdAid", "logdAid:dRestrict"])

# calculate upper and lower bounds of a 95% CI
upr <- dy.dx + 1.96*se.dy.dx
lwr <- dy.dx - 1.96*se.dy.dx

Y<-c(upr,lwr,dy.dx)
Z<-c(z0,z0,z0)

newdat3<-data.frame(Y,Z)

b<-matrix(1:3,nrow=length(newdat3$Y),ncol=1)
newdat3$b<-sort(b)
newdat3$Lines<-recode(newdat3$b,"1='1. Upper 95% Confidence Interval';2='3. Lower 95% Confidence Interval';3='2. Marginal Effect'")
library(mgcv)
library(ggplot2)
a <- ggplot(data = newdat3, aes(x = Z, y = Y,col=Lines))
a <- a + coord_cartesian(xlim=NULL,ylim=NULL)
a <- a + theme(legend.position=c(0.2,0.2))
a <- a + theme(legend.background=element_rect(fill="white",size=0.5,
                                              linetype="solid",
                                              color="darkblue"))
a <- a + geom_smooth(size=1)
a <- a + ylab("Marginal Effect of log(Bilateral Aid + 1)") + xlab("Migration Policy Restrictiveness")
b2<-a 

multiplot(b1,b2,cols=2)

## Estimate Marginal Effect of Bilateral Distance as Bilateral Foreign Aid Varies

# pull out coefficient estimates
beta.hat <- coef(ols6) 

# pull out the covariance matrix
cov <- dyad_c_vcov6

# a set of values of logdrDistance to compute the (instantaneous) 
# effect of logdAid 
z0 <- seq(min(ThesisNoNA$logdAid), max(ThesisNoNA$logdAid), length.out = 1000)

# calculate the instantaneous effect of logdAid as drDistance varies
dy.dx <- beta.hat["logdrDistance"] + beta.hat["logdAid:logdrDistance"]*z0

# calculate the standard error of each estimated effect
se.dy.dx <- sqrt(cov["logdrDistance", "logdrDistance"] 
                 + z0^2*cov["logdAid:logdrDistance", "logdAid:logdrDistance"] 
                 + 2*z0*cov["logdrDistance", "logdAid:logdrDistance"])

# calculate upper and lower bounds of a 95% CI
upr <- dy.dx + 1.96*se.dy.dx
lwr <- dy.dx - 1.96*se.dy.dx

Y<-c(upr,lwr,dy.dx)
Z<-c(z0,z0,z0)

newdat2<-data.frame(Y,Z)

b<-matrix(1:3,nrow=length(newdat2$Y),ncol=1)
newdat2$b<-sort(b)
newdat2$Lines<-recode(newdat2$b,"1='1. Upper 95% Confidence Interval';2='3. Lower 95% Confidence Interval';3='2. Marginal Effect'")
library(mgcv)
library(ggplot2)
a <- ggplot(data = newdat2, aes(x = Z, y = Y,col=Lines))
a <- a + coord_cartesian(xlim=NULL,ylim=NULL)
a <- a + theme(legend.position=c(0.2,0.2))
a <- a + theme(legend.background=element_rect(fill="white",size=0.5,
                                              linetype="solid",
                                              color="darkblue"))
a <- a + geom_smooth(size=1)
a <- a + xlab("log(Bilateral Aid + 1)") + ylab("Marginal Effect of log(Bilateral Distance)")
a1<-a 


## Estimate Marginal Effect of Migration Policy Restrictiveness as Bilateral Aid Varies

# pull out coefficient estimates
beta.hat <- coef(ols7) 

# pull out the covariance matrix
cov <- dyad_c_vcov7

# a set of values of logdrDistance to compute the (instantaneous) 
# effect of logdAid 
z0 <- seq(min(ThesisNoNA$logdAid), max(ThesisNoNA$logdAid), length.out = 1000)

# calculate the instantaneous effect of logdAid as drDistance varies
dy.dx <- beta.hat["dRestrict"] + beta.hat["logdAid:dRestrict"]*z0

# calculate the standard error of each estimated effect
se.dy.dx <- sqrt(cov["dRestrict", "dRestrict"] 
                 + z0^2*cov["logdAid:dRestrict", "logdAid:dRestrict"] 
                 + 2*z0*cov["dRestrict", "logdAid:dRestrict"])

# calculate upper and lower bounds of a 95% CI
upr <- dy.dx + 1.96*se.dy.dx
lwr <- dy.dx - 1.96*se.dy.dx

Y<-c(upr,lwr,dy.dx)
Z<-c(z0,z0,z0)

newdat1<-data.frame(Y,Z)

b<-matrix(1:3,nrow=length(newdat1$Y),ncol=1)
newdat1$b<-sort(b)
newdat1$Lines<-recode(newdat1$b,"1='1. Upper 95% Confidence Interval';2='3. Lower 95% Confidence Interval';3='2. Marginal Effect'")
library(mgcv)
library(ggplot2)
a <- ggplot(data = newdat1, aes(x = Z, y = Y,col=Lines))
a <- a + coord_cartesian(xlim=NULL,ylim=NULL)
a <- a + theme(legend.position=c(0.2,0.2))
a <- a + theme(legend.background=element_rect(fill="white",size=0.5,
                                              linetype="solid",
                                              color="darkblue"))
a <- a + geom_smooth(size=1)
a <- a + xlab("log(Bilateral Aid + 1)") + ylab("Marginal Effect of Migration Policy Restrictiveness")
a2<-a 

multiplot(a1,a2,cols=2)

## Make Cool qplots

qplot(logdAid,logrInflowLead,data=ThesisNoNA,geom="smooth",method="gam",formula=y~s(x),
      color=Donor,alpha=I(0.1),xlab="log(Bilateral Aid + 1) at Year t - 1",
      ylab="log(Bilateral Inflow + 1) at Year t")

qplot(logdAid,logrInflowLead,data=ThesisNoNA,geom="smooth",method="gam",formula=y~s(x),
      color=year,alpha=I(0.05),xlab="log(Bilateral Aid + 1) at Year t - 1",
      ylab="log(Bilateral Inflow + 1) at Year t")

qplot(logdAid,logrInflowLead,data=ThesisNoNA,geom="smooth",method="gam",formula=y~s(x),
      facets=Donor~year,alpha=I(0.05),xlab="log(Bilateral Aid + 1) at Year t - 1",
      ylab="log(Bilateral Inflow + 1) at Year t")

qplot(x1,y,data=mydata,geom="smooth",method="gam",formula=y~s(x),
      xlab="log(Bilateral Aid + 1) at Year t - 1",
      ylab="Predicted log(Bilateral Inflow + 1) at Year t")

## Estimate a Quadratic Model Using OLS Estimates and Robust SEs

library(plm)
library(lmtest)

# Extract Coefficients and SEs for Variables of Interest 
# from Quadratic OLS model with Robust Clustered SEs
b1<-ols8CSE[2,1]
b2<-ols8CSE[3,1]
b1.se<-ols8CSE[2,2]
b2.se<-ols8CSE[3,2]

# Create a Vector for X values in Quadratic Equation
x1<-seq.int(0,12,0.1)
x2<-x1^2

# Estimate y|x
y<-(b1*x1)+(b2*x2)

# Estimate Upper and Lower Bounds of y|x
upry<-((b1+b1.se)*x1)+((b2+b2.se)*x2)
lwry<-((b1-b1.se)*x1)+((b2-b2.se)*x2)

upry90<-((b1+(1.64*b1.se))*x1)+((b2+(1.64*b2.se))*x2)
lwry90<-((b1-(1.64*b1.se))*x1)+((b2-(1.64*b2.se))*x2)

upry95<-((b1+(1.96*b1.se))*x1)+((b2+(1.96*b2.se))*x2)
lwry95<-((b1-(1.96*b1.se))*x1)+((b2-(1.96*b2.se))*x2)

# create data frame
mydat1<-data.frame(y,upry,lwry,upry90,lwry90,upry95,lwry95,x1,x2)

# Plot Qudratic Model
plot(mydat1$y~mydat1$x1,ylim=c(0,0.8),type="l",lty=1,lwd=2,
     xlab="log(Bilateral Aid + 1) at Year t - 1",
     ylab="Fitted Values for log(Bilateral Inflow + 1) at Year t",cex.lab=.9)
lines(mydat1$x1,mydat1$upry,lty=3,lwd=1)
lines(mydat1$x1,mydat1$lwry,lty=3,lwd=1)
lines(mydat1$x1,mydat1$upry90,lty=4,lwd=1)
lines(mydat1$x1,mydat1$lwry90,lty=4,lwd=1)
lines(mydat1$x1,mydat1$upry95,lty=8,lwd=1)
lines(mydat1$x1,mydat1$lwry95,lty=8,lwd=1)
legend("topleft",c("Fitted Values","Fitted Values per 1 SE Above/Below the Mean Effect",
         "90% Confidence Interval","95% Confidence Interval"),bty="n",lty=c(1,3,4,8))

# Make Larger Data Frame

Y<-c(y,upry,lwry)
X1<-c(x1,x1,x1)
X2<-c(x2,x2,x2)

mydat2<-data.frame(Y,X1,X2)

# Add Identification

v<-matrix(1:3,nrow=length(Y),ncol=1)

mydat2$v<-sort(v)

library(car)

mydat2$CI<-recode(mydat2$v,"1='2. Predicted Value';2='1. Upper Bound';3='3. Lower Bound'")
  
# Plot Using 'ggplot2' package
library(ggplot2)
library(mgcv)

qplot(X1,Y,data=mydat2,geom="smooth",method="gam",formula=y~s(x),color=CI,
      ylim=c(0,0.8),
      xlab="log(Bilateral Aid + 1) at Year t - 1",
      ylab="Predicted log(Bilateral Inflow + 1) at Year t")


## Estimate a Quadratic Model Using OLS & Tobit Estimates

library(plm)
library(lmtest)
library(VGAM)

# Extract Coefficients for Variables of Interest 
# from Quadratic OLS model with Robust Clustered SEs
b1olsa<-ols8aCSE[2,1]
b2olsa<-ols8aCSE[3,1]
b1olsb<-ols8bCSE[2,1]
b2olsb<-ols8bCSE[3,1]
b1tob<-coef(summary(vglmTob8))[3,1]
b2tob<-coef(summary(vglmTob8))[4,1]

# Create a Vector for X values to use in Quadratic Equation
x1<-seq.int(0,10,0.01)
x2<-x1^2

# Create Quadratic Fitted Values
yolsa<-(b1olsa*x1)+(b2olsa*x2)
yolsb<-(b1olsb*x1)+(b2olsb*x2)
ytob<-(b1tob*x1)+(b2tob*x2)

# create data frame
mydat1<-data.frame(yolsa,yolsb,ytob,x1,x2)

# Plot Qudratic Models
plot(mydat1$yolsa~mydat1$x1,ylim=c(0,0.4),type="l",lwd=3,col="darkblue",
     xlab="log(Bilateral Aid + 1) at Year t - 1",
     ylab="Fitted Values for log(Bilateral Inflow + 1) at Year t",cex.lab=.9)
lines(mydat1$x1,mydat1$yolsb,lwd=3,col="darkgreen")
lines(mydat1$x1,mydat1$ytob,lwd=3,col="darkred")
legend("topleft",c("OLS","OLS w/ Same Fixed Effects as Tobit Model","Tobit"),
       bty="n",lwd=3,col=c("darkblue","darkgreen","darkred"))

# Make Larger Data Frame

Y<-c(yolsa,yolsb,ytob)
X1<-c(x1,x1,x1)
X2<-c(x2,x2,x2)

mydat2<-data.frame(Y,X1,X2)

# Add Identification

v<-matrix(1:3,nrow=length(Y),ncol=1)

mydat2$v<-sort(v)

library(car)

mydat2$Model<-recode(mydat2$v,"1='OLS';2='OLS w/ Same Fixed Effects as Tobit Model';3='Tobit'")

# Plot Using 'ggplot2' package
library(ggplot2)
library(mgcv)

qplot(X1,Y,data=mydat2,geom="smooth",method="gam",formula=y~s(x),color=Model,
      ylim=c(0,0.4),
      xlab="log(Bilateral Aid + 1) at Year t - 1",
      ylab="Fitted Values for log(Bilateral Inflow + 1) at Year t")

a <- ggplot(data = mydat2, aes(x = X1, y = Y,col=Model))
a <- a + coord_cartesian(xlim=c(0,10),ylim=c(0,0.4))
a <- a + geom_smooth(size=1.5)
a <- a + xlab("log(Bilateral Aid + 1) at Year t - 1") + ylab("Fitted Values for log(Bilateral Inflow + 1) at Year t") + ggtitle("Effect of Bilateral Foreign Aid on Bilateral Immigration")
a <- a + theme(legend.position=c(0.23,0.85))
a <- a + theme(legend.background=element_rect(fill="white",size=0.5,
                                              linetype="solid",
                                              color="darkblue"))
ggplotly(a)