#------------------------------------------------
#------------------------------------------------
# 'Ideology and Participation'
# written under version Version 0.99.486 of RStudio
# Data description, frequency analysis, and aggregate delegate analysis (both length and count)
# Origiinally written by DAG on 09/2/2014
# Cleaned and edited for purposes of positing for reproduction on 10/29/2017
#------------------------------------------------
#------------------------------------------------

#------------------------------------------------
# load in the needed libraries
library(stargazer) # for easy and clean LaTeX tables
library(ggplot2) # for figure creation
library(gridExtra) # for grid.arrange function
library(robust) # for robust standard errors
library(lmtest) # for breusch-pagan test
library(car) # for hccm variance/covariance correction
library(tseries) # for harque bera test
library(maxLik) # for using a self-coded poisson estimator





#------------------------------------------------
# optional, set working directory
setwd("~")

#------------------------------------------------
# load in the data
data = read.csv("Convention_Verbosity_Data_No_GWashington.csv", header = TRUE)
#note that this data does not contain George Washington (VA), George Wythe (VA), or William Houston (NJ)


#------------------------------------------------
#------------------------------------------------
# creating summary statitistics
# found in supplemental materials
aggregate_analysis_vars = cbind(data$verbnumb,
                                data$verblength,
                                data$verb.type,
                                data$ideology,
                                data$ideology_sq,
                                data$ab_ideology,
                                data$age,
                                data$college,
                                data$legexp,
                                data$days_present)
print(apply(aggregate_analysis_vars,2,summary))

#------------------------------------------------
#------------------------------------------------
# Dependent variable distributions

# Distribution of Number of Speeches
# Figure 4 in the Supplemental Materals
verbnumb_density = ggplot(data, aes(x=verbnumb))
verbnumb_density = verbnumb_density + geom_histogram(binwidth=10,
                                                     colour="black",
                                                     fill="gray80",
                                                     size=1)
verbnumb_density= verbnumb_density +
  xlab("Total # of Speeches") +
  ylab("Count") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(colour= "black"),
        axis.text.y = element_text(colour= "black"))
print(verbnumb_density)


# Distribution of Length of Speeches
# Figure 5 in the Supplemental Materals
# Length of speech
verblength_density = ggplot(data, aes(x=verblength/10))
verblength_density = verblength_density + geom_density(colour="black",
                                                         fill="gray80",
                                                         size=1)
verblength_density = verblength_density +
  xlab("Total Length of Speeches") +
  ylab("Density") + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(colour= "black"),
        axis.text.y = element_text(colour= "black"))
print(verblength_density)

#density of logverblength
logverblength_density = ggplot(data, aes(x=data$log.verblength/10))
logverblength_density = logverblength_density + geom_density(colour="black",
                                                               fill="gray80",
                                                               size=1)
logverblength_density = logverblength_density +
  xlab("Log of Total Speech") +
  ylab("Density") + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(colour= "black"),
        axis.text.y = element_text(colour= "black"))
print(logverblength_density)

#get both verblengths plots on one
grid.arrange(verblength_density,logverblength_density, ncol=1)


# Distribution of Verbosity Type
# Figure 6 in the Supplemental Materals
verb.type.hist= ggplot(data, aes(x=as.factor(verb.type)))
verb.type.hist = verb.type.hist + stat_count(colour="black",
                                                 fill="gray80",
                                                 size=1)
verb.type.hist= verb.type.hist + xlab("Verbosity Type") + ylab("Count")
verb.type.hist<- verb.type.hist + theme(axis.line = element_line(colour = "black"),
                                        panel.grid.major = element_blank(),
                                        panel.grid.minor = element_blank(),
                                        panel.background = element_blank(),
                                        axis.text.x = element_text(colour= "black"),
                                        axis.text.y = element_text(colour= "black"))
verb.type.hist<- verb.type.hist +
  scale_x_discrete(limits=c("1","2","3"),
                   labels=c("Low", "Medium", "High"))
print(verb.type.hist)

#------------------------------------------------
#------------------------------------------------
# Functional form plots from the Supplmentary Materials

# create ordered data for scatter plots
data2 <- data[order(data$ab_ideology),]
data3 <- data[order(data$ideology),]

# to reproduce Figure 7 from the supplementary materials
#ideology + ideology^2 X verbosity length scatter plot
ideolXverb.line.plot2 = ggplot(data3, aes(x=ideology, y=verblength))
ideolXverb.line.plot2 = ideolXverb.line.plot2 + geom_point(colour= "royalblue4")
ideolXverb.line.plot2 = ideolXverb.line.plot2 +
  stat_smooth(method="lm",
              formula=y~poly(x,2),
              se=T,
              colour="black")
ideolXverb.line.plot2 = ideolXverb.line.plot2 +
  xlab("Ideology") +
  ylab("Total Speech Length") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(colour= "black"),
        axis.text.y = element_text(colour= "black"))                              
print(ideolXverb.line.plot2)


# absolute ideology X verbosity length scatter plot
ab_ideolXverb.line.plot2 = ggplot(data2, aes(x=ab_ideology, y=verblength))
ab_ideolXverb.line.plot2 = ab_ideolXverb.line.plot2 +
  geom_point(colour= "royalblue1")
ab_ideolXverb.line.plot2 = ab_ideolXverb.line.plot2 +
  stat_smooth(method="lm",
              formula=y~poly(x,1),
              se=T,
              colour="black")
ab_ideolXverb.line.plot2 = ab_ideolXverb.line.plot2 +
  xlab("Absolute Ideology") +
  ylab("Total Speech Length") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(colour= "black"),
        axis.text.y = element_text(colour= "black")) 
print(ab_ideolXverb.line.plot2)

#put both on the same plot
grid.arrange(ab_ideolXverb.line.plot2,ideolXverb.line.plot2, ncol=1)

# to reproduce Figure 8 from the supplementary materials
# absolute ideology  X verbosity number scatter plot
ab_ideolXverb.line.plot = ggplot(data2, aes(x=ab_ideology, y=verbnumb))
ab_ideolXverb.line.plot = ab_ideolXverb.line.plot +
  geom_point(colour= "royalblue1")
ab_ideolXverb.line.plot = ab_ideolXverb.line.plot +
  stat_smooth(method="lm",
              formula=y~poly(x,1),
              se=T,
              colour="black")
ab_ideolXverb.line.plot = ab_ideolXverb.line.plot +
  xlab("Absolute Ideology") +
  ylab("Total # of Speeches") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(colour= "black"),
        axis.text.y = element_text(colour= "black")) 
print(ab_ideolXverb.line.plot)

# ideology + ideology^2 X verbosity number scatter plot
ideolXverb.line.plot = ggplot(data3, aes(x=ideology, y=verbnumb))
ideolXverb.line.plot = ideolXverb.line.plot + geom_point(colour= "royalblue4")
ideolXverb.line.plot = ideolXverb.line.plot +
  stat_smooth(method="lm",
              formula=y~poly(x,2),
              se=T,
              colour="black")
ideolXverb.line.plot = ideolXverb.line.plot +
  xlab("Ideology") +
  ylab("Total # of Speeches") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(colour= "black"),
        axis.text.y = element_text(colour= "black"))                              
print(ideolXverb.line.plot)

# to get both on the same plot
grid.arrange(ab_ideolXverb.line.plot, ideolXverb.line.plot, ncol=1)

#------------------------------------------------
#------------------------------------------------
# Frequency analysis from the suplementary materials
# To reproduce Table 6

#get the mean and sd of the ideology variable
sd.ab.ideol=sd(data$ab_ideology)
mean.ab.ideol<-mean(data$ab_ideology)

#make an empty bin for the variable to write into
ab_ideology_type=matrix(c(NA),nrow(data),1)  

#use a loop to move through all the observations
#use a nested IF to determine if the obeservation is a hi, medium, or low type
#hi, med, and low are based on mean +- sd.
#hi=3, med=2, low=1
for (i in 1:length(data$ab_ideology)){
  if (data$ab_ideology[i] >= (mean.ab.ideol + .45*sd.ab.ideol)) {
    ab_ideology_type[i] = 3
  } else if ((mean.ab.ideol + .45*sd.ab.ideol) >= data$ab_ideology[i] &
             data$ab_ideology[i] >= (mean.ab.ideol - .45*sd.ab.ideol)){
    ab_ideology_type[i] = 2
  }  else
    ab_ideology_type[i] = 1
}
#write the now filled matrix as a new variable into the data
data$ab_ideology_type = ab_ideology_type


#cross tabs
xmod<-table(data$verb.type,data$ab_ideology_type)
ftable(xmod)

# chi square test
chisq.test(xmod, simulate.p.value=T, B=999)

#------------------------------------------------
#------------------------------------------------
# Code to recreate and assess Table 1
# Quadratic Model
quadratic_ols<-lm(verblength~ideology+
             ideology_sq+
             age+
             college+
             legexp+
             days_present,
           data=data)
print(summary(quadratic_ols))

#breusch-pagan test for heteroskedasticity: null is homoskedasticity
bptest(quadratic_ols)

#RSEs
coeftest(quadratic_ols, vcov=hccm)
quadratic_ols_robust<-lmRob(verblength~ideology+
                 ideology_sq+
                 age+
                 college+
                 legexp+
                 days_present,
               data=data)

print(summary(quadratic_ols_robust))

#normality of residuals 
#(Jarque-Bera Test, null is normal)
jarque.bera.test(quadratic_ols$residuals)

#Shapiro-Wilk test: null is normal
shapiro.test(quadratic_ols$residuals)

hist(quadratic_ols$residuals, main='Histogram of Verb11 Residuals', xlab='Verb11 Residuals')
qqnorm(quadratic_ols$residuals)
qqline(quadratic_ols$residuals, col='orange')

#independent disturbances (no autocorrelation) (Breush-Goddfrey test: null is white noise)
bgtest(quadratic_ols)

# Absolute Model
absolute_ols<-lm(verblength~ab_ideology+
             age+
             college+
             legexp+
             days_present,
           data=data)
print(summary(absolute_ols))

#breusch-pagan test for heteroskedasticity: null is homo
bptest(absolute_ols)


coeftest(absolute_ols, vcov=hccm)

#normality of residuals 
#(Jarque-Bera Test, null is normal)
jarque.bera.test(absolute_ols$residuals)

#Shapiro-Wilk test: null is normal
shapiro.test(absolute_ols$residuals)

hist(absolute_ols$residuals, main='Histogram of absolute_ols Residuals', xlab='absolute_ols Residuals')
qqnorm(absolute_ols$residuals)
qqline(absolute_ols$residuals, col='orange')

#independent disturbances (no autocorrelation) (Breush-Goddfrey test: null is white noise)
bgtest(absolute_ols)


#--------------------
#which fits better, the quadratic or absolute model?
#cox test
coxtest(quadratic_ols,absolute_ols)

#Davidson-McKinnon J test
jtest(quadratic_ols,absolute_ols)

#no difference between the models

#--------------------
#the basic quadratic OLS model without Madison
new.data<-subset(data, data$delegate!="Madison")
quadratic_ols_no_madison<-lm(verblength~ideology+
              ideology_sq+
              age+
              college+
              legexp+
              days_present,
            data=new.data)
print(summary(quadratic_ols_no_madison))
coeftest(quadratic_ols_no_madison, vcov=hccm)

#------------------------------------------------
#------------------------------------------------
# Figure 1 replication

#make a new data frame
data.plot<-as.data.frame(matrix(NA,500,length(names(quadratic_ols$coefficients))-1))
data.plot<-cbind(1,data.plot)
names(data.plot)<-names(quadratic_ols$coefficients)

#now make a sequence of the variable of interest
data.plot$ideology<-as.double(seq(min(na.omit(data$ideology)),max(na.omit(data$ideology)),
                                  length.out=500))
data.plot$ideology_sq<-data.plot$ideology^2

#now get the median value for the other variables
data.plot[,4]<-median(na.omit(data$age))
data.plot[,5]<-median(na.omit(data$college))
data.plot[,6]<-median(na.omit(data$legexp))
data.plot[,7]<-median(na.omit(data$days_present))


#now create fitted values through matrix multiplication
data.plot<-as.matrix(data.plot)
b<-as.numeric(quadratic_ols$coefficients)
b<-t(b)
b<-t(b)
x<-data.plot
fitted.vals<-x%*%(b)

#now get the standard errors
m=summary(quadratic_ols)

#standard errors from the model
m$coef[,2]
blow<-c(b[1],b[2],
        b[3]-(1.96*m$coef[3,2]),
        b[4:length(b)])
blow<-t(blow)
blow<-t(blow)
selow<-x%*%blow

#now do the high
bhi<-c(b[1],b[2],
       b[3]+(1.96*m$coef[3,2]),
       b[4:length(b)])
bhi<-t(bhi)
bhi<-t(bhi)
sehi<-x%*%bhi



#now make the plot data
quadratic_ols.plot.data<-as.data.frame(cbind(data.plot[,2],
                                      fitted.vals,
                                      selow,
                                      sehi))
#now create the plot
quadratic_ols.plot<-ggplot(quadratic_ols.plot.data,aes(x=V1))+
  geom_line(aes(y = V2),
            color = 'blue')
quadratic_ols.plot<- quadratic_ols.plot+ geom_line(aes(y = V3),
                                     color = 'red',
                                     linetype = 'dashed')
quadratic_ols.plot<- quadratic_ols.plot+ geom_line(aes(y = V4),
                                     color = 'red',
                                     linetype = 'dashed')
quadratic_ols.plot<-quadratic_ols.plot + xlab("Delegate Ideology") + ylab("Predicted Verbosity") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(colour= "black"),
        axis.text.y = element_text(colour= "black"),
        legend.position= "none")
print(quadratic_ols.plot)

#------------------------------------------------
#------------------------------------------------
#do a quick analysis to look at the correlation between NOMINATE SE and delegate verbosity
# creates three plots to look at correlation between the standard error of the NOMINATE scores and various measures of delegate verbosity

assessment_data<-read.csv("DelegateVerbosityandIdeology.csv", header=T)

error.plot1<-ggplot(assessment_data, aes(x=se_boot, y=verbnumb))+
  geom_point(shape=1)
print((error.plot1))

error.plot2<-ggplot(assessment_data, aes(x=se_boot, y=verblength))+
  geom_point(shape=1)
print((error.plot2))

error.plot3<-ggplot(assessment_data, aes(x=se_boot, y=avg_spe_len))+
  geom_point(shape=16, size=1.5)+
  geom_smooth(method=lm)
error.plot3<- error.plot3 + xlab("W-NOMINATE SE") +
  ylab("Avg. Speech Length") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank())+
  theme(axis.text=element_text(size=14))+
  theme(axis.title=element_text(size=16, face="bold"))+
  theme(axis.line.x = element_line(color="black", size = .5),
        axis.line.y = element_line(color="black", size = .5))+
  theme(legend.position= "none")
print(error.plot3)

#------------------------------------------------
#------------------------------------------------
# Aggregate Count Analysis
#------------------------------------------------
# estimator functions
# Negative Binomial Log-likelihood
negbi=function(b,X,Y){
  X=cbind(1,X)
  lnt=b[length(b)]
  t=exp(lnt)
  beta=b[1:length(b)-1]
  lambda=exp(X%*%beta)
  r=lambda/(lambda+t)
  negbi=lgamma(t+Y)-lgamma(t)+Y*log(r)+t*log(1-r)
  negbi
}

LPoisson = function(beta, X, Y){
  X = cbind(1,X)
  beta = beta[1:length(beta)]
  xb = X%*%beta
  lambda = exp(xb)
  LPoisson = -lambda + Y*xb
  LPoisson 
}

#the variables
X1<-cbind(data$ideology,
          data$ideology_sq,
          data$age,
          data$college,
          data$legexp,
          data$days_present)

#create starting values to feed the function
stval7<-c(0,0,0,0,0,0,0)
stval8=c(0,0,0,0,0,0,0,0)

#run the negitive binomial and assess dispersion

count.ideology.plus.mod = maxLik(negbi,
                                 start=stval8,
                                 method="BHHH",
                                 X=X1,
                                 Y=data$verbnumb)

names(count.ideology.plus.mod$estimate)<- c("intercept",
                                            "ideology",
                                            "ideology_sq",
                                            "age",
                                            "college",
                                            "legexp",
                                            "days_present",
                                            "gamma")
print(summary(count.ideology.plus.mod))
print(exp(count.ideology.plus.mod$estimate[8]))
AIC(count.ideology.plus.mod)

#dispersion parameter (gamma) not statistically significant from zero so go ahead and use the Poisson
#------------------------------------------------
poisson.ideology.plus.mod = maxLik(LPoisson,
                                   start=stval7,
                                   method="BFGS",
                                   X=X1,
                                   Y=data$verbnumb)
names(poisson.ideology.plus.mod$estimate)<- c("intercept",
                                              "ideology",
                                              "ideology_sq",
                                              "age",
                                              "college",
                                              "legexp",
                                              "days_present")
print(summary(poisson.ideology.plus.mod))
print(AIC(poisson.ideology.plus.mod))
#------------------------------------------------
# becasue of small sample size, conduct standard error bootstrapping
#boots = # of iterations
boots<-500

b.boot <- matrix(NA,
                 nrow = boots,
                 ncol = length(stval7)
)

verbnumb<-as.numeric(data$verbnumb)                 
#now the bootstrap loop
for (j in 1:boots){
  
  index<- 1:nrow(X1)
  
  sampling <- sample(x = index, 
                     size= nrow(data),
                     replace= T)
  #sample the data
  X.boot<-X1[sampling,]
  Y.boot<-verbnumb[sampling]
  
  boot.pois.mod = maxLik(LPoisson,
                         start=stval7,
                         method="BFGS",
                         X=X.boot,
                         Y=Y.boot)
  
  b.boot[j,]=boot.pois.mod$estimate
  
  ### let us know how you're doing
  if(j/50 == floor(j/50)){
    cat(j, "out of", boots, "bootstraps completed.\n")
  }
  
  ### close bootstrap loop.
}

# get the coefficients right
b.boot[,ncol(b.boot)] <- exp(b.boot[,ncol(b.boot)])
bootstrapcoef<-apply(b.boot,2,mean)
# cat(bootstrapcoef, "Coeffient Estimates")

#now apply and get the proper model output
upper<-apply(b.boot, 2, quantile, probs=0.975)
cat(upper, "Upper Interval")
lower<-apply(b.boot, 2, quantile, probs=0.025)
cat(lower,"Lower Interval")

#but the actual SE
#SE=standard deviation of the mean/ sqrt(numbe rof observations in the sample)
n=41
sdb.boot<-apply(b.boot, 2, sd)
b.bootse<-sdb.boot/sqrt(n)


#plot the density of one of the bootstrapped coefficients to show hypothesis
# testing is still possible.
b.boots<-data.frame(b.boot)
boot.coef.2_density = ggplot(b.boots, aes(x=X1))
boot.coef.2_density = boot.coef.2_density + geom_density()
print(boot.coef.2_density)

#calculate p values for the bootstrapped poisson coefficients
n=41
se=c(b.bootse[1:7])
mean=c(0,0,0,0,0,0,0)
bhat= c(bootstrapcoef[1:7])
tvalue= (bhat-mean)/se
tvalue

#now test on the t distribution and get probablities:
tprob=2*pt(-abs(tvalue),df=n-1)
tprob

#put all the boostrapped results together into the actual results table:
# This is first half of Table 2
pois.model<-cbind(bootstrapcoef, b.bootse, tvalue,tprob)

#------------------------------------------------
# to make the second half of Table 2 
X1<-cbind(data$ab_ideology,
          data$age,
          data$college,
          data$legexp,
          data$days_present)
stval6=c(0,0,0,0,0,0)
poisson.abs.ideology.mod = maxLik(LPoisson,
                                  start=stval6,
                                  method="BFGS",
                                  X=X1,
                                  Y=data$verbnumb)
names(poisson.abs.ideology.mod$estimate)<- c("intercept",
                                             "ab_ideology",
                                             "age",
                                             "college",
                                             "legexp",
                                             "days_present")
print(summary(poisson.abs.ideology.mod))
print(AIC(poisson.abs.ideology.mod))

##########################################################
##########################################################
#boostrap the SEs

#boots= #of iterations
boots<-500

b.boot <- matrix(NA,
                 nrow = boots,
                 ncol = length(stval6)
)

verbnumb<-as.numeric(data$verbnumb)                 
#now the bootstrap loop
for (j in 1:boots){
  
  index<- 1:nrow(X1)
  
  sampling <- sample(x = index, 
                     size= nrow(data),
                     replace= T)
  #sample the data
  X.boot<-X1[sampling,]
  Y.boot<-verbnumb[sampling]
  
  boot.pois.mod = maxLik(LPoisson,
                         start=stval6,
                         method="BFGS",
                         X=X.boot,
                         Y=Y.boot)
  
  b.boot[j,]=boot.pois.mod$estimate
  
  ### let us know how you're doing
  if(j/50 == floor(j/50)){
    cat(j, "out of", boots, "bootstraps completed.\n")
  }
  
  ### close bootstrap loop.
}

### get the coefficients right
b.boot[,ncol(b.boot)] <- exp(b.boot[,ncol(b.boot)])
bootstrapcoef<-apply(b.boot,2,mean)


#now apply and get the proper model output
upper<-apply(b.boot, 2, quantile, probs=0.975)
cat(upper, "Upper Interval")
lower<-apply(b.boot, 2, quantile, probs=0.025)
cat(lower,"Lower Interval")

#but the actual SE
#SE=standard deviation of the mean/ sqrt(number of observations in the sample)
n=41
sdb.boot<-apply(b.boot, 2, sd)
b.bootse<-sdb.boot/sqrt(n)


#calculate p values for the bootstrapped poisson coefficients
n=41
se=c(b.bootse[1:6])
mean=c(0,0,0,0,0,0)
bhat= c(bootstrapcoef[1:6])
tvalue= (bhat-mean)/se
tvalue

#now test on the t distribution and get probablities:
tprob=2*pt(-abs(tvalue),df=n-1)
tprob

#put all the boostrapped estimates together:
# This is the actual results for the abolsute model in Table 2
ab.pois.model<-cbind(bootstrapcoef, b.bootse, tvalue,tprob)

#------------------------------------------------
#------------------------------------------------
# To make Figure 2
#predicted counts
pois.model.data<-as.data.frame(pois.model)

#make a data matrix of all the mean values of the regressors
data.medians<-apply(X1, 2, median)
#sequence ideology
ideol.seq<-seq(-1,1,length.out=500)
#make a bin for the pre.probs
prob.bin<-matrix(c(NA),length(ideol.seq), 1)
#run a loop to generate the pred. probs
for (i in 1: length(ideol.seq)){
  x.data<-cbind(1,
                ideol.seq[i],
                (ideol.seq[i])^2,
                data.medians[3],
                data.medians[4],
                data.medians[5])
  betas<-pois.model.data$bootstrapcoef[1:6]
  xb<-x.data%*%betas
  prob.bin[i]<-exp(xb)
}

prob.bin.low<-matrix(c(NA),length(ideol.seq), 1)
#run a loop to generate the pred. probs for low
for (i in 1: length(ideol.seq)){
  x.data<-cbind(1,
                ideol.seq[i],
                (ideol.seq[i])^2,
                data.medians[3],
                data.medians[4],
                data.medians[5])
  blow<-c(pois.model.data$bootstrapcoef[1],
          pois.model.data$bootstrapcoef[2],
          pois.model.data$bootstrapcoef[3]-(1.96*pois.model.data$b.bootse[3]),
          pois.model.data$bootstrapcoef[4],
          pois.model.data$bootstrapcoef[5],
          pois.model.data$bootstrapcoef[6])
  blow<-t(blow)
  blow<-t(blow)
  selow<-x.data%*%blow
  xb<-x.data%*%blow
  prob.bin.low[i]<-exp(xb)
}


prob.bin.hi<-matrix(c(NA),length(ideol.seq), 1)

#run a loop to generate the pred. probs for hi
for (i in 1: length(ideol.seq)){
  x.data<-cbind(1,
                ideol.seq[i],
                (ideol.seq[i])^2,
                data.medians[3],
                data.medians[4],
                data.medians[5])
  bhi<-c(pois.model.data$bootstrapcoef[1],
         pois.model.data$bootstrapcoef[2],
         pois.model.data$bootstrapcoef[3]+(1.96*pois.model.data$b.bootse[3]),
         pois.model.data$bootstrapcoef[4],
         pois.model.data$bootstrapcoef[5],
         pois.model.data$bootstrapcoef[6])
  bhi<-t(bhi)
  bhi<-t(bhi)
  sehi<-x.data%*%bhi
  xb<-x.data%*%bhi
  prob.bin.hi[i]<-exp(xb)
}



#now make the plot data
pois.plot.data<-as.data.frame(cbind(ideol.seq,
                                    prob.bin,
                                    prob.bin.low,
                                    prob.bin.hi))

#now make the plot
pois.plot<-ggplot(pois.plot.data,aes(x=ideol.seq))+
  geom_line(aes(y = V2),
            color = 'blue')
pois.plot<- pois.plot+ geom_line(aes(y = V3),
                                 color = 'red',
                                 linetype = 'dashed')
pois.plot<- pois.plot+ geom_line(aes(y = V4),
                                 color = 'red',
                                 linetype = 'dashed')
pois.plot<-pois.plot+ xlab("Delegate Ideology") + ylab("Predicted Count") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(colour= "black"),
        axis.text.y = element_text(colour= "black"),
        legend.position= "none")
print(pois.plot)

