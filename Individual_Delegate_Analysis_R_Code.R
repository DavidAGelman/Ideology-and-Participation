#------------------------------------------------
#------------------------------------------------
# 'Ideology and Participation'
# written under version Version 0.99.486 of RStudio
# Data description and individual delegate analysis (both length and count)
# Origiinally written by DAG on 09/2/2014
# Cleaned and edited for purposes of positing for reproduction on 10/29/2017
#------------------------------------------------
#------------------------------------------------

#------------------------------------------------
# optional, set working directory
setwd("~")

# load in the needed libraries
library(stargazer) # for easy and clean LaTeX tables
library(ggplot2) # for figure creation
library(gridExtra) # for grid.arrange function
library(robust) # for robust standard errors
library(lmtest) # for breusch-pagan test
library(car) # for hccm variance/covariance correction
library(tseries) # for harque bera test
library(orcutt) # for using a cochrane-orcutt FGLS model 


#------------------------------------------------
# load in the data
SpeechesData=read.csv("CC_SpeechesVerbosityDataSet.csv", header=TRUE)
attach(SpeechesData)

#------------------------------------------------
# variable creation
# Here I make new variable that indicates the average ideological distance of
#     the current speaker from the previous n speakers.

# Define number of previous speeches to include in variable calculation.
n <- 3

# Placeholder of NA for variable to calculate
SpeechesData$prevspeaker <- NA

for (i in (n+1):nrow(SpeechesData)){
  # Subset data to consider relevant speeches (last n speeches)
  pspeech <- SpeechesData[(i-(n+1)):(i-1),]
  pideology <- pspeech$Delegate_Ideology
  
  # Grab speech giver's ideology score
  speechgiver <- SpeechesData$Delegate_Ideology[i]
  
  # Calculate quantity of interest for observation i
  # This code assumes you want the average distance, across the previous 
  # n speeches, of the current speaker's ideal point vs the previous speaker's
  avg <- 0
  for (j in 1:n){
    avg <- avg + abs(pideology[j]-speechgiver)
  }
  SpeechesData$prevspeaker[i] <- avg/n
}


#####
#now make a variable to show distance from current speaker to specfic 5 previous speakers
n<-5

# Placeholder of NA for variable to calculate
SpeechesData$PrevDelegateSpeech1 <- NA
SpeechesData$PrevDelegateSpeech2 <- NA
SpeechesData$PrevDelegateSpeech3 <- NA
SpeechesData$PrevDelegateSpeech4 <- NA
SpeechesData$PrevDelegateSpeech5 <- NA


for (i in (n+1):nrow(SpeechesData)){
  # Subset data to consider relevant speeches (last n speeches)
  pspeech <- SpeechesData[(i-(n+1)):(i-1),]
  pideology <- pspeech$Delegate_Ideology
  
  # Grab speech giver's ideology score
  speechgiver <- SpeechesData$Delegate_Ideology[i]
  
  # Calculate quantity of interest for observation i
  # This code assumes you want the average distance, across the previous 
  # n speeches, of the current speaker's ideal point vs the previous speaker's
  SpeechesData$PrevDelegateSpeech1[i] <- abs(pideology[5]-speechgiver)
  SpeechesData$PrevDelegateSpeech2[i] <- abs(pideology[4]-speechgiver)
  SpeechesData$PrevDelegateSpeech3[i] <- abs(pideology[3]-speechgiver)
  SpeechesData$PrevDelegateSpeech4[i] <- abs(pideology[2]-speechgiver)
  SpeechesData$PrevDelegateSpeech5[i] <-abs(pideology[1]-speechgiver)
}

#make speech topic a factor
SpeechesData$Speech_Topic_Num<-as.factor(SpeechesData$Speech_Topic_Num)




#------------------------------------------------
#------------------------------------------------
# Table 3 creation
# Absolute model
data.drop<-subset(SpeechesData,SpeechesData$Speech_Length<=1000)
absolute_ols<-lm(Speech_Length~Abs_Delegate_Ideology+
             prevspeaker+
             Delegate_Attend+
             Delegate_Age+
             Delegate_College+
             Delegate_Leg_Exp+
             Delegate_Slave,
           data=data.drop)
print(summary(absolute_ols))
AIC(absolute_ols)

#breusch-pagan test for heteroskedasticity: null is homo
bptest(absolute_ols)
#use Robsut standard errors.
coeftest(absolute_ols, vcov=hccm)

#normality of residuals 
#(Jarque-Bera Test, null is normal)
jarque.bera.test(absolute_ols$residuals)

#Shapiro-Wilk test: null is normal
shapiro.test(absolute_ols$residuals)

hist(absolute_ols$residuals, main='Histogram of mod.2a Residuals', xlab='mod.2a Residuals')
qqnorm(absolute_ols$residuals)
qqline(absolute_ols$residuals, col='orange')

#independent disturbances (no autocorrelation) (Breush-Goddfrey test: null is white noise)
bgtest(absolute_ols)

#try doing the model with fGLS
#Correct for this with Cochrane-Orcutt FGLS:
absolute_ols.fgls <- cochrane.orcutt(absolute_ols)
print(summary(absolute_ols.fgls))
print(absolute_ols.fgls)

#------------------------------------------------
# The quadratic model from Table 3
quadratic_ols<-lm(Speech_Length~Delegate_Ideology+
             Delegate_Ideology_Sq+
             prevspeaker+
             Delegate_Attend+
             Delegate_Age+
             Delegate_College+
             Delegate_Leg_Exp+
             Delegate_Slave,
           data=data.drop)
print(summary(quadratic_ols))
AIC(quadratic_ols)

#breusch-pagan test for heteroskedasticity: null is homo
bptest(quadratic_ols)
#use Robsut standard errors.
coeftest(quadratic_ols, vcov=hccm)


#normality of residuals 
#(Jarque-Bera Test, null is normal)
jarque.bera.test(quadratic_ols$residuals)

#Shapiro-Wilk test: null is normal
shapiro.test(quadratic_ols$residuals)

hist(quadratic_ols$residuals, main='Histogram of mod.2f Residuals', xlab='mod.2f Residuals')
qqnorm(quadratic_ols$residuals)
qqline(quadratic_ols$residuals, col='orange')

#independent disturbances (no autocorrelation) (Breush-Goddfrey test: null is white noise)
bgtest(quadratic_ols)

#try doing the model with fGLS
#Correct for this with Cochrane-Orcutt FGLS:
quadratic_ols.fgls <- cochrane.orcutt(quadratic_ols)
print(summary(quadratic_ols))
print(quadratic_ols.fgls)

#------------------------------------------------
#------------------------------------------------
# The Quadratic OLS model with clustered standard Errors

#the SE clusterer
c.ols <- function(form, data, robust=FALSE, cluster=NULL,digits=3){
  r1 <- lm(form, data)
  if(length(cluster)!=0){
    data <- na.omit(data[,c(colnames(r1$model),cluster)])
    r1 <- lm(form, data)
  }
  X <- model.matrix(r1)
  n <- dim(X)[1]
  k <- dim(X)[2]
  if(robust==FALSE & length(cluster)==0){
    se <- sqrt(diag(solve(crossprod(X)) * as.numeric(crossprod(resid(r1))/(n-k))))
    res <- cbind(coef(r1),se)
  }
  if(robust==TRUE){
    u <- matrix(resid(r1))
    meat1 <- t(X) %*% diag(diag(crossprod(t(u)))) %*% X
    dfc <- n/(n-k)    
    se <- sqrt(dfc*diag(solve(crossprod(X)) %*% meat1 %*% solve(crossprod(X))))
    res <- cbind(coef(r1),se)
  }
  if(length(cluster)!=0){
    clus <- cbind(X,data[,cluster],resid(r1))
    colnames(clus)[(dim(clus)[2]-1):dim(clus)[2]] <- c(cluster,"resid")
    m <- dim(table(clus[,cluster]))
    dfc <- (m/(m-1))*((n-1)/(n-k))
    uclust  <- apply(resid(r1)*X,2, function(x) tapply(x, clus[,cluster], sum))
    se <- sqrt(diag(solve(crossprod(X)) %*% (t(uclust) %*% uclust) %*% solve(crossprod(X)))*dfc)   
    res <- cbind(coef(r1),se)
  }
  res <- cbind(res,res[,1]/res[,2],(1-pnorm(res[,1]/res[,2]))*2)
  res1 <- matrix(as.numeric(sprintf(paste("%.",paste(digits,"f",sep=""),sep=""),res)),nrow=dim(res)[1])
  rownames(res1) <- rownames(res)
  colnames(res1) <- c("Estimate","Std. Error","t value","Pr(>|t|)")
  return(res1)
}

#the model
c.ols(Speech_Length~Delegate_Ideology+
        Delegate_Ideology_Sq+
        prevspeaker+
        Delegate_Attend+
        Delegate_Age+
        Delegate_College+
        Delegate_Leg_Exp+
        Delegate_Slave,
      data=data.drop,
      cluster="Delegate")
print(summary(quadratic_ols))
coeftest(quadratic_ols, vcov=hccm)

#------------------------------------------------
#------------------------------------------------
# To make Figure 3, the predicted probbility plots

#make a new data frame
data2<-as.data.frame(matrix(NA,500,length(names(quadratic_ols$coefficients))-1))
data2<-cbind(1,data2)
names(data2)<-names(quadratic_ols$coefficients)

#now make a sequence of the variable of interest
data2$Delegate_Ideology<-as.double(seq(min(na.omit(data.drop$Delegate_Ideology)),max(na.omit(data.drop$Delegate_Ideology)),
                                       length.out=500))
data2$Delegate_Ideology_Sq<-data2$Delegate_Ideology^2

#now get the median value for the other variables
data2[,4]<-median(na.omit(data.drop$prevspeaker))
data2[,5]<-median(na.omit(data.drop$Delegate_Attend))
data2[,6]<-median(na.omit(data.drop$Delegate_Age))
data2[,7]<-median(na.omit(data.drop$Delegate_College))
data2[,8]<-median(na.omit(data.drop$Delegate_Leg_Exp))
data2[,9]<-median(na.omit(data.drop$Delegate_Slave))

#now create fitted values through matrix multiplication
data2<-as.matrix(data2)
b<-as.numeric(quadratic_ols$coefficients)
b<-t(b)
b<-t(b)
x<-data2
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
quadratic_ols.data<-as.data.frame(cbind(data2[,2],
                                      fitted.vals,
                                      selow,
                                      sehi))

#now make the plot
quadratic_ols.plot<-ggplot(quadratic_ols.data,aes(x=V1))+
  geom_line(aes(y = V2),
            color = 'blue')
quadratic_ols.plot<- quadratic_ols.plot+ geom_line(aes(y = V3),
                                     color = 'red',
                                     linetype = 'dashed')
quadratic_ols.plot<- quadratic_ols.plot+ geom_line(aes(y = V4),
                                     color = 'red',
                                     linetype = 'dashed')
quadratic_ols.plot<-quadratic_ols.plot+ xlab("Speaker Ideology") + ylab("Predicted Length") +
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
# descriptive statistics
# Table 7 in the supplementary materials

#Descriptive Statistics
variables<-cbind(data.drop$Speech_Length,
                 data.drop$Abs_Delegate_Ideology,
                 data.drop$Delegate_Ideology,
                 data.drop$Delegate_Ideology_Sq,
                 data.drop$prevspeaker,
                 data.drop$Delegate_Attend,
                 data.drop$Delegate_Age,
                 data.drop$Delegate_College,
                 data.drop$Delegate_Leg_Exp,
                 data.drop$Delegate_Slave)
print(apply(variables,2,summary))

#------------------------------------------------
# Figure 9 from the supplementary materials
#Speech Length Density
speechlength_density = ggplot(data.drop, aes(x=Speech_Length))
speechlength_density = speechlength_density + geom_density(kernel="rectangular",
                                                           colour="black",
                                                           fill="gray80",
                                                           size=1)
speechlength_density = speechlength_density +
  xlab("Speech Length") +
  ylab("Density") + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(colour= "black"),
        axis.text.y = element_text(colour= "black"))
print(speechlength_density)
