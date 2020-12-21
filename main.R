setwd("C:/Users/Itachi Bal/Desktop/GLM")
data <- read.table("stor1.txt",header=TRUE)
names(data)
#--------------------------------------------#making the dummy variables into factors.
data1<-subset(data[,c(22,23,24,25,26)])
data2<-subset(data[,c(16,17,18,19,20)])
data1<-subset(data1[,c(-1)])
data2<-subset(data2[,c(-1)])
data1<-as.matrix(data1)
data2<-as.matrix(data2)
region_factor<-factor((data1 %*%(1:ncol(data1)))+1, 
       labels = c("Aspect4Flat", colnames(data1)))
forest_factor<-factor((data2 %*%(1:ncol(data2))) + 1, 
                      labels = c("SMBDLeav", colnames(data2)))
#----------------------------------------------#making the dummy variables into factors.
my_data <- read.table("stor1.txt",header=TRUE)
org_data <- read.table("stor1.txt",header=TRUE)

org_data <- subset(org_data[,c((27:28))])

pellet_factor<-factor((org_data$Pellet_2009==1|org_data$Pellet_2010==1))##bringing the pellet count together 
###### factoring them
my_data <- my_data[,c(-1,-7,-9,-11,-13,-15)]### removal of id and log values
my_data <- subset(my_data[,c(-(10:20))])### removal of dummy variables
my_data <- subset(my_data[,c(-(10:11))])### removal of pellet(dummy variable)
my_data$region_factor <-    region_factor## adding the factorized variables
my_data$forest_factor <-    forest_factor
my_data$pellet_factor <-    pellet_factor
ncol(my_data)
summary(my_data)
#glm1<-glm(as.factor(Pellet_2009)~Elevation+Slope+VRM+kNN+Distpow+Distgruva+Distsmall
 #        +region_factor+forest_factor, family=binomial(link=logit),data=my_data)
#glm1<-glm(as.factor(Pellet_2009)~Elevation+kNN+Distpow+Distbig+Distgruva
 #         , family=binomial(link=logit),data=my_data)
#summary(glm1)
#plot(glm1)
#crPlots(glm1)
#(Elevation+Slope+VRM+kNN+Distpow+Distroad+Distbig+Distgruva+Distsmall+Pellet_2009+Pellet_2010+region_factor+forest_factor)

#glm2<-glm(as.factor(Pellet_2010)~Elevation+kNN+Distpow+Distbig+Distgruva
 #           , family=binomial(link=logit),data=my_data)
#glm2<-glm(as.factor(Pellet_2010)~Elevation+Slope+VRM+kNN+Distpow+Distgruva+Distsmall
 #         +region_factor+forest_factor, family=binomial(link=logit),data=my_data)
#summary(glm2)
#plot(glm2)
#crPlots(glm2)
########################################################################## 
df_present_2009<-subset(df,df$Pellet_2009==1)
df_not_present_2009<-subset(df,df$Pellet_2009==0)

df_present_2010<-subset(df,df$Pellet_2010==1)

df_not_present_2010<-subset(df,df$Pellet_2010==0)

df_present_2009_and_2010<-subset(df,df$Pellet_2009==1 & df$Pellet_2010==1)

df_not_present_2009_and_2010<-subset(df,df$Pellet_2009==0 & df$Pellet_2010==0)

summary(df_present_2009)
summary(df_not_present_2009)
summary(df_present_2010)
summary(df_not_present_2010)
summary(df_present_2009_and_2010)
summary(df_not_present_2009_and_2010)
########################################## combinations of pellets found and not found.
library(FactoMineR)
library(factoextra)
library(ggplot2)
library("reshape2")
library(corrplot)
plot_data<- subset(my_data[,c(-10,-11,-12)])

qplot(x=Var1,y=Var2,data=melt(cor(plot_data)),fill = value,geom="tile",colour=I("white"))

#correlation between variables
variables_correlation <- cor(as.matrix(plot_data))

corrplot(variables_correlation, method="circle",
         diag = FALSE , type = "upper")



##for both pellet_counts
## we are just considering how many times it came to the pellet circle.
## we are trying to estimate the response.
glm1<-glm(pellet_factor~Elevation+Slope+VRM+kNN+Distpow+Distgruva+Distsmall
          +forest_factor, family=binomial(link=logit),data=my_data)# we modelled it by using step function
summary(glm1)
plot(glm1)
crPlots(glm1)

#Identifing outliers
#outlierTest(glm1)
#influenceIndexPlot(glm1, varcs=c("Cook","hat"),id = list(n=5))
#compareCoefs(glm1,update(glm1,subset=-c(196,271,189,150)))
round(exp(cbind(Estimate=coef(glm1),confint(glm1))),2)## risk factors or odds ratio  with CI
glm.1.1<-update(glm1,.~.-Distpow)#### model comparision by removing the power line dist variable
anova(glm.1.1,glm1, test="Chisq")#### analysis of deviance for logistic regression.
Anova(glm1)## this is for Type2
head(predict(glm1,type="response"))
v<-pchisq(13.201, df=1, lower.tail=FALSE)## p value for chisq statistic
v### p value is lower than 0.05 alpha so we reject null hypothesis 
### the power cables and pellet count are dependent.
