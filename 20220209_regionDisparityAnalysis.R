# Title: Analysis of Regional Data
# Author: MAJ Devon Zillmer
# Date: began 20 OCT 2021

### Data Import ###
fileLoc <- "C:\\Users\\devon.zillmer\\OneDrive - West Point\\Documents\\DrThomasResearch\\Data\\IPUMS Extract\\nhis_00006.dat" #work computer
#fileLoc <- "C:\\Users\\Devon\\OneDrive - West Point\\Documents\\DrThomasResearch\\Data\\IPUMS Extract\\nhis_00006.dat" #home desktop computer
widths <- c(4,6,4,3,14,6,2,2,16,7,2,2,12,12,6,1,1,3,1,2,2,1,3,2,3,1,1,2,3,
            2,2,3,2,2,2,2,2,2,2,2,3,4,4,1,1,3,1,2,2,2,2,2,14,2)
titles <- c("Year","Serial","strata","PSU","NHIS HID","HH Weight","Region",
            "PerNum","NHISPID","HHX","FMX","PX","PerWeight","SampWeight",
            "FWeight","AStatFlg","CStatFlg","Age","Sex","MarStat","Marst",
            "MarStatCohab","RaceA","HispEth","RacesR","YrsInUS","HISPYN",
            "USBorn","RaceNew","EduCRec2","EduCRec1","EduC","IncFam97on2",
            "Poverty","PovImp1","PovImp2","PovImp3","PovImp4","PovImp5",
            "Height","Weight","BMICalc","BMI","UsualPl","HInotCov","AlcAMT",
            "AlcStat1","SmokeV","SmokeStatus2","Mod10FWk","Vig10Fwk",
            "StrongFwk","NIU","HrSleep")
df <- read.fwf(fileLoc, widths=widths,col.names=titles)



### Recoding Important Information ###
df$SexF <- ifelse(df$Sex ==1, "M",ifelse(df$Sex == 2, "F",NA)) #recode for read-ability
df$SexF <- as.factor(df$SexF)
df$RegionF <- ifelse(df$Region==1, "NE",ifelse(df$Region==2, "MW",ifelse(df$Region==3,"S",ifelse(df$Region==4,"W",NA)))) #recode for read-ability
df$RegionF <- as.factor(df$RegionF)
df$MarstF <- ifelse(df$Marst %in% c(10,11,12,13),"Married",ifelse(df$Marst %in% c(20,30,40),"ExMarried",ifelse(df$Marst==50,"NeverMarried",NA) ))
df$MarstF <- as.factor(df$MarstF)
df$RaceF <- ifelse(df$RaceNew==100,"White",ifelse(df$RaceNew==200,"Black",ifelse(df$RaceNew==300,"Native",ifelse(df$RaceNew==400,"Asian",ifelse(df$RaceNew==520,"Multi",NA)))))
df$RaceF <- as.factor(df$RaceF)
df$EduF <- ifelse(df$EduCRec2==20|df$EduCRec2==30|df$EduCRec2==31|df$EduCRec2==32,"Primary",ifelse(df$EduCRec2==40|df$EduCRec2==41|df$EduCRec2==42,"Secondary",ifelse(df$EduCRec2==50|df$EduCRec2==51|df$EduCRec2==52|df$EduCRec2==53|df$EduCRec2==54,"Undergrad",ifelse(df$EduCRec2==60,"Graduate",NA))))
df$EduF <- as.factor(df$EduF)
df$AlcAmtF <- ifelse(df$AlcAMT>950,NA,df$AlcAMT/10) #amount they had to drink on days which they drink
df$SmokeStatF <- ifelse(df$SmokeStatus2==30,"Never",ifelse(df$SmokeStatus2==20|df$SmokeStatus2==40,"Former",ifelse(df$SmokeStatus2==0|df$SmokeStatus2==90,NA,"Current")))
df$SmokeStatF <- as.factor(df$SmokeStatF)
df$Mod10FwkF <- ifelse(df$Mod10FWk==0|df$Mod10FWk>96,NA,ifelse(df$Mod10FWk==96|df$Mod10FWk==95|df$Mod10FWk==94,0,df$Mod10FWk))
df$Vig10FwkF <- ifelse(df$Vig10Fwk==0|df$Vig10Fwk>96,NA,ifelse(df$Vig10Fwk==96|df$Vig10Fwk==95|df$Vig10Fwk==94,0,df$Vig10Fwk))
df$StrongFwkF <- ifelse(df$StrongFwk==0|df$StrongFwk>96,NA,ifelse(df$StrongFwk==96|df$StrongFwk==95|df$StrongFwk==94,0,df$StrongFwk))
df$HrSleepF <- ifelse(df$HrSleep==0|df$HrSleep>96,NA,df$HrSleep)
df$USBornF <- ifelse(df$USBorn==20,1,ifelse(df$USBorn>96,NA,0))
df$AlcStatF <- ifelse(df$AlcStat1==3,"Current",ifelse(df$AlcStat1==2,"Former",ifelse(df$AlcStat1==1,"Never",NA)))
df$AlcStatF <- as.factor(df$AlcStatF)
# This next step overwrites the PerWeight with SampWeight/15 (because data pulled from 15 years), as that's what we're "supposed" to use
df$PerWeight <- df$SampWeight/15
df6 <- df[df$SampWeight!=0,]

### Data filtering to examine BMI versus no BMI, specifically using the BMI NOT BMICalc ###
# Using BMI rather than BMICalc
noBMIdf <- df6[df6$BMI %in% c(0,9999),]
BMIdf <- with(df6,BMI>0 && BMI<=9980)
temp <- df6[df6$BMI>0,]
BMIdf <- temp[temp$BMI<9980,] #<- gives how many HAVE BMI using original BMI
BMIdf$Obese <- with(BMIdf,ifelse(BMI>=3000,1,0))
BMIdf$ObeseF <- as.factor(BMIdf$Obese)
dim(BMIdf)

### This is to try to use the 'survey' package to utilize the 'perweight' feature of the NHANES data ###
library(survey)


### To reproduce Table 2.
library(mltools)
library(data.table)
table2List <- c("Obese","BMI","RegionF","SexF","Age","USBornF","MarstF","RaceF","SmokeStatF","AlcStatF","Mod10FwkF","Vig10FwkF","StrongFwkF","EduF","PSU","strata","PerWeight")

# This section builds my various datasets by region
df2 <- BMIdf[,table2List]
df2S <- as.data.frame(na.omit(df2[df2$RegionF=="S",]))
df2Soh <- one_hot(as.data.table(df2S))
# df2N <- as.data.frame(na.omit(df2[df2$RegionF=="NE",]))
# df2Noh <- one_hot(as.data.table(df2N))
# df2W <- as.data.frame(na.omit(df2[df2$RegionF=="W",]))
# df2Woh <- one_hot(as.data.table(df2W))
# df2MW <- as.data.frame(na.omit(df2[df2$RegionF=="MW",]))
# df2MWoh <- one_hot(as.data.table(df2MW))

options( survey.lonely.psu = "adjust" ) #this is in case the svyglm command throws an error flag because only 1 PSU

# Then I train a model on the Southern region's data, using individual data to predict BMI, then the region
# the most effective direct linear model uses BMI^2 and gets much close overall
svy2S <- svydesign(id=~PSU, strata=~strata,weights=~PerWeight,nest=TRUE,data=df2Soh)

glmS <- svyglm(formula = BMI ~ SexF_M+Age+USBornF+MarstF_ExMarried+MarstF_Married+RaceF_Asian+RaceF_Black+RaceF_Multi+RaceF_Native+SmokeStatF_Current+SmokeStatF_Former+AlcStatF_Current+AlcStatF_Former+Mod10FwkF+Vig10FwkF+StrongFwkF+EduF_Graduate+EduF_Primary+EduF_Undergrad, design = svy2S)
#Complete listing for laster reference/testing
#SexF_M+Age+USBornF+MarstF_Ex-Married+MarstF_Married+RaceF_Asian+RaceF_Black+RaceF_Multi+RaceF_Native+SmokeStatF_Current+SmokeStatF_Former+AlcStatF_Current+AlcStatF_Former+Mod10FwkF+Vig10FwkF+StrongFwkF+EduF_Graduate+EduF_Primary+EduF_Undergrad


df2S$predBMI <- predict(glmS,newdata=df2Soh)#note, we're trying to see how accurately it predicted the South's BMI
df2S$predObese <- with(df2S,ifelse(predBMI>=3000,1,0)) #classifying as "obese"
testS_svy <- svydesign(id=~PSU, strata=~strata,weights=~PerWeight,nest=TRUE,data=df2S)

#Examining the south's predicted stats
sOp <- svymean(~Obese,testS_svy)[1]*100
sBm <- svymean(~BMI,testS_svy)[1]/100
spOp <- svymean(~predObese,testS_svy)[1]*100
spBm <- svymean(~predBMI,testS_svy)[1]/100
southCheck<-data.frame(matrix(data=c(sOp,spOp,sBm,spBm),nrow=2,ncol=2),row.names = c("Data","Model"))
colnames(southCheck)<-c("Per Cent Obese","BMI")
southCheck


### First we build the larger survey, then one-hot code the variables
df2 <- BMIdf[,table2List]
df3 <- as.data.frame(na.omit(df2))
df3oh <- one_hot(as.data.table(df3))
svy3 <- svydesign(id=~PSU, strata=~strata,weights=~PerWeight,nest=TRUE,data=df3oh)

# Let's try to build a more general model, and see how that effects the populations
glmG <- svyglm(formula = BMI ~ SexF_M+Age+USBornF+MarstF_ExMarried+MarstF_Married+RaceF_Asian+RaceF_Black+RaceF_Multi+RaceF_Native+SmokeStatF_Current+SmokeStatF_Former+AlcStatF_Current+AlcStatF_Former+Mod10FwkF+Vig10FwkF+StrongFwkF+EduF_Graduate+EduF_Primary+EduF_Undergrad, design = svy3)
#Original List
#SexF_M+Age+USBornF+MarstF_Ex-Married+MarstF_Married+RaceF_Asian+RaceF_Black+RaceF_Multi+RaceF_Native+SmokeStatF_Current+SmokeStatF_Former+AlcStatF_Current+AlcStatF_Former+Mod10FwkF+Vig10FwkF+StrongFwkF+EduF_Graduate+EduF_Primary+EduF_Undergrad
df3$predBMI <- predict(glmG,newdata=df3oh)#verifying back against the training set
df3$predObese <- with(df3,ifelse(predBMI>=3000,1,0)) #classifying as "obese"
svy3 <- svydesign(id=~PSU, strata=~strata,weights=~PerWeight,nest=TRUE,data=df3)

#Examining the overall predicted stats
obeseVec <- svyby(~Obese,by=~RegionF,design=svy3,svymean)[,2]*100
bmiVec <- svyby(~BMI,by=~RegionF,design=svy3,svymean)[,2]/100
obesePredVec <- svyby(~predObese,by=~RegionF,design=svy3,svymean)[,2]*100
bmiPredVec <- svyby(~predBMI,by=~RegionF,design=svy3,svymean)[,2]/100
compareTable <- data.frame(matrix(data=c(obeseVec,bmiVec,obesePredVec,bmiPredVec),nrow=4,ncol=4),row.names = c("MW","NE","S","W"))
colnames(compareTable)<-c("Region Obesity %","Region Avg BMI","Pred Obesity","Pred BMI")
cat("This is the BMI~Factors Table:")
compareTable
## therefore, this is a terrible model to predict Obesity... let's see if we can get better numbers. 

#calculating R-squared and adjusted R-squared
df3$sqEr <- (df3$predBMI-df3$BMI)^2 * df3$PerWeight #this is me, clumsily trying to weight the SSE
bmiMean <- svymean(~BMI,svy3)[1]
df3$ssTotal <- (df3$BMI-bmiMean)^2 * df3$PerWeight #this is me, clumsily trying to weight the SST
rSqB <- 1 - sum(df3$sqEr)/sum(df3$ssTotal)
rSqAdjB <- 1- ((1-rSqB)*(dim(df3)[1]-1))/(dim(df3)[1]-length(glmG$coefficients)-1)
sprintf("For the baseline model, R-squared: %.4f, Adjusted R-Squared: %.4f",rSqB,rSqAdjB)


df3 <- as.data.frame(na.omit(df2))
df3oh <- one_hot(as.data.table(df3))
svy3 <- svydesign(id=~PSU, strata=~strata,weights=~PerWeight,nest=TRUE,data=df3oh)
glmG <- svyglm(formula = BMI^2 ~ SexF_M+Age+USBornF+MarstF_ExMarried+MarstF_Married+RaceF_Asian+RaceF_Black+RaceF_Multi+RaceF_Native+SmokeStatF_Current+SmokeStatF_Former+AlcStatF_Current+AlcStatF_Former+Mod10FwkF+Vig10FwkF+StrongFwkF+EduF_Graduate+EduF_Primary+EduF_Undergrad, design = svy3)
df3$predBMI <- sqrt(predict(glmG,newdata=df3oh))#verifying back against the training set
df3$predObese <- with(df3,ifelse(predBMI>=3000,1,0)) #classifying as "obese"
svy3 <- svydesign(id=~PSU, strata=~strata,weights=~PerWeight,nest=TRUE,data=df3)

#Examining the overall predicted stats
obeseVec <- svyby(~Obese,by=~RegionF,design=svy3,svymean)[,2]*100
bmiVec <- svyby(~BMI,by=~RegionF,design=svy3,svymean)[,2]/100
obesePredVec <- svyby(~predObese,by=~RegionF,design=svy3,svymean)[,2]*100
bmiPredVec <- svyby(~predBMI,by=~RegionF,design=svy3,svymean)[,2]/100
compareTable2 <- data.frame(matrix(data=c(obeseVec,bmiVec,obesePredVec,bmiPredVec),nrow=4,ncol=4),row.names = c("MW","NE","S","W"))
colnames(compareTable2)<-c("Region Obesity %","Region Avg BMI","Pred Obesity","Pred BMI")
cat("This is the BMI^2~Factors Table:")
compareTable2


### So let's try to build a model that incorporates a coefficient by region
df3 <- as.data.frame(na.omit(df2))
df3oh <- one_hot(as.data.table(df3))
svy3 <- svydesign(id=~PSU, strata=~strata,weights=~PerWeight,nest=TRUE,data=df3oh)
glm1 <- svyglm(formula = BMI ~ RegionF_MW+RegionF_NE+RegionF_S+SexF_M+Age+USBornF+MarstF_ExMarried+MarstF_Married+RaceF_Asian+RaceF_Black+RaceF_Multi+RaceF_Native+SmokeStatF_Current+SmokeStatF_Former+AlcStatF_Current+AlcStatF_Former+Mod10FwkF+Vig10FwkF+StrongFwkF+EduF_Graduate+EduF_Primary+EduF_Undergrad, design = svy3)
df3$predBMI <- predict(glm1,newdata=df3oh)#verifying back against the training set
df3$predObese <- with(df3,ifelse(predBMI>=3000,1,0)) #classifying as "obese"
svy3 <- svydesign(id=~PSU, strata=~strata,weights=~PerWeight,nest=TRUE,data=df3)

#Examining the overall predicted stats
obeseVec <- svyby(~Obese,by=~RegionF,design=svy3,svymean)[,2]*100
bmiVec <- svyby(~BMI,by=~RegionF,design=svy3,svymean)[,2]/100
obesePredVec <- svyby(~predObese,by=~RegionF,design=svy3,svymean)[,2]*100
bmiPredVec <- svyby(~predBMI,by=~RegionF,design=svy3,svymean)[,2]/100
compareTable3 <- data.frame(matrix(data=c(obeseVec,bmiVec,obesePredVec,bmiPredVec),nrow=4,ncol=4),row.names = c("MW","NE","S","W"))
colnames(compareTable3)<-c("Region Obesity %","Region Avg BMI","Pred Obesity","Pred BMI")
cat("This is the BMI~Region+Factors Table:")
compareTable3

#calculating R-squared and adjusted R-squared
df3$sqEr <- (df3$predBMI-df3$BMI)^2 * df3$PerWeight #this is me, clumsily trying to weight the SSE
bmiMean <- svymean(~BMI,svy3)[1]
df3$ssTotal <- (df3$BMI-bmiMean)^2 * df3$PerWeight #this is me, clumsily trying to weight the SST
rSqB <- 1-sum(df3$sqEr)/sum(df3$ssTotal)
rSqAdjB <- 1- ((1-rSqB)*(dim(df3)[1]-1))/(dim(df3)[1]-length(glm1$coefficients)-1)
sprintf("For the region+factor model, R-squared: %.4f, Adjusted R-Squared: %.4f",rSqB,rSqAdjB)


### So let's try to build a model that has coefficients by region!
df3 <- as.data.frame(na.omit(df2))
df3oh <- one_hot(as.data.table(df3))
svy3 <- svydesign(id=~PSU, strata=~strata,weights=~PerWeight,nest=TRUE,data=df3oh)
glm2 <- svyglm(formula = BMI ~ (RegionF_MW+RegionF_NE+RegionF_S)*(SexF_M+Age+USBornF+MarstF_ExMarried+MarstF_Married+RaceF_Asian+RaceF_Black+RaceF_Multi+RaceF_Native+SmokeStatF_Current+SmokeStatF_Former+AlcStatF_Current+AlcStatF_Former+Mod10FwkF+Vig10FwkF+StrongFwkF+EduF_Graduate+EduF_Primary+EduF_Undergrad), design = svy3)
df3$predBMI <- predict(glm2,newdata=df3oh)#verifying back against the training set
df3$predObese <- with(df3,ifelse(predBMI>=3000,1,0)) #classifying as "obese"
svy3 <- svydesign(id=~PSU, strata=~strata,weights=~PerWeight,nest=TRUE,data=df3)

#Examining the overall predicted stats
obeseVec <- svyby(~Obese,by=~RegionF,design=svy3,svymean)[,2]*100
bmiVec <- svyby(~BMI,by=~RegionF,design=svy3,svymean)[,2]/100
obesePredVec <- svyby(~predObese,by=~RegionF,design=svy3,svymean)[,2]*100
bmiPredVec <- svyby(~predBMI,by=~RegionF,design=svy3,svymean)[,2]/100
compareTable4 <- data.frame(matrix(data=c(obeseVec,bmiVec,obesePredVec,bmiPredVec),nrow=4,ncol=4),row.names = c("MW","NE","S","W"))
colnames(compareTable4)<-c("Region Obesity %","Region Avg BMI","Pred Obesity","Pred BMI")
cat("This is the BMI~(Region)*(Factors) Table:")
compareTable4

df3$sqEr <- (df3$predBMI-df3$BMI)^2 * df3$PerWeight #this is me, clumsily trying to weight the SSE
bmiMean <- svymean(~BMI,svy3)[1]
df3$ssTotal <- (df3$BMI-bmiMean)^2 * df3$PerWeight #this is me, clumsily trying to weight the SST
rSqB <- 1-sum(df3$sqEr)/sum(df3$ssTotal)
rSqAdjB <- 1- ((1-rSqB)*(dim(df3)[1]-1))/(dim(df3)[1]-length(glm2$coefficients)-1)
sprintf("For the region+factor model, R-squared: %.4f, Adjusted R-Squared: %.4f",rSqB,rSqAdjB)

### To Generate the by-function output, just use the "summary" command on ...
#... each of the GLMs, just one at a time
# df3 <- as.data.frame(na.omit(df2))
# df3oh <- one_hot(as.data.table(df3))
# svy3 <- svydesign(id=~PSU, strata=~strata,weights=~PerWeight,nest=TRUE,data=df3oh)
# glm2 <- svyglm(formula = BMI ~ (RaceF_Asian+RaceF_Black+RaceF_Multi+RaceF_Native+RegionF_MW+RegionF_NE+RegionF_S+SexF_M+Age+USBornF+MarstF_ExMarried+MarstF_Married+SmokeStatF_Current+SmokeStatF_Former+AlcStatF_Current+AlcStatF_Former+Mod10FwkF+Vig10FwkF+StrongFwkF)*(EduF_Graduate+EduF_Primary+EduF_Undergrad), design = svy3)
# df3$predBMI <- predict(glm2,newdata=df3oh)#verifying back against the training set
# df3$predObese <- with(df3,ifelse(predBMI>=3000,1,0)) #classifying as "obese"
# svy3 <- svydesign(id=~PSU, strata=~strata,weights=~PerWeight,nest=TRUE,data=df3)
# 
# #Examining the overall predicted stats
# obeseVec <- svyby(~Obese,by=~RegionF,design=svy3,svymean)[,2]*100
# bmiVec <- svyby(~BMI,by=~RegionF,design=svy3,svymean)[,2]/100
# obesePredVec <- svyby(~predObese,by=~RegionF,design=svy3,svymean)[,2]*100
# bmiPredVec <- svyby(~predBMI,by=~RegionF,design=svy3,svymean)[,2]/100
# compareTable5 <- data.frame(matrix(data=c(obeseVec,bmiVec,obesePredVec,bmiPredVec),nrow=4,ncol=4),row.names = c("MW","NE","S","W"))
# colnames(compareTable5)<-c("Region Obesity %","Region Avg BMI","Pred Obesity","Pred BMI")
# cat("This is the special BMI~(Region)*(Factors) Table:")
# compareTable5





################################## Logistic Regression with interactions

#first, we'll try a model trained on the whole data set
df3 <- as.data.frame(na.omit(df2))
df3oh <- one_hot(as.data.table(df3))
svy3 <- svydesign(id=~PSU, strata=~strata,weights=~PerWeight,nest=TRUE,data=df3oh)
glmL <- svyglm(formula = Obese ~ SexF_M+Age+USBornF+MarstF_ExMarried+MarstF_Married+RaceF_Asian+RaceF_Black+RaceF_Multi+RaceF_Native+SmokeStatF_Current+SmokeStatF_Former+AlcStatF_Current+AlcStatF_Former+Mod10FwkF+Vig10FwkF+StrongFwkF+EduF_Graduate+EduF_Primary+EduF_Undergrad, design = svy3, family="binomial")

df3$logitPredBMI <- predict(glmL,newdata=df3oh) #we're "checking our work" on the original data set...
df3$predObese <- with(df3,ifelse(logitPredBMI>=-.749,1,0)) #classifying as "obese"
svy3 <- svydesign(id=~PSU, strata=~strata,weights=~PerWeight,nest=TRUE,data=df3)

totObese <- svymean(~Obese,svy3)[1]*100
obeseVec <- svyby(~Obese,by=~RegionF,design=svy3,svymean)[,2]*100
predObese <- svymean(~predObese,svy3)[1]*100
obesePredVec <- svyby(~predObese,by=~RegionF,design=svy3,svymean)[,2]*100
compareTable4 <- data.frame(matrix(data=c(totObese,obeseVec,predObese,obesePredVec),nrow=5,ncol=2),row.names = c("Tot___","MW","NE","S","W"))
colnames(compareTable4)<-c("Region Obesity %","Pred Obesity")
cat("This is the Logistic Obesity ~ Factors Table:")
compareTable4

#R-squared for the first logistic model
df3$obeseTest <- df3$predObese-df3$Obese
svy3 <- svydesign(id=~PSU, strata=~strata,weights=~PerWeight,nest=TRUE,data=df3)
resultsBase <- svytable(~obeseTest,design=svy3)

#now, data with whole training set and then using regions to see if that makes us more accurate
df3 <- as.data.frame(na.omit(df2))
df3oh <- one_hot(as.data.table(df3))
svy3 <- svydesign(id=~PSU, strata=~strata,weights=~PerWeight,nest=TRUE,data=df3oh)
glmL <- svyglm(formula = Obese ~ RegionF_MW+RegionF_NE+RegionF_S+SexF_M+Age+USBornF+MarstF_ExMarried+MarstF_Married+RaceF_Asian+RaceF_Black+RaceF_Multi+RaceF_Native+SmokeStatF_Current+SmokeStatF_Former+AlcStatF_Current+AlcStatF_Former+Mod10FwkF+Vig10FwkF+StrongFwkF+EduF_Graduate+EduF_Primary+EduF_Undergrad, design = svy3, family="binomial")

df3$logitPredBMI <- predict(glmL,newdata=df3oh) #we're "checking our work" on the original data set...
df3$predObese <- with(df3,ifelse(logitPredBMI>=-.74124,1,0)) #classifying as "obese"
svy3 <- svydesign(id=~PSU, strata=~strata,weights=~PerWeight,nest=TRUE,data=df3)

totObese <- svymean(~Obese,svy3)[1]*100
obeseVec <- svyby(~Obese,by=~RegionF,design=svy3,svymean)[,2]*100
predObese <- svymean(~predObese,svy3)[1]*100
obesePredVec <- svyby(~predObese,by=~RegionF,design=svy3,svymean)[,2]*100
compareTable5 <- data.frame(matrix(data=c(totObese,obeseVec,predObese,obesePredVec),nrow=5,ncol=2),row.names = c("Tot___","MW","NE","S","W"))
colnames(compareTable5)<-c("Region Obesity %","Pred Obesity")
cat("This is the Logistic Obesity ~ Region+Factors Table:")
compareTable5

df3$obeseTest <- df3$predObese-df3$Obese
svy3 <- svydesign(id=~PSU, strata=~strata,weights=~PerWeight,nest=TRUE,data=df3)
resultsRegion <- svytable(~obeseTest,design=svy3)


#data with whole training set and then using interactions between region/factor
df3 <- as.data.frame(na.omit(df2))
df3oh <- one_hot(as.data.table(df3))
svy3 <- svydesign(id=~PSU, strata=~strata,weights=~PerWeight,nest=TRUE,data=df3oh)
glmL <- svyglm(formula = Obese ~ (RegionF_MW+RegionF_NE+RegionF_S)*(SexF_M+Age+USBornF+MarstF_ExMarried+MarstF_Married+RaceF_Asian+RaceF_Black+RaceF_Multi+RaceF_Native+SmokeStatF_Current+SmokeStatF_Former+AlcStatF_Current+AlcStatF_Former+Mod10FwkF+Vig10FwkF+StrongFwkF+EduF_Graduate+EduF_Primary+EduF_Undergrad), design = svy3, family="binomial")

df3$logitPredBMI <- predict(glmL,newdata=df3oh) #we're "checking our work" on the original data set...
df3$predObese <- with(df3,ifelse(logitPredBMI>=-.7339,1,0)) #classifying as "obese"
svy3 <- svydesign(id=~PSU, strata=~strata,weights=~PerWeight,nest=TRUE,data=df3)

totObese <- svymean(~Obese,svy3)[1]*100
obeseVec <- svyby(~Obese,by=~RegionF,design=svy3,svymean)[,2]*100
predObese <- svymean(~predObese,svy3)[1]*100
obesePredVec <- svyby(~predObese,by=~RegionF,design=svy3,svymean)[,2]*100
compareTable6 <- data.frame(matrix(data=c(totObese,obeseVec,predObese,obesePredVec),nrow=5,ncol=2),row.names = c("Tot___","MW","NE","S","W"))
colnames(compareTable6)<-c("Region Obesity %","Pred Obesity")
cat("This is the Logistic Obesity ~ Region*Factors Table:")
compareTable6

df3$obeseTest <- df3$predObese-df3$Obese
svy3 <- svydesign(id=~PSU, strata=~strata,weights=~PerWeight,nest=TRUE,data=df3)
resultsInteraction <- svytable(~obeseTest,design=svy3)

#This lets us compare results from across all three of the logistic regression models
n=sum(resultsBase)
accuracyTable <- data.frame(matrix(data=c(resultsBase/n,resultsRegion/n,resultsInteraction/n),nrow=3,ncol=3),row.names = c("F-Neg","Accurate","F-Pos"))
colnames(accuracyTable)<-c("Base","w/Regions","R*F")
cat("This compares the three logistic regression performances:")
accuracyTable

#Train on one region...
# df3S <- as.data.frame(na.omit(df2[df2$RegionF=="S",]))
# df3Soh <- one_hot(as.data.table(df3S))
# df3N <- as.data.frame(na.omit(df2[df2$RegionF=="NE",]))
# df3Noh <- one_hot(as.data.table(df2N))
# df3W <- as.data.frame(na.omit(df2[df2$RegionF=="W",]))
# df3Woh <- one_hot(as.data.table(df3W))
# df3MW <- as.data.frame(na.omit(df2[df2$RegionF=="MW",]))
# df3MWoh <- one_hot(as.data.table(df3MW))
# svy3 <- svydesign(id=~PSU, strata=~strata,weights=~PerWeight,nest=TRUE,data=df3Woh)
# glmT <- svyglm(formula = Obese ~ SexF_M+Age+USBornF+MarstF_ExMarried+MarstF_Married+RaceF_Asian+RaceF_Black+RaceF_Multi+RaceF_Native+SmokeStatF_Current+SmokeStatF_Former+AlcStatF_Current+AlcStatF_Former+Mod10FwkF+Vig10FwkF+StrongFwkF+EduF_Graduate+EduF_Primary+EduF_Undergrad, design = svy3, family="binomial")
# 
# df3 <- as.data.frame(na.omit(df2))
# df3oh <- one_hot(as.data.table(df3))
# df3$logitPredBMI <- predict(glmT,newdata=df3oh) #we're "checking our work" on the original data set...
# df3$predObese <- with(df3,ifelse(logitPredBMI>=-.83525,1,0)) #classifying as "obese"
# svy3 <- svydesign(id=~PSU, strata=~strata,weights=~PerWeight,nest=TRUE,data=df3)
# 
# totObese <- svymean(~Obese,svy3)[1]*100
# obeseVec <- svyby(~Obese,by=~RegionF,design=svy3,svymean)[,2]*100
# predObese <- svymean(~predObese,svy3)[1]*100
# obesePredVec <- svyby(~predObese,by=~RegionF,design=svy3,svymean)[,2]*100
# compareTable7 <- data.frame(matrix(data=c(totObese,obeseVec,predObese,obesePredVec),nrow=5,ncol=2),row.names = c("Tot___","MW","NE","S","W"))
# colnames(compareTable7)<-c("Region Obesity %","Pred Obesity")
# cat("This is the Logistic Obesity ~ Region+Factors Table:")
# compareTable7




######################################################## Trees
BMIdf2<-data.frame(BMIdf)
library(rpart)
library(rpart.plot)

df2 <- BMIdf2[,table2List]
df3 <- as.data.frame(na.omit(df2))

## this is the "basic" Tree model, using a basic modification of the complexity parameter (->9 levels)
treeModel <- rpart(Obese~SexF+Age+USBornF+MarstF+RaceF+SmokeStatF+AlcStatF+Mod10FwkF+Vig10FwkF+StrongFwkF+EduF+RegionF,
                   data = df3,weights = PerWeight,control=rpart.control(cp=.001))
rpart.plot(treeModel)
#testing utility against the training set...
options( survey.lonely.psu = "adjust" )
df3$OpredObese <- predict(treeModel,newdata=df3)
df3$predObese <- ifelse(df3$OpredObese>.33,1,0)
treeSvy <- svydesign(id=~PSU, strata=~strata,weights=~PerWeight,nest=TRUE,data=df3)

totObese <- svymean(~Obese,treeSvy)[1]*100
obeseVec <- svyby(~Obese,by=~RegionF,design=treeSvy,svymean)[,2]*100
predObese <- svymean(~predObese,treeSvy)[1]*100
obesePredVec <- svyby(~predObese,by=~RegionF,design=treeSvy,svymean)[,2]*100
compareTable7 <- data.frame(matrix(data=c(totObese,obeseVec,predObese,obesePredVec),nrow=5,ncol=2),row.names = c("Tot___","MW","NE","S","W"))
colnames(compareTable7)<-c("Region Obesity %","Pred Obesity")
cat("Basic Tree BMI ~ Factors, by region, table:")
compareTable7
#summary(treeModel)

df3$obeseTest <- df3$predObese-df3$Obese
svy3 <- svydesign(id=~PSU, strata=~strata,weights=~PerWeight,nest=TRUE,data=df3)
baseTree <- svytable(~obeseTest,design=svy3)

## Let's try a tree that allows for more nuanced splitting
df3 <- as.data.frame(na.omit(df2))

treeModel <- rpart(Obese~SexF+Age+USBornF+MarstF+RaceF+SmokeStatF+AlcStatF+Mod10FwkF+Vig10FwkF+StrongFwkF+EduF+RegionF,
                   data = df3,weights = PerWeight,control=rpart.control(cp=.0005))
rpart.plot(treeModel)
df3$OpredObese <- predict(treeModel,newdata=df3)
df3$predObese <- ifelse(df3$OpredObese>.31,1,0)
treeSvy <- svydesign(id=~PSU, strata=~strata,weights=~PerWeight,nest=TRUE,data=df3)

totObese <- svymean(~Obese,treeSvy)[1]*100
obeseVec <- svyby(~Obese,by=~RegionF,design=treeSvy,svymean)[,2]*100
predObese <- svymean(~predObese,treeSvy)[1]*100
obesePredVec <- svyby(~predObese,by=~RegionF,design=treeSvy,svymean)[,2]*100
compareTable8 <- data.frame(matrix(data=c(totObese,obeseVec,predObese,obesePredVec),nrow=5,ncol=2),row.names = c("Tot___","MW","NE","S","W"))
colnames(compareTable8)<-c("Region Obesity %","Pred Obesity")
cat("Sensitive Tree BMI ~ Factors, by region, table:")
compareTable8
#summary(treeModel)

df3$obeseTest <- df3$predObese-df3$Obese
svy3 <- svydesign(id=~PSU, strata=~strata,weights=~PerWeight,nest=TRUE,data=df3)
midTree <- svytable(~obeseTest,design=svy3)

## Let's try one more tree with an even smaller cp
df3 <- as.data.frame(na.omit(df2))

treeModel <- rpart(Obese~SexF+Age+USBornF+MarstF+RaceF+SmokeStatF+AlcStatF+Mod10FwkF+Vig10FwkF+StrongFwkF+EduF+RegionF,
                   data = df3,weights = PerWeight,control=rpart.control(cp=.0001))
rpart.plot(treeModel)
df3$OpredObese <- predict(treeModel,newdata=df3)
df3$predObese <- ifelse(df3$OpredObese>.35,1,0)
treeSvy <- svydesign(id=~PSU, strata=~strata,weights=~PerWeight,nest=TRUE,data=df3)

totObese <- svymean(~Obese,treeSvy)[1]*100
obeseVec <- svyby(~Obese,by=~RegionF,design=treeSvy,svymean)[,2]*100
predObese <- svymean(~predObese,treeSvy)[1]*100
obesePredVec <- svyby(~predObese,by=~RegionF,design=treeSvy,svymean)[,2]*100
compareTable9 <- data.frame(matrix(data=c(totObese,obeseVec,predObese,obesePredVec),nrow=5,ncol=2),row.names = c("Tot___","MW","NE","S","W"))
colnames(compareTable9)<-c("Region Obesity %","Pred Obesity")
cat("Most Sensitive Tree BMI ~ Factors, by region, table:")
compareTable9
#summary(treeModel)

df3$obeseTest <- df3$predObese-df3$Obese
svy3 <- svydesign(id=~PSU, strata=~strata,weights=~PerWeight,nest=TRUE,data=df3)
fineTree <- svytable(~obeseTest,design=svy3)

#this creates my modified confusion table for comparing the "goodness of fit" for the tree models
n=sum(fineTree)
accuracyTable <- 100*data.frame(matrix(data=c(baseTree/n,midTree/n,fineTree/n),nrow=3,ncol=3),row.names = c("F-Neg","Accurate","F-Pos"))
colnames(accuracyTable)<-c("Base","Mid","Fine")
cat("This compares the three tree model performances:")
accuracyTable


### to generate the Subject Characteristic Table
df3 <- as.data.frame(na.omit(df2))
bmiSurvey <- svydesign(id=~PSU, strata=~strata,weights=~PerWeight,nest=TRUE,data=df3)
# svymean(~Age,bmiSurvey) #for age
# svyvar(~Age,bmiSurvey)
# sexCt<-svytotal(~SexF,bmiSurvey) #for sex counts
# sexCt[1]/(sexCt[1]+sexCt[2])
# svymean(~USBornF,bmiSurvey) #for US Born
# svytotal(~USBornF,bmiSurvey)
# marCt<-svytotal(~MarstF,bmiSurvey) #for marriage stats
# marCt/sum(marCt)
# raceCt<-svytotal(~RaceF,bmiSurvey) #for race stats
# raceCt/sum(raceCt)
# smokeCt<-svytotal(~SmokeStatF,bmiSurvey) #for smoking stats
# smokeCt/sum(smokeCt)
# alcCt<-svytotal(~AlcStatF,bmiSurvey) #for alcohol stats
# alcCt/sum(alcCt)
# edCt<-svytotal(~EduF,bmiSurvey) #for regional comparisons
# edCt/sum(edCt)
# regCt<-svytotal(~RegionF,bmiSurvey) #for regional comparisons
# regCt/sum(regCt)

svyby(~Obese,by=~RegionF,design=bmiSurvey,FUN=svymean)
svyby(~Obese,by=~RegionF,design=bmiSurvey,FUN=svyvar)


### Trying to replicate the table of region by obese numbers (From Greg's email o/a 19 OCT)

