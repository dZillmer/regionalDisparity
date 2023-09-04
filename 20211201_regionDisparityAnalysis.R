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


### Data filtering to examine BMI versus no BMI, specifically using the BMI NOT BMICalc ###
# Using BMI rather than BMICalc
noBMIdf <- df[df$BMI %in% c(0,9999),]
temp <- df[df$BMI>0,]
BMIdf <- temp[temp$BMI<9980,] #<- gives how many HAVE BMI using original BMI
BMIdf$Obese <- with(BMIdf,ifelse(BMI>=3000,1,0))
BMIdf$Obese <- as.factor(BMIdf$Obese)
# Using BMICalc rather than BMI
# noBMIdf <- df[df$BMICalc %in% c(0,9960),]
# temp <- df[df$BMICalc>0,]
# BMIdf <- temp[temp$BMICalc<996,] #<- gives how many HAVE BMICalc using original BMICalc
# BMIdf$Obese <- with(BMIdf,ifelse(BMICalc>=300,1,0))




### This is to try to use the 'survey' package to utilize the 'perweight' feature of the NHANES data ###
library(survey)

### The following reproduces Table 1. ### Note, original numbers used BMICalc
yearList <- c(2004:2018)
tbl1Titles <- c("Year","Unweighted N","NE","MW","S","W")
tbl1 <- data.frame(matrix(ncol=6,nrow=length(yearList)))
colnames(tbl1) <- tbl1Titles


for (i in 1:length(yearList)) {
  tempDF <- BMIdf[BMIdf$Year==yearList[i],]
  tempSvy <- svydesign(id=~PSU, strata=~strata,weights=~PerWeight,nest=TRUE,data=tempDF)
  pctTbl <- svyby(~Obese,by=~RegionF,tempSvy,svytotal)
  pctTbl[,"se"] <- svytable(~RegionF,tempSvy)
  pctTbl[,"Per Cent"] <- round(100*pctTbl[,"Obese"]/pctTbl[,"se"],1)
  #now that we've built the table we want, we fill in the respective row for our Table 1
  tbl1[i,"Year"] <- yearList[i]
  tbl1[i,"Unweighted N"] <- dim(tempDF)[1]
  tbl1[i,"NE"] <- pctTbl["NE","Per Cent"]
  tbl1[i,"MW"] <- pctTbl["MW","Per Cent"]
  tbl1[i,"S"] <- pctTbl["S","Per Cent"]
  tbl1[i,"W"] <- pctTbl["W","Per Cent"]
}
tbl1



### To reproduce Table 2.
#library(caret)
library(mltools)
library(data.table)
table2List <- c("Obese","BMI","RegionF","SexF","Age","USBornF","MarstF","RaceF","SmokeStatF","AlcStatF","Mod10FwkF","Vig10FwkF","StrongFwkF","EduF","PSU","strata","PerWeight")

# This section builds my various datasets by region
df2 <- BMIdf[,table2List]
df2S <- as.data.frame(na.omit(df2[df2$RegionF=="S",]))
df2Soh <- one_hot(as.data.table(df2S))
df2N <- as.data.frame(na.omit(df2[df2$RegionF=="NE",]))
df2Noh <- one_hot(as.data.table(df2N))
df2W <- as.data.frame(na.omit(df2[df2$RegionF=="W",]))
df2Woh <- one_hot(as.data.table(df2W))
df2MW <- as.data.frame(na.omit(df2[df2$RegionF=="MW",]))
df2MWoh <- one_hot(as.data.table(df2MW))

options( survey.lonely.psu = "adjust" ) #this is in case the svyglm command throws an error flag because only 1 PSU

# Then I train a model on the Southern region's data, using individual data to predict BMI, then the region
# the most effective direct linear model uses BMI^2 and gets much close overall
svy2S <- svydesign(id=~PSU, strata=~strata,weights=~PerWeight,nest=TRUE,data=df2Soh)

glmS <- svyglm(formula = BMI^2 ~ SexF_M+Age+USBornF+MarstF_ExMarried+MarstF_Married+RaceF_Asian+RaceF_Black+RaceF_Multi+RaceF_Native+SmokeStatF_Current+SmokeStatF_Former+AlcStatF_Current+AlcStatF_Former+Mod10FwkF+Vig10FwkF+StrongFwkF+EduF_Graduate+EduF_Primary+EduF_Undergrad, design = svy2S)
#Complete listing for laster reference/testing
#SexF_M+Age+USBornF+MarstF_Ex-Married+MarstF_Married+RaceF_Asian+RaceF_Black+RaceF_Multi+RaceF_Native+SmokeStatF_Current+SmokeStatF_Former+AlcStatF_Current+AlcStatF_Former+Mod10FwkF+Vig10FwkF+StrongFwkF+EduF_Graduate+EduF_Primary+EduF_Undergrad


df2S$predBMI <- sqrt(predict(glmS,newdata=df2Soh))#note, we're trying to see how accurately it predicted the South's BMI
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



# Now we try to run a logistic regression model
svy2S <- svydesign(id=~PSU, strata=~strata,weights=~PerWeight,nest=TRUE,data=df2Soh)

glmSL <- svyglm(formula = Obese ~ SexF_M+Age+USBornF+MarstF_ExMarried+MarstF_Married+RaceF_Asian+RaceF_Black+RaceF_Multi+RaceF_Native+SmokeStatF_Current+SmokeStatF_Former+AlcStatF_Current+AlcStatF_Former+Mod10FwkF+Vig10FwkF+StrongFwkF+EduF_Graduate+EduF_Primary+EduF_Undergrad, design = svy2S, family="binomial")
#Complete listing for laster reference/testing
#SexF_M+Age+USBornF+MarstF_Ex-Married+MarstF_Married+RaceF_Asian+RaceF_Black+RaceF_Multi+RaceF_Native+SmokeStatF_Current+SmokeStatF_Former+AlcStatF_Current+AlcStatF_Former+Mod10FwkF+Vig10FwkF+StrongFwkF+EduF_Graduate+EduF_Primary+EduF_Undergrad


df2S$logitPredBMI <- predict(glmSL,newdata=df2Soh) #we're "checking our work" on the original data set...
df2S$predObese <- with(df2S,ifelse(logitPredBMI>=-.693285,1,0)) #classifying as "obese"
testS_svy <- svydesign(id=~PSU, strata=~strata,weights=~PerWeight,nest=TRUE,data=df2S)

#Let's compare this model against the other regions...
svy2S <- svydesign(id=~PSU, strata=~strata,weights=~PerWeight,nest=TRUE,data=df2Soh)

#this creates all of my predicted Obese observations
df2N$logitPredBMI <- predict(glmSL,newdata=df2Noh)
df2N$predObese <- with(df2N,ifelse(logitPredBMI>=-.693285,1,0))
df2W$logitPredBMI <- predict(glmSL,newdata=df2Woh)
df2W$predObese <- with(df2W,ifelse(logitPredBMI>=-.693285,1,0))
df2MW$logitPredBMI <- predict(glmSL,newdata=df2MWoh)
df2MW$predObese <- with(df2MW,ifelse(logitPredBMI>=-.693285,1,0))
#make new survey objects
testN_svy<-svydesign(id=~PSU, strata=~strata,weights=~PerWeight,nest=TRUE,data=df2N)
testW_svy<-svydesign(id=~PSU, strata=~strata,weights=~PerWeight,nest=TRUE,data=df2W)
testMW_svy<-svydesign(id=~PSU, strata=~strata,weights=~PerWeight,nest=TRUE,data=df2MW)
#calculating outputs from models
nOp <- svymean(~Obese,testN_svy)[1]*100
npOp <- svymean(~predObese,testN_svy)[1]*100
wOp <- svymean(~Obese,testW_svy)[1]*100
wpOp <- svymean(~predObese,testW_svy)[1]*100
mwOp <- svymean(~Obese,testMW_svy)[1]*100
mwpOp <- svymean(~predObese,testMW_svy)[1]*100
sOp <- svymean(~Obese,testS_svy)[1]*100
spOp <- svymean(~predObese,testS_svy)[1]*100
allCheck <- data.frame(matrix(data=c(nOp,wOp,mwOp,sOp,npOp,wpOp,mwpOp,spOp),nrow=4,ncol=2),row.names = c("NorthEast","West","MidWest","South (Tng)"))
colnames(allCheck)<-c("Data Per Cent","Predicted Per Cent")
allCheck



### Now we create a basic tree...
BMIdf<-data.frame(BMIdf)
library(rpart)
library(rpart.plot)

df2 <- BMIdf[,table2List]
df2S <- as.data.frame(na.omit(df2[df2$RegionF=="S",]))

treeModel <- rpart(Obese~SexF+Age+USBornF+MarstF+RaceF+SmokeStatF+AlcStatF+Mod10FwkF+Vig10FwkF+StrongFwkF+EduF,
                   data = data.frame(df2S),weights = PerWeight,control=rpart.control(cp=.0004))
rpart.plot(treeModel)
#testing utility against the training set...
options( survey.lonely.psu = "adjust" )
df2S$OpredObese <- predict(treeModel,newdata=df2S)#note, we're trying to see how accurately it predicted the South's BMI
df2S$predObese <- ifelse(df2S$OpredObese[,2]>.3,1,0)
testS_svy <- svydesign(id=~PSU, strata=~strata,weights=~PerWeight,nest=TRUE,data=df2S)

# #Examining the south's predicted stats
# sOp <- svymean(~Obese,testS_svy)[2]*100
# spOp <- svymean(~predObese,testS_svy)[1]*100
# southCheck <- data.frame(matrix(data=c(sOp,spOp),nrow=1,ncol=2))
# colnames(southCheck)<-c("Data Per Cent","Tree Prediction")
# southCheck

df2N <- as.data.frame(na.omit(df2[df2$RegionF=="NE",]))
df2N$OpredObese <- predict(treeModel,newdata=df2N)#note, we're trying to see how accurately it predicted the South's BMI
df2N$predObese <- ifelse(df2N$OpredObese[,2]>.3,1,0)
testN_svy <- svydesign(id=~PSU, strata=~strata,weights=~PerWeight,nest=TRUE,data=df2N)

df2W <- as.data.frame(na.omit(df2[df2$RegionF=="W",]))
df2W$OpredObese <- predict(treeModel,newdata=df2W)#note, we're trying to see how accurately it predicted the South's BMI
df2W$predObese <- ifelse(df2W$OpredObese[,2]>.3,1,0)
testW_svy <- svydesign(id=~PSU, strata=~strata,weights=~PerWeight,nest=TRUE,data=df2W)

df2MW <- as.data.frame(na.omit(df2[df2$RegionF=="MW",]))
df2MW$OpredObese <- predict(treeModel,newdata=df2MW)#note, we're trying to see how accurately it predicted the South's BMI
df2MW$predObese <- ifelse(df2MW$OpredObese[,2]>.3,1,0)
testMW_svy <- svydesign(id=~PSU, strata=~strata,weights=~PerWeight,nest=TRUE,data=df2MW)

#calculating outputs from models
nOp <- svymean(~Obese,testN_svy)[2]*100
npOp <- svymean(~predObese,testN_svy)[1]*100
wOp <- svymean(~Obese,testW_svy)[2]*100
wpOp <- svymean(~predObese,testW_svy)[1]*100
mwOp <- svymean(~Obese,testMW_svy)[2]*100
mwpOp <- svymean(~predObese,testMW_svy)[1]*100
sOp <- svymean(~Obese,testS_svy)[2]*100
spOp <- svymean(~predObese,testS_svy)[1]*100
allCheck <- data.frame(matrix(data=c(nOp,wOp,mwOp,sOp,npOp,wpOp,mwpOp,spOp),nrow=4,ncol=2),row.names = c("NorthEast","West","MidWest","South (Tng)"))
colnames(allCheck)<-c("Data Per Cent","Predicted Per Cent")
allCheck
# yuck... another bad model. We'll try an additional tree.

#final single tree:
df2 <- BMIdf[,table2List]

treeModel2 <- rpart(Obese~RegionF+SexF+Age+USBornF+MarstF+RaceF+SmokeStatF+AlcStatF+Mod10FwkF+Vig10FwkF+StrongFwkF+EduF,
                   data = data.frame(df2),weights = PerWeight,control=rpart.control(cp=.0001))
rpart.plot(treeModel2)

options( survey.lonely.psu = "adjust" )
df2S$OpredObese <- predict(treeModel2,newdata=df2S)#note, we're trying to see how accurately it predicted the South's BMI
df2S$predObese <- ifelse(df2S$OpredObese[,2]>.3,1,0)
testS_svy <- svydesign(id=~PSU, strata=~strata,weights=~PerWeight,nest=TRUE,data=df2S)

df2N <- as.data.frame(na.omit(df2[df2$RegionF=="NE",]))
df2N$OpredObese <- predict(treeModel2,newdata=df2N)#note, we're trying to see how accurately it predicted the South's BMI
df2N$predObese <- ifelse(df2N$OpredObese[,2]>.3,1,0)
testN_svy <- svydesign(id=~PSU, strata=~strata,weights=~PerWeight,nest=TRUE,data=df2N)

df2W <- as.data.frame(na.omit(df2[df2$RegionF=="W",]))
df2W$OpredObese <- predict(treeModel2,newdata=df2W)#note, we're trying to see how accurately it predicted the South's BMI
df2W$predObese <- ifelse(df2W$OpredObese[,2]>.3,1,0)
testW_svy <- svydesign(id=~PSU, strata=~strata,weights=~PerWeight,nest=TRUE,data=df2W)

df2MW <- as.data.frame(na.omit(df2[df2$RegionF=="MW",]))
df2MW$OpredObese <- predict(treeModel2,newdata=df2MW)#note, we're trying to see how accurately it predicted the South's BMI
df2MW$predObese <- ifelse(df2MW$OpredObese[,2]>.3,1,0)
testMW_svy <- svydesign(id=~PSU, strata=~strata,weights=~PerWeight,nest=TRUE,data=df2MW)

#calculating outputs from models
nOp <- svymean(~Obese,testN_svy)[2]*100
npOp <- svymean(~predObese,testN_svy)[1]*100
wOp <- svymean(~Obese,testW_svy)[2]*100
wpOp <- svymean(~predObese,testW_svy)[1]*100
mwOp <- svymean(~Obese,testMW_svy)[2]*100
mwpOp <- svymean(~predObese,testMW_svy)[1]*100
sOp <- svymean(~Obese,testS_svy)[2]*100
spOp <- svymean(~predObese,testS_svy)[1]*100
allCheck <- data.frame(matrix(data=c(nOp,wOp,mwOp,sOp,npOp,wpOp,mwpOp,spOp),nrow=4,ncol=2),row.names = c("NorthEast","West","MidWest","South (Tng)"))
colnames(allCheck)<-c("Data Per Cent","Predicted Per Cent")
allCheck



#Real final single tree:
df2 <- BMIdf[,table2List]

treeModel3 <- rpart(Obese~SexF+Age+USBornF+MarstF+RaceF+SmokeStatF+AlcStatF+Mod10FwkF+Vig10FwkF+StrongFwkF+EduF,
                    data = data.frame(df2),subset = RegionF=="S",weights = PerWeight,control=rpart.control(cp=.0001,maxdepth=15))
rpart.plot(treeModel3)


options( survey.lonely.psu = "adjust" )
df2S$OpredObese <- predict(treeModel3,newdata=df2S)#note, we're trying to see how accurately it predicted the South's BMI
df2S$predObese <- ifelse(df2S$OpredObese[,2]>.335,1,0)
testS_svy <- svydesign(id=~PSU, strata=~strata,weights=~PerWeight,nest=TRUE,data=df2S)

df2N <- as.data.frame(na.omit(df2[df2$RegionF=="NE",]))
df2N$OpredObese <- predict(treeModel3,newdata=df2N)#note, we're trying to see how accurately it predicted the South's BMI
df2N$predObese <- ifelse(df2N$OpredObese[,2]>.335,1,0)
testN_svy <- svydesign(id=~PSU, strata=~strata,weights=~PerWeight,nest=TRUE,data=df2N)

df2W <- as.data.frame(na.omit(df2[df2$RegionF=="W",]))
df2W$OpredObese <- predict(treeModel3,newdata=df2W)#note, we're trying to see how accurately it predicted the South's BMI
df2W$predObese <- ifelse(df2W$OpredObese[,2]>.335,1,0)
testW_svy <- svydesign(id=~PSU, strata=~strata,weights=~PerWeight,nest=TRUE,data=df2W)

df2MW <- as.data.frame(na.omit(df2[df2$RegionF=="MW",]))
df2MW$OpredObese <- predict(treeModel3,newdata=df2MW)#note, we're trying to see how accurately it predicted the South's BMI
df2MW$predObese <- ifelse(df2MW$OpredObese[,2]>.335,1,0)
testMW_svy <- svydesign(id=~PSU, strata=~strata,weights=~PerWeight,nest=TRUE,data=df2MW)

#calculating outputs from models
nOp <- svymean(~Obese,testN_svy)[2]*100
npOp <- svymean(~predObese,testN_svy)[1]*100
wOp <- svymean(~Obese,testW_svy)[2]*100
wpOp <- svymean(~predObese,testW_svy)[1]*100
mwOp <- svymean(~Obese,testMW_svy)[2]*100
mwpOp <- svymean(~predObese,testMW_svy)[1]*100
sOp <- svymean(~Obese,testS_svy)[2]*100
spOp <- svymean(~predObese,testS_svy)[1]*100
allCheck <- data.frame(matrix(data=c(nOp,wOp,mwOp,sOp,npOp,wpOp,mwpOp,spOp),nrow=4,ncol=2),row.names = c("NorthEast","West","MidWest","South (Tng)"))
colnames(allCheck)<-c("Data Per Cent","Predicted Per Cent")
allCheck