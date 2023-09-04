# Title: Analysis of Regional Data v2
# Author: MAJ Devon Zillmer
# Date: began 20 OCT 2022

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

### now beginning analysis to replicate his numbers!
# This next step overwrites the PerWeight with SampWeight/15 (because data pulled from 15 years), as that's what we're "supposed" to use
df$PerWeight <- df$SampWeight/15

#This reduces our df so that we only have those in the "sample" population, and converts NAs
df2 <- df[df$SampWeight!=0,]
df2 <- df2[df2$BMI>0,] # <- this gets us the magical 444,743 sample size he uses
temp <- which(df2$BMI==9999,arr.ind = TRUE)
df2[temp,"BMI"] <- NA
df2$Obese <- with(df2,ifelse(BMI>=3000,1,0))

library(survey)
options( survey.lonely.psu = "adjust" ) #this is in case the svyglm command throws an error flag because only 1 PSU
library(mltools)
library(data.table)
tableList <- c("Obese","BMI","RegionF","SexF","Age","USBornF","MarstF","RaceF","SmokeStatF","AlcStatF","Mod10FwkF","Vig10FwkF","StrongFwkF","EduF","PSU","strata","PerWeight")

# This section builds my various datasets by region
df3 <- df2[,tableList]
svy2 <- svydesign(id=~PSU, strata=~strata,weights=~PerWeight,nest=TRUE,data=df3)
#now to build a basic table to compare
svyby(~Obese,by=~RegionF,design=svy2,svymean)
#this table shows that our percents are the exact same! Victory. 


#Building R1
df3 <- as.data.frame(na.omit(df3))
df3oh <- one_hot(as.data.table(df3))
svy3 <- svydesign(id=~PSU, strata=~strata,weights=~PerWeight,nest=TRUE,data=df3oh)
glmG <- svyglm(formula = BMI ~ SexF_M+Age+USBornF+MarstF_ExMarried+MarstF_Married+RaceF_Asian+RaceF_Black+RaceF_Multi+RaceF_Native+SmokeStatF_Current+SmokeStatF_Former+AlcStatF_Current+AlcStatF_Former+Mod10FwkF+Vig10FwkF+StrongFwkF+EduF_Graduate+EduF_Primary+EduF_Undergrad, design = svy3)
df3$predBMI <- predict(glmG,newdata=df3oh)#verifying back against the training set
df3$predObese <- with(df3,ifelse(predBMI>=3000,1,0)) #classifying as "obese"
svy3 <- svydesign(id=~PSU, strata=~strata,weights=~PerWeight,nest=TRUE,data=df3)

obeseNat <- svymean(~Obese,design=svy3)[1]*100

obeseVec <- svyby(~Obese,by=~RegionF,design=svy3,svymean)[,2]*100
bmiVec <- svyby(~BMI,by=~RegionF,design=svy3,svymean)[,2]/100
obesePredVec <- svyby(~predObese,by=~RegionF,design=svy3,svymean)[,2]*100
bmiPredVec <- svyby(~predBMI,by=~RegionF,design=svy3,svymean)[,2]/100
compareTable <- data.frame(matrix(data=c(obeseVec,bmiVec,obesePredVec,bmiPredVec),nrow=4,ncol=4),row.names = c("MW","NE","S","W"))
colnames(compareTable)<-c("Region Obesity %","Region Avg BMI","Pred Obesity","Pred BMI")
cat("This is the BMI~Factors Table:")
compareTable





