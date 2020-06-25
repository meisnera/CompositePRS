#############
#### Clear workspace and load libraries and files
rm(list=ls())
library(data.table)
library(rms)
library(sandwich)
library(MASS)
library(survival)

#### Load PRS and phenotype data
load("/users/ameisner/CompositePRS/update20190415/outcomePRS20191016.Rdata") 
load("/users/ameisner/CompositePRS/update20190415/egfrdata.Rdata") 

#### UKBB participants who have withdrawn consent
to_exclude <- read.csv("/users/ameisner/CompositePRS/update20190415/w17712_20200204.csv",header=F,stringsAsFactors = F)
#############

#############
#### Subset data: keep only unrelated people of with British ancestry
alldata_subset <- alldata[which(alldata$tokeep==1),]
alldata_subset_prev <- merge(alldata_subset,egfrdata,by="subjectID",all.x=TRUE,all.y=FALSE,sort=FALSE) ## merge with eGFR data 
alldata_subset <- alldata_subset_prev[which(!(alldata_subset_prev$subjectID %in% to_exclude[,1])),] ## remove those who have withdrawn consent
####### split into male and female
alldata_subsetF <- alldata_subset[which(alldata_subset$sex=="Female"),]
alldata_subsetM <- alldata_subset[which(alldata_subset$sex=="Male"),]
#############

###############
## Split into training and test
set.seed(3158)
trainIDs <- sample(alldata_subset_prev$subjectID, floor((2/3)*nrow(alldata_subset_prev)), replace=F)
traindat <- alldata_subset_prev[which(alldata_subset_prev$subjectID %in% trainIDs),]
traindat <- traindat[which(!(traindat$subjectID %in% to_exclude[,1])),]
testdat <- alldata_subset_prev[which(!(alldata_subset_prev$subjectID %in% trainIDs)),]
testdat <- testdat[which(!(testdat$subjectID %in% to_exclude[,1])),]
###### Split into men and women
traindatF <- traindat[which(traindat$sex=="Female"),]
testdatF <- testdat[which(testdat$sex=="Female"),]
traindatM <- traindat[which(traindat$sex=="Male"),]
testdatM <- testdat[which(testdat$sex=="Male"),]
#############

###############
## Standardize PRS
traindatF$type2_diabetesPRS_std <- traindatF$type2_diabetesPRS/sd(alldata_subsetF$type2_diabetesPRS) 
traindatF$triglyceridesPRS_std <- traindatF$triglyceridesPRS/sd(alldata_subsetF$triglyceridesPRS) 
traindatF$total_cholesterolPRS_std <- traindatF$total_cholesterolPRS/sd(alldata_subsetF$total_cholesterolPRS) 
traindatF$smoking_statusPRS_std <- traindatF$smoking_statusPRS/sd(alldata_subsetF$smoking_statusPRS) 
traindatF$sleep_durationPRS_std <- traindatF$sleep_durationPRS/sd(alldata_subsetF$sleep_durationPRS) 
traindatF$parkinsonsPRS_std <- traindatF$parkinsonsPRS/sd(alldata_subsetF$parkinsonsPRS) 
traindatF$pancreatic_cancerPRS_std <- traindatF$pancreatic_cancerPRS/sd(alldata_subsetF$pancreatic_cancerPRS) 
traindatF$lung_cancerPRS_std <- traindatF$lung_cancerPRS/sd(alldata_subsetF$lung_cancerPRS) 
traindatF$alzheimersPRS_std <- traindatF$alzheimersPRS/sd(alldata_subsetF$alzheimersPRS) 
traindatF$ldl_cholesterolPRS_std <- traindatF$ldl_cholesterolPRS/sd(alldata_subsetF$ldl_cholesterolPRS) 
traindatF$hypertensionPRS_std <- traindatF$hypertensionPRS/sd(alldata_subsetF$hypertensionPRS) 
traindatF$heart_diseasePRS_std <- traindatF$heart_diseasePRS/sd(alldata_subsetF$heart_diseasePRS) 
traindatF$hdl_cholesterolPRS_std <- traindatF$hdl_cholesterolPRS/sd(alldata_subsetF$hdl_cholesterolPRS) 
traindatF$fasting_plasma_glucosePRS_std <- traindatF$fasting_plasma_glucosePRS/sd(alldata_subsetF$fasting_plasma_glucosePRS) 
traindatF$egfrPRS_std <- traindatF$egfrPRS/sd(alldata_subsetF$egfrPRS) 
traindatF$copdPRS_std <- traindatF$copdPRS/sd(alldata_subsetF$copdPRS) 
traindatF$colorectal_cancerPRS_std <- traindatF$colorectal_cancerPRS/sd(alldata_subsetF$colorectal_cancerPRS) 
traindatF$bmiPRS_std <- traindatF$bmiPRS/sd(alldata_subsetF$bmiPRS) 
traindatF$ckdPRS_std <- traindatF$ckdPRS/sd(alldata_subsetF$ckdPRS) 
traindatF$cirrhosisPRS_std <- traindatF$cirrhosisPRS/sd(alldata_subsetF$cirrhosisPRS) 
traindatF$breast_cancerPRS_std <- traindatF$breast_cancerPRS/sd(alldata_subsetF$breast_cancerPRS) 
traindatF$sbpPRS_std <- traindatF$sbpPRS/sd(alldata_subsetF$sbpPRS) 
traindatF$dbpPRS_std <- traindatF$dbpPRS/sd(alldata_subsetF$dbpPRS) 
traindatF$alcohol_consumptionPRS_std <- traindatF$alcohol_consumptionPRS/sd(alldata_subsetF$alcohol_consumptionPRS) 
traindatF$strokePRS_std <- traindatF$strokePRS/sd(alldata_subsetF$strokePRS) 

testdatF$type2_diabetesPRS_std <- testdatF$type2_diabetesPRS/sd(alldata_subsetF$type2_diabetesPRS) 
testdatF$triglyceridesPRS_std <- testdatF$triglyceridesPRS/sd(alldata_subsetF$triglyceridesPRS) 
testdatF$total_cholesterolPRS_std <- testdatF$total_cholesterolPRS/sd(alldata_subsetF$total_cholesterolPRS) 
testdatF$smoking_statusPRS_std <- testdatF$smoking_statusPRS/sd(alldata_subsetF$smoking_statusPRS) 
testdatF$sleep_durationPRS_std <- testdatF$sleep_durationPRS/sd(alldata_subsetF$sleep_durationPRS) 
testdatF$parkinsonsPRS_std <- testdatF$parkinsonsPRS/sd(alldata_subsetF$parkinsonsPRS) 
testdatF$pancreatic_cancerPRS_std <- testdatF$pancreatic_cancerPRS/sd(alldata_subsetF$pancreatic_cancerPRS) 
testdatF$lung_cancerPRS_std <- testdatF$lung_cancerPRS/sd(alldata_subsetF$lung_cancerPRS) 
testdatF$alzheimersPRS_std <- testdatF$alzheimersPRS/sd(alldata_subsetF$alzheimersPRS) 
testdatF$ldl_cholesterolPRS_std <- testdatF$ldl_cholesterolPRS/sd(alldata_subsetF$ldl_cholesterolPRS) 
testdatF$hypertensionPRS_std <- testdatF$hypertensionPRS/sd(alldata_subsetF$hypertensionPRS) 
testdatF$heart_diseasePRS_std <- testdatF$heart_diseasePRS/sd(alldata_subsetF$heart_diseasePRS) 
testdatF$hdl_cholesterolPRS_std <- testdatF$hdl_cholesterolPRS/sd(alldata_subsetF$hdl_cholesterolPRS) 
testdatF$fasting_plasma_glucosePRS_std <- testdatF$fasting_plasma_glucosePRS/sd(alldata_subsetF$fasting_plasma_glucosePRS) 
testdatF$egfrPRS_std <- testdatF$egfrPRS/sd(alldata_subsetF$egfrPRS) 
testdatF$copdPRS_std <- testdatF$copdPRS/sd(alldata_subsetF$copdPRS) 
testdatF$colorectal_cancerPRS_std <- testdatF$colorectal_cancerPRS/sd(alldata_subsetF$colorectal_cancerPRS) 
testdatF$bmiPRS_std <- testdatF$bmiPRS/sd(alldata_subsetF$bmiPRS) 
testdatF$ckdPRS_std <- testdatF$ckdPRS/sd(alldata_subsetF$ckdPRS) 
testdatF$cirrhosisPRS_std <- testdatF$cirrhosisPRS/sd(alldata_subsetF$cirrhosisPRS) 
testdatF$breast_cancerPRS_std <- testdatF$breast_cancerPRS/sd(alldata_subsetF$breast_cancerPRS) 
testdatF$sbpPRS_std <- testdatF$sbpPRS/sd(alldata_subsetF$sbpPRS) 
testdatF$dbpPRS_std <- testdatF$dbpPRS/sd(alldata_subsetF$dbpPRS) 
testdatF$alcohol_consumptionPRS_std <- testdatF$alcohol_consumptionPRS/sd(alldata_subsetF$alcohol_consumptionPRS) 
testdatF$strokePRS_std <- testdatF$strokePRS/sd(alldata_subsetF$strokePRS) 

traindatM$type2_diabetesPRS_std <- traindatM$type2_diabetesPRS/sd(alldata_subsetM$type2_diabetesPRS) 
traindatM$triglyceridesPRS_std <- traindatM$triglyceridesPRS/sd(alldata_subsetM$triglyceridesPRS) 
traindatM$total_cholesterolPRS_std <- traindatM$total_cholesterolPRS/sd(alldata_subsetM$total_cholesterolPRS) 
traindatM$smoking_statusPRS_std <- traindatM$smoking_statusPRS/sd(alldata_subsetM$smoking_statusPRS) 
traindatM$sleep_durationPRS_std <- traindatM$sleep_durationPRS/sd(alldata_subsetM$sleep_durationPRS) 
traindatM$parkinsonsPRS_std <- traindatM$parkinsonsPRS/sd(alldata_subsetM$parkinsonsPRS) 
traindatM$pancreatic_cancerPRS_std <- traindatM$pancreatic_cancerPRS/sd(alldata_subsetM$pancreatic_cancerPRS) 
traindatM$lung_cancerPRS_std <- traindatM$lung_cancerPRS/sd(alldata_subsetM$lung_cancerPRS) 
traindatM$alzheimersPRS_std <- traindatM$alzheimersPRS/sd(alldata_subsetM$alzheimersPRS) 
traindatM$ldl_cholesterolPRS_std <- traindatM$ldl_cholesterolPRS/sd(alldata_subsetM$ldl_cholesterolPRS) 
traindatM$hypertensionPRS_std <- traindatM$hypertensionPRS/sd(alldata_subsetM$hypertensionPRS) 
traindatM$heart_diseasePRS_std <- traindatM$heart_diseasePRS/sd(alldata_subsetM$heart_diseasePRS) 
traindatM$hdl_cholesterolPRS_std <- traindatM$hdl_cholesterolPRS/sd(alldata_subsetM$hdl_cholesterolPRS) 
traindatM$fasting_plasma_glucosePRS_std <- traindatM$fasting_plasma_glucosePRS/sd(alldata_subsetM$fasting_plasma_glucosePRS) 
traindatM$egfrPRS_std <- traindatM$egfrPRS/sd(alldata_subsetM$egfrPRS) 
traindatM$copdPRS_std <- traindatM$copdPRS/sd(alldata_subsetM$copdPRS) 
traindatM$colorectal_cancerPRS_std <- traindatM$colorectal_cancerPRS/sd(alldata_subsetM$colorectal_cancerPRS) 
traindatM$bmiPRS_std <- traindatM$bmiPRS/sd(alldata_subsetM$bmiPRS) 
traindatM$ckdPRS_std <- traindatM$ckdPRS/sd(alldata_subsetM$ckdPRS) 
traindatM$cirrhosisPRS_std <- traindatM$cirrhosisPRS/sd(alldata_subsetM$cirrhosisPRS) 
traindatM$prostate_cancerPRS_std <- traindatM$prostate_cancerPRS/sd(alldata_subsetM$prostate_cancerPRS) 
traindatM$sbpPRS_std <- traindatM$sbpPRS/sd(alldata_subsetM$sbpPRS) 
traindatM$dbpPRS_std <- traindatM$dbpPRS/sd(alldata_subsetM$dbpPRS) 
traindatM$alcohol_consumptionPRS_std <- traindatM$alcohol_consumptionPRS/sd(alldata_subsetM$alcohol_consumptionPRS) 
traindatM$strokePRS_std <- traindatM$strokePRS/sd(alldata_subsetM$strokePRS) 

testdatM$type2_diabetesPRS_std <- testdatM$type2_diabetesPRS/sd(alldata_subsetM$type2_diabetesPRS) 
testdatM$triglyceridesPRS_std <- testdatM$triglyceridesPRS/sd(alldata_subsetM$triglyceridesPRS) 
testdatM$total_cholesterolPRS_std <- testdatM$total_cholesterolPRS/sd(alldata_subsetM$total_cholesterolPRS) 
testdatM$smoking_statusPRS_std <- testdatM$smoking_statusPRS/sd(alldata_subsetM$smoking_statusPRS) 
testdatM$sleep_durationPRS_std <- testdatM$sleep_durationPRS/sd(alldata_subsetM$sleep_durationPRS) 
testdatM$parkinsonsPRS_std <- testdatM$parkinsonsPRS/sd(alldata_subsetM$parkinsonsPRS) 
testdatM$pancreatic_cancerPRS_std <- testdatM$pancreatic_cancerPRS/sd(alldata_subsetM$pancreatic_cancerPRS) 
testdatM$lung_cancerPRS_std <- testdatM$lung_cancerPRS/sd(alldata_subsetM$lung_cancerPRS) 
testdatM$alzheimersPRS_std <- testdatM$alzheimersPRS/sd(alldata_subsetM$alzheimersPRS) 
testdatM$ldl_cholesterolPRS_std <- testdatM$ldl_cholesterolPRS/sd(alldata_subsetM$ldl_cholesterolPRS) 
testdatM$hypertensionPRS_std <- testdatM$hypertensionPRS/sd(alldata_subsetM$hypertensionPRS) 
testdatM$heart_diseasePRS_std <- testdatM$heart_diseasePRS/sd(alldata_subsetM$heart_diseasePRS) 
testdatM$hdl_cholesterolPRS_std <- testdatM$hdl_cholesterolPRS/sd(alldata_subsetM$hdl_cholesterolPRS) 
testdatM$fasting_plasma_glucosePRS_std <- testdatM$fasting_plasma_glucosePRS/sd(alldata_subsetM$fasting_plasma_glucosePRS) 
testdatM$egfrPRS_std <- testdatM$egfrPRS/sd(alldata_subsetM$egfrPRS) 
testdatM$copdPRS_std <- testdatM$copdPRS/sd(alldata_subsetM$copdPRS) 
testdatM$colorectal_cancerPRS_std <- testdatM$colorectal_cancerPRS/sd(alldata_subsetM$colorectal_cancerPRS) 
testdatM$bmiPRS_std <- testdatM$bmiPRS/sd(alldata_subsetM$bmiPRS) 
testdatM$ckdPRS_std <- testdatM$ckdPRS/sd(alldata_subsetM$ckdPRS) 
testdatM$cirrhosisPRS_std <- testdatM$cirrhosisPRS/sd(alldata_subsetM$cirrhosisPRS) 
testdatM$prostate_cancerPRS_std <- testdatM$prostate_cancerPRS/sd(alldata_subsetM$prostate_cancerPRS) 
testdatM$sbpPRS_std <- testdatM$sbpPRS/sd(alldata_subsetM$sbpPRS) 
testdatM$dbpPRS_std <- testdatM$dbpPRS/sd(alldata_subsetM$dbpPRS) 
testdatM$alcohol_consumptionPRS_std <- testdatM$alcohol_consumptionPRS/sd(alldata_subsetM$alcohol_consumptionPRS) 
testdatM$strokePRS_std <- testdatM$strokePRS/sd(alldata_subsetM$strokePRS) 
#############


##################
### Construct composite PRS
##################

##################
## Females

#### Fit model with all 25 PRS and 10 PCs
ACjoint_F_model <- coxph(formula = Surv(age_entry, death_age, death_ind) ~ type2_diabetesPRS_std + triglyceridesPRS_std + total_cholesterolPRS_std +
	smoking_statusPRS_std + sleep_durationPRS_std + parkinsonsPRS_std + pancreatic_cancerPRS_std + lung_cancerPRS_std + alzheimersPRS_std + 
	ldl_cholesterolPRS_std + hypertensionPRS_std + heart_diseasePRS_std + hdl_cholesterolPRS_std + fasting_plasma_glucosePRS_std + 
	egfrPRS_std + copdPRS_std + colorectal_cancerPRS_std + bmiPRS_std + ckdPRS_std + cirrhosisPRS_std + breast_cancerPRS_std + sbpPRS_std + 
	dbpPRS_std + alcohol_consumptionPRS_std + strokePRS_std + PC1 + PC2 + PC3 + PC4 + 
	PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data=traindatF)

#### Create dataset based on training data with no PCs in order to generate cPRS in training data
ACjoint_F_newdata <- traindatF[,which(grepl("PRS",names(traindatF)))]
ACjoint_F_newdata$PC1 <- ACjoint_F_newdata$PC2 <- ACjoint_F_newdata$PC3 <- ACjoint_F_newdata$PC4 <- ACjoint_F_newdata$PC5 <- rep(0, nrow(traindatF))
ACjoint_F_newdata$PC6 <- ACjoint_F_newdata$PC7 <- ACjoint_F_newdata$PC8 <- ACjoint_F_newdata$PC9 <- ACjoint_F_newdata$PC10 <- rep(0, nrow(traindatF))
LPF <- predict(ACjoint_F_model, newdata=ACjoint_F_newdata,type="lp")

#### Create dataset based on test data with no PCs in order to generate cPRS in test data
testdatF_noPC <- testdatF
testdatF_noPC$PC1 <- testdatF_noPC$PC2 <- testdatF_noPC$PC3 <- testdatF_noPC$PC4 <- testdatF_noPC$PC5 <- testdatF_noPC$PC6 <- rep(0, nrow(testdatF_noPC))
testdatF_noPC$PC7 <- testdatF_noPC$PC8 <- testdatF_noPC$PC9 <- testdatF_noPC$PC10 <- rep(0, nrow(testdatF_noPC))
testdatF$LPF_test <- predict(ACjoint_F_model, newdata=testdatF_noPC,type="lp")
testdatF$LPF_std <- testdatF$LPF_test/sd(LPF) ## standardize cPRS in test based on SD of cPRS in training

##### Fit Cox PH model based on standardized cPRS in test data
table(testdatF$death_ind)
summary(coxph(formula = Surv(age_entry, death_age, death_ind) ~ LPF_std + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10,
	data=testdatF))$coefficients
summary(coxph(formula = Surv(age_entry, death_age, death_ind) ~ LPF_std + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10,
	data=testdatF))$conf.int

#### Estimate AUC
cph(formula = Surv(age_entry, death_age, death_ind) ~ LPF_std, data=testdatF, x=TRUE, y=TRUE)$stats

##### Fit Cox PH using quantiles of cPRS
testdatF$LPquantile <- cut(testdatF$LPF_test, breaks=c(min(testdatF$LPF_test), quantile(LPF, c(0.05, 0.4, 0.6, 0.95), type=8), max(testdatF$LPF_test)), include.lowest=TRUE)
table(testdatF$LPquantile)
testdatF$LPquantile <- relevel(testdatF$LPquantile, ref = names(table(testdatF$LPquantile))[3]) ## make middle 20% the reference
testquantF <- summary(coxph(formula = Surv(age_entry, death_age, death_ind) ~ as.factor(LPquantile) + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10,
	data=testdatF))
testquantF$coefficients
testquantF$conf.int
table(testdatF$LPquantile,testdatF$death_ind)

#### Same as above, but comparing top 5% vs. bottom 5%
testdatF$LPquantile_topbottom <- relevel(testdatF$LPquantile, ref = names(table(testdatF$LPquantile))[2]) ## now the reference is the bottom 5%
testquantF_topbottom <- summary(coxph(formula = Surv(age_entry, death_age, death_ind) ~ as.factor(LPquantile_topbottom) + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10,
	data=testdatF))
testquantF_topbottom$coefficients
testquantF_topbottom$conf.int
table(testdatF$LPquantile_topbottom,testdatF$death_ind)



##################
## Males

#### Fit model with all 25 PRS and 10 PCs
ACjoint_M_model <- coxph(formula = Surv(age_entry, death_age, death_ind) ~ type2_diabetesPRS_std + triglyceridesPRS_std + total_cholesterolPRS_std +
	smoking_statusPRS_std + sleep_durationPRS_std + prostate_cancerPRS_std + parkinsonsPRS_std + pancreatic_cancerPRS_std + lung_cancerPRS_std + alzheimersPRS_std + 
	ldl_cholesterolPRS_std + hypertensionPRS_std + heart_diseasePRS_std + hdl_cholesterolPRS_std + fasting_plasma_glucosePRS_std + 
	egfrPRS_std + copdPRS_std + colorectal_cancerPRS_std + bmiPRS_std + ckdPRS_std + cirrhosisPRS_std + sbpPRS_std + 
	dbpPRS_std + alcohol_consumptionPRS_std + strokePRS_std + PC1 + PC2 + PC3 + PC4 + 
	PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data=traindatM)

#### Create dataset based on training data with no PCs in order to generate cPRS in training data
ACjoint_M_newdata <- traindatM[,which(grepl("PRS",names(traindatM)))]
ACjoint_M_newdata$PC1 <- ACjoint_M_newdata$PC2 <- ACjoint_M_newdata$PC3 <- ACjoint_M_newdata$PC4 <- ACjoint_M_newdata$PC5 <- rep(0, nrow(traindatM))
ACjoint_M_newdata$PC6 <- ACjoint_M_newdata$PC7 <- ACjoint_M_newdata$PC8 <- ACjoint_M_newdata$PC9 <- ACjoint_M_newdata$PC10 <- rep(0, nrow(traindatM))
LPM <- predict(ACjoint_M_model, newdata=ACjoint_M_newdata,type="lp")

#### Create dataset based on test data with no PCs in order to generate cPRS in test data
testdatM_noPC <- testdatM
testdatM_noPC$PC1 <- testdatM_noPC$PC2 <- testdatM_noPC$PC3 <- testdatM_noPC$PC4 <- testdatM_noPC$PC5 <- testdatM_noPC$PC6 <- rep(0, nrow(testdatM_noPC))
testdatM_noPC$PC7 <- testdatM_noPC$PC8 <- testdatM_noPC$PC9 <- testdatM_noPC$PC10 <- rep(0, nrow(testdatM_noPC))
testdatM$LPM_test <- predict(ACjoint_M_model, newdata=testdatM_noPC,type="lp")
testdatM$LPM_std <- testdatM$LPM_test/sd(LPM) ## standardize cPRS in test based on SD of cPRS in training

##### Fit Cox PH model based on standardized cPRS in test data
table(testdatM$death_ind)
summary(coxph(formula = Surv(age_entry, death_age, death_ind) ~ LPM_std + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10,
	data=testdatM))$coefficients
summary(coxph(formula = Surv(age_entry, death_age, death_ind) ~ LPM_std + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10,
	data=testdatM))$conf.int

#### Estimate AUC
cph(formula = Surv(age_entry, death_age, death_ind) ~ LPM_std, data=testdatM, x=TRUE, y=TRUE)$stats

##### Fit Cox PH using quantiles of cPRS
testdatM$LPquantile <- cut(testdatM$LPM_test, breaks=c(min(testdatM$LPM_test), quantile(LPM, c(0.05, 0.4, 0.6, 0.95), type=8), max(testdatM$LPM_test)), include.lowest=TRUE)
table(testdatM$LPquantile)
testdatM$LPquantile <- relevel(testdatM$LPquantile, ref = names(table(testdatM$LPquantile))[3]) ## make middle 20% the reference
testquantM <- summary(coxph(formula = Surv(age_entry, death_age, death_ind) ~ as.factor(LPquantile) + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10,
	data=testdatM))
testquantM$coefficients
testquantM$conf.int
table(testdatM$LPquantile,testdatM$death_ind)

#### Same as above, but comparing top 5% vs. bottom 5%
testdatM$LPquantile_topbottom <- relevel(testdatM$LPquantile, ref = names(table(testdatM$LPquantile))[2]) ## now the reference is the bottom 5%
testquantM_topbottom <- summary(coxph(formula = Surv(age_entry, death_age, death_ind) ~ as.factor(LPquantile_topbottom) + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10,
	data=testdatM))
testquantM_topbottom$coefficients
testquantM_topbottom$conf.int
table(testdatM$LPquantile_topbottom,testdatM$death_ind)



