
#####################################
# Rouault*, Seow*, Gillan and Fleming. (2018) Biological Psychiatry
# Psychiatric symptom dimensions are associated with dissociable shifts in metacognition but not task performance.

# Figures for regression data and factor analysis in Experiment 2

# Data files needed
# 1. ("ME_phase2_excludqnadata_all.mat") questionnaire data
# 2. ("ME_phase2_excludanalyseddat_all.mat") task performance data
# 3. ('subjParams_2k_3chain.csv') HDDM variables

#####################################
# Sections are as follows:
#REGRESSIONS 1) PERFORMANCE/METACOG ~ DEMOGRAPHICS
#REGRESSIONS 2) HDDM VARIABLES ~ DEMOGRAPHICS
#REGRESSIONS 3) PERFORMANCE/METACOG ~ SYMPTOM SCORES 
#REGRESSIONS 4) HDDM VARIABLES ~ SYMPTOM SCORES 
#REGRESSIONS 5) PERFORMANCE/METCOG/HDDM ~ FACTOR SCORES 
#PLOT 1) PERFORMANCE/METACOG ~ DEMOGRAPHICS 
#PLOT 2) PERFORMANCE/METACOG ~ SYMPTOM SCORES 
#PLOT 3) PERFORMANCE/METACOG/HDDM ~ FACTOR SCORES



##########
## LOADING LIBRARIES/TOOL
# loading tools
library(ggplot2) # for plotting graphs
library(gridExtra) # for ploting graphs
library(lme4) # for linear regression functions
library(plyr) # for collapse-and-mean functions like ddply
library(psych)
library(GPArotation)
library(paran)
library(reshape)
library(polycor)
library(nFactors)
library(R.matlab)
library(reshape)
library(doBy)



##########
## LOADING DATA
qnData = readMat("data/ME_phase2_excludqnadata_all.mat") # load questionnaire data
taskData = readMat("data/ME_phase2_excludanalyseddat_all.mat") # load task performance data
HDDM = read.csv('data/subjParams_2k_3chain.csv') # load HDDM data
HDDMpara = data.frame(t(HDDM[1:nrow(HDDM),2:length(HDDM)]))
colnames(HDDMpara) <- c("a", "t", "v_inter", "v_delta")

##########
## CREATE EMPTY OBJECTS
# create objects for variables from task performance data
id<-matrix(0,length(taskData$analyseddata),1) # subject id
age<-matrix(0,length(taskData$analyseddata),1)
gender<-matrix(0,length(taskData$analyseddata),1)
accuracy<-matrix(0,length(taskData$analyseddata),1) # accuracy
mRatio<-matrix(0,length(taskData$analyseddata),1)   # metacognitive efficiency
confMean<-matrix(0,length(taskData$analyseddata),1) # mean confidence

# create objects for variables from task questionnaire data
qnid<-matrix(0,length(qnData$allqna),1) # subject id
zung<-matrix(0,length(qnData$allqna),1)
anxiety<-matrix(0,length(qnData$allqna),1)
ocir<-matrix(0,length(qnData$allqna),1)
leb<-matrix(0,length(qnData$allqna),1)
iq<-matrix(0,length(qnData$allqna),1)
bis<-matrix(0,length(qnData$allqna),1)
schizo<-matrix(0,length(qnData$allqna),1)
eat<-matrix(0,length(qnData$allqna),1)
apathy<-matrix(0,length(qnData$allqna),1)
alcohol<-matrix(0,length(qnData$allqna),1)

##########
## EXTRACTING DATA
# extracting data from allqna data file
# loop over for all subjects
for (i in 1:length(qnData$allqna)) 
{
  qnid[i] = qnData$allqna[[i]][[1]][,,1]$id
  zung[i] = qnData$allqna[[i]][[1]][,,1]$zung[,,1]$score #first brackets is subject number
  anxiety[i] = qnData$allqna[[i]][[1]][,,1]$anxiety[,,1]$score
  ocir[i] = qnData$allqna[[i]][[1]][,,1]$ocir[,,1]$score
  leb[i] = qnData$allqna[[i]][[1]][,,1]$leb[,,1]$score
  iq[i] = qnData$allqna[[i]][[1]][,,1]$iq[,,1]$score
  bis[i] = qnData$allqna[[i]][[1]][,,1]$bis[,,1]$score[,,1]$total
  schizo[i] = qnData$allqna[[i]][[1]][,,1]$schizo[,,1]$score[,,1]$total
  eat[i] = qnData$allqna[[i]][[1]][,,1]$eat[,,1]$score[,,1]$total
  apathy[i] = qnData$allqna[[i]][[1]][,,1]$apathy[,,1]$score
  alcohol[i] = qnData$allqna[[i]][[1]][,,1]$alcohol[,,1]$score
}

# extracting data from analysed data
# loop over for all subjects
for (i in 1:length(taskData$analyseddata))
{
  id[i]=taskData$analyseddata[[i]][[1]][,,1]$data[1,4]
  age[i] =taskData$analyseddata[[i]][[1]][,,1]$data[1,2]
  gender[i]=taskData$analyseddata[[i]][[1]][,,1]$data[1,3]
  confMean[i] = mean(taskData$analyseddata[[i]][[1]][,,1]$data[,9])
  accuracy[i] = mean(taskData$analyseddata[[i]][[1]][,,1]$data[,6])
  mRatio[i] = taskData$analyseddata[[i]][[1]][,,1]$mratio
}

# set gender as factor (male or female)
gender <- factor(gender)

##########
## MERGING DATA

# create dataframe to store questionnaire data
qnFrame = data.frame(qnid, anxiety, eat, apathy, alcohol, zung, ocir, leb, iq, bis, schizo)
# create dataframe to store task performance data
taskFrame = data.frame(id,age,gender,confMean,accuracy,mRatio)
# merge all data together into one data frame
allData =merge(taskFrame, qnFrame,by.x=c("id"), by.y=c("qnid"))
# join HDDM variables to existing dataframe
allData=data.frame(allData,HDDMpara)

##########
## SCALING DATA
#scaling the task performance
allData$age.sc = scale(allData$age)
allData$confMean.sc = scale(allData$confMean)
allData$accuracy.sc = scale(allData$accuracy)

# scaling the questionnaire scores
allData$zung.sc = scale(log(allData$zung))
allData$anxiety.sc = scale(log(allData$anxiety))
allData$ocir.sc = scale(log(allData$ocir+1))
allData$leb.sc = scale(log(allData$leb+1))
allData$iq.sc = scale(allData$iq)
allData$schizo.sc = scale(log(allData$schizo+1))
allData$bis.sc = scale(log(allData$bis))
allData$eat.sc = scale(log(allData$eat+1))
allData$apathy.sc = scale(log(allData$apathy))
allData$alcohol.sc = scale(log(allData$alcohol+1))

# scale HDDM variables
allData$a.sc = scale(allData$a)
allData$t.sc = scale(allData$t)
allData$v_inter.sc = scale(allData$v_inter)
allData$v_delta.sc = scale(allData$v_delta)

#exclude negative mRatios and scale the mRatios of the subjects left
mrExcludedData <- allData[allData$mRatio>0,] 
mrExcludedData$mRatio.sc = scale(log(mrExcludedData$mRatio))


####################
##  FACTOR ANALYSIS 
# LOAD ALL QUESTIONNAIRE (individual questions) DATA
# create objects
qnIndivid<-matrix(0,length(qnData$allqna),1)
zungAll<-matrix(0,length(qnData$allqna),length(qnData$allqna[[1]][[1]][,,1]$zung[,,1]$raw))
anxietyAll<-matrix(0,length(qnData$allqna),length(qnData$allqna[[1]][[1]][,,1]$anxiety[,,1]$raw))
ocirAll<-matrix(0,length(qnData$allqna),length(qnData$allqna[[1]][[1]][,,1]$ocir[,,1]$raw))
lebAll<-matrix(0,length(qnData$allqna),length(qnData$allqna[[1]][[1]][,,1]$leb[,,1]$raw[,,1]$avg))
bisAll<-matrix(0,length(qnData$allqna),length(qnData$allqna[[1]][[1]][,,1]$bis[,,1]$raw))
schizoAll<-matrix(0,length(qnData$allqna),length(qnData$allqna[[1]][[1]][,,1]$schizo[,,1]$raw))
eatAll<-matrix(0,length(qnData$allqna),length(qnData$allqna[[1]][[1]][,,1]$eat[,,1]$raw))
apathyAll<-matrix(0,length(qnData$allqna),length(qnData$allqna[[1]][[1]][,,1]$apathy[,,1]$raw))
alcoholAll<-matrix(0,length(qnData$allqna),length(qnData$allqna[[1]][[1]][,,1]$alcohol[,,1]$raw))

# extracting data from allqna
for (i in 1:length(qnData$allqna))
{
  qnIndivid[i,]=qnData$allqna[[i]][[1]][,,1]$id
  zungAll[i,] = qnData$allqna[[i]][[1]][,,1]$zung[,,1]$raw #first brackets is subject number
  anxietyAll[i,] = t(qnData$allqna[[i]][[1]][,,1]$anxiety[,,1]$raw)
  ocirAll[i,] = qnData$allqna[[i]][[1]][,,1]$ocir[,,1]$raw
  lebAll[i,] = (qnData$allqna[[i]][[1]][,,1]$leb[,,1]$raw[,,1]$avg)
  bisAll[i,] = qnData$allqna[[i]][[1]][,,1]$bis[,,1]$raw
  schizoAll[i,] = qnData$allqna[[i]][[1]][,,1]$schizo[,,1]$raw
  eatAll[i,]=qnData$allqna[[i]][[1]][,,1]$eat[,,1]$raw
  apathyAll[i,]=qnData$allqna[[i]][[1]][,,1]$apathy[,,1]$raw
  alcoholAll[i,]=qnData$allqna[[i]][[1]][,,1]$alcohol[,,1]$raw
}

qns = data.frame("qnid"=qnIndivid,"zung"=zungAll, "anxiety"=anxietyAll,"ocir"= ocirAll, "leb" =lebAll,"bis"= bisAll,"schizo"= schizoAll, 'alcohol'=alcoholAll,'eat'=eatAll,'apathy'=apathyAll)

# DO FACTOR ANALYSIS ON RAW QUESTIONAIRRE SCORES
# Produce covariance matrix using hetcor to account for both continuous and binary correlations
het.mat <- hetcor(qns[,2:length(qns)])$cor

fa <- fa(r = het.mat, nfactors = 3, n.obs = nrow(qns), rotate = "oblimin", fm="ml", scores="regression")
fa.scores <- factor.scores(x=qns[,2:length(qns)], f=fa)
scores = data.frame("id"=qns$qnid, fa.scores$scores)
loadings <- fa$loadings

# loadings plot m2= sa, m1 = a&d, m4 = impul, m3 = compul (THIS CAN CHANGE W THE FACTOR ANAYLSIS)
colnames(scores) <- c("id", "AD", "Compul", "SW")
factorData =merge(allData, scores,by.x=c("id"), by.y=c("id")) #join factor scores with main data matrix

##############################################################
##############################################################
# REGRESSIONS 1) PERFORMANCE/METACOG ~ DEMOGRAPHICS ##########
##############################################################
##############################################################
# Linear regressions for just demographics (reduced model: iq, age, gender) with variables of 
# 1) performance/ accuracy
# 2) mean confidence
# 3) m ratio

# linear regression
accuDemoReg= lm(accuracy.sc~iq.sc+age.sc+gender,allData) 
confMeanDemoReg= lm(confMean.sc~iq.sc+age.sc+gender,allData) 
mRatioDemoReg= lm(mRatio.sc~iq.sc+age.sc+gender,mrExcludedData) 

# extracting coefficients into dataframes
accuDemoRegFig <- data.frame(summary(accuDemoReg)$coefficients[2:4,1:4])
confMeanDemoRegFig <- data.frame(summary(confMeanDemoReg)$coefficients[2:4,1:4])
mRatioDemoRegFig <- data.frame(summary(mRatioDemoReg)$coefficients[2:4,1:4])

##############################################################
##############################################################
# REGRESSIONS 2) HDDM VARIABLES ~ DEMOGRAPHICS ###############
##############################################################
##############################################################
# Linear regressions for just demographics (reduced model: iq, age, gender) with HDDM variables of 
# 1) t (non decision time)
# 2) v delta (drift rate - dot difference)
# 3) a intercept (decision threshold)
# 4) v intercept (drift rate - baseline)

# linear regression
aDemoReg= lm(a.sc~iq.sc+age.sc+gender,allData)
tDemoReg= lm(t.sc~iq.sc+age.sc+gender,allData)
vDeltaDemoReg= lm(v_delta.sc~iq.sc+age.sc+gender,allData)
vInterDemoReg= lm(v_inter.sc~iq.sc+age.sc+gender,allData)

# extracting coefficients into dataframes
tDemoRegFig <- data.frame(summary(tDemoReg)$coefficients[2:4,1:4])
vDeltaDemoRegFig <- data.frame(summary(vDeltaDemoReg)$coefficients[2:4,1:4])
aDemoRegFig <- data.frame(summary(aDemoReg)$coefficients[2:4,1:4])
vInterDemoRegFig <- data.frame(summary(vInterDemoReg)$coefficients[2:4,1:4])

###########################################################
###########################################################
# REGRESSIONS 3) PERFORMANCE/METACOG ~ SYMPTOM SCORES #####
###########################################################
###########################################################
# Linear regressions for questionnaire scores with variables of 
# 1) performance/ accuracy
# 2) mean confidence
# 3) m ratio

## BETWEEN-SUBJECT MODELS
# Define function we can re-use for each model
generic.model <- function(DV, df) {
  # Loop over each symptom score variable in separate lm models, store symptom score coefficients
  plot1 <- matrix(NA,9,4)
  modelString = paste(DV, "~zung.sc+iq.sc+age.sc+gender")
  mod1 <- lm(modelString, df)
  plot1[1,] <- summary(mod1)$coefficients[2,1:4]
  
  modelString = paste(DV, "~anxiety.sc+iq.sc+age.sc+gender")
  mod1 <- lm(modelString, df)
  plot1[2,] <- summary(mod1)$coefficients[2,1:4]
  
  modelString = paste(DV, "~ocir.sc+iq.sc+age.sc+gender")
  mod1 <- lm(modelString, df)
  plot1[3,] <- summary(mod1)$coefficients[2,1:4]
  
  modelString = paste(DV, "~leb.sc+iq.sc+age.sc+gender")
  mod1 <- lm(modelString, df)
  plot1[4,] <- summary(mod1)$coefficients[2,1:4]
  
  modelString = paste(DV, "~bis.sc+iq.sc+age.sc+gender")
  mod1 <- lm(modelString, df)
  plot1[5,] <- summary(mod1)$coefficients[2,1:4]
  
  modelString = paste(DV, "~schizo.sc+iq.sc+age.sc+gender")
  mod1 <- lm(modelString, df)
  plot1[6,] <- summary(mod1)$coefficients[2,1:4]
  
  modelString = paste(DV, "~eat.sc+iq.sc+age.sc++gender")
  mod1 <- lm(modelString, df)
  plot1[7,] <- summary(mod1)$coefficients[2,1:4]
  
  modelString = paste(DV, "~apathy.sc+iq.sc+age.sc++gender")
  mod1 <- lm(modelString, df)
  plot1[8,] <- summary(mod1)$coefficients[2,1:4]
  
  modelString = paste(DV, "~alcohol.sc+iq.sc+age.sc+gender")
  mod1 <- lm(modelString, df)
  plot1[9,] <- summary(mod1)$coefficients[2,1:4]
  plot1 <- data.frame(plot1)
  names(plot1) <- c("Estimate", "Std..Error", "t value", "P value")
  row.names(plot1) <- c("Depression" ,"Generalised Anxiety" ,"OCD" ,"Social Anxiety", "Impulsivity" ,"Schizotypy", "Eating Disorders", "Apathy","Alcoholism")
  
  return(plot1)
}

#Linear regressions for 
accuSympReg <- generic.model("accuracy.sc", allData)# Performance/Accuracy
confMeanSympReg <- generic.model("confMean.sc", allData)# Mean confidence
mRatioSympReg <- generic.model("mRatio.sc", mrExcludedData)# mRatio

#############################################################
#############################################################
# REGRESSIONS 4) HDDM VARIABLES ~ SYMPTOM SCORES ############
#############################################################
#############################################################
# Linear regressions for questionnaire scores with HDDM variables of 
# 1) t (non decision time)
# 2) v delta (drift rate - dot difference)
# 3) a intercept (decision threshold)
# 4) v intercept (drift rate - baseline)

# linear regressions
tSympReg <- generic.model("t.sc", allData)# t (non decision time)
vDeltaSympReg <- generic.model("v_delta.sc", allData)# v delta (drift rate - dot difference)
aSympReg <- generic.model("a.sc", allData)# a intercept (decision threshold)
vInterSympReg <- generic.model("v_inter.sc", allData)# v intercept (drift rate - baseline)


################################################################
################################################################
# REGRESSIONS 5) PERFORMANCE/METCOG/HDDM ~ FACTOR SCORES  ######
################################################################
################################################################
# Linear regressions for factor scores with task performance & HDDM variables of 
# 1) performance/ accuracy
# 2) mean confidence
# 3) m ratio
# 4) a
# 5) t
# 6) v_delta
# 7) v_intercept

# exclude mRatio < 0 and scale it
mrExcludFactorData <- factorData[factorData$mRatio>0,]
mrExcludFactorData$mRatio.sc = scale(log(mrExcludFactorData$mRatio))

# linear regressions
accuFactorReg= lm(accuracy.sc~AD+Compul+SW+iq.sc+age.sc+gender,factorData) # accuracy
confMeanFactorReg= lm(confMean.sc~AD+Compul+SW+iq.sc+age.sc+gender,factorData) # mean confidence
mRatioFactorReg= lm(mRatio.sc~AD+Compul+SW+iq.sc+age.sc+gender,mrExcludFactorData) #mRatio
aFactorReg= lm(a.sc~AD+Compul+SW+iq.sc+age.sc+gender,factorData) # a
tFactorRreg= lm(t.sc~AD+Compul+SW+iq.sc+age.sc+gender,factorData) # t
vDeltaFactorReg= lm(v_delta.sc~AD+Compul+SW+iq.sc+age.sc+gender,factorData) # v delta
vInterFactorReg= lm(v_inter.sc~AD+Compul+SW+iq.sc+age.sc+gender,factorData)  #v intercept

# test magnitudes of contrasts for mean confidence
lambda1 <- c(0,1,0,0,0,0,0)
esticon(confMeanFactorReg, lambda1, beta0=0)

# extract coefficients into dataframes
accuFactorRegFig <- data.frame(summary(accuFactorReg)$coefficients[2:4,1:4])
confMeanFactorRegFig <- data.frame(summary(confMeanFactorReg)$coefficients[2:4,1:4])
mRatioFactorRegFig <- data.frame(summary(mRatioFactorReg)$coefficients[2:4,1:4])
tFactorRregFig <- data.frame(summary(tFactorRreg)$coefficients[2:4,1:4])
vDeltaFactorRegFig <- data.frame(summary(vDeltaFactorReg)$coefficients[2:4,1:4])
aFactorRegFig <- data.frame(summary(aFactorReg)$coefficients[2:4,1:4])
vInterFactorRegFig <- data.frame(summary(vInterFactorReg)$coefficients[2:4,1:4])


#######################################################
#######################################################
# PLOT 1) PERFORMANCE/METACOG ~ DEMOGRAPHICS ##########
#######################################################
#######################################################

# set and label dataframe to plot
accuDemoRegFig$Type<-rownames(accuDemoRegFig)
accuDemoRegFig$Label<- 'Accuracy'
confMeanDemoRegFig$Type<-rownames(confMeanDemoRegFig)
confMeanDemoRegFig$Label<- 'Mean Confidence'
mRatioDemoRegFig$Type<-rownames(mRatioDemoRegFig)
mRatioDemoRegFig$Label<- 'M Ratio'

# join dataframes together
demoRegFig<-rbind(accuDemoRegFig,confMeanDemoRegFig,mRatioDemoRegFig)
demoRegFig <- demoRegFig[demoRegFig$Type!='gender1',]
demoRegFig$Type[demoRegFig$Type=='iq.sc']<-'IQ'
demoRegFig$Type[demoRegFig$Type=='age.sc']<-'Age'
demoRegFig$Label<-factor(demoRegFig$Label, levels=c("Accuracy",'Mean Confidence','M Ratio'))


# Plot: Task performance + Metacognition ~ Demographics
demoFig <- ggplot(data = demoRegFig, aes(x = Label, y = Estimate, group=Type)) + 
  geom_bar(aes(fill = Type), color="black",size=1.2,stat="identity", position = "dodge",width=0.6) +
  geom_errorbar(aes(ymin=demoRegFig$Estimate-demoRegFig$Std..Error, ymax=demoRegFig$Estimate+demoRegFig$Std..Error),colour="black", width=.3, size=1.2, position=position_dodge(.6)) +
  labs(title=" ", x=" ", y = "Regression Coefficient") + geom_hline(yintercept=0,size=1) + theme_classic() + 
  theme(axis.title.y = element_text(size = rel(2.5), angle = 90, margin=margin(0,20,0,0)), axis.title.x = element_text(size = rel(3), angle = 00, margin=margin(20,0,0,0))) +
  theme(plot.title = element_text(size = rel(3), angle = 00)) +
  theme(axis.text.x = element_text(angle = 00, size=25), axis.text.y = element_text(angle = 00, size=25)) +
  theme(legend.text = element_text(size = 20), legend.title = element_blank(), legend.position="none") +
  theme(axis.line.x = element_line(color="black", size = 1.2), axis.line.y = element_line(color="black", size = 1.2)) +
  theme(axis.ticks.y=element_line(size=(1.5)), axis.ticks.x=element_line(size=(1.5)), axis.ticks.length=unit(0.4, "cm")) +
  scale_fill_manual(values=c("#ffffff", "#555555"))+ ylim(-0.3,0.4)



########################################################
########################################################
# PLOT 2) PERFORMANCE/METACOG ~ SYMPTOM SCORES #########
########################################################
########################################################

# set and label dataframe to plot
accuSympReg$Type<-rownames(accuSympReg)
accuSympReg$Label<- 'Accuracy'
confMeanSympReg$Type<-rownames(confMeanSympReg)
confMeanSympReg$Label<- 'Mean Confidence'
mRatioSympReg$Type<-rownames(mRatioSympReg)
mRatioSympReg$Label<- 'M Ratio'

# join dataframes together
sympRegFig<-rbind(accuSympReg,confMeanSympReg,mRatioSympReg)
sympRegFig$Label<-factor(sympRegFig$Label, levels=c("Accuracy",'Mean Confidence','M Ratio'))
sympRegFig$Type<-factor(sympRegFig$Type, levels=c("Apathy","Social Anxiety","Generalised Anxiety","bis",'zung',"Alcoholism","Schizotypy","ocir",'Eating Disorders'))

# Plot: Task performance + Metacognition ~ Symptom Scores
sympReg <- ggplot(data = sympRegFig, aes(x = Label, y = Estimate, group=Type)) +       
  geom_bar(aes(fill = Type), colour="black",size=1.2,stat="identity", position = "dodge",width=0.8) +
  geom_errorbar(aes(ymin=sympRegFig$Estimate-sympRegFig$Std..Error, ymax=sympRegFig$Estimate+sympRegFig$Std..Error),colour="black", width=0.3, size=1.2, position=position_dodge(.8)) +
  geom_hline(yintercept=0,size=1) + theme_classic() + labs(title=" ", x=" ", y = "Regression Coefficient") +
  theme(axis.title.y = element_text(size = rel(2.5), angle = 90, margin=margin(0,20,0,0)),axis.title.x = element_text(size = rel(3), angle = 00, margin=margin(20,0,0,0))) +
  theme(plot.title = element_text(size = rel(3), angle = 00),legend.text = element_text(size = 20),legend.title = element_blank(),legend.position="none") +
  theme(axis.text.x = element_text(angle = 00, size=25),axis.text.y = element_text(angle = 00, size=25))+ scale_x_discrete(expand = c(0,0.5)) +
  theme(axis.line.x = element_line(color="black", size = 1.5),axis.line.y = element_line(color="black", size = 1.5)) +
  theme(axis.ticks.y=element_line(size=(1.5)), axis.ticks.x=element_line(size=(1.5)),axis.ticks.length=unit(0.4, "cm")) +
  scale_fill_manual(values=c("#999999", "#377db8","#e31a1c","#984ea3","#4daf4a","#f781bf",'#ffff33','#ff7f00','#a65628'))+ ylim(-0.3,0.4)


############################################################
############################################################
# PLOT 3) PERFORMANCE/METACOG/HDDM ~ FACTOR SCORES #########
############################################################
############################################################

# set and label dataframe to plot
accuFactorRegFig$Label<- 'Accuracy'
accuFactorRegFig$Type<-rownames(accuFactorRegFig)
tFactorRregFig$Label<- 't'
tFactorRregFig$Type<-rownames(tFactorRregFig)
vDeltaFactorRegFig$Label<- 'v delta'
vDeltaFactorRegFig$Type<-rownames(vDeltaFactorRegFig)
aFactorRegFig$Label<- 'a'
aFactorRegFig$Type<-rownames(aFactorRegFig)
vInterFactorRegFig$Label<- 'v intercept'
vInterFactorRegFig$Type<-rownames(vInterFactorRegFig)
confMeanFactorRegFig$Type<-rownames(confMeanFactorRegFig)
confMeanFactorRegFig$Label<- 'Mean Confidence'
mRatioFactorRegFig$Type<-rownames(mRatioFactorRegFig)
mRatioFactorRegFig$Label<- 'M Ratio'

# Plot: Task performance + HDDM + Metacognition ~ Factor Scores
factorRegFig<-rbind(accuFactorRegFig,tFactorRregFig,vInterFactorRegFig,vDeltaFactorRegFig,aFactorRegFig,confMeanFactorRegFig,mRatioFactorRegFig)
factorRegFig$Label<-factor(factorRegFig$Label, levels=c("Accuracy","t","v intercept", "v delta",'a','Mean Confidence','M Ratio'))
factorRegFig$Type[factorRegFig$Type=="AD"]<-"Anxious"
factorRegFig$Type[factorRegFig$Type=="Compul"]<-"Compulsivity"
factorRegFig$Type[factorRegFig$Type=="SW"]<-"Social Withdrawal"

factorFig <- ggplot(data = factorRegFig, aes(x = Label, y = Estimate, group=Type)) +       
  geom_bar(aes(fill = Type), colour="black",size=1.2,stat="identity", position = "dodge",width=0.8) +
  geom_errorbar(aes(ymin=factorRegFig$Estimate-factorRegFig$Std..Error, ymax=factorRegFig$Estimate+factorRegFig$Std..Error),colour="black", width=0.3, size=1.2, position=position_dodge(.8))+
  geom_hline(yintercept=0,size=1) + theme_classic() + labs(title=" ", x=" ", y = "Regression Coefficient") +
  theme(axis.title.y = element_text(size = rel(2.5), angle = 90, margin=margin(0,20,0,0)), axis.title.x = element_text(size = rel(3), angle = 00, margin=margin(20,0,0,0)))+
  theme(plot.title = element_text(size = rel(3), angle = 00),legend.text = element_text(size = 20),legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 00, size=25), axis.text.y = element_text(angle = 00, size=25))+ scale_x_discrete(expand = c(0,0.5)) +
  theme(axis.line.x = element_line(color="black", size = 1.2), axis.line.y = element_line(color="black", size = 1.2)) +
  theme(axis.ticks.y=element_line(size=(1.5)), axis.ticks.x=element_line(size=(1.5)), axis.ticks.length=unit(0.4, "cm")) +
  scale_fill_manual(values=c("#8dd3c7", "#ffffbc","#bebada")) + theme(legend.position="none") + ylim(-0.3,0.3)
