## fMRI behavioural effects
## elisavanderplasATgmail.com
library(Rmisc) ##for summ stats
library(ggplot2)
#library(nlme)
#library(plyr)
library(outliers)
library(reshape2)##to rearrange data for plot autho
library(R.matlab)

## set the path & load in data
dataDir <- "~/Dropbox/Github/fMRI_politics/data/"
data <- read.csv(paste(dataDir,"fMRI_behavioural.csv", sep = ""), header=TRUE, sep=",") 
demo <- read.csv(paste(dataDir, "subjectLog_exp2.csv", sep = ""), header = TRUE, sep = ",")

#Facors
data$subj_idx <- factor(data$subj_idx)
data$Topic<-factor(data$Topic, levels = c(1,2,3), labels = c("immigration","climate change","healthcare"))
data$Appraisal<-factor(data$Appraisal, levels = c(1,2,3), labels = c("uncertain", "threat","blame"))

## initiate new variables
gender = NULL
age = NULL
edu = NULL
pol_autho = NULL
pol_scep = NULL
PositionImm = NULL
PositionCl = NULL
PositionHe = NULL

### get some code to import each individual's demographics as covariates
for (s in 1:length(data$subj_idx))
{
  subj_demo = demo[demo$sj_nr==data$subj_idx[s],] 
  gender <- rbind(gender, subj_demo$gender2[1])
  age <- rbind(age, subj_demo$age[1])
  edu <- rbind(edu, subj_demo$education[1])
  pol_autho <- rbind(pol_autho, subj_demo$pol_autho[1])
  pol_scep <- rbind(pol_scep, subj_demo$pol_scep[1])
  PositionImm <- rbind(PositionImm, subj_demo$issue_positions_immigration[1])
  PositionCl <- rbind(PositionCl, subj_demo$issue_positions_climate[1])
  PositionHe <- rbind(PositionHe, subj_demo$issue_positions_health[1])
}

## Add covariates to full data
data$gender <- gender
data$age <- age
data$edu <- edu
data$pol_autho <- pol_autho
data$pol_scep <- pol_scep
data$PositionImm <- PositionImm
data$PositionCl <- PositionCl
data$PositionHe <- PositionHe

## Now calculating & zscore dependent variables
data$donatie <-scale((data$donatie/10)) ##get donation in euros
data$clap <- scale(data$clap)
data$importance <- scale(data$importance)
#rm.outlier(data, fill = FALSE, median = FALSE, opposite = FALSE)

## PART 1: EMOTION EFFECTS
negative_appraisal <- lm(NEG_AFFECT ~ Appraisal + age + gender + edu + pol_autho + pol_scep + PositionImm + PositionCl + PositionHe, data=data)
print(summary(negative_appraisal))
print(Anova(negative_appraisal, type = 3))
coef(summary(negative_appraisal)) 

fear_appraisal <- lm(FEAR ~ Appraisal + age + gender + edu + pol_autho + pol_scep + PositionImm + PositionCl + PositionHe,data=data)
print(summary(fear_appraisal))
print(Anova(fear_appraisal, type = 3))
coef(summary(fear_appraisal)) 

anger_appraisal <- lm(ANGER ~ Appraisal + age + gender + edu + pol_autho + pol_scep+ PositionImm + PositionCl + PositionHe, data=data)
print(summary(anger_appraisal))
print(Anova(anger_appraisal, type = 3))
coef(summary(anger_appraisal)) 

anger_data <- split(data$ANGER, data$Appraisal)
t.test(anger_data$threat, anger_data$blame)

## PART 2: BEHAVIOURAL EFFECTS 
imp_appraisal <- lm(importance ~ Appraisal + age + gender + edu + pol_autho + pol_scep+ PositionImm + PositionCl + PositionHe, data=data)
print(summary(imp_appraisal))
print(Anova(imp_appraisal, type = 3))
coef(summary(imp_appraisal)) 

#split claps per appraisal condition for post-hoc contrast
imp_data <- split(data$importance, data$Appraisal)
t.test(imp_data$threat, imp_data$blame)

clap_appraisal <- lm(clap ~ Appraisal + age + gender + edu + pol_autho + pol_scep+ PositionImm + PositionCl + PositionHe, data=data)
print(summary(clap_appraisal))
print(Anova(clap_appraisal, type = 3))
coef(summary(clap_appraisal)) 

#split claps per appraisal condition for post-hoc contrast
clap_data <- split(data$clap, data$Appraisal)
t.test(clap_data$threat, clap_data$blame)

#get the coefficients for plotting
setwd(dataDir)
co_fear <- coef(summary(fear_appraisal)) 
betas_fear <- c(co_fear[2:3,1], co_fear[2:3,2])
write.csv(betas_fear, file = paste('fear_betas.csv'))

co_anger <- coef(summary(anger_appraisal)) 
betas_anger <- c(co_anger[2:3,1], co_anger[2:3,2])
write.csv(betas_anger, file = paste('anger_betas.csv'))

co_imp <- coef(summary(imp_appraisal)) 
betas_imp <- c(co_imp[2:3,1], co_imp[2:3,2])
write.csv(betas_fear, file = paste('imp_betas.csv'))

co_clap <- coef(summary(clap_appraisal)) 
betas_clap <- c(co_clap[2:3,1], co_clap[2:3,2])
write.csv(betas_clap, file = paste('clap_betas.csv'))

filename <- paste(tempfile(), ".mat", sep = "")
writeMat(filename, data)

writeMat(data, file = paste('ID-data.csv'))

##Supplementary materials
ggplot(data, aes(x=donatie, y=importance)) + 
  geom_point()+ geom_smooth(method="lm", se=TRUE, fullrange=FALSE, level=0.95) + labs(y="Issue importance (z-scored)", x = "Party donations (z-scored)")+theme_minimal() + 
  theme(axis.text=element_text(size=18),axis.title=element_text(size=25))
cor.test(data$donatie, data$importance, method = "pearson")

ggplot(data, aes(x=clap, y=importance)) + 
  geom_point()+ geom_smooth(method="lm", se=TRUE, fullrange=FALSE, level=0.95) + labs(y="Issue importance (z-scored)", x = "Video-clip sharing (z-scored)")+theme_minimal() + 
  theme(axis.text=element_text(size=18),axis.title=element_text(size=25))
cor.test(data$clap, data$importance, method = "pearson")

ggplot(data, aes(x=clap, y=donatie)) + 
  geom_point()+ geom_smooth(method="lm", se=TRUE, fullrange=FALSE, level=0.95) + labs(y="Party donations (z-scored)", x = "Video-clip sharing (z-scored)")+theme_minimal() + 
  theme(axis.text=element_text(size=18),axis.title=element_text(size=25))
cor.test(data$clap, data$donatie, method = "pearson")


