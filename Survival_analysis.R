install.packages("survival")
library(survival)
load('/Users/richardtsai/Documents/DataScience/Classes/Genetics/HW/Assignment3/info.read.RData')

quantile(info.read$OverallSurvival)
#  0%     25%     50%     75%    100% 
# 0.0000  3.1250  5.1950  7.1625 11.4800
table(info.read$PatientStatus)
# 0  1 
#48 64
info.read$PatientStatus2 <- ifelse(info.read$PatientStatus == 1, c(0), c(1)) 

' Quesiton 1 a) Do any of the three SNPs genotyped provide a survival advantage for the patients?
Provide statistical and visual evidence for your answer, and interpret the results. (5 points)' 
### plot KM rs119256552 2 survive longer
fitRs119<-survfit(Surv(OverallSurvival,PatientStatus)~rs11925655,data = info.read)
plot(fitRs119,col=c("red","blue","orange"), mark.time = TRUE,xlab = "Survival Time",ylab = "%Overall survival", main="rs11925655")
legend("topright", legend=c(0,1,2),fill=c("red","blue","orange"))
# log-rank test (rho=0)  p= 5.51e-14 
survdiff(Surv(OverallSurvival,PatientStatus)~rs11925655, data=info.read, rho=0)
# Wilcoxon test (rho=1)  p= 2.86e-12 
survdiff(Surv(OverallSurvival,PatientStatus)~rs11925655, data=info.read, rho=1)

### plot KM rs119256552 2 survive longer patientStatus2
fitRs119<-survfit(Surv(OverallSurvival,PatientStatus2)~rs11925655,data = info.read)
plot(fitRs119,col=c("red","blue","orange"), mark.time = TRUE,xlab = "Survival Time",ylab = "%Overall survival", main="rs11925655 2")
legend("topright", legend=c(0,1,2),fill=c("red","blue","orange"))
# log-rank test (rho=0)  p= 6.04e-13  
survdiff(Surv(OverallSurvival,PatientStatus2)~rs11925655, data=info.read, rho=0)
# Wilcoxon test (rho=1)  p= 1.74e-11 
survdiff(Surv(OverallSurvival,PatientStatus2)~rs11925655, data=info.read, rho=1)


### plot KM rs2677760,  2 survive longer
fitRs267<-survfit(Surv(OverallSurvival,PatientStatus)~rs2677760,data = info.read)
plot(fitRs267,col=c("red","blue","orange"), mark.time = TRUE,xlab = "Survival Time",ylab = "%Overall survival", main="rs2677760")
legend("topright", legend=c(0,1,2),fill=c("red","blue","orange"))
#log-rank test (rho=0)  p= <0.001
survdiff(Surv(OverallSurvival,PatientStatus)~rs2677760, data=info.read, rho=0)
# Wilcoxon test (rho=1)  p= 7.77e-16  
survdiff(Surv(OverallSurvival,PatientStatus)~rs2677760, data=info.read, rho=1)

### plot KM rs374429930,  
fitRs374<-survfit(Surv(OverallSurvival,PatientStatus)~rs374429930,data = info.read)
plot(fitRs374,col=c("red","blue","orange"), mark.time = TRUE,xlab = "Survival Time",ylab = "%Overall survival", main="rs374429930")
legend("topright", legend=c(0,1,2),fill=c("red","blue","orange"))
#log-rank test (rho=0)  p= 0.844
survdiff(Surv(OverallSurvival,PatientStatus)~rs374429930, data=info.read, rho=0)
# Wilcoxon test (rho=1)  p= 0.394  
survdiff(Surv(OverallSurvival,PatientStatus)~rs374429930, data=info.read, rho=1)


'Question 2 Is the patients’ overall survival influenced by any of the following 
(individually, not in combination): sex, ethnicity, cancer stage, tumour grade, 
infection status, alcohol consumption. Provide statistical and visual evidence for your answer,
and interpret the results. (5 points)'
### plot KM sex,  
fitSex<-survfit(Surv(OverallSurvival,PatientStatus)~Sex,data = info.read)
plot(fitSex,col=c("red","blue"), mark.time = TRUE,xlab = "Survival Time",ylab = "%Overall survival", main="Sex")
legend("topright", legend=c('Female','Male'),fill=c("red","blue"))
#log-rank test (rho=0)  p=  0.124 
survdiff(Surv(OverallSurvival,PatientStatus)~Sex, data=info.read, rho=0)
# Wilcoxon test (rho=1)  p= 0.146   
survdiff(Surv(OverallSurvival,PatientStatus)~Sex, data=info.read, rho=1)

### plot KM  Ethnicity,  
fitEthnicity<-survfit(Surv(OverallSurvival,PatientStatus)~Ethnicity,data = info.read)
plot(fitEthnicity,col=c("red","blue","green"), mark.time = TRUE,xlab = "Survival Time",ylab = "%Overall survival", main="Ethnicity")
legend("topright", legend=c('Caucasion','African American','Hispanic'),fill=c("red","blue","green"))
#log-rank test (rho=0)  p=  0.303  
survdiff(Surv(OverallSurvival,PatientStatus)~Ethnicity, data=info.read, rho=0)
# Wilcoxon test (rho=1)  p=  0.327   
survdiff(Surv(OverallSurvival,PatientStatus)~Ethnicity, data=info.read, rho=1)

### plot KM CancerStage,  
fitCancerStage<-survfit(Surv(OverallSurvival,PatientStatus)~CancerStage,data = info.read)
plot(fitCancerStage,col=c("red","blue","green","orange"), mark.time = TRUE,xlab = "Survival Time",ylab = "%Overall survival", main="CancerStage")
legend("topright", legend=c('I','II','III','IV'),fill=c("red","blue","green","orange"))
#log-rank test (rho=0)  p=   0.36   
survdiff(Surv(OverallSurvival,PatientStatus)~CancerStage, data=info.read, rho=0)
# Wilcoxon test (rho=1)  p=  0.482   
survdiff(Surv(OverallSurvival,PatientStatus)~CancerStage, data=info.read, rho=1)

### plot KM TumourGrade,  
fitTumourGrade<-survfit(Surv(OverallSurvival,PatientStatus)~TumourGrade,data = info.read)
plot(fitTumourGrade,col=c("red","blue","green","orange"), mark.time = TRUE,xlab = "Survival Time",ylab = "%Overall survival", main="TumourGrade")
legend("topright", legend=c('1','2','3','4'),fill=c("red","blue","green","orange"))
#log-rank test (rho=0)  p=  0.243    
survdiff(Surv(OverallSurvival,PatientStatus)~TumourGrade, data=info.read, rho=0)
# Wilcoxon test (rho=1)  p=  0.282   
survdiff(Surv(OverallSurvival,PatientStatus)~TumourGrade, data=info.read, rho=1)

### plot KM InfectionStatus,  
fitInfectionStatus<-survfit(Surv(OverallSurvival,PatientStatus)~InfectionStatus,data = info.read)
plot(fitInfectionStatus,col=c("blue","red"), mark.time = TRUE,xlab = "Survival Time",ylab = "%Overall survival", main="InfectionStatus")
legend("topright", legend=c('Negative','Positive'),fill=c("blue","red"))
#log-rank test (rho=0)  p=  0.151     
survdiff(Surv(OverallSurvival,PatientStatus)~InfectionStatus, data=info.read, rho=0)
# Wilcoxon test (rho=1)  p=  0.0473***  
survdiff(Surv(OverallSurvival,PatientStatus)~InfectionStatus, data=info.read, rho=1)

### plot KM AlcoholConsumption,  
fitAlcoholConsumption<-survfit(Surv(OverallSurvival,PatientStatus)~AlcoholConsumption,data = info.read)
plot(fitAlcoholConsumption,col=c("blue","red","orange"), mark.time = TRUE,xlab = "Survival Time",ylab = "%Overall survival", main="AlcoholConsumption")
legend("topright", legend=c('Never','Past','Present'),fill=c("blue","red","orange"))
#log-rank test (rho=0)  p=   0.0179 ***    
survdiff(Surv(OverallSurvival,PatientStatus)~AlcoholConsumption, data=info.read, rho=0)
# Wilcoxon test (rho=1)  p=  0.0435 ***  
survdiff(Surv(OverallSurvival,PatientStatus)~AlcoholConsumption, data=info.read, rho=1)

'Question c Can you define a cut-off for T cell infiltration that would significantly
distinguish better and worse survival outcomes? What about a cut-off for macrophage infiltration?'

quantile (info.read$Tcells)
#   0%     25%     50%     75%    100% 
#-1.7900 -0.9225 -0.0550  0.5500  1.7700 

##split Tcell by -0.55
info.read$TcellsCat<- cut(info.read$Tcells,c(-1.79,-0.55,1.77),label=c(0,1))
fitTcell <- survfit(Surv(OverallSurvival,PatientStatus)~TcellsCat,data=info.read)
summary(fitTcell)
#log-rank test (rho=0)  p=   0.0353  ***    
survdiff(Surv(OverallSurvival,PatientStatus)~TcellsCat, data=info.read, rho=0)
# Wilcoxon test (rho=1)  p=  0.0329  ***  
survdiff(Surv(OverallSurvival,PatientStatus)~TcellsCat, data=info.read, rho=1)

##Split TCell by -0.5
info.read$TcellsCat2<- cut(info.read$Tcells,c(-1.79,-0.5,1.77),label=c(0,1))
fitTcell <- survfit(Surv(OverallSurvival,PatientStatus)~TcellsCat2,data=info.read)
summary(fitTcell)
#log-rank test (rho=0)  p=   0.022  ***    
survdiff(Surv(OverallSurvival,PatientStatus)~TcellsCat2, data=info.read, rho=0)
# Wilcoxon test (rho=1)  p=  0.0219  ***  
survdiff(Surv(OverallSurvival,PatientStatus)~TcellsCat2, data=info.read, rho=1)

##split Macrophage by 0.5
quantile(info.read$Macrophages)
info.read$MacrophagesCat<- cut(info.read$Macrophages,c(-1.95,0.5,1.9),label=c(0,1))
fitMacrophage<- survfit(Surv(OverallSurvival,PatientStatus)~MacrophagesCat,data=info.read)
#log-rank test (rho=0)  p=   0.049  ***    
survdiff(Surv(OverallSurvival,PatientStatus)~MacrophagesCat, data=info.read, rho=0)
# Wilcoxon test (rho=1)  p=  0.117    
survdiff(Surv(OverallSurvival,PatientStatus)~MacrophagesCat, data=info.read, rho=1)

##split Macrophage by 0.55
quantile(info.read$Macrophages)
info.read$MacrophagesCat2<- cut(info.read$Macrophages,c(-1.95,0.55,1.9),label=c(0,1))
fitMacrophage2<- survfit(Surv(OverallSurvival,PatientStatus)~MacrophagesCat2,data=info.read)
#log-rank test (rho=0)  p=   0.208  ***    
survdiff(Surv(OverallSurvival,PatientStatus)~MacrophagesCat2, data=info.read, rho=0)
# Wilcoxon test (rho=1)  p=  0.275 
survdiff(Surv(OverallSurvival,PatientStatus)~MacrophagesCat2, data=info.read, rho=1)

'Question d Using Cox PH regression, determine an optimal prognostic model in this cohort 
based on the expression of the measured genes only (ARG2, FOXJ1, CD274, CCL5, JAM3, MUC1, NFKB1).
Is this a good prognostic model? (10 points)'
### Cox 
fitCoxByGene <- coxph(Surv(OverallSurvival, PatientStatus)~ARG2+FOXJ1+CD274+CCL5+JAM3+MUC1+NFKB1, data=info.read)
summary(fitCoxByGene)

'e) Can you improve the model that you derived in c) by adding any of the other 
variables available, excluding (!) the three SNPs? (10 points)'
##Tcell=-0.5 Macro=0.5 ---> Concordance= 0.602, likelihood p=0.01226
fitCoxByTcellMacro <- coxph(Surv(OverallSurvival, PatientStatus)~TcellsCat2+MacrophagesCat, data=info.read)
summary(fitCoxByTcellMacro)

## Tcell, Infection, Alcohol ---> Concordance= 0.65, Likelihood ratio test:  p=0.004179
fitCoxByAll<- coxph(Surv(OverallSurvival, PatientStatus)~TcellsCat2+InfectionStatus+AlcoholConsumption, data=info.read)
summary(fitCoxByAll)

## Tcell, Infection, Alcohol ---> Concordance= 0.65, Likelihood ratio test:  p=0.004179
fitCoxByAll<- coxph(Surv(OverallSurvival, PatientStatus)~TcellsCat2+InfectionStatus+AlcoholConsumption, data=info.read)
summary(fitCoxByAll)

'f) What happens if you include the SNP genotype information as well? Can you 
derive a final optimally prognostic model? How does this model perform compared 
to the ones in c) and d)? Report the hazard ratios for each resulting parameter 
and interpret them. Interpret your overall results by taking into account the 
biology of the cancer. Can you find evidence in the literature to support your 
conclusions? (12.5 points)'
# Concordance= 0.823, Likelihood ratio test  p=1.532e-14
fitCoxByAll2<- coxph(Surv(OverallSurvival, PatientStatus)~TcellsCat2+
                       InfectionStatus+AlcoholConsumption+ARG2+rs11925655+rs2677760,
                       data=info.read)
summary(fitCoxByAll2)

# Concordance=  0.832, Likelihood ratio test  p=4.819e-13
fitCoxByAll2<- coxph(Surv(OverallSurvival, PatientStatus)~TcellsCat2+InfectionStatus+
                       AlcoholConsumption+ARG2+rs11925655+rs2677760+FOXJ1+CD274+CCL5+
                       JAM3+MUC1+NFKB1+KRAS_status+, data=info.read)
summary(fitCoxByAll2)


