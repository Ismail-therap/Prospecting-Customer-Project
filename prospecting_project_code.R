#########################################
###### Data Loading and preperation #####
#########################################


library(readr)

dat <- read_csv("C:/Users/Ayota/Desktop/Data/Prospecting Modelv1.csv")

str(dat)

dat_2017 <- read_csv("C:/Users/Ayota/Desktop/Data/Historical_TF_File_Qith_NBN_All_Presale_2017_Update.csv")
dat_2018 <- read_csv("C:/Users/Ayota/Desktop/Data/Historical_TF_File_Qith_NBN_All_Presale_2018_Update.csv")




dat_2017 <- transform(dat_2017, minimum_value = pmin(nbn_aetna,nbn_bcbs,nbn_united))
dat_17 <- dat_2017[,c("OID","minimum_value")]

dat_2018 <- transform(dat_2018, minimum_value = pmin(nbn_aetna,nbn_bcbs,nbn_united))
dat_18 <- dat_2018[,c("OID","minimum_value")]


# merging 17 and 18 file and renaming the ID
comb_17_and_18 <- rbind(dat_17,dat_18)
colnames(comb_17_and_18)[1] <- "ID"

# Left joining with the main data:

comb_dat <- merge(x = dat, y = comb_17_and_18, by = "ID", all.x = TRUE)


### Recoding the dependent variable:


str(comb_dat)
#########################################
#### 






#############################Fitting Logistic Regression####################################


myData <-read.csv("logistic_regression.csv")
head(myData)
attach(myData)

GENDERCODE<-as.factor(GENDERCODE)
LOCALITY<-as.factor(LOCALITY)
STATECODE<-as.factor(STATECODE)
income_band_dsc<-as.factor(income_band_dsc)

age_band_dsc<-as.factor(age_band_dsc)
PD_DSC<-as.factor(PD_DSC)
Churn<-as.factor(Churn)

summary(myData)



my.glm<-glm(Churn~LOCALITY, family=binomial(link = "logit"))

summary(my.glm)
names(my.glm)

#################ANOVA  and ODDS RATIO #########################

anova(my.glm,test="F")
odds.ratio<-exp(my.glm$coefficients)










###################################################################################

dat<-read.csv("main_data.csv")
head(dat)
attach(dat)


GENDERCODE<-as.factor(GENDERCODE)
STATECODE<-as.factor(STATECODE)
Churn_stat<-as.factor(Churn_stat)
summary(dat)


##### Effect of Gender #####

gen.glm<-glm(Churn_stat~GENDERCODE, family=binomial(link = "logit"))
summary(gen.glm)
names(gen.glm)

####### Effect of other variables #######
library(ISLR)
other.glm<-glm(Churn_stat~STATECODE+income_band_dsc+age_band_dsc, family=binomial(link = "logit"))
summary(other.glm)
anova(other.glm)

predicted_probability_of_churn<-predict(other.glm,newdata=dat,type="response")
Predicted<-round(predicted_probability_of_churn)
Actual<-dat$Churn_stat
pred_dat<-cbind(Actual,Predicted)
#write.csv(pred_dat,"Submission.csv",row.names=FALSE)
head(pred_dat)

#model_pred_Direction=rep("Not_churn",299)
#model_pred_Direction[predicted_probability_of_churn>0.5]="Churn"




#### Checking Model Accuracy ###
require(caret)
confusionMatrix(Predicted,dat$Churn_stat)




