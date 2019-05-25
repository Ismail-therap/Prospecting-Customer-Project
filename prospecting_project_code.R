#########################################
###### Data Loading and preperation #####
#########################################


library(readr)

dat <- read_csv("C:/Users/Ayota/Desktop/Data/Prospecting Modelv1.csv",na = "NULL")


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



#########################################
#### 




# Function to calculate the most frequent object in a vector:
getMode <- function(myvector) {
  mytable <- table(myvector)
  return(names(mytable)[which.max(mytable)])
}


#### Subsetting the data file with our selected columns:

comb_data <- comb_dat[,c("ID","primary_medical_funding__c","segment_sub","salesoffice","market",
                         "minimum_value","avg_weighted_tf")]

colnames(comb_data)[6] <- "minimum_of_nbn"

# Missing value percentage by variable:
missing_value_per <- colMeans(is.na(comb_data))*100
missing_value_per

### Exploring the distribution of avg_weighted_tf and the minimum of nbn variables:

library(ggplot2)
require(reshape2)
df <- comb_data[,c("minimum_of_nbn","avg_weighted_tf")]
ggplot(melt(df), aes(value, fill = variable)) + geom_histogram(position = "dodge")


### Replacing missing values:

library(data.table)
comb_data <- data.table(comb_data)


comb_data[is.na(primary_medical_funding__c), primary_medical_funding__c:= getMode(comb_data[,primary_medical_funding__c])]
comb_data[is.na(segment_sub), segment_sub:= getMode(comb_data[,segment_sub])]
comb_data[is.na(salesoffice), salesoffice:= getMode(comb_data[,salesoffice])]
comb_data[is.na(market), market:= getMode(comb_data[,market])]

comb_data[is.na(minimum_of_nbn), minimum_of_nbn:= mean(comb_data[,minimum_of_nbn],na.rm = T)]
comb_data[is.na(avg_weighted_tf), avg_weighted_tf:= mean(comb_data[,avg_weighted_tf],na.rm = T)]

# making sure there is no missing values:
colMeans(is.na(comb_data))*100 # if every entry is 0 then we are done!


#####################################
######Fitting Logistic Regression ###
#####################################

# Recategorizing the dependent variable:
str(comb_data)



my.glm <-glm(Churn~LOCALITY, family=binomial(link = "logit"))

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




