#########################################
###### Data Loading and preperation #####
#########################################


library(readr)

dat <- read_csv("C:/Users/Ayota/Desktop/Data/Prospecting Modelv1.csv",na = c("NULL",""))
View(dat)

dat_2017 <- read_csv("C:/Users/Ayota/Desktop/Data/Historical_TF_File_Qith_NBN_All_Presale_2017_Update.csv",na = c("NULL",""))
dat_2018 <- read_csv("C:/Users/Ayota/Desktop/Data/Historical_TF_File_Qith_NBN_All_Presale_2018_Update.csv",na = c("NULL",""))




dat_2017 <- transform(dat_2017, minimum_of_nbn = pmin(nbn_aetna,nbn_bcbs,nbn_united))
dat_17 <- dat_2017[,c("OID","minimum_of_nbn")]

dat_2018 <- transform(dat_2018, minimum_of_nbn = pmin(nbn_aetna,nbn_bcbs,nbn_united))
dat_18 <- dat_2018[,c("OID","minimum_of_nbn")]


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

comb_data <- comb_dat[,c("ID","stagename","primary_medical_funding__c","segment_sub","salesoffice","market",
                         "minimum_of_nbn","avg_weighted_tf")]


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

comb_data[is.na(stagename), stagename:= getMode(comb_data[,stagename])]
comb_data[is.na(primary_medical_funding__c), primary_medical_funding__c:= getMode(comb_data[,primary_medical_funding__c])]
comb_data[is.na(segment_sub), segment_sub:= getMode(comb_data[,segment_sub])]
comb_data[is.na(salesoffice), salesoffice:= getMode(comb_data[,salesoffice])]
comb_data[is.na(market), market:= getMode(comb_data[,market])]

comb_data[is.na(minimum_of_nbn), minimum_of_nbn:= mean(comb_data[,minimum_of_nbn],na.rm = T)]
comb_data[is.na(avg_weighted_tf), avg_weighted_tf:= mean(comb_data[,avg_weighted_tf],na.rm = T)]

# making sure there is no missing values:
colSums(is.na(comb_data)) # if every entry is 0 then we are done!

comb_data <- na.omit(comb_data) # Removing the row which do not have any id

rmdtq_data <- subset(comb_data, stagename!="DTQ")


#####################################
######Fitting Logistic Regression ###
#####################################

# Recategorizing the dependent variable:
comb_data$stagename_cat <- ifelse(comb_data$stagename == "Sold"|comb_data$stagename =="Lost","1","0")
table(comb_data$stagename_cat)


#Convert all charecter variables to factor
comb_data <- as.data.frame(unclass(comb_data))

## 80% of the sample size
smp_size <- floor(0.80 * nrow(comb_data))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(comb_data)), size = smp_size)

TrainData <- comb_data[train_ind, ]
TestData <- comb_data[-train_ind, ]



# Fitting Logistic Regression model:

attach(comb_data)
fit1 <-glm(stagename_cat~primary_medical_funding__c+segment_sub+salesoffice+market+minimum_of_nbn
               ,family=binomial(link = "logit"),data = TrainData)

summary(fit1)

# There are aliased/linearly dependent coefficients in the model. So, we should remove them and run the model again.
ld.vars <- attributes(alias(fit1)$Complete)$dimnames[[1]]
ld.vars

fit2 <-glm(stagename_cat~primary_medical_funding__c+segment_sub+salesoffice+minimum_of_nbn
           ,family=binomial(link = "logit"),data = TrainData)


ld.vars2 <- attributes(alias(fit2)$Complete)$dimnames[[1]]
ld.vars2
summary(fit2)


# Prediction:

test_data_in_prediction <- TestData[,c("primary_medical_funding__c","segment_sub","salesoffice","minimum_of_nbn")]

predicted_column <- predict(fit2,newdata=test_data_in_prediction,type="response")
predicted_value <- ifelse(predicted_column > 0.5,"1","0")

TestData_predicted <-  data.frame(TestData,predicted_value)
TestData_predicted$predicted_value <- as.factor(TestData_predicted$predicted_value)

# Predicted data with ID and predicted column:
TestData_predicted



##################
###### ROC #######
##################

library(pROC)

# ROC curve for train data:
roc(TrainData$stagename_cat, as.vector(fitted.values(fit2)), plot=TRUE,grid=TRUE, reuse.auc = TRUE,
    print.auc = TRUE, ci=TRUE, ci.type="bars", 
    main = paste("ROC curve using train data","(N = ",nrow(TrainData),")") )


# ROC curve for test data:

fit3 <-glm(stagename_cat~primary_medical_funding__c+segment_sub+salesoffice+minimum_of_nbn
           , family=binomial(link = "logit"),data = TestData)

roc(TestData$stagename_cat, as.vector(fitted.values(fit3)), plot=TRUE,grid=TRUE, reuse.auc = TRUE,
    print.auc = TRUE, ci=TRUE, ci.type="bars", 
    main = paste("ROC curve using test data","(N = ",nrow(TestData),")"))

# So, we found similar types of ROC curve for both test and train data.

######################################################################
######Fitting Logistic Regression after removing DTQ from the data ###
######################################################################


comb_data <- rmdtq_data

# Recategorizing the dependent variable:
comb_data$stagename_cat <- ifelse(comb_data$stagename == "Sold"|comb_data$stagename =="Lost","1","0")
table(comb_data$stagename_cat)


#Convert all charecter variables to factor
comb_data <- as.data.frame(unclass(comb_data))

## 80% of the sample size
smp_size <- floor(0.80 * nrow(comb_data))

## set the seed to make your partition reproducible
set.seed(1234)
train_ind <- sample(seq_len(nrow(comb_data)), size = smp_size)

TrainData <- comb_data[train_ind, ]
TestData <- comb_data[-train_ind, ]



# Fitting Logistic Regression model:

attach(comb_data)
fit1 <-glm(stagename_cat~primary_medical_funding__c+segment_sub+salesoffice+market+minimum_of_nbn
           ,family=binomial(link = "logit"),data = TrainData)

summary(fit1)

# There are aliased/linearly dependent coefficients in the model. So, we should remove them and run the model again.
ld.vars <- attributes(alias(fit1)$Complete)$dimnames[[1]]
ld.vars

fit2 <-glm(stagename_cat~primary_medical_funding__c+segment_sub+salesoffice+minimum_of_nbn
           , family=binomial(link = "logit"),data = TrainData)


ld.vars2 <- attributes(alias(fit2)$Complete)$dimnames[[1]]
ld.vars2
summary(fit2)


# Prediction:

test_data_in_prediction <- TestData[,c("primary_medical_funding__c","segment_sub","salesoffice","minimum_of_nbn")]

predicted_column <- predict(fit2,newdata=test_data_in_prediction,type="response")
predicted_value <- ifelse(predicted_column > 0.5,"1","0")

TestData_predicted <-  data.frame(TestData,predicted_value)
TestData_predicted$predicted_value <- as.factor(TestData_predicted$predicted_value)

# Predicted data with ID and predicted column:
dim(TestData_predicted)



##################
###### ROC #######
##################

library(pROC)

# ROC curve for train data:
roc(TrainData$stagename_cat, as.vector(fitted.values(fit2)), plot=TRUE,grid=TRUE, reuse.auc = TRUE,
    print.auc = TRUE, ci=TRUE, ci.type="bars", 
    main = paste("ROC curve using train data","(N = ",nrow(TrainData),")") )


# ROC curve for test data:

fit3 <-glm(stagename_cat~primary_medical_funding__c+segment_sub+salesoffice+minimum_of_nbn
           , family=binomial(link = "logit"),data = TestData)

roc(TestData$stagename_cat, as.vector(fitted.values(fit3)), plot=TRUE,grid=TRUE, reuse.auc = TRUE,
    print.auc = TRUE, ci=TRUE, ci.type="bars", 
    main = paste("ROC curve using test data","(N = ",nrow(TestData),")"))

# So, we found similar types of ROC curve for both test and train data after removing the dtq values from the data as well.

