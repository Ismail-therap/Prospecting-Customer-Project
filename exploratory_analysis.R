###### Useful functions ######



#### Function to find frequency and percentage for a specific factor:

count_percentage <- function(var="close_month"){
  count <- table(dat[,var])
  per <- round((count/sum(count))*100,2)
  data_comb <- data.frame(count,per)
  data_comb <- data_comb[,-3]
  colnames(data_comb) <- c(paste(var),"Frequency","Percentage")
  data_comb
  
}


#### Function to find Cost volumn and Rate Rate:

cost_volumn_and_prospecting_rate <- function(var="close_month"){
  
  count <- table(dat[,var])
  prospecting_rate <- table(dat[,var],dat[,"stagename_cat"])[,2]
  data_comb <- data.frame(count,prospecting_rate)
  colnames(data_comb)[1] <- c(paste(var))
  data_comb$prospecting_rate <- (data_comb$prospecting_rate/data_comb$Freq)*100
  
  data_comb
  
}





# For other variables:
Plotting_function_other <- function(df,xlabel="x",scale=150,ang=45){
  
  library(ggplot2)
  ggplot(df)  + 
    geom_line(aes(x=reorder(df[,1],-Freq), y = prospecting_rate,group = 1,linetype = ""),stat="identity",size=1,colour="red")+
    scale_y_continuous(name = expression("Prospecting Rate(%)"), 
                       sec.axis = sec_axis(~.*scale,name = "Case Volumn"),limits = c(0,100))+
    geom_bar(aes(x=df[,1], y=Freq/scale),stat="identity", fill="grey")+
    theme(axis.text.x=element_text(angle= ang, vjust=.5))+
    labs(x=xlabel,y="",linetype = "Prospecting Rate")+
    geom_line(aes(x=df[,1], y=prospecting_rate,group = 1,linetype = ""),stat="identity",size=1,colour="red")
}


# For months

Plotting_function_month <- function(df,xlabel="x"){
  
  library(ggplot2)
  ggplot(df)  + 
    geom_line(aes(x=df[,1], y=prospecting_rate,group = 1,linetype = ""),stat="identity",size=1,colour="red")+
    scale_y_continuous(name = expression("Prospecting Rate(%)"), 
                       sec.axis = sec_axis(~.*300,name = "Case Volumn"))+
    geom_bar(aes(x=df[,1], y=Freq/300),stat="identity", fill="grey")+
    theme(axis.text.x=element_text(angle= 45, vjust=.5))+
    labs(x=xlabel,y="",linetype = "Prospecting Rate")+
    scale_x_discrete(limits = month.abb)+
    geom_line(aes(x=df[,1], y=prospecting_rate,group = 1,linetype = ""),stat="identity",size=1,colour="red")
  
}



###########################################

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


comb_data <- comb_dat[,c("ID","stagename","primary_medical_funding__c","segment_sub","salesoffice","market",
                         "minimum_of_nbn","avg_weighted_tf","close_year","close_month")]

comb_data$stagename_cat <- ifelse(comb_data$stagename == "Sold"|comb_data$stagename =="Lost","1","0")





### Renaming the data frame to plot
dat <- comb_data




#### primary_medical_funding__c
count_percentage(var="primary_medical_funding__c") 
df <- cost_volumn_and_prospecting_rate(var="primary_medical_funding__c")
Plotting_function_other(df,xlabel="primary_medical_funding__c",scale=800,ang=90)



#### psalesoffice
count_percentage(var="salesoffice") 
df <- cost_volumn_and_prospecting_rate(var="salesoffice")
Plotting_function_other(df,xlabel="salesoffice",scale=100,ang=90)


#### market
count_percentage(var="market") 
df <- cost_volumn_and_prospecting_rate(var="market")
df <- df[order(-df$Freq),] 
Plotting_function_other(df,xlabel="market",scale=100,ang=90)



#segment_sub

count_percentage(var="segment_sub") 
df5 <- cost_volumn_and_prospecting_rate(var="segment_sub")



ggplot(df5)  + 
  geom_line(aes(x=df5[,1], y=prospecting_rate,group = 1,linetype = ""),stat="identity",size=1,colour="red")+
  scale_y_continuous(name = expression("Prospecting Rate(%)"), 
                     sec.axis = sec_axis(~.*500,name = "Case Volumn"))+
  geom_bar(aes(x=df5[,1], y=Freq/500),stat="identity", fill="grey")+
  theme(axis.text.x=element_text(angle= 45, vjust=.5))+
  labs(x="Segment",y="",linetype = "Prospecting Rate")+
  scale_x_discrete(limits = c("2-99","100-249","250-500","O500"))+
  geom_line(aes(x=df5[,1], y=prospecting_rate,group = 1,linetype = ""),stat="identity",size=1,colour="red")

#### Month 
count_percentage(var="close_month") 
df <- cost_volumn_and_prospecting_rate(var="close_month")
Plotting_function_month(df,xlabel="Month")


### Year

count_percentage(var="close_year") 
df2 <- cost_volumn_and_prospecting_rate(var="close_year")
Plotting_function_other(df=df2,xlabel="Year",scale=500,ang=0)

### Year and Month

dat$Year_month <- zoo::as.yearmon(paste(dat$close_year, dat$close_month), "%Y %b")

count_percentage(var="Year_month") 
df3 <- cost_volumn_and_prospecting_rate(var="Year_month")
Plotting_function_other(df=df3,xlabel="Year Month",scale=200,ang = 90)













