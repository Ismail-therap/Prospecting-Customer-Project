comb_data <- comb_dat[,c("ID","stagename","primary_medical_funding__c","segment_sub","salesoffice","market",
                         "minimum_of_nbn","avg_weighted_tf","close_year","close_month")]

comb_data$stagename_cat <- ifelse(comb_data$stagename == "Sold"|comb_data$stagename =="Lost","1","0")

dat <- comb_data

head(comb_data)

dat <- data.frame(comb_data)

var = "primary_medical_funding__c"

View(head(dat))
#### Month 
count_percentage(var="primary_medical_funding__c") 
df <- cost_volumn_and_prospecting_rate(var="primary_medical_funding__c")
Plotting_function_month(df,xlabel="Month")

count_percentage(var="segment_sub") 
df <- cost_volumn_and_prospecting_rate(var="segment_sub")
Plotting_function_month(df,xlabel="Month")


library(ggplot2)
ggplot(df)  + 
  geom_line(aes(x=df[,1], y=prospecting_rate,group = 1,linetype = ""),stat="identity",size=1,colour="red")+
  scale_y_continuous(name = expression("Prospecting Rate(%)"), 
                     sec.axis = sec_axis(~.*100,name = "Case Volumn"),limits = c(0,100))+
  geom_bar(aes(x=df[,1], y=Freq/100),stat="identity", fill="grey")+
  theme(axis.text.x=element_text(angle= 45, vjust=.5))+
  labs(x="Month",y="",linetype = "Rate")+
  scale_x_discrete(limits = month.abb)




count_percentage(var="segment_sub") 
df5 <- cost_volumn_and_prospecting_rate(var="segment_sub")

str(df5)
df5$Freq <- as.numeric(df5$Freq)
ggplot(df5)  + 
  geom_line(aes(x=df5[,1], y=prospecting_rate,group = 1,linetype = ""),stat="identity",size=1,colour="red")+
  scale_y_continuous(name = expression("Prospecting Rate(%)"), 
                     sec.axis = sec_axis(~.*100,name = "Case Volumn"))+
  geom_bar(aes(x=df5[,1], y=Freq/100),stat="identity", fill="grey")+
  theme(axis.text.x=element_text(angle= 45, vjust=.5))+
  labs(x="Segment",y="",linetype = "Prospecting Rate")+
  scale_x_discrete(limits = c("2-99","100-249","250-500","O500"))+
  geom_line(aes(x=df5[,1], y=prospecting_rate,group = 1,linetype = ""),stat="identity",size=1,colour="red")


#### Function to find frequency and percentage for a specific factor:

count_percentage <- function(var="close_month"){
  count <- table(dat[,var])
  per <- round((count/sum(count))*100,2)
  data_comb <- data.frame(count,per)
  data_comb <- data_comb[,-3]
  colnames(data_comb) <- c(paste(var),"Frequency","Percentage")
  data_comb
  
}

head(dat)

#### Function to find Cost volumn and Rate Rate:

cost_volumn_and_prospecting_rate <- function(var="close_month"){
  
  count <- table(dat[,var])
  prospecting_rate <- table(dat[,var],dat[,"stagename_cat"])[,2]
  data_comb <- data.frame(count,prospecting_rate)
  colnames(data_comb)[1] <- c(paste(var))
  data_comb$prospecting_rate <- (data_comb$prospecting_rate/data_comb$Freq)*100
  
  data_comb
  
}



##### Plotting function


Plotting_function_month <- function(df,xlabel="x"){
  
  library(ggplot2)
  ggplot(df)  + 
    geom_line(aes(x=df[,1], y=Rate,group = 1,linetype = ""),stat="identity",size=1,colour="red")+
    scale_y_continuous(name = expression(" Rate(%)"), 
                       sec.axis = sec_axis(~.*100,name = "Case Volumn"),limits = c(0,100))+
    geom_bar(aes(x=df[,1], y=Freq/100),stat="identity", fill="grey")+
    theme(axis.text.x=element_text(angle= 45, vjust=.5))+
    labs(x=xlabel,y="",linetype = "Rate Rate")+
    scale_x_discrete(limits = month.abb)
  
  
}


Plotting_function_other <- function(df,xlabel="x",scale=150){
  
  library(ggplot2)
  ggplot(df)  + 
    geom_line(aes(x=df[,1], y=Rate_Rate,group = 1,linetype = ""),stat="identity",size=1,colour="red")+
    scale_y_continuous(name = expression("Rate Rate(%)"), 
                       sec.axis = sec_axis(~.*scale,name = "Case Volumn"),limits = c(0,100))+
    geom_bar(aes(x=df[,1], y=Freq/scale),stat="identity", fill="grey")+
    theme(axis.text.x=element_text(angle= 45, vjust=.5))+
    labs(x=xlabel,y="",linetype = "Rate Rate")
}





#### Month 
count_percentage(var="close_month") 
df <- cost_volumn_and_Rate_rate(var="close_month")
Plotting_function_month(df,xlabel="Month")


### Year

count_percentage(var="close_year") 
df2 <- cost_volumn_and_Rate_rate(var="close_year")
Plotting_function_other(df=df2,xlabel="Year",scale=150)

### Year and Month

dat$Year_month <- zoo::as.yearmon(paste(dat$close_year, dat$close_month), "%Y %b")

count_percentage(var="Year_month") 
df3 <- cost_volumn_and_Rate_rate(var="Year_month")
Plotting_function_other(df=df3,xlabel="Year Month",scale=60)

### Market
count_percentage(var="market") 
df4 <- cost_volumn_and_Rate_rate(var="market")
Plotting_function_other(df=df4,xlabel="Market",scale=50)


#### segment_sub
dat$segment_sub <- as.factor(dat$segment_sub)
dat$segment_sub <- ifelse(dat$segment_sub=="Feb-99","2-99",
                          ifelse(dat$segment_sub=="100-249","100-249",
                                 ifelse(dat$segment_sub=="250-500","250-500",
                                        ifelse(dat$segment_sub=="O500","O500",NA))))
count_percentage(var="segment_sub") 
df5 <- cost_volumn_and_Rate_rate(var="segment_sub")


ggplot(df5)  + 
  geom_line(aes(x=df5[,1], y=Rate_Rate,group = 1,linetype = ""),stat="identity",size=1,colour="red")+
  scale_y_continuous(name = expression("Rate Rate(%)"), 
                     sec.axis = sec_axis(~.*110,name = "Case Volumn"),limits = c(0,100))+
  geom_bar(aes(x=df5[,1], y=Freq/110),stat="identity", fill="grey")+
  theme(axis.text.x=element_text(angle= 45, vjust=.5))+
  labs(x="Segment",y="",linetype = "Rate Rate")+
  scale_x_discrete(limits = c("2-99","100-249","250-500","O500"))


#### fundng_ty

count_percentage(var="fundng_ty") 
df6 <- cost_volumn_and_Rate_rate(var="fundng_ty")
df6 <- df6[-4,]
Plotting_function_other(df=df6,xlabel="Funding Type",scale=60)


#### sic_division


count_percentage(var="sic_division") 
df7 <- cost_volumn_and_Rate_rate(var="sic_division")
Plotting_function_other(df=df7,xlabel="Division",scale=100)


### Duration Years:

# Bucket (a):

dat$bucket_a <- ifelse(dat$duration_yrs>=0 & dat$duration_yrs<=1,"0-1",
                       ifelse(dat$duration_yrs>1 & dat$duration_yrs<=2,"1-2",
                              ifelse(dat$duration_yrs>2 & dat$duration_yrs<=3,"2-3",
                                     ifelse(dat$duration_yrs>3 & dat$duration_yrs<=4,"3-4",
                                            ifelse(dat$duration_yrs>4 & dat$duration_yrs<=5,"4-5","5+")))))

count_percentage(var="bucket_a") 
df8 <- cost_volumn_and_Rate_rate(var="bucket_a")
Plotting_function_other(df=df8,xlabel="Duration(Years)",scale=100)



# Bucket (b):

dat$bucket_b <- ifelse(dat$duration_yrs>=0 & dat$duration_yrs<=3,"0-3",
                       ifelse(dat$duration_yrs>3 & dat$duration_yrs<=5,"3-5","5+"))

count_percentage(var="bucket_b") 
df9 <- cost_volumn_and_Rate_rate(var="bucket_b")
Plotting_function_other(df=df9,xlabel="Duration(Years)",scale=150)


# Bucket (c):

dat$bucket_c <- ifelse(dat$duration_yrs>=0 & dat$duration_yrs<=5,"0-5",
                       ifelse(dat$duration_yrs>5 & dat$duration_yrs<=10,"5-10",
                              ifelse(dat$duration_yrs>10 & dat$duration_yrs<=15,"10-15",
                                     ifelse(dat$duration_yrs>15 & dat$duration_yrs<=20,"15-20","20+"))))

count_percentage(var="bucket_c") 
df10 <- cost_volumn_and_Rate_rate(var="bucket_c")


ggplot(df10)  + 
  geom_line(aes(x=df10[,1], y=Rate_Rate,group = 1,linetype = ""),stat="identity",size=1,colour="red")+
  scale_y_continuous(name = expression("Rate Rate(%)"), 
                     sec.axis = sec_axis(~.*200,name = "Case Volumn"),limits = c(0,100))+
  geom_bar(aes(x=df10[,1], y=Freq/200),stat="identity", fill="grey")+
  theme(axis.text.x=element_text(angle= 45, vjust=.5))+
  labs(x="Duration(Years)",y="",linetype = "Rate Rate")+
  scale_x_discrete(limits = c("0-5","5-10","10-15","15-20","20+"))

