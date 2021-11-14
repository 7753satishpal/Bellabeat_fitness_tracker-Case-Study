library(lubridate)
library(readr)
library(tidyverse)
library(dplyr)
#options(dplyr.summarise.inform = FALSE) # to ommit text about the new .groups parameter
library(tidyr)
library(ggplot2)
library(plotly)  #for interactive plots
install.packages("scales")
library(scales)#The inverse of scaling, making guides (legends and axes) that can be used to read the graph
install.packages("janitor") #
install.packages("skimr") #
library(janitor)
library(skimr)
install.packages("groupdata2")# to 
library(groupdata2)
install.packages("corrplot") #to create correlation masp
library(corrplot)
install.packages("repr")# to resize plot
library(repr)
# data manipulation
dailyActivity_merged %>%
  select(TotalSteps,TotalDistance,Calories)%>%
  summary()
#heart time column seprate
heartrate_seconds_merged<-heartrate_seconds_merged%>%
  separate(Time,into=c('date','time'),sep=" ")
#sleep day col separate
sleepdata<-sleepDay_merged%>%
  separate(SleepDay,into=c('date','time'),sep=" ")
#weightdata  date col seprate
weightdata<-M%>%
  separate(Date,into=c('date','time'),sep=" ")
#analysis
str(weightdata)
srt(sleepdata)
str(dailyActivity)
summary(weightdata)
summary(sleepdata)
summary(dailyActivity)
str(heartrate_seconds_merged)
summary(heartrate_seconds_merged)
n_distinct(weightdata$Id)
n_distinct(heartrate_seconds_merged$Id)
n_distinct(dailyActivity$Id)
n_distinct(dailyActivity$Date)
#total step vs calories
ggplot(data=dailyActivity,aes(TotalSteps,Calories))+
  geom_point(color="navy blue")+geom_smooth(color="red")+labs(title = "TotalSteps vs Calories")
#type of user distribution
catogory<-c("VeryActive","FairlyActive","LightActive","SedentryActive")
catogory_per<-c(mean(dailyActivity$VeryActiveMinutes),mean(dailyActivity$FairlyActiveMinutes),mean(dailyActivity$LightlyActiveMinutes),mean(dailyActivity$SedentaryMinutes))
user_type<-data.frame(catogory,catogory_per)
ggplot(user_type,aes(catogory,catogory_per))+
  geom_bar(aes(fill=factor(catogory_per)),stat= "identity")+
  geom_text(aes(label=catogory_per),vjust=2,color="black",size=3)+
  labs(title = "User_Type_Distribution")
#Is mannual report
report_method_type<-c("Manual Report","Auto Report")
report_method_t_f<-c(length(which(weightdata$IsManualReport=="TRUE")),length(which(weightdata$IsManualReport=="FALSE")))
mannual_report_or_not<-data.frame(report_method_type,report_method_t_f)
ggplot(mannual_report_or_not,aes(report_method_type,report_method_t_f))+
  geom_bar(aes(fill=factor(report_method_type)),stat= "identity",show.legend = FALSE)+ 
  geom_text(aes(label=report_method_t_f), vjust=3, color="white", size=5)+
  labs(title="Weight Log Methods_Type", x= "Weight Log Methods", y="Number of Records")
#user record for each type
user_type<-c("DailyActivity_Records","SleepDay_Records","HeartRate_Records","WeightLog_Records")
user_record<-c(n_distinct(dailyActivity$Id),n_distinct(sleepdata$Id),n_distinct(heartrate_seconds_merged$Id),n_distinct(weightdata$Id))
user_type_record<-data.frame(user_type,user_record)
ggplot(user_type_record,aes(user_type,user_record))+
  geom_bar(stat="identity")+
  geom_text(aes(label=user_record), vjust=3, color="white", size=5)+
  labs(title = "Comparision of Users Activity",x="UserActivity",y="No_of_users")

#daily steps
ggplot(dailyActivity,aes(Date,TotalSteps))+
  geom_point(stat = "identity",position = "identity",aes(color=factor(Date)),show.legend = FALSE,na.rm = FALSE,inherit.aes = TRUE)+
  labs(title = "Daily Steps by Customers")
