library(readxl)
library(dplyr)
setwd("C://Users//User//Desktop")
Data2017<-read_xlsx("CNUH sleep center PSG data_1028.xlsx", sheet='2017', col_names=T)
Data2018<-read_xlsx("CNUH sleep center PSG data_1028.xlsx", sheet='2018', col_names=T)
Data2019<-read_xlsx("CNUH sleep center PSG data_1028.xlsx", sheet='2019', col_names=T)
Data2020<-read_xlsx("CNUH sleep center PSG data_1028.xlsx", sheet='2020', col_names=T)
Data2021<-read_xlsx("CNUH sleep center PSG data_1028.xlsx", sheet='2021', col_names=T)
Data2022<-read_xlsx("CNUH sleep center PSG data_1028.xlsx", sheet='2022', col_names=T)

dictionary<-Data2017 %>% select("나이 (years)", "성별 (M/F)", "Neck (cm)", "Abdomen (cm)", "BMI (kg/m2)")

names(Data2017)<-names(Data2018) #변수명이 자꾸 다르다고 나와서 전부 통일
names(Data2019)<-names(Data2018)
names(Data2020)<-names(Data2018)
names(Data2021)<-names(Data2018)
names(Data2022)<-names(Data2018)

DATA<-rbind(Data2017, Data2018, Data2019, Data2020, Data2021, Data2022)

colnames(DATA)[c(1,5,6,7,8,9)]<-c('date', 'name', 'age', 'sex', 'height(m)', 'weight')

year<-substr(DATA$date, 1, 4)

DATA<-cbind(DATA, year)
