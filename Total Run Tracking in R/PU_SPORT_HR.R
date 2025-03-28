library(dplyr)
library(lubridate)
library(tidyverse)
library(ggplot2)


#This script will be used to estimate harvest rates of the PU and sport fisheries projections

#Update Each Year
Current.Year<-2024

#Each season, the PU_Sport_HR file will need updated using Comm AMR Appendix A17 and Sport AMR T18 (Columns C and N)
#Note: Kasilof sport harvest is available from the statewide harvest survey webpage
#Also, historical sockeye counts will need updated as well.

#Season Dates
#Kasilof Gillnet-6/15 to 6/24
#Kasilof Dipnet- 6/25 to 8/7
#Kenai Dipnet- 7/10 to 7/31

#Data Wrangling and Reformatting
Hist.KeKa.Sonar<-read.csv(file("O:/DCF/UCI/Research/MNGMT/Ins/24/Total Run Tracking in R/Data/KEKA_Hist_Sonar.csv"))
Hist.KeKa.Sonar$Date.Full<-as.Date(Hist.KeKa.Sonar$Date,format = "%m/%d")#create date that may be sorted chronologically


#Estimate Mean Harvest for PU, Sub and Sport to apply to sonar counts 
KA.Gillnet<-Hist.KeKa.Sonar%>%filter(River=="Kasilof"&Date.Full>"2024-06-14"&Date.Full<"2024-06-25")%>%group_by(Year)%>%summarize(Kasilof.Sonar.Gillnet=sum(Daily.Count))
KA.Dip<-Hist.KeKa.Sonar%>%filter(River=="Kasilof",Date.Full>"2023-06-25"&Date.Full<"2023-08-07")%>%group_by(Year)%>%summarize(Kasilof.Sonar.Dipnet=sum(Daily.Count))
KA.Sport<-Hist.KeKa.Sonar%>%filter(River=="Kasilof")%>%group_by(Year)%>%summarize(Kasilof.Sonar=sum(Daily.Count))
KE.Dip<-Hist.KeKa.Sonar%>%filter(River=="Kenai",Date.Full>"2023-07-10"&Date.Full<"2023-07-31")%>%group_by(Year)%>%summarize(Kenai.Sonar.Dipnet=sum(Daily.Count))
KE.Sport<-Hist.KeKa.Sonar%>%filter(River=="Kenai")%>%group_by(Year)%>%summarize(Kenai.Sonar=sum(Daily.Count))

Inriver.Harvest<-PU_SPORT_HR%>%replace(is.na(.),0)%>%mutate(Kasilof.Harvest.Below=Kasilof.Gillnet+Kasilof.Dipnet+Kasilof.Sport.Below.Sonar,
                                           Kenai.Harvest.Below=Kenai.Dipnet+Kenai.Sport.Below.Sonar)%>%
  left_join(KA.Gillnet)%>%left_join(KA.Dip)%>%left_join(KA.Sport)%>%left_join(KE.Dip)%>%left_join(KE.Sport)%>%
  mutate(Kasilof.Gillnet.Harvestrate=Kasilof.Gillnet/(Kasilof.Gillnet+Kasilof.Sonar.Gillnet),
        Kasilof.Dipnet.Harvestrate=Kasilof.Dipnet/(Kasilof.Dipnet+Kasilof.Sonar.Dipnet),
        Kasilof.Sport.Harvestrate=Kasilof.Sport.Below.Sonar/(Kasilof.Sport.Below.Sonar+Kasilof.Sonar),
       Kenai.Dipnet.Harvestrate=Kenai.Dipnet/(Kenai.Dipnet+Kenai.Sonar.Dipnet),
        Kenai.Sport.Harvestrate=Kenai.Sport.Below.Sonar/(Kenai.Sport.Below.Sonar+Kenai.Sonar))%>%select(1,16:20)%>%
  filter(Year<Current.Year&Year>Current.Year-6)%>%summarize(across(2:6,mean))

















