library(dplyr)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(stringr)
library(fuzzyjoin)
library(ggrepel)
library(openxlsx)
library(tidyr)
library(zoo)
library(scales)
library(flextable)

#Assign the stat week for later trimming
Stat.Today<-
  (Stat.Week2024%>%rowwise()%>%do(data.frame(Stat.Week=.$Stat.Week,Date=seq(.$Stat.Start,.$Stat.Stop,by='1 day')))%>%filter(Date%in%Today))$Stat.Week

################################################################################
#Historically, a UCI Age Composition Report was pulled from FDMS and put into the Age Compositions folder. In 2024, the aging team decided entered data in excel would be much more efficient.
#For details and code for pre-generated UCI age comp report, see Age_Allocation_Inseason in the 2023 folder.
# This season we will generate this report here.
###############################################################################


##Generating Age Comp Report For Commercial Harvest
UCI.Commercial.Ages.0<-read.csv(file("O:/SHAREDAT/Research/UCI Research Core/Projects/Catch and Escapement/2024 Catch & Escapement/Data/TOTAL_RUN_AGE_TRACKING/COMMERCIAL_CATCH_AGES.csv"))%>%
  select(Date.of.Fishery,Sampling.Group,Age)%>%filter(!Age%in%NA)%>% 
  mutate(Date.of.Fishery=make_date(year=Current.Year,day=day(as.Date(Date.of.Fishery,format = "%d-%b")),month=month(as.Date(Date.of.Fishery,format = "%d-%b"))))%>%
  rename(Fishery.Project=Sampling.Group,Date=Date.of.Fishery)%>%
  fuzzy_left_join(Stat.Week2024,by=c("Date"="Stat.Start","Date"="Stat.Stop"),match_fun=list(`>=`,`<=`))%>%select(-c(Stat.Start,Stat.Stop))

UCI.Commercial.Ages<-UCI.Commercial.Ages.0%>%group_by(Fishery.Project,Stat.Week,Age)%>%summarize(Count=n())%>%group_by(Fishery.Project,Stat.Week)%>%mutate(Samples=sum(Count))

#Add Commercial Harvest Data by sampling period for later Weighting
UCI.Commercial.Ages<-UCI.Commercial.Ages%>%
  left_join(Commercial.Harvest%>%fuzzy_left_join(Stat.Week2024,by=c("Date"="Stat.Start","Date"="Stat.Stop"),match_fun=list(`>=`,`<=`))%>%select(-c(Stat.Start,Stat.Stop))%>%
              group_by(Fishery.Project,Stat.Week)%>%summarize(Index=sum(Season.Total)))


##Generating Age Comp Report For Escapements
UCI.Escapement.Ages.0<-read.csv(file("O:/SHAREDAT/Research/UCI Research Core/Projects/Catch and Escapement/2024 Catch & Escapement/Data/TOTAL_RUN_AGE_TRACKING/ESCAPEMENT_AGES.csv"))%>%
  select(Date.of.Fishery,Sampling.Group,Age)%>%filter(!Age%in%NA)%>% 
  mutate(Date.of.Fishery=make_date(year=Current.Year,day=day(as.Date(Date.of.Fishery,format = "%d-%b")),month=month(as.Date(Date.of.Fishery,format = "%d-%b"))))%>%
  rename(Fishery.Project=Sampling.Group,Date=Date.of.Fishery)%>%
  fuzzy_left_join(Stat.Week2024,by=c("Date"="Stat.Start","Date"="Stat.Stop"),match_fun=list(`>=`,`<=`))%>%select(-c(Stat.Start,Stat.Stop))

UCI.Escapement.Ages<-UCI.Escapement.Ages.0%>%group_by(Fishery.Project,Stat.Week,Age)%>%summarize(Count=n())%>%left_join(
  UCI.Escapement.Ages.0%>%group_by(Fishery.Project,Stat.Week)%>%summarize(Samples=n()))


#Add Escapement Data by sampling period for later Weighting
UCI.Escapement.Ages<-UCI.Escapement.Ages%>%left_join(Escapement%>%
                                                       fuzzy_left_join(Stat.Week2024,by=c("Date"="Stat.Start","Date"="Stat.Stop"),match_fun=list(`>=`,`<=`))%>%select(-c(Stat.Start,Stat.Stop))%>%
                                                       group_by(Fishery.Project,Stat.Week)%>%summarize(Index=sum(Daily.Count)))


##Combing Commercial and Escapement Age Comp Data
Age.Comp<-rbind(UCI.Escapement.Ages, UCI.Commercial.Ages)%>%
  mutate(Gear=ifelse(Fishery.Project%in%c("Central District Drift - State Waters","UCI EEZ"),3, #Assigning gear codes for later analysis
                     ifelse(Fishery.Project%in%c("Kenai River Escapement", "Kasilof River Escapement", "Fish Creek Escapement", "Judd Lake", "Larson Creek"),14,4))) #Assigning Weir gear code)

#Because we do not sample every stat week we must account for this in our cumulative index counts and only cumulatively count the escapement and harvests of sampled periods
Cumu.Index<-Age.Comp%>%group_by(Fishery.Project,Stat.Week)%>%summarize(Index=unique(Index))%>%
  group_by(Fishery.Project)%>%mutate(Index.Cumu=cumsum(Index))%>%select(Fishery.Project,Stat.Week,Index.Cumu)


#Now add to age comp report
Age.Comp<-Age.Comp%>%left_join(Cumu.Index)


#Generating weighted Comps

A1<-Age.Comp%>%mutate(Count.Period=round((Count/Samples)*Index))%>% #Proportion an age class makes up -> number of individuals in that age class for that sampled period
  group_by(Fishery.Project,Age)%>%mutate(Cumu.Count=cumsum(Count.Period))%>% #Cumulative count of individuals in that age class across sampled periods
  ungroup()%>%#The proportion an age class makes up of the cumulative sampled periods (weighted age compositions)
  complete(Fishery.Project,Stat.Week,Age)%>%arrange(Fishery.Project,Stat.Week,Age)%>%
  group_by(Fishery.Project,Stat.Week)%>%fill(Index.Cumu,.direction = "updown")%>%arrange(Fishery.Project,Stat.Week,Age)%>%
  group_by(Fishery.Project,Age)%>%fill(Cumu.Count,.direction = "down")%>%
  mutate(Weighted=round(Cumu.Count/Index.Cumu,digits = 3))%>%
  select(Fishery.Project,Stat.Week,Age,Weighted)%>%ungroup()%>%
  group_by(Fishery.Project,Stat.Week)%>%mutate(Weighted.Total=sum(Weighted,na.rm=T),Weighted.Corrected=ifelse(Weighted.Total>0&Weighted%in%NA,0,Weighted))%>%
  mutate(Weighted.Corrected=Weighted.Corrected/Weighted.Total)

A2<-A1%>%group_by(Fishery.Project,Age)%>%
  complete(Stat.Week = seq(22,Stat.Today, by=1))%>%arrange(Fishery.Project,Stat.Week,Age)%>%
  group_by(Fishery.Project,Age)%>%
  fill(Weighted.Corrected,.direction = "updown")%>% #fill forward age comps until we get updated age comps
  ungroup()%>%select(-c(Weighted,Weighted.Total))%>%
  left_join(Stat.Week2024)%>%
  group_by(Fishery.Project,Stat.Week,Age,Weighted.Corrected)%>%
  expand(Date=as.Date(Stat.Start:Stat.Stop),.name_repair = "unique")%>%ungroup()%>% #expand to weighted age comps to daily 
  filter(Date<=Today&Date>=as.Date("2024-06-01",format("%Y-%m-%d")))


Age.Comp.Weighted<-A2%>%rename(Weighted=Weighted.Corrected)%>%arrange(Fishery.Project,Date,Age)

#Adding Cumulative Season Totals
Age.Comp.Weighted<-Age.Comp.Weighted%>%group_by(Fishery.Project,Date)%>%mutate(Weighted.Diff=1-sum(Weighted),Count=sum(ifelse(Weighted>0,1,0)))%>%
  mutate(Weighted=ifelse(Weighted>0,Weighted+(Weighted.Diff/3),Weighted))%>%select(Fishery.Project,Stat.Week,Age,Weighted,Date)%>%left_join(Page1%>%select(-Fate))%>%arrange(Fishery.Project,Date)

Age.Comp.Weighted<- Age.Comp.Weighted%>%filter(!Fishery.Project%in%"Fish Creek Escapement") #Fish Creek Age Comps are not correct

###############################################################################
# Harvest Age Compositions
# The UCI age composition report pulls harvest data from Mariner. Thus, the "index" column is the number of fish harvested within each specific period. 
# Each year, the periods will need to be updated in FDMS. 

# Because we will not have the data inseason for most escapement projects and some will not operate, we will use alternatives. (see TR2023worksheet->Page 2)

#Escapement
# Susitna = Larson-> Susitna Forecast 
# Yentna = Judd-> Susitna Forecast
# Fish Creek = Fish Creek Forecast
# Cresent = Western Commercial Harvest
# Other = Central District Drift Fishery

# HARVEST
# Kenai and Kasilof Sections- Drift Harvest age comp


#Adding in alternative age comps but using harvests and escapements (see Page 2 of the TR2023worksheet)

Alternatives<- rbind(
  
  # Adding unsampled Kenai and Kasilof Dipnet/Beach Seine fisheries
  Age.Comp.Weighted%>%filter(Fishery.Project%in%"Central District Drift - State Waters")%>% #Kenai Section
    transform(Fishery.Project=plyr::revalue(Fishery.Project,c("Central District Drift - State Waters"="Kenai Section Set Net Fishery")))%>%select(-Season.Total)%>%
    left_join(Commercial.Harvest.Cumu%>%filter(Fishery.Project%in%"Kenai Section Set Net Fishery")%>%select(1:3)),
  
  Age.Comp.Weighted%>%filter(Fishery.Project%in%"Central District Drift - State Waters")%>% #Kasilof Section
    transform(Fishery.Project=plyr::revalue(Fishery.Project,c("Central District Drift - State Waters"="Kasilof Section Set Net Fishery")))%>%select(-Season.Total)%>%
    left_join(Commercial.Harvest.Cumu%>%filter(Fishery.Project%in%"Kasilof Section Set Net Fishery")%>%select(1:3)),
  
  
  #Adding Remaining Escapements
  Escapement.Cumu%>%filter(Fishery.Project%in%c("Susitna Escapement","Fish Creek Escapement"))%>%select(-Fate)%>%
    left_join(read.csv(file("O:/DCF/UCI/Research/MNGMT/Ins/24/Total Run Tracking in R/Data/Age.Comp.Forecasts.csv")),relationship = "many-to-many")%>%
    fuzzy_left_join(Stat.Week2024,by=c("Date"="Stat.Start","Date"="Stat.Stop"),match_fun=list(`>=`,`<=`))%>%select(-c(Stat.Start,Stat.Stop))%>%filter(Date<=Today),
  
  Age.Comp.Weighted%>%filter(Fishery.Project%in%"Western Subdistrict Set Net Fishery")%>% #For Crescent 
    transform(Fishery.Project=plyr::revalue(Fishery.Project,c("Western Subdistrict Set Net Fishery"="Crescent Escapement")))%>%select(-Season.Total)%>%
    left_join(Escapement.Cumu%>%filter(Fishery.Project%in%"Crescent Escapement")%>%select(1:3)),
  
  Age.Comp.Weighted%>%filter(Fishery.Project%in%"Central District Drift - State Waters")%>% #Unmonitored Systems
    transform(Fishery.Project=plyr::revalue(Fishery.Project,c("Central District Drift - State Waters"="Other")))%>%select(-Season.Total)%>%
    left_join(Escapement.Cumu%>%filter(Fishery.Project%in%"Other")%>%select(1:3)),
  
  #Kalgin and Kustatan Harvests and age comps are grouped in western District Harvest
  
  # PAGE 4:Adding Personal Use and Sportfish Harvest Projections
  Age.Comp.Weighted%>%filter(Fishery.Project%in%"Kasilof River Escapement")%>% 
    transform(Fishery.Project=plyr::revalue(Fishery.Project,c("Kasilof River Escapement"="Kasilof Personal Use Gillnet")))%>%select(-Season.Total)%>%
    left_join(UCI.PU.SF.Cumu%>%filter(Fishery.Project%in%"Kasilof Personal Use Gillnet")%>%select(1:3)),
  
  Age.Comp.Weighted%>%filter(Fishery.Project%in%"Kasilof River Escapement")%>% 
    transform(Fishery.Project=plyr::revalue(Fishery.Project,c("Kasilof River Escapement"="Kasilof Personal Use Dipnet")))%>%select(-Season.Total)%>%
    left_join(UCI.PU.SF.Cumu%>%filter(Fishery.Project%in%"Kasilof Personal Use Dipnet")%>%select(1:3)),
  
  Age.Comp.Weighted%>%filter(Fishery.Project%in%"Kasilof River Escapement")%>%
    transform(Fishery.Project=plyr::revalue(Fishery.Project,c("Kasilof River Escapement"="Kasilof Sport")))%>%select(-Season.Total)%>%
    left_join(UCI.PU.SF.Cumu%>%filter(Fishery.Project%in%"Kasilof Sport")%>%select(1:3)),
  
  Age.Comp.Weighted%>%filter(Fishery.Project%in%"Kenai River Escapement")%>%
    transform(Fishery.Project=plyr::revalue(Fishery.Project,c("Kenai River Escapement"="Kenai Personal Use Dipnet")))%>%select(-Season.Total)%>%
    left_join(UCI.PU.SF.Cumu%>%filter(Fishery.Project%in%"Kenai Personal Use Dipnet")%>%select(1:3)),
  
  Age.Comp.Weighted%>%filter(Fishery.Project%in%"Kenai River Escapement")%>% 
    transform(Fishery.Project=plyr::revalue(Fishery.Project,c("Kenai River Escapement"="Kenai Sport")))%>%select(-Season.Total)%>%
    left_join(UCI.PU.SF.Cumu%>%filter(Fishery.Project%in%"Kenai Sport")%>%select(1:3)),
  
  UCI.PU.SF.Cumu%>%filter(Fishery.Project%in%"Fish Creek Personal Use")%>%select(-Fate)%>%
    left_join(read.csv(file("O:/DCF/UCI/Research/MNGMT/Ins/24/Total Run Tracking in R/Data/Age.Comp.Forecasts.csv")),relationship = "many-to-many")%>%
    fuzzy_left_join(Stat.Week2024,by=c("Date"="Stat.Start","Date"="Stat.Stop"),match_fun=list(`>=`,`<=`))%>%select(-c(Stat.Start,Stat.Stop))%>%filter(Date<=Today))



#Combine alternative age comp data to Age.Comp.Weighted dataset
Page2<- Age.Comp.Weighted%>%rbind(Alternatives)%>%mutate(Total.Fish=Season.Total*Weighted)%>%ungroup()


#Page2.Transposed<- Page2%>%select(1:3)%>%reshape(idvar = "Fishery.Project",direction = "wide",timevar = "Age.Class")%>%replace(is.na(.),0)%>%left_join(Page1[,2:3])

################################################################################ 
############################################################################### 
# AGE ALLOCATION MODELING
# This section is based in the TRXXXworksheet.xlsx (Tab: Pages 6:12)

# Page 6
Adjusted.Age.Comp<-Page2%>%mutate(Stock=ifelse(Fishery.Project%in%c("Kasilof River Escapement", "Kasilof Personal Use Gillnet","Kasilof Personal Use Dipnet", "Kasilof Sport"),"Kasilof",
                                               ifelse(Fishery.Project%in%c("Kenai River Escapement","Kenai Personal Use Dipnet","Kenai Sport"),"Kenai",
                                                      ifelse(Fishery.Project%in%"Susitna Escapement","Susitna",ifelse(
                                                        Fishery.Project%in%"Crescent Escapement","Crescent",
                                                        ifelse(Fishery.Project%in%c("Fish Creek Escapement","Fish Creek Personal Use"),"Fish Creek",
                                                               ifelse(Fishery.Project%in%"Other","Other","NA")))))))%>%filter(!Stock%in%"NA")%>%
  group_by(Stock,Age,Date)%>%summarize(Stock.Total=sum(Total.Fish))


################# Page 7-Area Adjustments #####################################
  #Taking known age compositions for each stock based on escapement sampling or otherwise and apportioning out totals within each age class of commercial harvest. 

#Adjusted percent of age classes for Susitna, Kenai, Kasilof, Fish Creek, and Other
Adjusted.2<-Adjusted.Age.Comp%>%filter(!Stock%in%"Crescent")%>%group_by(Age,Date)%>%mutate(Adjusted.Percent=Stock.Total/sum(Stock.Total,na.rm=T))%>%replace(is.na(.), 0)
#Adjusted percent of age classes for Kenai, and Kasilof
Adjusted.3<-Adjusted.Age.Comp%>%filter(Stock%in%c("Kenai",'Kasilof'))%>%group_by(Age,Date)%>%mutate(Adjusted.Percent=Stock.Total/sum(Stock.Total,na.rm=T))%>%replace(is.na(.), 0)
#Adjusted percent of age classes for Susitna, Fish Creek, and Other
Adjusted.4<-Adjusted.Age.Comp%>%filter(Stock%in%c("Susitna",'Fish Creek',"Other"))%>%group_by(Age,Date)%>%mutate(Adjusted.Percent=Stock.Total/sum(Stock.Total,na.rm=T))%>%replace(is.na(.), 0)
#Adjusted percent of age classes for Crescent
Adjusted.5<-Adjusted.Age.Comp%>%filter(Stock%in%c("Crescent"))%>%group_by(Age,Date)%>%mutate(Adjusted.Percent=Stock.Total/sum(Stock.Total,na.rm=T))%>%replace(is.na(.), 0)
#Adjusted percent of age classes for All (NEW in 2024)
Adjusted.6<-Adjusted.Age.Comp%>%group_by(Age,Date)%>%mutate(Adjusted.Percent=Stock.Total/sum(Stock.Total,na.rm=T))%>%replace(is.na(.), 0)


################## Page 8 and 9- Applying area adjustment to commercial harvest #
#Allocation of Commercial Harvest
  # Adjusted percents are replaced with GSI estimates per fishery. You cannot simply apply the GSI estimates to the commerical harvest becuase you have to apportion the age comps to stocks
  # PLEASE Note: Total commercial harvest may not be apportioned to a stock because not all age classes observed in the commercial
  # are observed in the escapement. 

Allocation.Commercial<-rbind(
  # State Drift- GSI Estimates sum the stock specific GSI estimates (in fish) for both non-corridor only and corridor only periods over the whole season and using those proportions (Table 2 of MSA Tables
  Adjusted.6%>%left_join(Page2%>%filter(Fishery.Project%in%"Central District Drift - State Waters")%>%select(Date,Age,Total.Fish))%>%
    mutate(GSI=ifelse(Date>=Yesterday&Stock%in%"Crescent",.01,
                      ifelse(Date>=Yesterday&Stock%in%"Fish Creek",.01,
                             ifelse(Date>=Yesterday&Stock%in%"Susitna",.06,
                                    ifelse(Date>=Yesterday&Stock%in%"Kenai",.75,
                                           ifelse(Date>=Yesterday&Stock%in%"Kasilof",.03,
                                                  ifelse(Date>=Yesterday&Stock%in%"Other",.14,Adjusted.Percent*Total.Fish)))))))%>%
    mutate(Total.Adjusted=Total.Fish*GSI)%>%
    group_by(Date,Age)%>%mutate(Total.GSI.Adjusted=sum(Total.Adjusted,na.rm=T),Total.In.Catch=unique(Total.Fish),Difference=Total.In.Catch-Total.GSI.Adjusted)%>%
    mutate(Total.Adjusted=Total.Adjusted+(Difference*Adjusted.Percent))%>%mutate(Fishery.Project="Central District Drift - State Waters")%>%
    select(Stock,Age,Date,Stock.Total,Adjusted.Percent,Total.Fish,Total.Adjusted,Fishery.Project),
  
  # Federal Drift- GSI Estimates sum the stock specific GSI estimates (in fish) 
  Adjusted.6%>%left_join(Page2%>%filter(Fishery.Project%in%"UCI EEZ")%>%select(Date,Age,Total.Fish))%>%
    mutate(GSI=ifelse(Date>=Yesterday&Stock%in%"Crescent",.02,
                      ifelse(Date>=Yesterday&Stock%in%"Fish Creek",.01,
                             ifelse(Date>=Yesterday&Stock%in%"Susitna",.09,
                                    ifelse(Date>=Yesterday&Stock%in%"Kenai",.61,
                                           ifelse(Date>=Yesterday&Stock%in%"Kasilof",.12,
                                                  ifelse(Date>=Yesterday&Stock%in%"Other",.16,Adjusted.Percent*Total.Fish)))))))%>%
    mutate(Total.Adjusted=Total.Fish*GSI)%>%
    group_by(Date,Age)%>%mutate(Total.GSI.Adjusted=sum(Total.Adjusted,na.rm=T),Total.In.Catch=unique(Total.Fish),Difference=Total.In.Catch-Total.GSI.Adjusted)%>%
    mutate(Total.Adjusted=Total.Adjusted+(Difference*Adjusted.Percent))%>%mutate(Fishery.Project="UCI EEZ")%>%
    select(Stock,Age,Date,Stock.Total,Adjusted.Percent,Total.Fish,Total.Adjusted,Fishery.Project),
  
  
  # Kenai Section- We have no data for this fishery so we will use the drift age comp and the overall GSI estimates across all fishery in 2024 
  Adjusted.3%>%left_join(Page2%>%filter(Fishery.Project%in%"Kenai Section Set Net Fishery")%>%select(Date,Age,Total.Fish))%>%
    mutate(GSI=ifelse(Date>=Yesterday&Stock%in%"Crescent",.01,
                      ifelse(Date>=Yesterday&Stock%in%"Fish Creek",.01,
                             ifelse(Date>=Yesterday&Stock%in%"Susitna",.09,
                                    ifelse(Date>=Yesterday&Stock%in%"Kenai",.69,
                                           ifelse(Date>=Yesterday&Stock%in%"Kasilof",.07,
                                                  ifelse(Date>=Yesterday&Stock%in%"Other",.17,Adjusted.Percent*Total.Fish)))))))%>%
    mutate(Total.Adjusted=Total.Fish*GSI)%>%
    group_by(Date,Age)%>%mutate(Total.GSI.Adjusted=sum(Total.Adjusted,na.rm=T),Total.In.Catch=unique(Total.Fish),Difference=Total.In.Catch-Total.GSI.Adjusted)%>%
    mutate(Total.Adjusted=Total.Adjusted+(Difference*Adjusted.Percent))%>%mutate(Fishery.Project="Kenai Section Set Net Fishery")%>%
    select(Stock,Age,Date,Stock.Total,Adjusted.Percent,Total.Fish,Total.Adjusted,Fishery.Project),
  
  # Kasilof Section
  Adjusted.3%>%left_join(Page2%>%filter(Fishery.Project%in%"Kasilof Section Set Net Fishery")%>%select(Date,Age,Total.Fish))%>%
    mutate(GSI=ifelse(Date>=Yesterday&Stock%in%"Crescent",.01,
                      ifelse(Date>=Yesterday&Stock%in%"Fish Creek",.01,
                             ifelse(Date>=Yesterday&Stock%in%"Susitna",.09,
                                    ifelse(Date>=Yesterday&Stock%in%"Kenai",.69,
                                           ifelse(Date>=Yesterday&Stock%in%"Kasilof",.07,
                                                  ifelse(Date>=Yesterday&Stock%in%"Other",.17,Adjusted.Percent*Total.Fish)))))))%>%
    mutate(Total.Adjusted=Total.Fish*GSI)%>%
    group_by(Date,Age)%>%mutate(Total.GSI.Adjusted=sum(Total.Adjusted,na.rm=T),Total.In.Catch=unique(Total.Fish),Difference=Total.In.Catch-Total.GSI.Adjusted)%>%
    mutate(Total.Adjusted=Total.Adjusted+(Difference*Adjusted.Percent))%>%mutate(Fishery.Project="Kasilof Section Set Net Fishery")%>%
    select(Stock,Age,Date,Stock.Total,Adjusted.Percent,Total.Fish,Total.Adjusted,Fishery.Project),
  
  
  
  # Western
  Adjusted.6%>%left_join(Page2%>%filter(Fishery.Project%in%"Western Subdistrict Set Net Fishery")%>%select(Date,Age,Total.Fish))%>%
    mutate(GSI=ifelse(Date>=Yesterday&Stock%in%"Crescent",.06,
                      ifelse(Date>=Yesterday&Stock%in%"Fish Creek",.01,
                             ifelse(Date>=Yesterday&Stock%in%"Susitna",.06,
                                    ifelse(Date>=Yesterday&Stock%in%"Kenai",.38,
                                           ifelse(Date>=Yesterday&Stock%in%"Kasilof",.08,
                                                  ifelse(Date>=Yesterday&Stock%in%"Other",.4,Adjusted.Percent*Total.Fish)))))))%>%
    mutate(Total.Adjusted=Total.Fish*GSI)%>%
    group_by(Date,Age)%>%mutate(Total.GSI.Adjusted=sum(Total.Adjusted,na.rm=T),Total.In.Catch=unique(Total.Fish),Difference=Total.In.Catch-Total.GSI.Adjusted)%>%
    mutate(Total.Adjusted=Total.Adjusted+(Difference*Adjusted.Percent))%>%mutate(Fishery.Project="Western Subdistrict Set Net Fishery")%>%
    select(Stock,Age,Date,Stock.Total,Adjusted.Percent,Total.Fish,Total.Adjusted,Fishery.Project),
  
  # Northern District-General 
  Adjusted.6%>%left_join(Page2%>%filter(Fishery.Project%in%"Northern District Set Net Fishery - General Subdistrict")%>%select(Date,Age,Total.Fish))%>%
    mutate(GSI=ifelse(Date>=Yesterday&Stock%in%"Crescent",.009,
                      ifelse(Date>=Yesterday&Stock%in%"Fish Creek",.127,
                             ifelse(Date>=Yesterday&Stock%in%"Susitna",.301,
                                    ifelse(Date>=Yesterday&Stock%in%"Kenai",.1,
                                           ifelse(Date>=Yesterday&Stock%in%"Kasilof",.026,
                                                  ifelse(Date>=Yesterday&Stock%in%"Other",.437,Adjusted.Percent*Total.Fish)))))))%>%
    mutate(Total.Adjusted=Total.Fish*GSI)%>%
    group_by(Date,Age)%>%mutate(Total.GSI.Adjusted=sum(Total.Adjusted,na.rm=T),Total.In.Catch=unique(Total.Fish),Difference=Total.In.Catch-Total.GSI.Adjusted)%>%
    mutate(Total.Adjusted=Total.Adjusted+(Difference*Adjusted.Percent))%>%mutate(Fishery.Project="Northern District Set Net Fishery - General Subdistrict")%>%
    select(Stock,Age,Date,Stock.Total,Adjusted.Percent,Total.Fish,Total.Adjusted,Fishery.Project),
  
  #Northern District-Eastern
  Adjusted.6%>%left_join(Page2%>%filter(Fishery.Project%in%"Northern District Set Net Fishery - Eastern Subdistrict")%>%select(Date,Age,Total.Fish))%>%
    mutate(GSI=ifelse(Date>=Yesterday&Stock%in%"Crescent",.009,
                      ifelse(Date>=Yesterday&Stock%in%"Fish Creek",.127,
                             ifelse(Date>=Yesterday&Stock%in%"Susitna",.301,
                                    ifelse(Date>=Yesterday&Stock%in%"Kenai",.1,
                                           ifelse(Date>=Yesterday&Stock%in%"Kasilof",.026,
                                                  ifelse(Date>=Yesterday&Stock%in%"Other",.437,Adjusted.Percent*Total.Fish)))))))%>%
    mutate(Total.Adjusted=Total.Fish*GSI)%>%
    group_by(Date,Age)%>%mutate(Total.GSI.Adjusted=sum(Total.Adjusted,na.rm=T),Total.In.Catch=unique(Total.Fish),Difference=Total.In.Catch-Total.GSI.Adjusted)%>%
    mutate(Total.Adjusted=Total.Adjusted+(Difference*Adjusted.Percent))%>%mutate(Fishery.Project="Northern District Set Net Fishery - Eastern Subdistrict")%>%
    select(Stock,Age,Date,Stock.Total,Adjusted.Percent,Total.Fish,Total.Adjusted,Fishery.Project))


###################### Pages 10 and 11 ########################################
#Page9<-Allocation.Commercial%>%group_by(Fishery.Project,Age,Date)%>%mutate(Fishery.Age.Total=sum(Total.Adjusted))%>%
#  select(Age,Date,Total.Fish,Stock)%>%
#  left_join(Page2%>%select(Fishery.Project,Date,Age,Weighted,Total.Fish))%>%
#  mutate(Remainder=Total.Fish-Fishery.Age.Total)

#Total Commercial 
Total.Allocation.Commercial<-Allocation.Commercial%>%group_by(Stock,Age,Date)%>%summarize(UCI.Total=sum(Total.Adjusted))%>%mutate(Fate="Commercial Harvest")

#Total Escapement
Total.Allocation.Escapement<-Page2%>%filter(Fishery.Project%in%c("Kasilof River Escapement","Kenai River Escapement","Crescent Escapement","Fish Creek Escapement","Susitna Escapement","Other"))%>%
  transform(Stock=plyr::revalue(Fishery.Project,c("Kasilof River Escapement"="Kasilof", "Kenai River Escapement"="Kenai", 
                                                  "Crescent Escapement"="Crescent","Fish Creek Escapement"="Fish Creek","Susitna Escapement"="Susitna","Other"="Other")))%>%
  select(Age,Date,Total.Fish,Stock)%>%rename(UCI.Total=Total.Fish)%>%mutate(Fate="Escapement")

#Total PU & Sport
Total.Allocation.PUSPORT<-Page2%>%filter(Fishery.Project%in%c("Kasilof Personal Use Gillnet","Kasilof Personal Use Dipnet","Kasilof Sport","Kenai Personal Use Dipnet","Kenai Sport","Fish Creek Personal Use"))%>%
  transform(Stock=plyr::revalue(Fishery.Project,c("Kasilof Personal Use Gillnet"="Kasilof","Kasilof Personal Use Dipnet"="Kasilof","Kasilof Sport"="Kasilof",
                                                  "Kenai Personal Use Dipnet"="Kenai","Kenai Sport"="Kenai","Fish Creek Personal Use"="Fish Creek")))%>%
  select(Age,Date,Total.Fish,Stock)%>%rename(UCI.Total=Total.Fish)%>%mutate(Fate="Personal Use and Sport")


###Combining All Fates
Total.Allocation.UCI<-rbind(Total.Allocation.Commercial,Total.Allocation.Escapement,Total.Allocation.PUSPORT)

write.csv(Total.Allocation.UCI,"O:/DCF/UCI/Research/MNGMT/Ins/24/Final Results/Final.GSI.Adjusted.Results.csv",row.names = F)


##################################### Page 12 ##################################
# Page 12
Inseason.Totals<-Total.Allocation.UCI%>%group_by(Date)%>%summarize(AccumRun1=sum(UCI.Total))%>%left_join(Total.Run.By.Date)


#Compare AccumRun1 with Page 1 Totals
left_join(Page1%>%filter(Date%in%Today)%>%group_by(Fate)%>%summarize(Actual=sum(Season.Total)),
          Total.Allocation.UCI%>%filter(Date%in%Today)%>%ungroup()%>%group_by(Fate)%>%summarize(Apportioned=sum(UCI.Total)))%>%
  mutate(Error=Actual-Apportioned)%>%mutate(Actual.Total=sum(Actual),Apportioned.Total=sum(Apportioned))%>%
  mutate(Error.Percent=(Apportioned.Total-Actual.Total)/Actual.Total)



#Stock specific run estimates- These will be fed into the "Inseason Projection" project
Stock.Fate.Breakout<-Total.Allocation.UCI%>%group_by(Stock,Date)%>%summarize(Total=as.integer(sum(UCI.Total,na.rm=T)))
Labels<-Stock.Fate.Breakout%>%filter(Date%in%Today)

ggplot()+
  geom_point(aes(Stock.Fate.Breakout$Date,Stock.Fate.Breakout$Total,colour=Stock.Fate.Breakout$Stock,lty=Stock.Fate.Breakout$Stock),size=.75)+
  geom_text_repel(aes(Labels$Date,Labels$Total,label=Labels$Total),size=3,box.padding =.9)+
  theme_bw()+
  scale_y_continuous(labels = comma)+
  xlab("")+ylab("Cumulative Total Run")+
  theme(text=element_text(size=12,  family="serif"))+
  theme(legend.title = element_blank())


##################################################################################
#################################################################################
# Grooming data for brood tables

## Age Class breakout by stock
Stock.Age.Summary<-Total.Allocation.UCI%>%filter(Date%in%Yesterday)%>%group_by(Stock,Age)%>%summarize(Total=round(sum(UCI.Total),digits = 0))%>%
  mutate(Total.Age=ifelse(Age%in%c("0.2","1.1"),3,
                    ifelse(Age%in%c("0.3","1.2","2.1"),4,
                           ifelse(Age%in%c("0.4","1.3","2.2","3.1"),5,
                                  ifelse(Age%in%c("1.4","2.3"),6,
                                         ifelse(Age%in%c("2.4","3.3"),7,NA))))))%>%mutate(Brood.Year=2024-Total.Age)%>%arrange(Stock,Total.Age)%>%
  select(Stock,Age,Total,Brood.Year)%>%
  pivot_wider(names_from = "Age",values_from = "Total")

# This code will be used to fill in the brood table
write.csv(Stock.Age.Summary,
          "O:/DCF/UCI/Research/Forecast/Preseason/2025/2025 UCI Brood Update.csv",row.names = F)

#Use this code to fill in ""Spawners" column of brood table. Subtract upriver harvest from sonar passage by age class for total composition of escapement
Page1%>%filter(Fishery.Project%in%c("Kenai River Escapement","Kasilof River Escapement"))%>%filter(Date%in%Today)%>%
  left_join(Age.Comp.Weighted%>%filter(Stat.Week%in%Stat.Today&Fishery.Project%in%"Kenai River Escapement"))%>%select(1,2,3,6)%>% #grabbing weighted age comp to apply to total upriver harvest
  mutate(Upriver.Harvest=ifelse(Stock%in%"Kenai",Weighted*371827,ifelse(Stock%in%"Kasilof",Weighted*1)))%>% #Average 5-year upriver harvest AMR T18. Kasilof has no significant upriver harvest
  mutate(Spawners=UCI.Total-Upriver.Harvest)%>%group_by(Stock)%>%summarize(Spawners=sum(Spawners))



Total.Allocation.UCI%>%filter(Date%in%Yesterday)%>%group_by(Stock,Fate)%>%summarize(Total=sum(UCI.Total))%>%mutate(Grand.Total=sum(Total))

  (Total.Allocation.UCI%>%filter(Date%in%Yesterday)%>%group_by(Stock)%>%summarize(Total=sum(UCI.Total))%>%summarize(Grand.Total=sum(Total)))-
  (Page1%>%filter(Date%in%Today)%>%group_by(Fate)%>%mutate(Total=sum(Season.Total))%>%ungroup()%>%summarize(Grand.Total=sum(Season.Total)))



