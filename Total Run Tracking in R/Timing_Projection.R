library(dplyr)
library(stringr)
library(fuzzyjoin)
library(ggrepel)
library(openxlsx)
library(tidyr)
library(zoo)
library(scales)
library(flextable)
library(gghighlight)
library(lubridate)


#################################################################################
######## DATA WRANGLING AND GROOMING ############################################

#Loading prior year total inseason allocation results
Allocation.23<-read.csv(file("O:/DCF/UCI/Research/MNGMT/Ins/23/Final Results/Final.Age.Allocations.Results.csv"))%>%group_by(Stock,Date)%>%summarize(Total=sum(UCI.Total))%>%filter(Stock%in%c("Kenai","Kasilof"))%>%
  mutate(Date=as.Date(Date,format="%m/%d/%Y"),Date=Date%m+% years(1))%>%mutate(Year=2023)

#Gathering inseason allocation results
Allocation.24<-Total.Allocation.UCI%>%filter(Stock%in%c("Kasilof","Kenai"))%>%group_by(Stock,Date)%>%summarize(Total=sum(UCI.Total))%>%mutate(Year=2024)


Hist.Allocations<-read.csv(file("O:/DCF/UCI/Research/MNGMT/Ins/24/Total Run Tracking in R/Total.Run.Allocations.csv"))%>%mutate(Date=as.Date(Date,format="%m/%d/%Y"))%>%arrange(Year,Stock,Date)%>%select(-Method)


Total.Runs<-rbind(Allocation.23,Allocation.24,Hist.Allocations)

  #group_by(Stock,Year)%>%
  #complete(Date = seq(as.Date("06-01-2024",format="%m-%d-%Y"),as.Date("08-15-2024",format="%m-%d-%Y"), by="1 day"))%>%
  #fill(Total,Method,.direction = "updown")


### PREPARING TIMING DAT
#Generating Inriver Run Timing
Inriver.Timing<-Hist.KeKa.Sonar%>%select(River,Year,Date,Daily.Count)%>%filter(Year>2000)%>%
  group_by(River,Year)%>%mutate(Timing=cumsum(Daily.Count)/max(cumsum(Daily.Count)),Date=as.Date(Date,format="%d-%b"))%>%select(-Daily.Count)%>%
  group_by(River)%>%
  complete(Year,Date)%>%
  group_by(River,Year)%>%
  fill(Timing,.direction = "updown")%>%rename(Stock=River)




Timing.Date<-rbind(Inriver.Timing%>%arrange(Year,Date)%>%group_by(Stock,Date)%>%mutate(Timing_5=lag(rollapplyr(Timing, 5, mean, partial = T)),
                                                                                                             Timing_10=lag(rollapplyr(Timing, 10, mean, partial = T))),
                   Inriver.Timing%>%group_by(Stock,Date)%>%summarize(Timing_5=mean(ifelse(Year>2018,Timing,NA),na.rm=T),Timing_10=mean(ifelse(Year>2013,Timing,NA),na.rm=T))%>%mutate(Year=2024))

Timing.Date%>%filter(Year%in%c(2009,2003,2010)&Date<as.Date("2024-09-01",format="%Y-%m-%d")&Stock%in%"Kenai")%>%
  ggplot()+
  geom_line(aes(Date,Timing,lty=as.factor(Year)))+
  scale_x_date(date_breaks = "1 week")





## Bringing in Total Runs
Actual.Runs<-rbind(read.csv(file("O:/DCF/UCI/Research/Forecast/Preseason/2024/Kenai/Kenai.Brood.csv"))%>%select(Brood.Year,Run)%>%rename(Year=Brood.Year)%>%mutate(Stock="Kenai"),
                  read.csv(file("O:/DCF/UCI/Research/Forecast/Preseason/2024/Kasilof/Kasilof.Brood.csv"))%>%select(Brood.Year,Run)%>%rename(Year=Brood.Year)%>%mutate(Stock="Kasilof"))

#################################################################################
################################################################################  
### Step 1: Selecting Run Timing Window

Projections.Combined<-Total.Runs%>%left_join(Timing.Date%>%select(Year,Date,Timing_5,Timing_10,Stock))%>%replace(is.na(.), 0)%>%
  mutate(Projection_5=Total/Timing_5,
         Projection_10=Total/Timing_10)%>%
  left_join(Actual.Runs)%>% 
  mutate(Error_5=abs(Run-Projection_5)/Run, 
         Error_10=abs(Run-Projection_10)/Run)%>%
  filter(Year>2016)

## selecting run timing window (recent 5 or 10 year average)
Projections.Combined%>%select(Stock,Date,Error_5,Error_10)%>%filter(Date>=as.Date("2024-07-22",format="%Y-%m-%d")&Date<=as.Date("2024-07-26",format="%Y-%m-%d"))%>%group_by(Stock)%>%
  summarize(Error_5=mean(Error_5),Error_10=mean(Error_10))

## Visualize Run Projections
Projections.Combined%>%filter(Stock%in%"Kenai")%>%
  ggplot()+
  geom_point(aes(Date,Projection_5))+
  geom_point(aes(Date,Projection_10),color=2)+
  facet_grid(Year~.,scales = "free")+
  scale_y_continuous(labels = comma)+
  theme_bw()

Projections.Combined%>%filter(Stock%in%"Kasilof")%>%
  ggplot()+
  geom_point(aes(Date,Projection_5))+
  geom_point(aes(Date,Projection_10),color=2)+
  facet_grid(Year~.,scales = "free")+
  scale_y_continuous(labels = comma)+
  theme_bw()

## Projection Error By Date
Projections.Combined%>%select(Stock,Date,Year,Error_5,Error_10)%>%
  pivot_longer(cols = 4:5,names_to = "Timing",values_to = "Error")%>%filter(Year<2024)%>%
  filter(Stock%in%"Kasilof")%>%
  ggplot()+
  geom_rect(aes(xmin=as.Date("2024-07-22",format="%Y-%m-%d"), xmax=as.Date("2024-07-26",format="%Y-%m-%d"), ymin=0, ymax=1), fill="grey", alpha=0.7, inherit.aes = FALSE)+
  geom_point(aes(Date,Error,color=Timing))+
  facet_grid(Year~.,scales = "free")+
  theme_bw()

Projections.Combined%>%select(Stock,Date,Year,Error_5,Error_10)%>%
  pivot_longer(cols = 4:5,names_to = "Timing",values_to = "Error")%>%filter(Year<2024)%>%
  filter(Stock%in%"Kenai")%>%
  ggplot()+
  geom_rect(aes(xmin=as.Date("2024-07-22",format="%Y-%m-%d"), xmax=as.Date("2024-07-26",format="%Y-%m-%d"), ymin=0, ymax=1), fill="grey", alpha=0.7, inherit.aes = FALSE)+
  geom_point(aes(Date,Error,color=Timing))+
  facet_grid(Year~.,scales = "free")+
  theme_bw()



#Average Error by Stock, Date, Timing Window
Projections.Combined%>%select(Stock,Date,Year,Error_5,Error_10)%>%
  pivot_longer(cols = 4:5,names_to = "Timing",values_to = "Error")%>%
  group_by(Stock,Date,Timing)%>%summarize(Error=mean(Error,na.rm=T))%>%
  ggplot()+
  geom_rect(aes(xmin=as.Date("2024-07-22",format="%Y-%m-%d"), xmax=as.Date("2024-07-26",format="%Y-%m-%d"), ymin=0, ymax=1), fill="grey", alpha=0.7, inherit.aes = FALSE)+
  geom_point(aes(Date,Error,color=Timing),size=2)+
  facet_grid(Stock~.,scales = "free")+
  theme_bw()


Projections.Combined%>%
  filter(Year>2016)%>%
  group_by(Stock,Date)%>%summarize(Direction_5=mean(Projection_5-Run),Direction_10=mean(Projection_10-Run))%>%
  ggplot()+
  geom_point(aes(Date,Direction_5),size=2)+
  geom_point(aes(Date,Direction_10),color=2,size=2)+
  facet_grid(Stock~.,scales="free")+
  scale_y_continuous(labels = comma)+
  theme_bw()+
  geom_hline(yintercept = 0,size=1)+
  geom_vline(xintercept=as.Date("2024-07-22",format="%Y-%m-%d"))+
  geom_vline(xintercept=as.Date("2024-07-26",format="%Y-%m-%d"))+
  ylab("Projected - Realized")




###############################################################################
####### Step 2: Select Run Timing Model 
# Applying lag and lead to to account for potential run timing differences
Total.Runs%>%left_join(Timing.Date%>%select(Year,Date,Timing_5,Timing_10,Stock))%>%replace(is.na(.), 0)%>%
  mutate(Projection_5=Total/Timing_5,
         Projection_10=Total/Timing_10)%>%
  left_join(Actual.Runs)%>%group_by(Stock,Year)%>%mutate(Early_10.1=Total/lead(Timing_10),
                                                          Early_10.2=Total/lead(Timing_10,n=2),
                                                         Early_10.3=Total/lead(Timing_10,n=3),
                                                         Ontime_10=Projection_10,
                                                         Late_10.1=Total/lag(Timing_10),
                                                         Late_10.2=Total/lag(Timing_10,n=2),
                                                         Late_10.3=Total/lag(Timing_10,n=3))

#Looking at Error
Projections.Lag.Error<-Total.Runs%>%left_join(Timing.Date%>%select(Year,Date,Timing_5,Timing_10,Stock))%>%replace(is.na(.), 0)%>%
  mutate(Projection_5=Total/Timing_5,
         Projection_10=Total/Timing_10)%>%
  left_join(Actual.Runs)%>%group_by(Stock,Year)%>%mutate(Early_10.3=abs(Run-(Total/lead(Timing_10,n=3)))/Run,
                                                         Early_10.2=abs(Run-(Total/lead(Timing_10,n=2)))/Run,
                                                         Early_10.1=abs(Run-(Total/lead(Timing_10,n=1)))/Run,
                                                         Ontime_10=abs(Run-Projection_10)/Run,
                                                         Late_10.1=abs(Run-(Total/lag(Timing_10)))/Run,
                                                         Late_10.2=abs(Run-(Total/lag(Timing_10,n=2)))/Run,
                                                         Late_10.3=abs(Run-(Total/lag(Timing_10,n=3)))/Run)

# TRIMMING FOR PROJECTION WEEK
Projections.Lag.Error%>%
  filter(Date>=as.Date("2024-07-22",format="%Y-%m-%d")&Date<=as.Date("2024-07-26",format="%Y-%m-%d"))%>%
group_by(Stock)%>%
 summarize(across(Early_10.3:Late_10.3,mean,na.rm = TRUE)) 
  
  
################################################################################
################################################################################ 
### 2024 PROJECTION
 
Inseason.Projection<-Timing.Date%>%filter(Year==2024)%>%select(-c(Timing,Timing_5))%>%left_join(Allocation.24)%>%
  group_by(Stock)%>%
  mutate(Early_10.1=Total/lead(Timing_10),
         Early_10.2=Total/lead(Timing_10,n=2),
         Early_10.3=Total/lead(Timing_10,n=3),
        Ontime_10=Total/Timing_10,
         Late_10.1=Total/lag(Timing_10),
         Late_10.2=Total/lag(Timing_10,n=2),
         Late_10.3=Total/lag(Timing_10,n=3))


#Looking at Projection scenarios
Inseason.Daily.Projection<-Timing.Date%>%filter(Year==2024)%>%select(Stock,Date,Timing_10)%>%
  left_join(Inseason.Projection%>%filter(Date%in%Yesterday)%>%select(Stock,Early_10.1:Late_10.3))%>%
  mutate(Ontime.Projection=Ontime_10*Timing_10)%>%group_by(Stock)%>%
  mutate(Early3.Projection=Early_10.3*lead(Timing_10,n=3),
         Early2.Projection=Early_10.2*lead(Timing_10,n=2),
         Early1.Projection=Early_10.1*lead(Timing_10,n=2),
         Late3.Projection=Late_10.3*lag(Timing_10,n=3),
         Late2.Projection=Late_10.2*lag(Timing_10,n=2),
         Late1.Projection=Late_10.1*lag(Timing_10,n=1))%>%
  select(-c(Early_10.1:Late_10.3))%>%
  left_join(Total.Allocation.UCI%>%filter(Stock%in%c("Kasilof","Kenai"))%>%group_by(Stock,Date)%>%summarize(Total=sum(UCI.Total)))%>%
  mutate(Forecast=ifelse(Stock%in%"Kenai",3380460,1115161))


###################################################
#Looking model stability
Inseason.Projection%>%filter(Date<=Yesterday)%>%
  ggplot()+
  geom_point(aes(Date,Early_10.3),color="grey80")+
  geom_point(aes(Date,Early_10.2),color="grey80")+
  geom_point(aes(Date,Early_10.1),color="grey80")+
  geom_point(aes(Date,Late_10.3),color="grey80")+
  geom_point(aes(Date,Late_10.2),color="grey80")+
  geom_point(aes(Date,Late_10.1),color="grey80")+
  geom_point(aes(Date,Ontime_10),size=4)+
  facet_grid(Stock~.,scales="free")+
  scale_y_continuous(labels = comma)+
  theme_bw()+
  ylab("Projected Total Run")+
  xlab("")



Inseason.Daily.Projection%>%mutate(Ontime.Error=abs(Total-Ontime.Projection)/Total,
                                   Early3.Error=abs(Total-Early3.Projection)/Total,
                                   Early2.Error=abs(Total-Early2.Projection)/Total,
                                   Early1.Error=abs(Total-Early1.Projection)/Total,
                                   Late3.Error=abs(Total-Late3.Projection)/Total,
                                   Late2.Error=abs(Total-Late2.Projection)/Total,
                                   Late1.Error=abs(Total-Late1.Projection)/Total)%>%
  filter_all(all_vars(!is.infinite(.)))%>%
  group_by(Stock)%>%summarise(across(Ontime.Error:Late1.Error, mean, na.rm=TRUE))

  

# Visualizing actual versus projection scenarios
Test<-Inseason.Daily.Projection%>%pivot_longer(Ontime.Projection:Late1.Projection, names_to = "Timing_Senario",values_to = "Projection")%>%filter(Date>=as.Date("2024-06-15",format="%Y-%m-%d"))%>%
  mutate(Error=abs(Total-Projection)/Total)%>% filter_all(all_vars(!is.infinite(.)))%>%group_by(Stock,Timing_Senario)%>%
  mutate(Count=1,Running.Error=cumsum(coalesce(Error,0))/cumsum(Count))

Test.2<-Test%>%group_by(Stock,Timing_Senario)%>%summarize(MAPE=mean(Error,na.rm=T))%>%group_by(Stock)%>%filter(MAPE%in%min(MAPE))%>%mutate(Timing_Select=Timing_Senario)%>%select(Stock,Timing_Select)
  
Test%>%left_join(Test.2)%>%mutate(Projection=round(Projection,digits=0))%>%
  ggplot()+
  geom_line(aes(Date,Projection,group=Timing_Senario),size=1.5)+
  facet_grid(Stock~.,scales="free")+
  gghighlight(Timing_Senario==Timing_Select ,calculate_per_facet = TRUE,label_key = Projection,
              unhighlighted_params = list(linewidth = 1, colour = alpha("grey", 0.7)))+
  geom_line(aes(Date,Total),size=2,color=2)+
  scale_y_continuous(labels = comma)+
  ylab("Total Run")+
  xlab("")+
  theme_bw()
  
 



#Method 2: Using raw run timing curves for projection.

Test.1<-Inriver.Timing%>%left_join(
  Inriver.Timing%>%left_join(Allocation.24%>%select(Stock,Date,Total))%>%mutate(Projection=Total/Timing)%>%filter(Date%in%Yesterday)%>%ungroup()%>%select(Stock,Year,Projection))%>%
  mutate(Daily=Timing*Projection)%>%left_join(Allocation.24%>%select(Stock,Date,Total))%>%mutate(Error=abs(Total-Daily)/Total)%>%
  filter_all(all_vars(!is.infinite(.)))%>%group_by(Stock,Year)%>%
  mutate(Count=1,Running.Error=cumsum(Error)/cumsum(Count))

Test.2<-Test.1%>%group_by(Stock,Year)%>%summarize(MAPE=mean(Error,na.rm=T))%>%arrange(Stock,MAPE)%>%group_by(Stock)%>%
  slice(1:3)%>%mutate(Year_Select=Year)%>%select(Stock,Year_Select)


Test.1%>%group_by(Stock,Year)%>%summarize(MAPE=mean(Error,na.rm=T))%>%arrange(Stock,MAPE)%>%group_by(Stock)%>%slice(1:3)%>%
  left_join(Test.1%>%filter(Date%in%Yesterday)%>%select(Stock,Year,Projection))%>%mutate(Year=as.character(Year))%>%
  rbind(Test.1%>%group_by(Stock,Year)%>%summarize(MAPE=mean(Error,na.rm=T))%>%arrange(Stock,MAPE)%>%group_by(Stock)%>%slice(1:3)%>%
          left_join(Test.1%>%filter(Date%in%Yesterday)%>%select(Stock,Year,Projection))%>%group_by(Stock)%>%summarize(Projection=mean(Projection),MAPE=mean(MAPE),Year="Subtotal"))%>%arrange(Stock)%>%
  flextable()%>%
  merge_v(j=1)%>%
  align(align="left",j=1:2,part="all")%>%
  valign(valign = "top")%>%
  line_spacing(space=1,part = "all")%>%
  bg(i= ~Year%in%"Subtotal",bg="grey95")%>%
  colformat_double(j=2,digits=2)%>%
  colformat_double(j=4,digits=0)%>%
  font(fontname="serif",part="all")%>%
  hline(i=4,j=1:4,part="body")%>%
  fix_border_issues()%>%
  autofit()



Test.1%>%left_join(Test.2)%>%mutate(Projection=round(Projection,digits=0))%>%
  ggplot()+
  geom_line(aes(Date,Daily,group=Year))+
  facet_grid(Stock~.,scales="free")+
  gghighlight(Year==Year_Select ,calculate_per_facet = TRUE,label_key = Projection,
              unhighlighted_params = list(linewidth = 1, colour = alpha("grey", 0.7)))+
  geom_line(aes(Date,Total),size=2,color=2)+
  scale_y_continuous(labels = comma)+
  ylab("Total Run")+
  xlab("")+
  theme_bw()




# Looking Model Stability
   Projected.Daily<- Inriver.Timing%>%left_join(Allocation.24%>%select(Stock,Date,Total))%>%mutate(Projection=Total/Timing)%>%filter(Date<=Yesterday)%>%mutate(Projection.Date=Date)%>%
    select(Stock,Year,Projection.Date,Projection)%>%left_join(Inriver.Timing,relationship = "many-to-many")%>%filter(Projection>0)%>%
    mutate(Projected.Daily=Timing*Projection)%>%left_join(Allocation.24%>%select(-Year))%>%mutate(Error=abs(Projected.Daily-Total)/Total)%>%
    filter_all(all_vars(!is.infinite(.)))%>%group_by(Stock,Projection.Date,Year)%>%
    mutate(Count=1,Running.Error=cumsum(Error)/cumsum(Count))%>%
    group_by(Stock,Projection.Date,Year)%>%summarize(MAPE=mean(Error,na.rm=T))%>%arrange(Stock,Projection.Date,MAPE)%>%group_by(Stock,Projection.Date)%>%slice(1:3)%>%
      left_join(Inriver.Timing%>%left_join(Allocation.24%>%select(Stock,Date,Total))%>%mutate(Projection=Total/Timing)%>%filter(Date<=Yesterday)%>%mutate(Projection.Date=Date)%>%
                  select(Stock,Year,Projection.Date,Projection))%>%
     group_by(Stock,Projection.Date)%>%mutate(Weight=1-(MAPE/sum(MAPE)))%>%
     mutate(Weighted.Projection=wei ghted.mean(Projection,Weight))
   
   Projected.Daily%>%group_by(Stock,Projection.Date)%>%mutate(Average=mean(Projection))%>%
      ggplot()+
      geom_point(aes(Projection.Date,Projection),alpha=.2)+
      geom_point(aes(Projection.Date,Average),size=4)+
      facet_grid(Stock~.,scales="free")+
      scale_y_continuous(labels = comma)+
      ylab("Projected Total Run")+
      xlab("")+
      theme_bw()


