#########################################################
# ModelThePlanDistricts.R
#
# Model the governor's plan for each district.
#########################################################
# 10 May 2020, Ryan Hastings
#########################################################

rm(list=ls()) # clear variables

library(tidyverse)

################## configuration variables ##############

R0_vec<-c(2.8,2.35,2.6,2.05,3.35,2.8,2.25,2.3,2.75,2.2) # base transmission rate
Tinc<-5 # incubation time (days)
Tinf<-3 # time of infection before either hospitalization or recovery
Thosp<-8.6 # time in hospitalization until recovery
Tdeath<-10.4 # time in hospital until death
Tcrit<-7.9 # time in ICU until recovery
Npop<-6.692e6 # population of the state
maxt<-350 # maximum number of time steps
Rhosp_vec<-0.0278*c(1.287,1.4375,1.1625,3.78,0.872,1.0,1.0,2.16,0.7527,1.818) # hospitalization rates
Rcrit_vec<-0.0094*c(1.176,1.0,1.78571,2.5,1.0,1.0,1.0,2.44,0.667,2.6) # ICU rates
Rdeath<-0.004884 # death rates

# dates for each phase
DayZero<-as.Date('2020-01-20')
PhaseOneDate<-as.Date('2020-03-25')
PhaseTwoDate<-as.Date('2020-05-04')
PhaseThreeDate<-as.Date('2020-05-24')
PhaseFourDate<-as.Date('2020-06-14')
PhaseFiveDate<-as.Date('2020-07-04')
DayZeroOffset<-55 # difference in days between first death in district and day zero for that district
init_filename<-'../../../COVID19Response/CountyFirstCase.csv' # first death cases
state_name<-'Indiana'
state_name<-paste(",",state_name)
outdir<-'out/20200511/'

PhaseOneReduction_vec<-c(0.65,0.5,0.65,0.45,0.75,0.7,0.7,0.6,0.7,0.55)
PhaseTwoReduction_vec<-PhaseOneReduction_vec-PhaseOneReduction_vec*0.25
PhaseThreeReduction_vec<-PhaseOneReduction_vec-PhaseOneReduction_vec*0.5
PhaseFourReduction_vec<-PhaseOneReduction_vec-PhaseOneReduction_vec*0.75
PhaseFiveReduction<-0.0

##################### run the model ###################

for (n in 1:10) {

  # set up district-dependent variables
  R0<-R0_vec[n]
  PhaseOneReduction<-PhaseOneReduction_vec[n]
  PhaseTwoReduction<-PhaseTwoReduction_vec[n]
  PhaseThreeReduction<-PhaseThreeReduction_vec[n]
  PhaseFourReduction<-PhaseFourReduction_vec[n]
  Rhosp<-Rhosp_vec[n]
  Rcrit<-Rcrit_vec[n]
  
  source("ModelThePlanDistricts_initialization.R")
  source("ModelThePlanDistricts_dynamics.R")
  
  # create data frame output
  D.frame1<-data.frame( Date=dates,
                       Day=day,
                       #Susceptible=S,
                       #Infectious=In,
                       Hospitalized=H+Q+G,
                       Critical=Q+G,
                       DeathsPerDay=Dday,
                       #CumulativeExposed=Ecum,
                       #CumulativeInfectious=Icum,
                       #CumulativeHospitalized=Hcum,
                       #CumulativeCritical=Ccum,
                       CumulativeDeaths=D )
  
  #---------- model zero reduction after phase one -------------#
  
  PhaseTwoReduction<-0.0
  PhaseThreeReduction<-0.0
  PhaseFourReduction<-0.0
  PhaseFiveReduction<-0.0
  
  source("ModelThePlan_initialization.R")
  source("ModelThePlan_dynamics.R")

  # create the data frame
  D.frame2<-data.frame( Date=dates,
                        Day=day,
                        #Susceptible=S,
                        #Infectious=In,
                        Hospitalized=round(H+Q+G),
                        Critical=round(Q+G),
                        DeathsPerDay=round(Dday),
                        #CumulativeExposed=Ecum,
                        #CumulativeInfectious=Icum,
                        #CumulativeHospitalized=Hcum,
                        #CumulativeCritical=Ccum,
                        CumulativeDeaths=round(D) )
  
  D.frame<-bind_rows(D.frame1,D.frame2)
  
  ##################### output ######################
  
  #-------------- prepare output table -------------#
  forecast_model<-rep("seir_isdh",maxt*4)
  forecast_assumptions<-c(rep("ISDH_1",maxt*2),rep("No_Reduction",maxt*2))
  forecast_version<-rep("2020-05-11",maxt*4)
  forecast_version_current<-rep(1,maxt*4)
  location_geography_level<-rep("district",maxt*4)
  location_name<-rep(paste("District",n),maxt*4)
  location<-rep(18,maxt*4)
  date<-rep(dates,4)
  variable<-c(rep("hospital_census_total_covid",maxt),rep("hospital_consus_icu_covid",maxt),
              rep("hospital_census_total_covid",maxt),rep("hospital_consus_icu_covid",maxt))
  forecast<-append(D.frame$Hospitalized[1:maxt],D.frame$Critical[1:maxt])
  forecast<-append(forecast,D.frame2$Hospitalized[1:maxt])
  forecast<-append(forecast,D.frame2$Critical)
  
  Output.Table<-data.frame(forecast_model,
                           forecast_assumptions,
                           forecast_version,
                           forecast_version_current,
                           location_geography_level,
                           location_name,
                           location,
                           date,
                           variable,
                           forecast)
  
  write_csv(Output.Table,path=paste(outdir,"SEIR_ISDH_District",n,"_output.csv",sep=""))
  
  
  #################### plotting #######################
  
  plt<-ggplot(D.frame1,aes(x=Date))+geom_line(aes(y=Hospitalized,color="Hospital Beds"))+
    geom_line(aes(y=Critical,color="ICU beds"))+
    geom_line(aes(y=DeathsPerDay,color="Deaths per day"))+
    geom_line(aes(y=CumulativeDeaths,color="Total Deaths"))+
    scale_x_date(limits=c(as.Date('2020-03-15'),as.Date('2020-10-15')),
                       breaks=seq.Date(as.Date('2020-03-01'),as.Date('2020-11-01'),'month'),
                 labels=c("Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov"))
  #  xlim(as.Date('2020-03-15'),as.Date('2020-10-01'))
  print(plt+ggtitle(paste("District",n,"Overview")))
  ggsave(paste(outdir,"District",n,"Overview.png",sep=""))
  
  print(plt+ggtitle(paste("District",n,'Overview'))+scale_y_log10(limits=c(1,1e4)))
  ggsave(paste(outdir,"District",n,"OverviewLog.png",sep=""))
  
  #--------------- hospitalizations and ICU by county ------------------#
  HospByRegion<-read_csv("../model_v3.0/DeathCurves/COVID_EMResources_By_Region_By_Date_Fix_0427.csv")
  
  dum<-as.Date(HospByRegion$Date,format='%m/%d/%y')
  hosp_date1<-min(dum)
  hosp_date2<-max(dum)
  
  h1<-as.numeric(hosp_date1-DayZero+1)
  h2<-as.numeric(hosp_date2-DayZero+1)
  
  dum<-HospByRegion%>%filter(Region==n)
  df.hosp<-data.frame( Dates=seq.Date(hosp_date1,hosp_date2,'days'),
                       H=dum$`Beds COVID Total` )
  #write_csv(df.hosp,path=paste(plt_outdir,'District',n,'_hosp.csv',sep=''))
  df.crit<-data.frame( Dates=seq.Date(hosp_date1,hosp_date2,'days'),
                       C=dum$`ICU Beds COVID Total`)
  #write_csv(df.crit,path=paste(plt_outdir,'District',n,'_ICU.csv',sep=''))
  
  print( ggplot()+geom_point(data=df.hosp,aes(x=Dates,y=H))+
         #  geom_line(data=df.MaxMin,aes(x=Dates,y=MaxHosp))+
        #   geom_line(data=df.MaxMin,aes(x=Dates,y=MinHosp))+
           xlim(hosp_date1-7,hosp_date2+14)+
          geom_line(data=D.frame1,aes(x=Date,y=Hospitalized))+
          #ylim(0,ylimH[n])+
          # geom_ribbon(data=df.MaxMin,aes(x=Dates,ymin=MinHosp,ymax=MaxHosp),fill="red",alpha="0.5")+
           ylab("Hospitalized")+ggtitle(n)
  )
  # ggsave(paste(plt_outdir,"district",n,"_plt12.png",sep=""))
  print( ggplot()+geom_point(data=df.crit,aes(x=Dates,y=C))+
           #geom_line(data=df.MaxMin,aes(x=Dates,y=MaxCrit))+
           #geom_line(data=df.MaxMin,aes(x=Dates,y=MinCrit))+
           #geom_ribbon(data=df.MaxMin,aes(x=Dates,ymin=MinCrit,ymax=MaxCrit),fill="red",alpha="0.5")+
           xlim(hosp_date1-7,hosp_date2+14)+#ylim(0,ylimC[n])+
           geom_line(data=D.frame1,aes(x=Date,y=Critical))+
           ylab("ICU beds")+ggtitle(n)
  )
  
  # plt1<-plt+xlim(as.Date('2020-03-01'),PhaseTwoDate)+ylim(0,2000)+ggtitle(paste("District",n,"Phase One"))
  # print(plt1)
  # ggsave(paste("District",n,"PhaseOne.png",sep=""))
  # 
  # plt2<-plt+xlim(PhaseTwoDate,PhaseThreeDate)+ylim(0,2500)+ggtitle(paste("District",n,"Phase Two"))
  # print(plt2)
  # ggsave(paste("District",n,"PhaseTwo.png",sep=""))
  # 
  # plt3<-plt+xlim(PhaseThreeDate,PhaseFourDate)+ylim(0,10e3)+ggtitle(paste("District",n,"Phase Three"))
  # print(plt3)
  # ggsave(paste("District",n,"PhaseThree.png",sep=""))
  # 
  # plt4<-plt+xlim(PhaseFourDate,PhaseFiveDate)+ggtitle(paste("District",n,"Phase Four"))
  # print(plt4)
  # ggsave(paste("District",n,"PhaseFour.png",sep=""))
  # 
  # plt5<-plt+xlim(PhaseFiveDate,as.Date('2020-10-01'))+ggtitle(paste("District",n,"Phase Five and Beyond"))
  # print(plt5)
  # ggsave(paste("District",n,"PhaseFive.png",sep=""))
  # 
}