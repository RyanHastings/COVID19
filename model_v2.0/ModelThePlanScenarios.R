#########################################
# ModelThePlanScenarios.R
#
# Model for the plan.  Model the governor's five-stage
# plan according to the scenarios presented in the
# document for the modeling team.
#########################################
# 11 May 2020, Ryan Hastings
#########################################

rm(list=ls()) # clear variables

# load libraries
library(tidyverse)

####### configuration variables ##########

R0<-3.0 # base transmission rate
Tinc<-5 # incubation time (days)
Tinf<-3 # time of infection before either hospitalization or recovery
Thosp<-8.6 # time in hospitalization until recovery
Tdeath<-10.4 # time in hospital until death
Tcrit<-7.9
Npop<-6.692e6 # population of the state
maxt<-350 # maximum number of time steps
Rhosp<-0.0278 # hospitalization rate
Rcrit<-0.0094 # ICU rate
Rdeath<-0.004884 # death rate

outdir<-'../../../COVID19Response/model_v2.0_out/20200521/'

DayZero<-as.Date('2020-01-20')
PhaseOneDate<-as.Date('2020-03-25')
PhaseOneReduction<-0.7

################## Scenario One ###########

PhaseTwoDate<-as.Date('2020-12-31')
PhaseTwoReduction<-0.0
PhaseThreeDate<-as.Date('2021-01-01')
PhaseThreeReduction<-0.0
PhaseFourDate<-as.Date('2021-01-02')
PhaseFourReduction<-0.0
PhaseFiveDate<-as.Date('2021-01-03')
PhaseFiveReduction<-0.0

# source("ModelThePlan_initialization.R")
# source("ModelThePlan_dynamics.R")

#--------- create the data frame ---------#

# D.frame1<-data.frame( Date=dates,
#                      Day=day,
#                      #Susceptible=S,
#                      #Infectious=In,
#                      HospitalizedUpper=round(H+Q+G),
#                      HospitalizedLower=round(H+Q+G),
#                      Hospitalized=round(H+Q+G),
#                      CriticalUpper=round(Q+G),
#                      CriticalLower=round(Q+G),
#                      Critical=round(Q+G),
#                      DeathsPerDayUpper=round(Dday),
#                      DeathsPerDayLower=round(Dday),
#                      DeathsPerDay=round(Dday),
#                      #CumulativeExposed=Ecum,
#                      #CumulativeInfectious=Icum,
#                      #CumulativeHospitalized=Hcum,
#                      #CumulativeCritical=Ccum,
#                      CumulativeDeathsUpper=round(D),
#                      CumulativeDeathsLower=round(D),
#                      CumulativeDeaths=round(D),
#                      Scenario=rep("Scenario One",maxt))
# 
# #################### Scenario Two ##########
# 
# PhaseTwoDate<-as.Date('2020-05-04')
# PhaseTwoReduction<-0.7-0.25*0.7
# 
# source("ModelThePlan_initialization.R")
# source("ModelThePlan_dynamics.R")
# 
# D.frame2<-data.frame( Date=dates,
#                       Day=day,
#                       #Susceptible=S,
#                       #Infectious=In,
#                       HospitalizedUpper=round(H+Q+G),
#                       CriticalUpper=round(Q+G),
#                       DeathsPerDayUpper=round(Dday),
#                       #CumulativeExposed=Ecum,
#                       #CumulativeInfectious=Icum,
#                       #CumulativeHospitalized=Hcum,
#                       #CumulativeCritical=Ccum,
#                       CumulativeDeathsUpper=round(D),
#                       Scenario=rep("Scenario Two",maxt))
# 
# PhaseTwoReduction<-0.7
# 
# source("ModelThePlan_initialization.R")
# source("ModelThePlan_dynamics.R")
# 
# D.frame2<-D.frame2%>%bind_cols( 
#                       #Susceptible=S,
#                       #Infectious=In,
#                       HospitalizedLower=round(H+Q+G),
#                       CriticalLower=round(Q+G),
#                       DeathsPerDayLower=round(Dday),
#                       #CumulativeExposed=Ecum,
#                       #CumulativeInfectious=Icum,
#                       #CumulativeHospitalized=Hcum,
#                       #CumulativeCritical=Ccum,
#                       CumulativeDeathsLower=round(D)
#                       )
# 
# PhaseTwoReduction<-0.6
# 
# source("ModelThePlan_initialization.R")
# source("ModelThePlan_dynamics.R")
# 
# D.frame2<-D.frame2%>%bind_cols( 
#                       #Susceptible=S,
#                       #Infectious=In,
#                       Hospitalized=round(H+Q+G),
#                       Critical=round(Q+G),
#                       DeathsPerDay=round(Dday),
#                       #CumulativeExposed=Ecum,
#                       #CumulativeInfectious=Icum,
#                       #CumulativeHospitalized=Hcum,
#                       #CumulativeCritical=Ccum,
#                       CumulativeDeaths=round(D))
# 
# 
# 
# ################## Scenario Three ############
# 
# PhaseTwoReduction<-0.7-0.25*0.7
# PhaseThreeDate<-as.Date('2020-05-22')
# PhaseThreeReduction<-0.7-0.5*0.7
# 
# source("ModelThePlan_initialization.R")
# source("ModelThePlan_dynamics.R")
# #source("ModelThePlan_out.R")
# 
# D.frame3<-data.frame( Date=dates,
#                       Day=day,
#                       #Susceptible=S,
#                       #Infectious=In,
#                       HospitalizedUpper=round(H+Q+G),
#                       CriticalUpper=round(Q+G),
#                       DeathsPerDayUpper=round(Dday),
#                       #CumulativeExposed=Ecum,
#                       #CumulativeInfectious=Icum,
#                       #CumulativeHospitalized=Hcum,
#                       #CumulativeCritical=Ccum,
#                       CumulativeDeathsUpper=round(D),
#                       Scenario=rep("Scenario Three",maxt))
# 
# 
# PhaseTwoReduction<-0.7
# PhaseThreeReduction<-0.7
# 
# source("ModelThePlan_initialization.R")
# source("ModelThePlan_dynamics.R")
# #source("ModelThePlan_out.R")
# 
# D.frame3<-D.frame3%>%bind_cols( 
#                       #Susceptible=S,
#                       #Infectious=In,
#                       HospitalizedLower=round(H+Q+G),
#                       CriticalLower=round(Q+G),
#                       DeathsPerDayLower=round(Dday),
#                       #CumulativeExposed=Ecum,
#                       #CumulativeInfectious=Icum,
#                       #CumulativeHospitalized=Hcum,
#                       #CumulativeCritical=Ccum,
#                       CumulativeDeathsLower=round(D)
#                       )
# 
# PhaseTwoReduction<-0.6
# PhaseThreeReduction<-0.5
# 
# source("ModelThePlan_initialization.R")
# source("ModelThePlan_dynamics.R")
# #source("ModelThePlan_out.R")
# 
# D.frame3<-D.frame3%>%bind_cols( 
#                       #Susceptible=S,
#                       #Infectious=In,
#                       Hospitalized=round(H+Q+G),
#                       Critical=round(Q+G),
#                       DeathsPerDay=round(Dday),
#                       #CumulativeExposed=Ecum,
#                       #CumulativeInfectious=Icum,
#                       #CumulativeHospitalized=Hcum,
#                       #CumulativeCritical=Ccum,
#                       CumulativeDeaths=round(D)
# )

################ Scenario Four ####################

PhaseTwoDate<-as.Date('2020-05-02')
PhaseTwoReduction<-0.7-0.25*0.7
PhaseThreeDate<-as.Date('2020-05-22')
PhaseThreeReduction<-0.7-0.5*0.7
PhaseFourDate<-as.Date('2020-06-14')
PhaseFourReduction<-0.7-0.75*0.7
PhaseFiveReduction<-0.0

source("ModelThePlan_initialization.R")
source("ModelThePlan_dynamics.R")
#source("ModelThePlan_out.R")

D.frame<-data.frame( Date=dates,
                      Day=day,
                      #Susceptible=S,
                      #Infectious=In,
                      HospitalizedUpper=round(H+Q+G),
                      CriticalUpper=round(Q+G),
                      DeathsPerDayUpper=round(Dday),
                      #CumulativeExposed=Ecum,
                      #CumulativeInfectious=Icum,
                      #CumulativeHospitalized=Hcum,
                      #CumulativeCritical=Ccum,
                      CumulativeDeathsUpper=round(D),
                      Scenario=rep("Scenario Three",maxt))


PhaseTwoReduction<-0.7
PhaseThreeReduction<-0.7
PhaseFourReduction<-0.7

source("ModelThePlan_initialization.R")
source("ModelThePlan_dynamics.R")
#source("ModelThePlan_out.R")

D.frame<-D.frame%>%bind_cols( 
  #Susceptible=S,
  #Infectious=In,
  HospitalizedLower=round(H+Q+G),
  CriticalLower=round(Q+G),
  DeathsPerDayLower=round(Dday),
  #CumulativeExposed=Ecum,
  #CumulativeInfectious=Icum,
  #CumulativeHospitalized=Hcum,
  #CumulativeCritical=Ccum,
  CumulativeDeathsLower=round(D)
)

PhaseTwoReduction<-0.6
PhaseThreeReduction<-0.5
PhaseFourReduction<-0.4

source("ModelThePlan_initialization.R")
source("ModelThePlan_dynamics.R")
#source("ModelThePlan_out.R")

D.frame<-D.frame%>%bind_cols( 
  #Susceptible=S,
  #Infectious=In,
  Hospitalized=round(H+Q+G),
  Critical=round(Q+G),
  DeathsPerDay=round(Dday),
  #CumulativeExposed=Ecum,
  #CumulativeInfectious=Icum,
  #CumulativeHospitalized=Hcum,
  #CumulativeCritical=Ccum,
  CumulativeDeaths=round(D)
)



################################################

#D.frame<-bind_rows(D.frame1,D.frame2,D.frame3)

################# output #####################

# #--------- prepare output table -------------#
# forecast_model<-rep("seir_isdh",maxt*6)
# forecast_assumptions<-c(rep("Scenario 1",maxt*2),rep("Scenario 2",maxt*2),rep("Scenario 3",maxt*2))
# forecast_version<-rep("2020-05-11",maxt*6)
# forecast_version_current<-rep(1,maxt*6)
# location_geography_level<-rep("state",maxt*6)
# location_name<-rep("Indiana",maxt*6)
# location_id<-rep(18,maxt*6)
# date<-rep(dates,6)
# variable<-c(rep("hospital_census_total_covid",maxt),rep("hospital_census_icu_covid",maxt),
#             rep("hospital_census_total_covid",maxt),rep("hospital_census_icu_covid",maxt),
#             rep("hospital_census_total_covid",maxt),rep("hospital_census_icu_covid",maxt))
# forecast<-append(D.frame1$Hospitalized[1:maxt],D.frame1$Critical[1:maxt])
# forecast<-append(forecast,D.frame2$Hospitalized[1:maxt])
# forecast<-append(forecast,D.frame2$Critical)
# forecast<-append(forecast,D.frame3$Hospitalized)
# forecast<-append(forecast,D.frame3$Critical)
# 
# forecast_lower<-append(D.frame1$HospitalizedLower,D.frame1$CriticalLower)
# forecast_lower<-append(forecast_lower,D.frame2$HospitalizedLower)
# forecast_lower<-append(forecast_lower,D.frame2$CriticalLower)
# forecast_lower<-append(forecast_lower,D.frame3$HospitalizedLower)
# forecast_lower<-append(forecast_lower,D.frame3$CriticalLower)
# 
# forecast_upper<-append(D.frame1$HospitalizedUpper,D.frame1$CriticalUpper)
# forecast_upper<-append(forecast_upper,D.frame2$HospitalizedUpper)
# forecast_upper<-append(forecast_upper,D.frame2$CriticalUpper)
# forecast_upper<-append(forecast_upper,D.frame3$HospitalizedUpper)
# forecast_upper<-append(forecast_upper,D.frame3$CriticalUpper)
# 
# Output.Table<-data.frame(forecast_model,
#                          forecast_assumptions,
#                          forecast_version,
#                          forecast_version_current,
#                          location_geography_level,
#                          location_name,
#                          location_id,
#                          date,
#                          variable,
#                          forecast,
#                          forecast_lower,
#                          forecast_upper)
# write_csv(Output.Table,path=paste(outdir,"SEIR_ISDH_output.csv",sep=""))

#--------- prepare output table -------------#
forecast_model<-rep("seir_isdh",maxt*2)
forecast_assumptions<-rep("Scenario 4",maxt*2)
forecast_version<-rep("2020-05-21",maxt*2)
forecast_version_current<-rep(1,maxt*2)
location_geography_level<-rep("state",maxt*2)
location_name<-rep("Indiana",maxt*2)
location_id<-rep(18,maxt*2)
date<-rep(dates,2)
variable<-c(rep("hospital_census_total_covid",maxt),rep("hospital_census_icu_covid",maxt))#,
           # rep("hospital_census_total_covid",maxt),rep("hospital_census_icu_covid",maxt),
           # rep("hospital_census_total_covid",maxt),rep("hospital_census_icu_covid",maxt))
forecast<-append(D.frame$Hospitalized[1:maxt],D.frame$Critical[1:maxt])
# forecast<-append(forecast,D.frame2$Hospitalized[1:maxt])
# forecast<-append(forecast,D.frame2$Critical)
# forecast<-append(forecast,D.frame3$Hospitalized)
# forecast<-append(forecast,D.frame3$Critical)

forecast_lower<-append(D.frame$HospitalizedLower,D.frame$CriticalLower)
# forecast_lower<-append(forecast_lower,D.frame2$HospitalizedLower)
# forecast_lower<-append(forecast_lower,D.frame2$CriticalLower)
# forecast_lower<-append(forecast_lower,D.frame3$HospitalizedLower)
# forecast_lower<-append(forecast_lower,D.frame3$CriticalLower)

forecast_upper<-append(D.frame$HospitalizedUpper,D.frame$CriticalUpper)
# forecast_upper<-append(forecast_upper,D.frame2$HospitalizedUpper)
# forecast_upper<-append(forecast_upper,D.frame2$CriticalUpper)
# forecast_upper<-append(forecast_upper,D.frame3$HospitalizedUpper)
# forecast_upper<-append(forecast_upper,D.frame3$CriticalUpper)

Output.Table<-data.frame(forecast_model,
                         forecast_assumptions,
                         forecast_version,
                         forecast_version_current,
                         location_geography_level,
                         location_name,
                         location_id,
                         date,
                         variable,
                         forecast,
                         forecast_lower,
                         forecast_upper)
write_csv(Output.Table,path=paste(outdir,"SEIR_ISDH_output.csv",sep=""))


############ plotting ########################

# print( ggplot(D.frame1,aes(x=Date))+geom_line(aes(y=Hospitalized))+
#          geom_ribbon(aes(ymin=HospitalizedLower,ymax=HospitalizedUpper,alpha=0.5))+
#          ggtitle("Scenario 1"))
# print( ggplot(D.frame1,aes(x=Date))+geom_line(aes(y=Critical))+
#          geom_ribbon(aes(ymin=CriticalLower,ymax=CriticalUpper,alpha=0.5))+
#          ggtitle("Scenario 1"))
# print( ggplot(D.frame2,aes(x=Date))+geom_line(aes(y=Hospitalized))+
#          geom_ribbon(aes(ymin=HospitalizedLower,ymax=HospitalizedUpper,alpha=0.5))+
#          ggtitle("Scenario 2"))
# print( ggplot(D.frame2,aes(x=Date))+geom_line(aes(y=Critical))+
#          geom_ribbon(aes(ymin=CriticalLower,ymax=CriticalUpper,alpha=0.5))+
#          ggtitle("Scenario 2"))
print( ggplot(D.frame,aes(x=Date))+geom_line(aes(y=Hospitalized))+
                                        geom_ribbon(aes(ymin=HospitalizedLower,ymax=HospitalizedUpper,alpha=0.5))+
                                        ggtitle("Scenario 4"))
print( ggplot(D.frame,aes(x=Date))+geom_line(aes(y=Critical))+
         geom_ribbon(aes(ymin=CriticalLower,ymax=CriticalUpper,alpha=0.5))+
         ggtitle("Scenario 4"))

# 
# # plt<-ggplot(D.frame,aes(x=Date))+geom_ribbon(aes(ymin=Hlow,ymax=Hospitalized,color="Hospital beds"),alpha="0.5",fill="red")+
# #   geom_ribbon(aes(ymin=Clow,ymax=Critical,color="ICU  beds"),alpha="0.5",fill="blue")
# # print(plt)
# plt<-ggplot(D.frame,aes(x=Date))+geom_line(aes(y=Hospitalized,color="Hospital Beds"))+
#   geom_line(aes(y=Critical,color="ICU beds"))+
#   geom_line(aes(y=DeathsPerDay,color="Deaths per day"))+
#   geom_line(aes(y=CumulativeDeaths,color="Total Deaths"))
# print(plt+ggtitle("Overview"))
# ggsave(paste(outdir,"StatewideOverview.png",sep=""))
# 
# print(plt+ggtitle('Overview')+scale_y_log10(limits=c(1,1e5)))
# ggsave(paste(outdir,"StatewideOverviewLog.png",sep=""))
# 
# plt1<-plt+xlim(as.Date('2020-03-01'),PhaseTwoDate)+ylim(0,2000)+ggtitle("Phase One")
# print(plt1)
# ggsave(paste(outdir,"StatewidePhaseOne.png",sep=""))
# 
# plt2<-plt+xlim(PhaseTwoDate,PhaseThreeDate)+ylim(0,2500)+ggtitle("Phase Two")
# print(plt2)
# ggsave(paste(outdir,"StatewidePhaseTwo.png",sep=""))
# 
# plt3<-plt+xlim(PhaseThreeDate,PhaseFourDate)+ylim(0,10e3)+ggtitle("Phase Three")
# print(plt3)
# ggsave(paste(outdir,"StatewidePhaseThree.png",sep=""))
# 
# plt4<-plt+xlim(PhaseFourDate,PhaseFiveDate)+ggtitle("Phase Four")
# print(plt4)
# ggsave(paste(outdir,"StatewidePhaseFour.png",sep=""))
# 
# plt5<-plt+xlim(PhaseFiveDate,as.Date('2020-10-01'))+ggtitle("Phase Five and Beyond")
# print(plt5)
# ggsave(paste(outdir,"StatewidePhaseFive.png",sep=""))
