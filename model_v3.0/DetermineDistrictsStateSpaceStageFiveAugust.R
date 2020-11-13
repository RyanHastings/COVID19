#####################################################################
# DetermineDistrictStateSpaceStageThree.R
#
#-------- rewrite this-----we're doing four-dimensional fits on all of them
# Simulates an entire state space by district for Stage Three of Indiana's
# lifting of NPIs.  The plan is to lift restrictions on two dates:  22 May
# for most of Indiana, but 1 Jun for Lake and Marion and Cass counties, disticts
# 1 and 5 and 8 respectively.  Because the rest of the districts have only one
# lift date, they are modeled as a three-dimensional StateSpace in postlift
# reduction, Rhosp, and Rcrit.  Counties 1 and 5 and 8 are four-dimensional,
# with two postlift reductions.  The base R0 and phase one intervention
# reduction are taken from previous runs (see DistrictFit3curves.R).
######################################################################
# Ryan Hastings, 23 May 2020 / 12 June 2020
######################################################################
rm(list=ls()) # clear out variables

# this would be just library(tidyverse) but for some reason that
# doesn't work on my work machine
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(haven)
library(readxl)

#######################################################################
##################### CONFIGURATION VARIABLES #########################
maxt=300 # maximum time in days
R0_vec<-c(2.8,2.35,2.6,2.05,3.35,2.8,2.25,2.3,2.75,2.2) # R0s for each district
StageOneReduction_vec<-c(0.65,0.5,0.65,0.45,0.75,0.7,0.7,0.6,0.7,0.55) # phase one reduction for each district
StageTwoReductionA_vec<-c(0.65,0.65,0.65,0.5,0.65,0.7,0.45,0.6,0.7,0.55)
StageTwoReductionB_vec<-c(0.7,0.65,0.65,0.35,0.7,0.7,0.45,0.6,0.7,0.55)
StageThreeReduction_vec<-c(0.7,0.7,0.55,0.65,0.7,0.65,0.65,0.45,0.7,0.55)
StageFourReduction_vec<-c(0.6,0.5,0.6,0.35,0.7,0.65,0.4,0.7,0.7,0.7)
JulyReduction_vec<-c(0.7,0.5,0.65,0.55,0.7,0.55,0.5,0.45,0.45,0.2)
lift_rdxn_vec<-seq(0.0,0.7,0.05) # field of phase two NPI reduction rate over which to simulate
stage5_weeks_vec<-seq(0,14)
nvars<-10 # number of variables for output (you probably don't want to change this unless you want a huge coding headache)
state_name="Indiana"
DayZero<-as.Date("2020-01-20")

# Hospitalization and death rates
Rdeath<-0.0066 # IFR (infection fatality rate)
RhospStageTwo<-c(0.05,0.05,0.05,0.1,0.025,0.03,0.08,0.08,0.025,0.095)
RcritStageTwo<-c(0.015,0.012,0.017,0.026,0.012,0.01,0.026,0.024,0.01,0.03)
RhospStageThree<-c(0.055,0.06,0.075,0.055,0.03,0.065,0.095,0.045,0.035,0.06)
RcritStageThree<-c(0.018,0.017,0.028,0.018,0.011,0.015,0.025,0.013,0.01,0.03)
RhospStageFour<-c(0.04,0.095,0.055,0.085,0.025,0.04,0.06,0.035,0.03,0.04)
RcritStageFour<-c(0.013,0.03,0.019,0.024,0.01,0.01,0.03,0.01,0.01,0.01)
RhospJuly<-c(0.07,0.07,0.05,0.08,0.04,0.05,0.05,0.1,0.06,0.09)
RcritJuly<-c(0.021,0.028,0.017,0.018,0.016,0.014,0.03,0.023,0.023,0.03)
Rhosp_vec<-seq(0.01,0.1,0.01)#seq(0.01,0.02,0.005)#0.06 # hospitalization rate
Rcrit_vec<-seq(0.01,0.04,0.001)#seq(0.01,0.03,0.001)#0.024 # critical rate

# NPI timing
intervention_time<-65#65#79 # days after day 0 phase one NPI is introduced
lift_time1_vec<-rep(105,10) # days after day 0 that second phase two NPI is introduced (4 May)
lift_time2_vec<-c(112,107,107,119,112,107,107,116,107,107) # days after day 0 that second phase two NPI is introduced (11 May)
Stage3ADays<-rep(123,10)
Stage3BDays<-c(133,125,125,133,133,125,125,125,125,125)
dt=1.0 # time increment in days
DayZeroOffset<-55 # difference in days between first death in district and day zero for that district

StageFourDate<-as.Date("2020-06-12")
StageFourDay<-as.numeric(StageFourDate-DayZero)
StageFiveDate<-as.Date("2020-07-04")
StageFiveDay<-as.numeric(StageFiveDate-DayZero)
AugustDate<-as.Date("2020-08-01")
AugustDay<-as.numeric(AugustDate-DayZero)
SeptemberDate<-as.Date("2020-09-01")
SeptemberDay<-as.numeric(SeptemberDate-DayZero)
# disease timing
Tinc<-5 # incubation time (days)
Tinf<-3 # time of infection before either hospitalization or recovery
Thosp<-8.6 # time in hospitalization until recovery
Tdeath<-10.4 # time in hospital until death
Tcrit<-7.9 # time in ICU until recovery
Pinf<-1.0 # max proportion of population to get infected

init_filename<-'../../../COVID19Response/CountyFirstCase.csv' # first death cases
outdir<-"../../../COVID19Response/model_v3.0_out/StateSpace/" # output directory
output<-1 # produce output? 0=no, 1=yes.  Output produces is a csv with all of simulations
outfile<-'StateSpace_August' # beginning of state space filename

#tageThreeReductionA_vec<-seq(0.0,0.7,0.05)
#StageThreeReductionB_vec<-StageThreeReductionA_vec
#StageThreeReduction_vec<-StageThreeReductionA_vec
#StageFourReduction_vec<-StageThreeReduction_vec

#######################################################################
## initialization

# get county and state populations
US.pop.raw<-read_csv('../PEP_2018_PEPAGESEX/PEP_2018_PEPAGESEX_with_ann.csv')
state_name<-paste(",",state_name)
IN.pop.raw<-US.pop.raw%>%filter(grepl(state_name,`GEO.display-label`))
county_names<-IN.pop.raw$`GEO.display-label`
county_names<-append(county_names,state_name)
fips<-IN.pop.raw$GEO.id2
county_pops<-IN.pop.raw$est42010sex0_age999

# crosswalk districts and counties
DistrictCounty.crosswalk<-read_csv("../CountyDistrict.csv")

# get district populations
Npops<-rep(0.0,10)
for (i in 1:92) {
  dum<-DistrictCounty.crosswalk%>%filter(FIPS==fips[i])
  ndistrict<-dum$DISTRICT
  Npops[ndistrict]<-Npops[ndistrict]+as.numeric(county_pops[i])
}

# find first death, substract 55\
In<-array(0.0,dim=c(10,maxt))
init_t<-rep(300,10)
init_date<-rep(as.Date('2020-12-31'),10)

init_data<-read_csv(init_filename,col_types=cols(
  FIPS=col_integer(),
  County=col_character(),
  DateFirstConfirmed=col_date(format="%m/%d/%Y"),
  DateFirstDeath=col_date(format="%m/%d/%Y"),
  X5=col_character()
))
null_start_date<-Sys.Date()
init_data$DateFirstDeath[is.na(init_data$DateFirstDeath)]<-null_start_date

for (i in 1:92) {
  dum<-DistrictCounty.crosswalk%>%filter(FIPS==init_data$FIPS[i])
  ndistrict<-dum$DISTRICT
  if (init_date[ndistrict]>init_data$DateFirstDeath[i]-DayZeroOffset) {
    init_date[ndistrict]<-init_data$DateFirstDeath[i]-DayZeroOffset
  }
}
for (i in 1:10) {
  init_t[i]<-as.numeric(init_date-as.Date('2020-01-20'))+1
}


#######################################################################
########################## RUN THE MODEL ##############################

times<-c(seq(0,maxt))

#------------------ simulate for all districts  1 and 5 ------------#
#for (n in c(1,4,5)) {
for (n in 1:10) {
  # initialize population and time variables
  Npop<-Npops[n]
  init_time<-init_t[n]
  
  StageOneReduction<-StageOneReduction_vec[n]
  StageTwoReductionA<-StageTwoReductionA_vec[n]
  StageTwoReductionB<-StageTwoReductionB_vec[n]
  StageThreeReductionA<-StageThreeReduction_vec[n]
  StageThreeReductionB<-StageThreeReduction_vec[n]
  StageFourReduction<-StageFourReduction_vec[n]
  JulyReduction<-JulyReduction_vec[n]
  # dum1<-StageTwoReductionA-StageTwoReductionA*0.5
  # dum2<-StageTwoReductionA
  # StageThreeReductionA_vec<-seq(dum1,dum2,0.05)
  R0<-R0_vec[n]
  
  lift_time1<-lift_time1_vec[n]
  lift_time2<-lift_time2_vec[n]
  
  
  # set up state space
  # StateSpace<-array(0.0,dim=c(length(lift_rdxn_vec),
  #                             length(stage5_weeks_vec),
  #                             length(Rhosp_vec),length(Rcrit_vec),nvars,maxt))
  
  StateSpace<-array(0.0,dim=c(length(Rcrit_vec),
                              length(Rhosp_vec),
                              length(stage5_weeks_vec),
                              length(lift_rdxn_vec),nvars,maxt))
  
  for (lift_rdxn_i in 1:length(lift_rdxn_vec)) {
    for (weeks_j in 1:length(stage5_weeks_vec)) {
      
      for (Rhosp_vec_i in 1:length(Rhosp_vec)) {
        for (Rcrit_vec_i in 1:length(Rcrit_vec)) {
          # iterate through lift reduction, Rhosp, and Rcrit vectors
          
          AugReduction<-lift_rdxn_vec[lift_rdxn_i]
          RhospAugust<-Rhosp_vec[Rhosp_vec_i]
          RcritAugust<-Rcrit_vec[Rcrit_vec_i]
          stage5_weeks<-stage5_weeks_vec[weeks_j]
          
          # StageFourReduction<-0.5*StageThreeReductionB
          
          # to keep track of where we are in the simulation
          print(paste("StageFourReduction=",AugReduction,"weeks=",stage5_weeks,"Rcrit=",
                      RcritAugust,"Rhosp=",RhospAugust,',district',n))#"intervention_time=",intervention_time,))#
          
          # run the model
          source("DetermineDistrictStateSpace_initialization.R")
          source("DetermineDistrictsStateSpaceStageFiveAugust_dynamics.R")
          if (is.na(Rhosp)) { stop() }
          source("DetermineDistrictsStateSpaceStageFiveAugust_out.R")
        }
      }
    }
  }
  
  # save state space
  filename<-paste(outfile,'_District',n,'.Rdata',sep='')
  save(StateSpace,file=filename)
  
}
