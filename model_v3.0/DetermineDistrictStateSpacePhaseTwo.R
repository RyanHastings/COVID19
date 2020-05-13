#####################################################################
# DetermineDistrictStateSpacePhaseTwo.R
#
# Simulates an entire state space by district for Phase Two of Indiana's
# lifting of NPIs.  The plan is to lift restrictions on two dates:  4 May
# for most of Indiana, but 11 May for Lake and Marion counties, disticts
# 1 and 5 respectively.  Because the rest of the districts have only one
# lift date, they are modeled as a three-dimensional StateSpace in postlift
# reduction, Rhosp, and Rcrit.  Counties 1 and 5 are four-dimensional,
# with two postlift reductions.  The base R0 and phase one intervention
# reduction are taken from previous runs (see DistrictFit3curves.R).
######################################################################
# Ryan Hastings, 5 May 2020
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
intervention_R_rdxn_vec<-c(0.65,0.5,0.65,0.45,0.75,0.7,0.7,0.6,0.7,0.55) # phase one reduction for each district
lift_rdxn_vec<-c(seq(0.0,0.6,0.05)) # field of phase two NPI reduction rate over which to simulate
nvars<-6 # number of variables for output (you probably don't want to change this unless you want a huge coding headache)
state_name="Indiana"

# Hospitalization and death rates
Rdeath<-0.0066 # IFR (infection fatality rate)
Rhosp_vec<-seq(0.01,0.1,0.005)#0.06 # hospitalization rate
Rcrit_vec<-seq(0.01,0.03,0.001)#0.024 # critical rate

# NPI timing
intervention_time<-65#65#79 # days after day 0 phase one NPI is introduced
lift_time1_vec<-seq(105,10) # days after day 0 that second phase two NPI is introduced (4 May)
lift_time2_vec<-c(112,500,500,119,112,500,500,116,500,500) # days after day 0 that second phase two NPI is introduced (11 May)
dt=1.0 # time increment in days
DayZeroOffset<-55 # difference in days between first death in district and day zero for that district

# disease timing
Tinc<-5 # incubation time (days)
Tinf<-3 # time of infection before either hospitalization or recovery
Thosp<-8.6 # time in hospitalization until recovery
Tdeath<-10.4 # time in hospital until death
Tcrit<-7.9 # time in ICU until recovery
Pinf<-1.0 # max proportion of population to get infected

init_filename<-'../../../COVID19Response/CountyFirstCase.csv' # first death cases
outdir<-"StateSpace/" # output directory
output<-1 # produce output? 0=no, 1=yes.  Output produces is a csv with all of simulations
outfile<-'StateSpace' # beginning of state space filename

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

lift_time2<-500
lift_rdxn2<-0.0

#------------------ simulate for all districts but 1 and 5 ------------#
for (n in 7) {

  # initialize population and time variables
  Npop<-Npops[n]
  init_time<-init_t[n]

  # set up state space
  StateSpace<-array(0.0,dim=c(length(lift_rdxn_vec),length(Rhosp_vec),length(Rcrit_vec),nvars,maxt))

  for (lift_rdxn_i in 1:length(lift_rdxn_vec)) {
    for (Rhosp_vec_i in 1:length(Rhosp_vec)) {
      for (Rcrit_vec_i in 1:length(Rcrit_vec)) {
        # iterate through lift reduction, Rhosp, and Rcrit vectors
        R0<-R0_vec[n]
        intervention_R_rdxn<-intervention_R_rdxn_vec[n]
        lift_rdxn1<-lift_rdxn_vec[lift_rdxn_i]
        Rhosp<-Rhosp_vec[Rhosp_vec_i]
        Rcrit<-Rcrit_vec[Rcrit_vec_i]
        lift_time1<-lift_time1_vec[n]
        # to keep track of where we are in the simulation
        print(paste("R0=",R0,",rdxn=",lift_rdxn1,",Rcrit=",Rcrit,"Rhosp=",Rhosp,',district',n))#"intervention_time=",intervention_time,))#

        # run the model
        source("DetermineDistrictStateSpace_initialization.R")
        source("DetermineDistrictStateSpacePhaseTwo_dynamics1.R")
        source("DetermineDistrictStateSpacePhaseTwo_out1.R")
      }
    }
  }

  # save state space
  filename<-paste(outfile,'_District',n,'.Rdata',sep='')
  save(StateSpace,file=filename)

}

# #------------------- simulate for districts 1 and 5 --------------------#

# lift_time2<-112
# 
# #for (n in c(1,4,5,8)) {
# for (n in 8) { 
#  
#   # initialize some variables
#   Npop<-Npops[n]
#   init_time<-init_t[n]
#   
#   # set up state space
#   StateSpace<-array(0.0,dim=c(length(lift_rdxn_vec),length(lift_rdxn_vec),length(Rhosp_vec),length(Rcrit_vec),nvars,maxt))
#   
#   for (lift_rdxn_i in 1:length(lift_rdxn_vec)) {
#     for (lift_rdxn_j in 1:length(lift_rdxn_vec)) {
#       for (Rhosp_vec_i in 1:length(Rhosp_vec)) {
#         for (Rcrit_vec_i in 1:length(Rcrit_vec)) {            
#           # interate through lift reductions, Rhosp, and Rcrit
#           R0<-R0_vec[n]
#           intervention_R_rdxn<-intervention_R_rdxn_vec[n]
#           lift_rdxn1<-lift_rdxn_vec[lift_rdxn_i]
#           lift_rdxn2<-lift_rdxn_vec[lift_rdxn_j]
#           lift_time1<-lift_time1_vec[n]
#           lift_time2<-lift_time2_vec[n]
#           Rhosp<-Rhosp_vec[Rhosp_vec_i]
#           Rcrit<-Rcrit_vec[Rcrit_vec_i]
#           # to keep track of where we are in the simulation
#           print(paste("lift_rdxn1=",lift_rdxn1,"lift_rdxn2=",lift_rdxn2,",Rcrit=",Rcrit,
#                       "Rhosp=",Rhosp,',district',n))#"intervention_time=",intervention_time,))#
#         
#           # run the model
#           source("DetermineDistrictStateSpace_initialization.R")
#           source("DetermineDistrictStateSpacePhaseTwo_dynamics1.R")
#           source("DetermineDistrictStateSpacePhaseTwo_out2.R")
#           
#           if (D[maxt]==0) {stop()}
#         }
#       }
#     }
#   }
#   
#   # save state space
#   filename<-paste(outfile,'_District',n,'.Rdata',sep='')
#   save(StateSpace,file=filename)
# }
