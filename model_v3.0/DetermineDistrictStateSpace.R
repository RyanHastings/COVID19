###################################################################
# DetermineDistrictStateSpace.R
#
# Create a state space containing various values of R0, intervention
# reduction, hospitalization and critical rates, for the purpose of
# fitting it to observed data.  This simulates one state space for
# each Indiana public health district.
###################################################################
# Ryan Hastings, 5 May 2020
###################################################################

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
R0_vec<-c(seq(2.0,3.0,0.05)) # field of R0 over which to simulate
intervention_R_rdxn_vec<-c(seq(0.1,0.8,0.05)) # field of NPI reduction rate over which to simulate
ndistricts<-10 # number of health districts
nvars<-13 # number of variables for output (you probably don't want to change this unless you want a huge coding headache)
state_name="Indiana"

# hospitalization and death rates
Rdeath<-0.0066 # IFR (infection fatality rate)
Rhosp_vec<-seq(0.01,0.1,0.005)#seq(0.01,0.09,0.005)#0.06 # hospitalization rate
Rcrit_vec<-seq(0.01,0.03,0.001)#0.024 # critical rate

# NPI timing
intervention_time<-65#65#79 # days after day 0 NPI is introduced
lift_time1<-500 # days after day 0 that NPI is ceased
dt=1.0 # time step in days
DayZeroOffset<-55 # difference in days between district day zero and first death

# disease timing
Tinc<-5 # incubation time (days)
Tinf<-3 # time of infection before either hospitalization or recovery
Thosp<-8.6 # time in hospitalization until recovery
Tdeath<-10.4 # time in hospital until death
Tcrit<-7.9 # time in ICU before recovery
Pinf<-1.0 # max proportion of population to get infected

init_filename<-'../../../COVID19Response/CountyFirstCase.csv' # file with first cases for each county
outdir<-"StateSpace/" # output directory
output<-1 # produce output? 0=no, 1=yes.  Output produces is a csv with all of
outfile<-'StateSpacePhaseOne' # output file beginning

#######################################################################
# initialization of variables

# find state and county populations
US.pop.raw<-read_csv('../PEP_2018_PEPAGESEX/PEP_2018_PEPAGESEX_with_ann.csv')
state_name<-paste(",",state_name)
IN.pop.raw<-US.pop.raw%>%filter(grepl(state_name,`GEO.display-label`))
county_names<-IN.pop.raw$`GEO.display-label`
county_names<-append(county_names,state_name)
fips<-IN.pop.raw$GEO.id2
county_pops<-IN.pop.raw$est42010sex0_age999

# match counties to districts
DistrictCounty.crosswalk<-read_csv("../CountyDistrict.csv")

# determine district populations based on county populations
Npops<-rep(0.0,ndistricts)

for (i in 1:92) {
  dum<-DistrictCounty.crosswalk%>%filter(FIPS==fips[i])
  ndistrict<-dum$DISTRICT
  Npops[ndistrict]<-Npops[ndistrict]+as.numeric(county_pops[i])
}

# find first death, substract 55

In<-array(0.0,dim=c(ndistricts,maxt))
init_t<-rep(300,ndistricts)
init_date<-rep(as.Date('2020-12-31'),ndistricts)
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

for (i in 1:ndistricts) {
  init_t[i]<-as.numeric(init_date-as.Date('2020-01-20'))+1
}

#######################################################################
########################## RUN THE MODEL ##############################
times<-c(seq(0,maxt))

for (n in 7) {
  
  # initialize population and time
  Npop<-Npops[n]
  init_time<-init_t[n]

  # set up empty state space
  StateSpace<-array(0.0,dim=c(length(R0_vec),length(intervention_R_rdxn_vec),length(Rhosp_vec),length(Rcrit_vec),nvars,maxt))
  
  # loop through models
  for (R0i in 1:length(R0_vec)) {
    for (intervention_R_rdxn_i in 1:length(intervention_R_rdxn_vec)) {
      for (Rhosp_vec_i in 1:length(Rhosp_vec)) {
        for (Rcrit_vec_i in 1:length(Rcrit_vec)) {            
          # iterate through R0, intervention_R_rdxn vectors, Rhosp and Rcrit
          R0<-R0_vec[R0i]
          intervention_R_rdxn<-intervention_R_rdxn_vec[intervention_R_rdxn_i]
          Rhosp<-Rhosp_vec[Rhosp_vec_i]
          Rcrit<-Rcrit_vec[Rcrit_vec_i]
          # to keep track of where we are in the simulation
          print(paste("R0=",R0,",rdxn=",intervention_R_rdxn,",Rcrit=",Rcrit,"Rhosp=",Rhosp,',district',n))#"intervention_time=",intervention_time,))#
          
          # run the model
          source("DetermineDistrictStateSpace_initialization.R")
          source("DetermineDistrictStateSpace_dynamics.R")
          source("DetermineDistrictStateSpace_out.R")
        }
      }
    }
  }
  
  # save state space
  filename<-paste(outfile,'_District',n,'.Rdata',sep='')
  save(StateSpace,file=filename)
}

# save as Rdata file
