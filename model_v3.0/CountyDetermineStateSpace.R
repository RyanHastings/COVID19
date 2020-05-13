######################################################################
# CountyDetermineStateSpace.R
#
# A bit mistitled.  Computes and saves the entire state space for
# each Indiana public health district for Phase One, that is, no lifting
# of NPIs.
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
R0_vec<-c(seq(2.0,3.5,0.05)) # field of R0 over which to simulate
intervention_R_rdxn_vec<-c(seq(0.0,0.95,0.05)) # field of NPI reduction rate over which to simulate
ndistricts<-10 # number of health districts
nvars<-13 # number of variables for output
state_name="Indiana"

# Death and hospitalization rates
Rdeath<-0.0066 # IFR (infection fatality rate)
Rhosp_vec<-seq(0.01,0.09,0.005) # hospitalization rate
Rcrit_vec<-seq(0.01,0.03,0.001) # critical rate

# NPI timing
intervention_time<-65 # days after day 0 NPI is introduced
lift_time<-500 # days after day 0 that NPI is ceased
dt=1.0 # time increment in days
DayZeroOffset<-55 # number of days between day zero for that district and first death
DayZero<-as.Date("2020-01-20") # Earliest day zero

Tinc<-5 # incubation time (days)
Tinf<-3# # time of infection before either hospitalization or recovery
Thosp<-8.6 # time in hospitalization until recovery
Tdeath<-10.4 # time in hospital until death
Tcrit<-7.9 # time in ICU until recovery
Pinf<-1.0 # max proportion of population to get infected

init_filename<-'../../../COVID19Response/CountyFirstCase.csv' # file with county/district crosswalk
outdir<-"StateSpace/" # output directory
output<-1 # produce output? 0=no, 1=yes.  Output produces is a csv with all of
outfile<-'StateSpace' # first part of name for output files

#######################################################################
# execution part

# read in state and county populations
US.pop.raw<-read_csv('../PEP_2018_PEPAGESEX/PEP_2018_PEPAGESEX_with_ann.csv')
state_name<-paste(",",state_name)
IN.pop.raw<-US.pop.raw%>%filter(grepl(state_name,`GEO.display-label`))
county_names<-IN.pop.raw$`GEO.display-label`
county_names<-append(county_names,state_name)
fips<-IN.pop.raw$GEO.id2
county_pops<-IN.pop.raw$est42010sex0_age999

# for initialiaztion, find first death, substract 55
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

init_date<-init_data$DateFirstDeath-DayZeroOffset
init_t<-as.numeric(init_date-DayZero)+1

# initialize times array
times<-c(seq(0,maxt))

#-------------------- run the state space -------------------------#
for (n in 10:92) {
  
  # some initialization of population and time
  Npop<-as.numeric(county_pops[n])
  init_time<-init_t[n]
  
  # set up state space
  StateSpace<-array(0.0,dim=c(length(R0_vec),length(intervention_R_rdxn_vec),length(Rhosp_vec),length(Rcrit_vec),nvars,maxt))
  
  # loop through model
  for (R0i in 1:length(R0_vec)) {
    for (intervention_R_rdxn_i in 1:length(intervention_R_rdxn_vec)) {
      for (Rhosp_vec_i in 1:length(Rhosp_vec)) {
        for (Rcrit_vec_i in 1:length(Rcrit_vec)) {            
          
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
  
  # output the results
  filename<-paste(outdir,outfile,'_County',n,'.Rdata',sep='')
  save(StateSpace,file=filename)
}
