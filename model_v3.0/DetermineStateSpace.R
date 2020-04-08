######################################################################
# DetermineStateSpace.R
#
# Creates a state space over R0 and intervention reduction percentage.
######################################################################
# Ryan Hastings, 8 Apr 2020
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
R0_vec<-c(seq(2.0,3.0,0.05)) # field of R0 over which to simulate
intervention_R_rdxn_vec<-c(seq(0.0,0.95,0.05)) # field of NPI reduction rate over which to simulate
nvars<-12 # number of variables for output (you probably don't want to change this unless you want a huge coding headache)
Npop<-6.692e6 # population of state

Rdeath<-0.0066 # IFR (infection fatality rate)
Rhosp<-0.06 # hospitalization rate
Rcrit<-0.024 # critical rate

# NPI timing
intervention_time<-65#79 # days after day 0 NPI is introduced
lift_time<-500 # days after day 0 that NPI is ceased

Tinc<-5 # incubation time (days)
Tinf<-3 # time of infection before either hospitalization or recovery
Thosp<-23 # time in hospitalization until recovery
Tdeath<-18 # time in hospital until death
Tcrit<-23
Pinf<-1.0 # max proportion of population to get infected

outdir<-"StateSpace/"
output<-1 # produce output? 0=no, 1=yes.  Output produces is a csv with all of
outfile<-'StateSpace_day65.RData'

#######################################################################
########################## RUN THE MODEL ##############################
times<-c(seq(0,maxt))
StateSpace<-array(0.0,dim=c(length(R0_vec),length(intervention_R_rdxn_vec),nvars,maxt))

for (R0i in 1:length(R0_vec)) {
  for (intervention_R_rdxn_i in 1:length(intervention_R_rdxn_vec)) {
    
    # interate through R0 and intervention_R_rdxn vectors
    R0<-R0_vec[R0i]
    intervention_R_rdxn<-intervention_R_rdxn_vec[intervention_R_rdxn_i]
    
    # to keep track of where we are in the simulation
    print(paste("R0=",R0,",rdxn=",intervention_R_rdxn))
    
    source("DetermineStateSpace_initalization.R")
    source("DetermineStateSpace_dynamics.R")
    source("DetermineStateSpace_out.R")
  }
}

# save as Rdata file
save(StateSpace,file=outfile)