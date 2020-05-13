##############################################################
# DetermineRcritStateSpace.R
#
# Depracated by Fit3curves.R.  This originally generated a state
# space holding R0 constant (determined by fitting the death curve)
# and varying intervention_R_rdxn and Rcrit.  Intended to be fitted
# to the ICU curve.
##############################################################
# Ryan Hastings, 5 May 2020
##############################################################

rm(list=ls()) # clear out variables

# this would be just library(tidyverse) but for some reason that
# doesn't work on my machine
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(haven)
library(readxl)

###############################################################
maxt=300 # maximum number of time steps
R0=2.95 # base transmission rate
Rcrit_vec<-seq(0.01,0.03,0.001) # field of Rcrit over which to simulate
intervention_R_rdxn_vec<-c(seq(0.0,0.95,0.05)) # field of intervention_R_rdxn over which to simulate
times<-c(seq(0,maxt)) # set up times array
nvars<-12 # number of variables in output
Npop<-6.692e6 # state population

# set up state space
StateSpace<-array(0.0,dim=c(length(Rcrit_vec),length(intervention_R_rdxn_vec),nvars,maxt))

# loop and run model
for (Rcriti in 1:length(Rcrit_vec)) {
  for (intervention_R_rdxn_i in 1:length(intervention_R_rdxn_vec)) {
    
    Rcrit<-Rcrit_vec[Rcriti]
    intervention_R_rdxn<-intervention_R_rdxn_vec[intervention_R_rdxn_i]
    
    Rdeath<-0.0066
    Rhosp<-0.03
    print(paste("Rcrit=",Rcrit,",rdxn=",intervention_R_rdxn))
    outdir<-"StateSpace/"
    
    # NPI timing
    intervention_time<-65#79 # days after day 0
    lift_time<-500 # days after day 0 that NPI is ceased
    
    # first case initialization method
    Tinc<-5 # incubation time (days)
    Tinf<-3 # time of infection before either hospitalization or recovery
    Thosp<-23 # time in hospitalization until recovery
    Tdeath<-18 # time in hospital until death
    Tcrit<-23
    Pinf<-1.0 # max proportion of population to get infected
    
    output<-1 # produce output? 0=no, 1=yes.  Output produces is a csv with all of
    
    # run the model
    source("DetermineStateSpace_initalization.R")
    source("DetermineStateSpace_dynamics.R")
    source("StateSpaceCrit_out.R")
  }
}

save(StateSpace,file='StateSpace_Crit.RData')