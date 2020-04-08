# fitting critical curve

# first generate a series of curves keeping R0 constant but varying intervention_R_rdxn and Rcrit

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

maxt=300
R0=2.8
Rcrit_vec<-seq(0.01,0.03,0.001)
#R0_vec<-c(seq(2.85,2.95,0.005))
intervention_R_rdxn_vec<-c(seq(0.0,0.95,0.05))
times<-c(seq(0,maxt))
nvars<-12
Npop<-6.692e6


StateSpace<-array(0.0,dim=c(length(Rcrit_vec),length(intervention_R_rdxn_vec),nvars,maxt))

for (Rcriti in 1:length(Rcrit_vec)) {
  for (intervention_R_rdxn_i in 1:length(intervention_R_rdxn_vec)) {
    
    Rcrit<-Rcrit_vec[Rcriti]
    intervention_R_rdxn<-intervention_R_rdxn_vec[intervention_R_rdxn_i]
    
    Rdeath<-0.0066
    Rhosp<-0.03
#    Rcrit<-0.013
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
    
    source("DetermineStateSpace_initalization.R")
    source("DetermineStateSpace_dynamics.R")
    source("StateSpaceCrit_out.R")
  }
}

save(StateSpace,file='StateSpace_Crit.RData')

