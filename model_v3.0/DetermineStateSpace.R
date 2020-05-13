######################################################################
# DetermineStateSpace.R
#
# Originally creates a state space over R0, intervention reduction percentage,
# hospitalization rate, and critical rate, for statewide simulation.
# The state space is subsequently used in curve fitting (Fit3curves.R)
######################################################################
# Ryan Hastings, 8 Apr 2020
######################################################################
# Modified to create state space over field of two different intervention
# reductions for Phase Two of Indiana NPIs.
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
#R0_vec<-c(seq(2.5,3.5,0.05)) # field of R0 over which to simulate
intervention_R_rdxn_vec<-c(seq(0.0,0.7,0.05)) # field of NPI reduction rate over which to simulate
nvars<-4#13 # number of variables for output (you probably don't want to change this unless you want a huge coding headache)
Npop<-6.692e6 # population of state
R0<-3.0
intervention_R_rdxn<-0.7

Rdeath<-0.0066 # IFR (infection fatality rate)
Rhosp_vec<-seq(0.025,0.09,0.005)#0.06 # hospitalization rate
Rcrit_vec<-seq(0.01,0.04,0.001)#0.024 # critical rate

# NPI timing
intervention_time<-65#65#79 # days after day 0 NPI is introduced
lift_time1<-103 # days after day 0 that NPI is modified in most counties (4 May)
lift_time2<-112 # days after day 0 that NPI is modified in Marion and Lake counties (11 May)
dt=1.0 # time step (days)

# disease timing
Tinc<-5 # incubation time (days)
Tinf<-3 # time of infection before either hospitalization or recovery
Thosp<-8.6 # time in hospitalization until recovery
Tdeath<-10.4 # time in hospital until death
Tcrit<-7.9 # time in ICU until recovery
Pinf<-1.0 # max proportion of population to get infected

outdir<-"StateSpace/" # directory for output
output<-1 # produce output? 0=no, 1=yes.  Output produces is a csv with all of
outfile<-'StateSpace_phase2.RData' # output filename

#######################################################################
########################## RUN THE MODEL ##############################
times<-c(seq(0,maxt))

# set up state space
StateSpace<-array(0.0,dim=c(length(intervention_R_rdxn_vec),length(intervention_R_rdxn_vec),length(Rhosp_vec),length(Rcrit_vec),nvars,maxt))

#for (R0i in 1:length(R0_vec)) {
for (intervention_R_rdxn_i in 1:length(intervention_R_rdxn_vec)) {
  for (intervention_R_rdxn_j in 1:length(intervention_R_rdxn_vec)) {
 #   for (intervention_time_vec_i in 1:length(intervention_time_vec)) {
      for (Rhosp_vec_i in 1:length(Rhosp_vec)) {
        for (Rcrit_vec_i in 1:length(Rcrit_vec)) {            
            
         #   R0<-R0_vec[R0i]
         #   intervention_R_rdxn<-intervention_R_rdxn_vec[intervention_R_rdxn_i]
      #      intervention_time<-intervention_time_vec[intervention_time_vec_i]
    
            lift_rdxn1<-intervention_R_rdxn_vec[intervention_R_rdxn_i]
            lift_rdxn2<-intervention_R_rdxn_vec[intervention_R_rdxn_j]
            Rhosp<-Rhosp_vec[Rhosp_vec_i]
            Rcrit<-Rcrit_vec[Rcrit_vec_i]
            # to keep track of where we are in the simulation
            #print(paste("R0=",R0,",rdxn=",intervention_R_rdxn,",Rcrit=",Rcrit,"Rhosp=",Rhosp))#"intervention_time=",intervention_time,))#
                     # "Tinf=",Tinf,"Thosp=",Thosp,"Tdeath=",Tdeath
                     # ))
    
            print(paste("lift1=",lift_rdxn1,",lift2=",lift_rdxn2,"Rcrit=",Rcrit,"Rhosp=",Rhosp))
            source("DetermineStateSpace_initalization.R")
            source("DetermineStateSpace_dynamics.R")
            source("DetermineStateSpace_out.R")
            #save(StateSpace,file=paste("StateSpace_Tinf",Tinf,"_Thosp",Thosp,"_Tdeath",Tdeath))
      #    }
     #   }
      }
    }
  }
}

# save as Rdata file
save(StateSpace,file=outfile)