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
# Stage Four and Five:  11 June 2020
# July: 29 June 2020
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
intervention_R_rdxn_vec<-seq(0.7,0.0,-0.05)#c(seq(0.0,0.7,0.05)) # field of NPI reduction rate over which to simulate
stage5_weeks_vec<-seq(14,0,-1)#seq(0,14)
nvars<-6#13 # number of variables for output (you probably don't want to change this unless you want a huge coding headache)
Npop<-6.692e6 # population of state
R0<-3.0
PhaseOneReduction<-0.7
PhaseTwoReductionA<-0.7
PhaseTwoReductionB<-0.65
PhaseThreeReductionA<-0.7
PhaseThreeReductionB<-0.7
PhaseFourReduction<-0.7
JulyReduction<-0.55
AugReduction<-0.65
SepReduction_vec<-seq(0.7,0.0,-0.05)
#PhaseFourReduction_vec<-seq(0.5,0.7,0.05)

DayZero<-as.Date("2020-01-20")
PhaseOneDate<-as.Date("2020-03-25")
PhaseTwoDateA<-as.Date("2020-05-04")
PhaseTwoDateB<-as.Date("2020-05-11")
PhaseThreeDateA<-as.Date("2020-05-22")
PhaseThreeDateB<-as.Date("2020-06-01")
PhaseFourDate<-as.Date("2020-06-12")
PhaseFiveDate<-as.Date("2020-07-04")
AugustDate<-as.Date("2020-07-21")
SeptemberDate<-as.Date("2020-09-01")
OctoberDate<-as.Date("2020-10-01")
AugustDay<-as.numeric(AugustDate-DayZero)
SeptemberDay<-as.numeric(SeptemberDate-DayZero)
OctoberDay<-as.numeric(OctoberDate-DayZero)

Rdeath<-0.0066 # IFR (infection fatality rate)
RhospPhaseOne<-0.03
RcritPhaseOne<-0.012
RhospPhaseThree<-0.035
RcritPhaseThree<-0.011
RhospPhaseFour<-0.035
RcritPhaseFour<-0.01
RhospPhaseFive<-0.05
RcritPhaseFive<-0.013
RhospAug<-0.045
RcritAug<-0.013
Rhosp_vec<-seq(0.025,0.04,0.005)#0.06 # hospitalization rate
Rcrit_vec<-seq(0.01,0.02,0.001)#0.024 # critical rate

# NPI timing
# intervention_time<-65#65#79 # days after day 0 NPI is introduced
# lift_time1<-103 # days after day 0 that NPI is modified in most counties (4 May)
# lift_time2<-112 # days after day 0 that NPI is modified in Marion and Lake counties (11 May)
dt=1.0 # time step (days)

# disease timing
Tinc<-5 # incubation time (days)
Tinf<-3 # time of infection before either hospitalization or recovery
Thosp<-8.6 # time in hospitalization until recovery
Tdeath<-10.4 # time in hospital until death
Tcrit<-7.9 # time in ICU until recovery
Pinf<-1.0 # max proportion of population to get infected

outdir<-"./" # directory for output
output<-1 # produce output? 0=no, 1=yes.  Output produces is a csv with all of
outfile<-'StateSpace_September.RData' # output filename

#######################################################################
########################## RUN THE MODEL ##############################
times<-c(seq(0,maxt))

# set up state space
#StateSpace<-array(0.0,dim=c(length(JulyReduction_vec),length(stage5_weeks_vec),length(Rhosp_vec),length(Rcrit_vec),nvars,maxt))
StateSpace<-array(0.0,dim=c(length(Rcrit_vec),length(Rhosp_vec),length(SepReduction_vec),nvars,maxt))
#haseFourReduction_vec<-PhaseThreeReduction_vec

#for (R0i in 1:length(R0_vec)) {
# for (i in 1:length(JulyReduction_vec)) {
#   for (j in 1:length(stage5_weeks_vec)) {
#     for (Rhosp_vec_i in 1:length(Rhosp_vec)) {
#       for (Rcrit_vec_i in 1:length(Rcrit_vec)) {            
for (Rhosp_vec_i in 1:length(Rhosp_vec)) {
  for (Rcrit_vec_i in 1:length(Rcrit_vec)) {
    for (j in 1:length(SepReduction_vec)) {
      #for (i in 1:length(AugReduction_vec)) {
        
        PhaseFiveReduction<-JulyReduction
        #AugReduction<-AugReduction_vec[i]
        SepReduction<-SepReduction_vec[j]
        #stage5_weeks<-stage5_weeks_vec[j]
        RhospSep<-Rhosp_vec[Rhosp_vec_i]
        RcritSep<-Rcrit_vec[Rcrit_vec_i]
        # to keep track of where we are in the simulation
        #print(paste("R0=",R0,",rdxn=",intervention_R_rdxn,",Rcrit=",Rcrit,"Rhosp=",Rhosp))#"intervention_time=",intervention_time,))#
        # "Tinf=",Tinf,"Thosp=",Thosp,"Tdeath=",Tdeath
        # ))
        
        #print(paste("lift1=",PhaseFiveReduction,",weeks=",stage5_weeks,"Rcrit=",RcritPhaseFive,"Rhosp=",RhospPhaseFive))
        print(paste("Rcrit=",RcritSep,"Rhosp=",RhospSep,"weeks=",SepReduction,"lift=",AugReduction))
        source("DetermineStateSpaceStageThree_initialization.R")
        source("DetermineStateSpaceSeptember_dynamics.R")
        source("DetermineStateSpaceSeptember_out.R")
        #save(StateSpace,file=paste("StateSpace_Tinf",Tinf,"_Thosp",Thosp,"_Tdeath",Tdeath))
        
     # }
    }
  }
}

# save as Rdata file
save(StateSpace,file=outfile)