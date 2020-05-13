###############################################################
# IntegralModel_configuration.R
#
# integral model configuration and running.  Abandoned for bad
# results.
###############################################################
# 12 May 2020, Ryan Hastings
###############################################################
rm(list=ls())

library(tidyverse)

#####

Npop<-6.692e6
Tinc<-5
Tinf<-3
Rdeath<-0.0066
maxt<-300
intervention_time<-65
lift_time<-5000
R0_vec<-seq(1.0,10.0,0.1)#seq(1.0,10.0,0.1)
intervention_R_rdxn_vec<-seq(0.0,0.95,0.05)
Trecov<-9
Tdeath<-10
Rhosp_vec<-seq(0.5,4.0,0.5)
Rcrit_vec<-seq(0.01,0.04,0.001)
#Rhosp<-0.03
#Rcrit<-0.015
nvars<-13

####

StateSpace<-array(0.0,dim=c(length(R0_vec),length(intervention_R_rdxn_vec),length(Rhosp_vec),length(Rcrit_vec),nvars,maxt))

for (i in 1:length(R0_vec)) {
  for (j in 1:length(intervention_R_rdxn_vec)) {
    for (k in 1:length(Rhosp_vec)) {
      for (l in 1:length(Rcrit_vec)) {
        R0<-R0_vec[i]
        intervention_R_rdxn<-intervention_R_rdxn_vec[j]
        Rhosp<-Rhosp_vec[k]
        Rcrit<-Rcrit_vec[l]

# R0<-5.0
# intervention_R_rdxn<-0.0

        print(paste("R0=",R0,",intervention=",intervention_R_rdxn,",Rhosp=",Rhosp,"Rcrit=",Rcrit,sep=""))
  
        
        source("IntegralModel_initialization.R")
        source("IntegralModel_engine.R")
        source("IntegralModel_out.R")
      }
    }
  }
}


save(StateSpace,file="Integral_StateSpace.Rdata")