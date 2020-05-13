###############################################
# IntegralModel_configuration.R
#
# Implementation of integral version of the SEIR model.
# I found the results too deterministic and not reflective
# of the realities of the disease, so I abandoned it,
# but am keeping it here for posterity.
##############################################
# Ryan Hastings, 4 May 2020
##############################################

# clear out the variables
rm(list=ls())

# load in the packages
library(tidyverse)

###############################################
# configuration variables

Npop<-6.692e6 # population
Tinc<-5 # incubation time
Tinf<-3 # mild infection time
Rdeath<-0.0066 # IFR
maxt<-300 # number of days for run
intervention_time<-500 # day of intervention
lift_time<-5000 # day of lifting of intervention
R0<-3 # base transmission rate
Trecov<-9 # hospitalization days until recovery
Tdeath<-10 # hospitalization days until death
Rhosp<-0.03 # infection hospitalization rate
Rcrit<-0.015 # critical hospitalization rate

##############################################
# run the model

source("IntegralModel_initialization.R")
source("IntegralModel_engine.R")
source("IntegralModel_out.R")