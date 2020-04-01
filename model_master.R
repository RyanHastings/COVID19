############################################################
#### MODEL.MASTER
### Master code for running the SEIHCRD model for Indiana (or
### any other state with appropriate modification). This is based
### on an epidemiological SEIR model, but three populations are
### added:  Hospitalized, Critically hospitalized, and Deceased.
### More info can be found in the accompanying documentation.
############################################################
## Ryan Hastings, 26 March 2020
############################################################
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

for (R0 in c(2.4,2.6,2.8,3.0)) {
#for (intervention_R_rdxn in c(0.0,0.25,0.5)) {
for (nage in c(1,8)) {
for (model_comorbidities in c(0,1)) {

# R0<-2.8
intervention_R_rdxn<-0.75
# nage<-1
# model_comorbidities<-0


Rdeath<-c(seq(0,nage))
Rhosp<-c(seq(0,nage))
if (nage==1 & model_comorbidities==1) {
  tag<-'ComorbidOnly'
  Rdeath[1]<-0.002
  Rhosp[1]<-0.015
} else if (nage==8 & model_comorbidities==0) {
  tag<-'AgeOnly'
  Rdeath[1]<-0.00007
  Rhosp[1]<-0.001
} else if (nage==8 & model_comorbidities==1) {
  tag<-'Both'
  Rdeath[1]<-0.00007
  Rhosp[1]<-0.001
} else if (nage==1 & model_comorbidities==0) {
  tag<-'Neither'
  Rdeath[1]<-0.0066
  Rhosp[1]<-0.015
}

output_base<-paste("Scenario_R",format(R0,nsmall=1),"_",100*intervention_R_rdxn,"rdxn_",tag,sep="")

model_outfile<-paste(output_base,".csv",sep="")
param_outfile<-paste(output_base,"_param.txt",sep="")
print(output_base)

# run code to configure model
source("model_configuration.R")

# run code to initialize model
source("model_initialization.R")

# run model core
source("model_dynamic_core.R")

# output results
source("model_out.R")

}
}
}
#}