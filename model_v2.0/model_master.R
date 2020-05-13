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

# It is frequently the case that we wish to run several scenarios,
# so we loop through the main model with different settings.
# for (R0 in seq(2.0,3.4,0.2)) { # loop through R0
# for (intervention_R_rdxn in c(0.0,0.5,0.65,0.8,0.85)) { # loop through intervention reductions
# for (nage in c(1,8)) { # loop through age settings
# for (model_comorbidities in c(0,1)) { # loop through comorbidity settings
# for (lift_time in c(500,102,133,163)) { #c(500,102,133,163,194)) { # loop through lift times
# for (postlift_rdxn in seq(0.0,0.5,0.1)) { # loop through postlift reductions

R0<-3.0
intervention_R_rdxn<-0.7
nage<-1
model_comorbidities<-0
lift_time<-166
postlift_rdxn<-0.0

# some more setting options
R0urban<-2.9
R0rural<-0.0
intervention_R_rdxn_urban<-0.85
intervention_R_rdxn_rural<-0.75

# Rdeath and Rhosp for age and comorbidity settings
Rdeath<-c(seq(0,nage))
Rhosp<-c(seq(0,nage))
if (nage==1 & model_comorbidities==1) {
  tag<-'ComorbidOnly'
  Rdeath[1]<-0.0066
  Rhosp[1]<-0.03
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
  Rhosp[1]<-0.03
}

output_base<-paste("Scenario_R",format(R0,nsmall=1),"_",100*intervention_R_rdxn,"rdxn_lift",lift_time,'_postlift',postlift_rdxn*100,'_',tag,sep="")
# output_base<-paste("Scenario_R",format(R0,nsmall=1),"_",100*intervention_R_rdxn,
#                    "rdxn_urbanR",format(R0urban,nsmall=1),"_",100*intervention_R_rdxn_urban,
#                    "rdxn_ruralR",format(R0rural,nsmall=1),"_",100*intervention_R_rdxn_rural,
#                    "rdxn_",tag,sep="")

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

# print(Dout[,maxt]/Icumout[,maxt])
# df1<-df1%>%mutate(ExcessiveDeath=as.numeric(Deceased)-as.numeric(CriticalCumulative))
# print(ggplot(df1,aes(x=Day))+geom_line(aes(y=ExcessiveDeath,group=County))+ggtitle(output_base))

# }
# }
# }
# }
# }
# }