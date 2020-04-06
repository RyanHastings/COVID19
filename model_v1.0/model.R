################################################################
#
# model.R
#
# An SEIRHD epidemeological model for COVID-19 (or really any virus,
# I just developed it for COVID-19).  See ModelNotes.docx for
# model notes.  While this is in use by the state of Indiana, it
# in no way is official nor should be regarded as such.  It is
# my own work.
###############################################################
# Ryan Hastings, 19 March 2020
###############################################################
# Updated equation set to be correct, 25 March 2020
#    Known bug:  Recovery numbers are way too low.  Every other
#      number appears correct, though, and the other numbers
#      are what we care about for disaster planning.
###############################################################

###############################################################
# PREAMBLE
###############################################################
rm(list=ls())

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(haven)
library(readxl)

###############################################################
# CONFIGURATION OF VARIABLES
###############################################################

datadir<-'../../COVID19Response/'
outdir<-'../../COVID19Response/'

<<<<<<< HEAD
### The next few lines are for processing a number of scenarios at once
# for (R0 in c(2.0,2.2,2.4,2.6)) {
# for (intervention_R_rdxn in c(0.0, 0.25, 0.5)) {
# for (model_by_age in c(0,1,1)) {
# for (model_comorbidities in c(1,0,1)) {
# 
# if (model_by_age==0 & model_comorbidities==1) {
#   tag<-"ComorbidOnly"
# } else if (model_by_age==1 & model_comorbidities==0) {
#   tag<-"AgeOnly"
# } else if (model_by_age==1 & model_comorbidities==1) {
#   tag<-"Both"
# }
#   
# model_outfile<-paste("Scenario_R",format(R0,nsmall=1),"_",
#   10*intervention_R_rdxn,"rdxn_",tag,".csv",sep="")
# print(model_outfile)
# param_outfile<-paste("Scenario_R",format(R0,nsmall=1),"_",
#                      10*intervention_R_rdxn,"rdxn_",tag,"_param.txt",sep="")

R0<-2.2 # rate of infection for exposed people
=======
R0<-2.6 # rate of infection for exposed people
>>>>>>> parent of ea63aa9... added flowchart
intervention_R_rdxn<-0.0 # % reduction in R after intervention
intervention_time<-40 # intervention time (days)
lift_time<-300 # time at which intervention is ceased
Pinf<-1.0 # proportion of population we expect to be nonresistant

Rdeath<-0.01 # death rate per infected (if model_by_age==0)
Rhosp<-0.15 # hospitalization rate per infected (if model_by_age==0)
Trecov<-28; # time (days) between hospitalization and recovery
Tdeath<-21; # time (days) between indeterminate hospitalization and death
Rcrit<-0.05; # % of infectious that become critically hospitalized

Tinf<-2.9; # duration of infection (not including hospitalization)
Tinc<-5.2; # duration of incubation

model_by_age<-0 # Include age demographics or not, 0=no, 1=yes
Rdeath_10to19<-0.002 # Death rate per infected for age groups
Rdeath_20to39<-0.002
Rdeath_40to49<-0.004
Rdeath_50to59<-0.013
Rdeath_60to69<-0.036
Rdeath_70to79<-0.080
Rdeath_80plus<-0.148
Rhosp_10to19<-0.001 # Hospitalization rate per infected for age groups
Rhosp_20to39<-0.100
Rhosp_40to49<-0.100
Rhosp_50to59<-0.100
Rhosp_60to69<-0.223
Rhosp_70to79<-0.223
Rhosp_80plus<-0.223

model_comorbidities<-1 # Include comorbidities, or no, 0=no, 1=yes
Rhosp_diabetes<-0.073 # Rate of hospitalization
Rhosp_heartdisease<-0.105
Rhosp_hypertension<-0.06
Rhosp_maligneoplasm<-0.056
Rhosp_copd<-0.063
Rdeath_diabetes<-0.073 # Rate of death
Rdeath_heartdisease<-0.105
Rdeath_hypertension<-0.06
Rdeath_maligneoplasm<-0.056
Rdeath_copd<-0.063

maxt<-300 # Max time to simulate (days)
ncounties<-92 # number of counties

output<-0 # produce output? 0=no, 1=yes.  Output produces is a csv with all of
          # the values as columns and a file containing all of the parameter settings
model_outfile<-"Scenario_R2.6_0rdxn_Both.csv" # name of model output
param_outfile<-"Scenario_R2.6_0rdxn_Both_params.txt" # name of parameter file
  #notes to include in parameter text file
param_notes<-"No county-county transmission, all counties seeded with one case."

# names for some plots that are produced...these are Indiana-specific and will
# require modification for your state.
pic1<-"Marion_R2.6_0rdxn_Both.png"
tit1<-"Marion, R0=2.6, no reduction, both"
pic2<-"Monre_R2.2_50p.png"
tit2<-"Monroe, R0=2.2, 50% reduction"
pic3<-"Lake_R2.2_no_50p.png"
tit3<-"Lake, R0=2.2, 50% reduction"
################################################
# SET INITIAL CASES
#
# The number of initial cases in each county.

if (model_by_age==0) {
  In<-matrix(0,nrow=ncounties,ncol=maxt)
  for (i in 1:ncounties) {
    In[i,1]=1
    In[i,2:maxt]=0
  }
} else if (model_by_age==1) {
  I_10to19<-matrix(0.0,nrow=ncounties,ncol=maxt)
  I_20to39<-matrix(0.0,nrow=ncounties,ncol=maxt)
  I_40to49<-matrix(0.0,nrow=ncounties,ncol=maxt)
  I_50to59<-matrix(0.0,nrow=ncounties,ncol=maxt)
  I_60to69<-matrix(0.0,nrow=ncounties,ncol=maxt)
  I_70to79<-matrix(0.0,nrow=ncounties,ncol=maxt)
  I_80plus<-matrix(0.0,nrow=ncounties,ncol=maxt)
  for (i in 1:ncounties) {
    
    # I'll admit it, I'm GenX and thought it would be funny if Millenials gave it to us all
    I_20to39[i,1]=1
    
  }
}

################################################
# set county-to-county transmission rates
#
# I have not yet included intercounty transmission in my model.
# Ideally these will be numbers between 0-1 and will add to 1 for
# any given county.
M<-matrix(0.0,nrow=ncounties,ncol=ncounties)
for (i in 1:ncounties) { # v1: assume no transmission between counties
  for (j in 1:ncounties) {
    if (i==j) {
      M[i,j]=1.0
    } else{
      M[i,j]=0.0
    }
  }
}

#################################################
# get county populations
#
# This will need to be modified for your state
US.pop.raw<-read_csv('PEP_2018_PEPAGESEX/PEP_2018_PEPAGESEX_with_ann.csv')
IN.pop.raw<-US.pop.raw%>%filter(grepl(", Indiana",`GEO.display-label`))
county_names<-IN.pop.raw$`GEO.display-label`

if (model_by_age==0) {
  S<-matrix(0,nrow=ncounties,ncol=maxt)
  for (i in 1:ncounties) {
    S[i,1]<-as.numeric(IN.pop.raw$est42010sex0_age999[i])*Pinf
    S[i,2:maxt]<-0
  }
} else if (model_by_age==1) {
  
  S_10to19<-matrix(0,nrow=ncounties,ncol=maxt)
  S_20to39<-matrix(0,nrow=ncounties,ncol=maxt)
  S_40to49<-matrix(0,nrow=ncounties,ncol=maxt)
  S_50to59<-matrix(0,nrow=ncounties,ncol=maxt)
  S_60to69<-matrix(0,nrow=ncounties,ncol=maxt)
  S_70to79<-matrix(0,nrow=ncounties,ncol=maxt)
  S_80plus<-matrix(0,nrow=ncounties,ncol=maxt)
  
  for (i in 1:ncounties) {
    
    S_10to19[i,1]<-as.numeric(IN.pop.raw$est72018sex0_age0to4[i])+
                  as.numeric(IN.pop.raw$est72018sex0_age5to9[i])+
                  as.numeric(IN.pop.raw$est72018sex0_age10to14[i])+
                  as.numeric(IN.pop.raw$est72010sex0_age15to19[i])
  
  
    
    S_20to39[i,1]<-as.numeric(IN.pop.raw$est72018sex0_age20to24[i])+
                  as.numeric(IN.pop.raw$est72018sex0_age25to29[i])+
                  as.numeric(IN.pop.raw$est72018sex0_age30to34[i])+
                  as.numeric(IN.pop.raw$est72018sex0_age35to39[i])
  
    
    S_40to49[i,1]<-as.numeric(IN.pop.raw$est72018sex0_age40to44[i])+
                  as.numeric(IN.pop.raw$est72018sex0_age45to49[i])
  
    
    S_50to59[i,1]<-as.numeric(IN.pop.raw$est72018sex0_age50to54[i])+
                  as.numeric(IN.pop.raw$est72018sex0_age55to59[i])
  
    
    S_60to69[i,1]<-as.numeric(IN.pop.raw$est72018sex0_age60to64[i])+
                   as.numeric(IN.pop.raw$est72018sex0_age65to69[i])
  

    S_70to79[i,1]<-as.numeric(IN.pop.raw$est72018sex0_age70to74[i])+
                   as.numeric(IN.pop.raw$est72018sex0_age75to79[i])
  

    S_80plus[i,1]<-as.numeric(IN.pop.raw$est72018sex0_age80to84[i])+
                   as.numeric(IN.pop.raw$est72018sex0_age85plus[i])
  }
}

#################################################
# comorbidities
#
# This involves setting up an array, with each element being a
# multiplier for the county that will multiply either the hospitalization
# or death rate.  The array is determined by weighting according to the
# comorbidities present in the population.  You'll have to supply your
# own state's numbers.

comorbid_hosp<-rep(1.0,ncounties)
comorbid_death<-rep(1.0,ncounties)
comorbid_hosp_over60<-rep(1.0,ncounties)
comorbid_death_over60<-rep(1.0,ncounties)

if (model_comorbidities==1) {
  
  comorbid_pop<-read_excel(paste(datadir,"Hospitalizations with comorbidities.xlsx",sep=""),
                           sheet="Percents")
  
  if (model_by_age==0) {
    for (i in 1:ncounties) {
      
      row<-filter(comorbid_pop,county_id==i)
      comorbid_hosp[i]<-(1+row$diabetes_percent*Rhosp_diabetes+
                           row$heartdisease_percent*Rhosp_heartdisease+
                           row$hypertension_percent*Rhosp_hypertension+
                           row$maligneoplasm_percent*Rhosp_maligneoplasm+
                           row$copd_percent*Rhosp_copd)
      comorbid_death[i]<-1+row$diabetes_percent*Rdeath_diabetes+
                           row$heartdisease_percent*Rdeath_heartdisease+
                           row$hypertension_percent*Rhosp_hypertension+
                           row$maligneoplasm_percent*Rhosp_maligneoplasm+
                           row$copd_percent*Rhosp_copd
      
    }
  } else if (model_by_age==1) {
    
    comorbid_hosp<-rep(1.0,ncounties)
    comorbid_death<-rep(1.0,ncounties)
    comorbid_hosp_over60<-rep(1.0,ncounties)
    comorbid_death_over60<-rep(1.0,ncounties)
    
    for (i in 1:ncounties) {
      
      row<-filter(comorbid_pop,county_id==i)
      
      comorbid_hosp[i]<-(1+row$diabetes_percent*Rhosp_diabetes+
                           row$heartdisease_percent*Rhosp_heartdisease+
                           row$hypertension_percent*Rhosp_hypertension+
                           row$maligneoplasm_percent*Rhosp_maligneoplasm+
                           row$copd_percent*Rhosp_copd)
      comorbid_death[i]<-1+row$diabetes_percent*Rdeath_diabetes+
                           row$heartdisease_percent*Rdeath_heartdisease+
                           row$hypertension_percent*Rhosp_hypertension+
                           row$maligneoplasm_percent*Rhosp_maligneoplasm+
                           row$copd_percent*Rhosp_copd
      comorbid_hosp_over60[i]<-1+row$age60up_diabetes_percent*Rhosp_diabetes+
                                 row$age60up_heartdisease_percent*Rhosp_heartdisease+
                                 row$age60up_hypertension_percent*Rhosp_hypertension+
                                 row$age60up_maligneoplasm_percent*Rhosp_maligneoplasm+
                                 row$age60up_copd_percent*Rhosp_copd
      comorbid_death_over60[i]<-1+row$age60up_diabetes_percent*Rdeath_diabetes+
                                  row$age60up_heartdisease_percent*Rdeath_heartdisease+
                                  row$age60up_hypertension_percent*Rdeath_hypertension+
                                  row$age60up_maligneoplasm_percent*Rdeath_maligneoplasm+
                                  row$age60up_copd_percent*Rdeath_copd
    }
  }
}


#################################################
# initialize other values
#
# This just initializes a bunch of matricies and arrays
# that will be used by the model.  Probably not much to worry about
# here.

if (Tdeath>Trecov) {
  Tcrit<-0.5*Trecov
} else {
  Tcrit<-0.5*Tdeath
}

Trecovcrit<-Trecov-Tcrit
Tdeath<-Tdeath-Tcrit

if (model_by_age==0) {
  dSdt<-matrix(0.0,nrow=ncounties,ncol=maxt)

  E<-matrix(0,nrow=ncounties,ncol=maxt)
  dEdt<-matrix(0.0,nrow=ncounties,ncol=maxt)

  dIdt<-matrix(0.0,nrow=ncounties,ncol=maxt)

  R<-matrix(0,nrow=ncounties,ncol=maxt)
  dRdt<-matrix(0,nrow=ncounties,ncol=maxt)

  H<-matrix(0,nrow=ncounties,ncol=maxt)
  dHdt<-matrix(0,nrow=ncounties,ncol=maxt)

  Q<-matrix(0,nrow=ncounties,ncol=maxt)
  dQdt<-matrix(0,nrow=ncounties,ncol=maxt)
  
  G<-matrix(0,nrow=ncounties,ncol=maxt)
  dGdt<-matrix(0,nrow=ncounties,ncol=maxt)
  
  D<-matrix(0,nrow=ncounties,ncol=maxt)
  dDdt<-matrix(0,nrow=ncounties,ncol=maxt)

  Rdeath<-Rdeath/Rcrit

  crit<-matrix(0.0,nrow=ncounties,ncol=maxt)
  dcritdt<-matrix(0.0,nrow=ncounties,ncol=maxt)

  Ecum<-matrix(0.0,nrow=ncounties,ncol=maxt)
  Icum<-matrix(0.0,nrow=ncounties,ncol=maxt)
  Hcum<-matrix(0.0,nrow=ncounties,ncol=maxt)
  critcum<-matrix(0.0,nrow=ncounties,ncol=maxt)
  Gcum<-matrix(0.0,nrow=ncounties,ncol=maxt)
  Qcum<-matrix(0.0,nrow=ncounties,ncol=maxt)

} else if (model_by_age==1) {
  
  dS_10to19dt<-matrix(0.0,nrow=ncounties,ncol=maxt)
  dS_20to39dt<-matrix(0.0,nrow=ncounties,ncol=maxt)
  dS_40to49dt<-matrix(0.0,nrow=ncounties,ncol=maxt)
  dS_50to59dt<-matrix(0.0,nrow=ncounties,ncol=maxt)
  dS_60to69dt<-matrix(0.0,nrow=ncounties,ncol=maxt)
  dS_70to79dt<-matrix(0.0,nrow=ncounties,ncol=maxt)
  dS_80plusdt<-matrix(0.0,nrow=ncounties,ncol=maxt)
  
  E_10to19<-matrix(0.0,nrow=ncounties,ncol=maxt)
  E_20to39<-matrix(0.0,nrow=ncounties,ncol=maxt)
  E_40to49<-matrix(0.0,nrow=ncounties,ncol=maxt)
  E_50to59<-matrix(0.0,nrow=ncounties,ncol=maxt)
  E_60to69<-matrix(0.0,nrow=ncounties,ncol=maxt)
  E_70to79<-matrix(0.0,nrow=ncounties,ncol=maxt)
  E_80plus<-matrix(0.0,nrow=ncounties,ncol=maxt)
  
  dE_10to19dt<-matrix(0.0,nrow=ncounties,ncol=maxt)
  dE_20to39dt<-matrix(0.0,nrow=ncounties,ncol=maxt)
  dE_40to49dt<-matrix(0.0,nrow=ncounties,ncol=maxt)
  dE_50to59dt<-matrix(0.0,nrow=ncounties,ncol=maxt)
  dE_60to69dt<-matrix(0.0,nrow=ncounties,ncol=maxt)
  dE_70to79dt<-matrix(0.0,nrow=ncounties,ncol=maxt)
  dE_80plusdt<-matrix(0.0,nrow=ncounties,ncol=maxt)
  
  dI_10to19dt<-matrix(0.0,nrow=ncounties,ncol=maxt)
  dI_20to39dt<-matrix(0.0,nrow=ncounties,ncol=maxt)
  dI_40to49dt<-matrix(0.0,nrow=ncounties,ncol=maxt)
  dI_50to59dt<-matrix(0.0,nrow=ncounties,ncol=maxt)
  dI_60to69dt<-matrix(0.0,nrow=ncounties,ncol=maxt)
  dI_70to79dt<-matrix(0.0,nrow=ncounties,ncol=maxt)
  dI_80plusdt<-matrix(0.0,nrow=ncounties,ncol=maxt)

  R<-matrix(0,nrow=ncounties,ncol=maxt)
  dRdt<-matrix(0,nrow=ncounties,ncol=maxt)
  
  H_10to19<-matrix(0.0,nrow=ncounties,ncol=maxt)
  H_20to39<-matrix(0.0,nrow=ncounties,ncol=maxt)
  H_40to49<-matrix(0.0,nrow=ncounties,ncol=maxt)
  H_50to59<-matrix(0.0,nrow=ncounties,ncol=maxt)
  H_60to69<-matrix(0.0,nrow=ncounties,ncol=maxt)
  H_70to79<-matrix(0.0,nrow=ncounties,ncol=maxt)
  H_80plus<-matrix(0.0,nrow=ncounties,ncol=maxt)
  
  dH_10to19dt<-matrix(0.0,nrow=ncounties,ncol=maxt)
  dH_20to39dt<-matrix(0.0,nrow=ncounties,ncol=maxt)
  dH_40to49dt<-matrix(0.0,nrow=ncounties,ncol=maxt)
  dH_50to59dt<-matrix(0.0,nrow=ncounties,ncol=maxt)
  dH_60to69dt<-matrix(0.0,nrow=ncounties,ncol=maxt)
  dH_70to79dt<-matrix(0.0,nrow=ncounties,ncol=maxt)
  dH_80plusdt<-matrix(0.0,nrow=ncounties,ncol=maxt)
  
  D<-matrix(0,nrow=ncounties,ncol=maxt)
  dDdt<-matrix(0,nrow=ncounties,ncol=maxt)
  
  Rdeath_10to19<-Rdeath_10to19/Rcrit
  Rdeath_20to39<-Rdeath_20to39/Rcrit
  Rdeath_40to49<-Rdeath_40to49/Rcrit
  Rdeath_50to59<-Rdeath_50to59/Rcrit
  Rdeath_60to69<-Rdeath_60to69/Rcrit
  Rdeath_70to79<-Rdeath_70to79/Rcrit
  Rdeath_80plus<-Rdeath_80plus/Rcrit
  
  G_10to19<-matrix(0,nrow=ncounties,ncol=maxt)
  G_20to39<-matrix(0,nrow=ncounties,ncol=maxt)
  G_40to49<-matrix(0,nrow=ncounties,ncol=maxt)
  G_50to59<-matrix(0,nrow=ncounties,ncol=maxt)
  G_60to69<-matrix(0,nrow=ncounties,ncol=maxt)
  G_70to79<-matrix(0,nrow=ncounties,ncol=maxt)
  G_80plus<-matrix(0,nrow=ncounties,ncol=maxt)
  
  dG_10to19dt<-matrix(0,nrow=ncounties,ncol=maxt)
  dG_20to39dt<-matrix(0,nrow=ncounties,ncol=maxt)
  dG_40to49dt<-matrix(0,nrow=ncounties,ncol=maxt)
  dG_50to59dt<-matrix(0,nrow=ncounties,ncol=maxt)
  dG_60to69dt<-matrix(0,nrow=ncounties,ncol=maxt)
  dG_70to79dt<-matrix(0,nrow=ncounties,ncol=maxt)
  dG_80plusdt<-matrix(0,nrow=ncounties,ncol=maxt)
  
  Q_10to19<-matrix(0,nrow=ncounties,ncol=maxt)
  Q_20to39<-matrix(0,nrow=ncounties,ncol=maxt)
  Q_40to49<-matrix(0,nrow=ncounties,ncol=maxt)
  Q_50to59<-matrix(0,nrow=ncounties,ncol=maxt)
  Q_60to69<-matrix(0,nrow=ncounties,ncol=maxt)
  Q_70to79<-matrix(0,nrow=ncounties,ncol=maxt)
  Q_80plus<-matrix(0,nrow=ncounties,ncol=maxt)
  
  dQ_10to19dt<-matrix(0,nrow=ncounties,ncol=maxt)
  dQ_20to39dt<-matrix(0,nrow=ncounties,ncol=maxt)
  dQ_40to49dt<-matrix(0,nrow=ncounties,ncol=maxt)
  dQ_50to59dt<-matrix(0,nrow=ncounties,ncol=maxt)
  dQ_60to69dt<-matrix(0,nrow=ncounties,ncol=maxt)
  dQ_70to79dt<-matrix(0,nrow=ncounties,ncol=maxt)
  dQ_80plusdt<-matrix(0,nrow=ncounties,ncol=maxt)
  
  crit_10to19<-matrix(0.0,nrow=ncounties,ncol=maxt)
  crit_20to39<-matrix(0.0,nrow=ncounties,ncol=maxt)
  crit_40to49<-matrix(0.0,nrow=ncounties,ncol=maxt)
  crit_50to59<-matrix(0.0,nrow=ncounties,ncol=maxt)
  crit_60to69<-matrix(0.0,nrow=ncounties,ncol=maxt)
  crit_70to79<-matrix(0.0,nrow=ncounties,ncol=maxt)
  crit_80plus<-matrix(0.0,nrow=ncounties,ncol=maxt)
  
  Ecum_10to19<-matrix(0.0,nrow=ncounties,ncol=maxt)
  Ecum_20to39<-matrix(0.0,nrow=ncounties,ncol=maxt)
  Ecum_40to49<-matrix(0.0,nrow=ncounties,ncol=maxt)
  Ecum_50to59<-matrix(0.0,nrow=ncounties,ncol=maxt)
  Ecum_60to69<-matrix(0.0,nrow=ncounties,ncol=maxt)
  Ecum_70to79<-matrix(0.0,nrow=ncounties,ncol=maxt)
  Ecum_80plus<-matrix(0.0,nrow=ncounties,ncol=maxt)
  
  Icum_10to19<-matrix(0.0,nrow=ncounties,ncol=maxt)
  Icum_20to39<-matrix(0.0,nrow=ncounties,ncol=maxt)
  Icum_40to49<-matrix(0.0,nrow=ncounties,ncol=maxt)
  Icum_50to59<-matrix(0.0,nrow=ncounties,ncol=maxt)
  Icum_60to69<-matrix(0.0,nrow=ncounties,ncol=maxt)
  Icum_70to79<-matrix(0.0,nrow=ncounties,ncol=maxt)
  Icum_80plus<-matrix(0.0,nrow=ncounties,ncol=maxt)
  
  Hcum_10to19<-matrix(0.0,nrow=ncounties,ncol=maxt)
  Hcum_20to39<-matrix(0.0,nrow=ncounties,ncol=maxt)
  Hcum_40to49<-matrix(0.0,nrow=ncounties,ncol=maxt)
  Hcum_50to59<-matrix(0.0,nrow=ncounties,ncol=maxt)
  Hcum_60to69<-matrix(0.0,nrow=ncounties,ncol=maxt)
  Hcum_70to79<-matrix(0.0,nrow=ncounties,ncol=maxt)
  Hcum_80plus<-matrix(0.0,nrow=ncounties,ncol=maxt)
  
  critcum_10to19<-matrix(0.0,nrow=ncounties,ncol=maxt)
  critcum_20to39<-matrix(0.0,nrow=ncounties,ncol=maxt)
  critcum_40to49<-matrix(0.0,nrow=ncounties,ncol=maxt)
  critcum_50to59<-matrix(0.0,nrow=ncounties,ncol=maxt)
  critcum_60to69<-matrix(0.0,nrow=ncounties,ncol=maxt)
  critcum_70to79<-matrix(0.0,nrow=ncounties,ncol=maxt)
  critcum_80plus<-matrix(0.0,nrow=ncounties,ncol=maxt)
  
  dcrit_10to19dt<-matrix(0.0,nrow=ncounties,ncol=maxt)
  dcrit_20to39dt<-matrix(0.0,nrow=ncounties,ncol=maxt)
  dcrit_40to49dt<-matrix(0.0,nrow=ncounties,ncol=maxt)
  dcrit_50to59dt<-matrix(0.0,nrow=ncounties,ncol=maxt)
  dcrit_60to69dt<-matrix(0.0,nrow=ncounties,ncol=maxt)
  dcrit_70to79dt<-matrix(0.0,nrow=ncounties,ncol=maxt)
  dcrit_80plusdt<-matrix(0.0,nrow=ncounties,ncol=maxt)
  
}
  
#################################################
# run the model
#################################################
for (t in 2:maxt) {
  
  if (t<intervention_time & t>lift_time) {
    Rt<-R0
  } else {
    Rt<-(1-intervention_R_rdxn)*R0
  }
  
  if (model_by_age==0) {
    for (i in 1:ncounties) {
    
      dSdt[i,t]<-0
      dEdt[i,t]<-0
      for (j in 1:ncounties) {
        dSdt[i,t]<-dSdt[i,t]-Rt*M[i,j]*In[j,t-1]*S[i,t-1]/(S[i,1]*Tinf)
      }
      if (dSdt[i,t]+S[i,t-1]<0) {
        dSdt[i,t]<--S[i,t-1]
      }
      dEdt[i,t]<--dSdt[i,t]-E[i,t-1]/Tinc

      dIdt[i,t]<-E[i,t-1]/Tinc-In[i,t-1]/Tinf
      dHdt[i,t]<-(Rhosp-Rcrit)*comorbid_hosp[i]*In[i,t-1]/Tinf-
        H[i,t-1]/Trecov
      dcritdt[i,t]<-Rcrit*comorbid_hosp[i]*In[i,t-1]/Tinf-
        crit[i,t-1]/Tcrit
      dGdt[i,t]<-Rdeath*comorbid_death[i]*crit[i,t-1]/Tcrit-
        G[i,t-1]/Tdeath
      dQdt[i,t]<-(1-Rdeath*comorbid_death[i])*crit[i,t-1]/Tcrit-
        Q[i,t-1]/Trecovcrit
      dRdt[i,t]<-(1-Rhosp*comorbid_hosp[i])*In[i,t-1]/Tinf+
        Q[i,t-1]/Trecovcrit
      dDdt[i,t]<-G[i,t-1]/Tdeath
    

      S[i,t]<-S[i,t-1]+dSdt[i,t]
      E[i,t]<-E[i,t-1]+dEdt[i,t]
      In[i,t]<-In[i,t-1]+dIdt[i,t]
      R[i,t]<-R[i,t-1]+dRdt[i,t]
      H[i,t]<-H[i,t-1]+dHdt[i,t]
      D[i,t]<-D[i,t-1]+dDdt[i,t]
      crit[i,t]<-crit[i,t-1]+dcritdt[i,t]
      Q[i,t]<-Q[i,t-1]+dQdt[i,t]
      G[i,t]<-G[i,t-1]+dGdt[i,t]
      # Cumulative values
      Ecum[i,t]<-Ecum[i,t-1]-dSdt[i,t]
      Icum[i,t]<-Icum[i,t-1]+E[i,t-1]/Tinc
      Hcum[i,t]<-Hcum[i,t-1]+(Rhosp-Rcrit)*comorbid_hosp[i]*In[i,t-1]/Tinf
      critcum[i,t]<-critcum[i,t-1]+Rcrit*comorbid_hosp[i]*In[i,t-1]/Tinf
      Qcum[i,t]<-Qcum[i,t-1]+Rdeath*comorbid_death[i]*crit[i,t-1]/Tcrit
      Gcum[i,t]<-Gcum[i,t-1]+(1-Rdeath)*comorbid_death[i]*crit[i,t-1]/Tcrit
    
      #if (R[i,t]>S[i,1]) {
      #  print("Error: More recovered than original population.")
      #  stop()
      #}
    }
  } else {
      
    for (i in 1:ncounties) {
      
      inf_rate<-0
      S<-S_10to19+
        S_20to39+
        S_40to49+
        S_50to59+
        S_60to69+
        S_70to79+
        S_80plus
      #stop()
      In<-I_10to19+
        I_20to39+
        I_40to49+
        I_50to59+
        I_60to69+
        I_70to79+
        I_80plus
      dum<-0
      for (j in 1:ncounties) {
        dum<-dum-Rt*M[i,j]*In[j,t-1]*S[i,t-1]/(S[i,1]*Tinf)
      }
      #if(dum==0) {stop()}
      dS_10to19dt[i,t]<-dum*S_10to19[i,t-1]/S[i,t-1]
      dS_20to39dt[i,t]<-dum*S_20to39[i,t-1]/S[i,t-1]
      dS_40to49dt[i,t]<-dum*S_40to49[i,t-1]/S[i,t-1]
      dS_50to59dt[i,t]<-dum*S_50to59[i,t-1]/S[i,t-1]
      dS_60to69dt[i,t]<-dum*S_60to69[i,t-1]/S[i,t-1]
      dS_70to79dt[i,t]<-dum*S_70to79[i,t-1]/S[i,t-1]
      dS_80plusdt[i,t]<-dum*S_80plus[i,t-1]/S[i,t-1]
      
      dE_10to19dt[i,t]<- -dS_10to19dt[i,t] - 
        E_10to19[i,t-1]/Tinc
      dE_20to39dt[i,t]<- -dS_20to39dt[i,t] - 
        E_20to39[i,t-1]/Tinc
      dE_40to49dt[i,t]<- -dS_40to49dt[i,t] - 
        E_40to49[i,t-1]/Tinc
      dE_50to59dt[i,t]<- -dS_50to59dt[i,t] - 
        E_50to59[i,t-1]/Tinc
      dE_60to69dt[i,t]<- -dS_60to69dt[i,t] - 
        E_60to69[i,t-1]/Tinc
      dE_70to79dt[i,t]<- -dS_70to79dt[i,t] - 
        E_70to79[i,t-1]/Tinc
      dE_80plusdt[i,t]<- -dS_80plusdt[i,t] - 
        E_80plus[i,t-1]/Tinc
      
      dI_10to19dt[i,t]<-E_10to19[i,t-1]/Tinc -
        I_10to19[i,t-1]/Tinf
      dI_20to39dt[i,t]<-E_20to39[i,t-1]/Tinc -
        I_20to39[i,t-1]/Tinf
      dI_40to49dt[i,t]<-E_40to49[i,t-1]/Tinc -
        I_40to49[i,t-1]/Tinf
      dI_50to59dt[i,t]<-E_50to59[i,t-1]/Tinc -
        I_50to59[i,t-1]/Tinf
      dI_60to69dt[i,t]<-E_60to69[i,t-1]/Tinc -
        I_60to69[i,t-1]/Tinf
      dI_70to79dt[i,t]<-E_70to79[i,t-1]/Tinc -
        I_70to79[i,t-1]/Tinf
      dI_80plusdt[i,t]<-E_80plus[i,t-1]/Tinc -
        I_80plus[i,t-1]/Tinf
      
      dH_10to19dt[i,t]<-(Rhosp_10to19-Rcrit)*comorbid_hosp[i]*I_10to19[i,t-1]/Tinf -
        H_10to19[i,t-1]/Trecov
      dH_20to39dt[i,t]<-(Rhosp_20to39-Rcrit)*comorbid_hosp[i]*I_20to39[i,t-1]/Tinf -
        H_20to39[i,t-1]/Trecov
      dH_40to49dt[i,t]<-(Rhosp_40to49-Rcrit)*comorbid_hosp[i]*I_40to49[i,t-1]/Tinf -
        H_40to49[i,t-1]/Trecov
      dH_50to59dt[i,t]<-(Rhosp_50to59-Rcrit)*comorbid_hosp[i]*I_50to59[i,t-1]/Tinf -
        H_50to59[i,t-1]/Trecov
      dH_60to69dt[i,t]<-(Rhosp_60to69-Rcrit)*comorbid_hosp_over60[i]*I_60to69[i,t-1]/Tinf -
        H_60to69[i,t-1]/Trecov
      dH_70to79dt[i,t]<-(Rhosp_70to79-Rcrit)*comorbid_hosp_over60[i]*I_70to79[i,t-1]/Tinf -
        H_70to79[i,t-1]/Trecov
      dH_80plusdt[i,t]<-(Rhosp_80plus-Rcrit)*comorbid_hosp_over60[i]*I_80plus[i,t-1]/Tinf -
        H_80plus[i,t-1]/Trecov
      
      dcrit_10to19dt[i,t]<-(Rcrit*comorbid_hosp[i])*I_10to19[i,t-1]/Tinf -
        crit_10to19[i,t-1]/Tcrit
      dcrit_20to39dt[i,t]<-(Rcrit*comorbid_hosp[i])*I_20to39[i,t-1]/Tinf -
        crit_20to39[i,t-1]/Tcrit
      dcrit_40to49dt[i,t]<-(Rcrit*comorbid_hosp[i])*I_40to49[i,t-1]/Tinf -
        crit_40to49[i,t-1]/Tcrit
      dcrit_50to59dt[i,t]<-(Rcrit*comorbid_hosp[i])*I_50to59[i,t-1]/Tinf -
        crit_50to59[i,t-1]/Tcrit
      dcrit_60to69dt[i,t]<-(Rcrit*comorbid_hosp_over60[i])*I_60to69[i,t-1]/Tinf -
        crit_60to69[i,t-1]/Tcrit
      dcrit_70to79dt[i,t]<-(Rcrit*comorbid_hosp_over60[i])*I_70to79[i,t-1]/Tinf -
        crit_70to79[i,t-1]/Tcrit
      dcrit_80plusdt[i,t]<-(Rcrit*comorbid_hosp_over60[i])*I_80plus[i,t-1]/Tinf -
        crit_80plus[i,t-1]/Tcrit
      
      dG_10to19dt[i,t]<-Rdeath_10to19*comorbid_death[i]*crit_10to19[i,t-1]/Tcrit-
        G_10to19[i,t-1]/Tdeath
      dG_20to39dt[i,t]<-Rdeath_20to39*comorbid_death[i]*crit_20to39[i,t-1]/Tcrit-
        G_20to39[i,t-1]/Tdeath
      dG_40to49dt[i,t]<-Rdeath_40to49*comorbid_death[i]*crit_40to49[i,t-1]/Tcrit-
        G_40to49[i,t-1]/Tdeath
      dG_50to59dt[i,t]<-Rdeath_50to59*comorbid_death[i]*crit_50to59[i,t-1]/Tcrit-
        G_50to59[i,t-1]/Tdeath
      dG_60to69dt[i,t]<-Rdeath_60to69*comorbid_death_over60[i]*crit_60to69[i,t-1]/Tcrit-
        G_60to69[i,t-1]/Tdeath
      dG_70to79dt[i,t]<-Rdeath_70to79*comorbid_death_over60[i]*crit_70to79[i,t-1]/Tcrit-
        G_70to79[i,t-1]/Tdeath
      dG_80plusdt[i,t]<-Rdeath_80plus*comorbid_death_over60[i]*crit_80plus[i,t-1]/Tcrit-
        G_80plus[i,t-1]/Tdeath
      
      dQ_10to19dt[i,t]<-(1-Rdeath_10to19*comorbid_death[i])*crit_10to19[i,t-1]/Tcrit-
        Q_10to19[i,t-1]/Trecovcrit
      dQ_20to39dt[i,t]<-(1-Rdeath_20to39*comorbid_death[i])*crit_20to39[i,t-1]/Tcrit-
        Q_20to39[i,t-1]/Trecovcrit
      dQ_40to49dt[i,t]<-(1-Rdeath_40to49*comorbid_death[i])*crit_40to49[i,t-1]/Tcrit-
        Q_40to49[i,t-1]/Trecovcrit
      dQ_50to59dt[i,t]<-(1-Rdeath_50to59*comorbid_death[i])*crit_50to59[i,t-1]/Tcrit-
        Q_50to59[i,t-1]/Trecovcrit
      dQ_60to69dt[i,t]<-(1-Rdeath_60to69*comorbid_death_over60[i])*crit_60to69[i,t-1]/Tcrit-
        Q_60to69[i,t-1]/Trecovcrit
      dQ_70to79dt[i,t]<-(1-Rdeath_70to79*comorbid_death_over60[i])*crit_70to79[i,t-1]/Tcrit-
        Q_70to79[i,t-1]/Trecovcrit
      dQ_80plusdt[i,t]<-(1-Rdeath_80plus*comorbid_death_over60[i])*crit_80plus[i,t-1]/Tcrit-
        Q_80plus[i,t-1]/Trecovcrit
      
      
      dDdt[i,t]<-G_10to19[i,t-1]/Tdeath +
        G_20to39[i,t-1]/Tdeath +
        G_40to49[i,t-1]/Tdeath +
        G_50to59[i,t-1]/Tdeath +
        G_60to69[i,t-1]/Tdeath +
        G_70to79[i,t-1]/Tdeath +
        G_80plus[i,t-1]/Tdeath
      
      
      #S[i,t]<-S[i,t-1]+dSdt[i,t]
      S_10to19[i,t]<-S_10to19[i,t-1]+dS_10to19dt[i,t]
      S_20to39[i,t]<-S_20to39[i,t-1]+dS_20to39dt[i,t]
      S_40to49[i,t]<-S_40to49[i,t-1]+dS_40to49dt[i,t]
      S_50to59[i,t]<-S_50to59[i,t-1]+dS_50to59dt[i,t]
      S_60to69[i,t]<-S_60to69[i,t-1]+dS_60to69dt[i,t]
      S_70to79[i,t]<-S_70to79[i,t-1]+dS_70to79dt[i,t]
      S_80plus[i,t]<-S_80plus[i,t-1]+dS_80plusdt[i,t]
      #E[i,t]<-E[i,t-1]+dEdt[i,t]
      E_10to19[i,t]<-E_10to19[i,t-1]+dE_10to19dt[i,t]
      E_20to39[i,t]<-E_20to39[i,t-1]+dE_20to39dt[i,t]
      E_40to49[i,t]<-E_40to49[i,t-1]+dE_40to49dt[i,t]
      E_50to59[i,t]<-E_50to59[i,t-1]+dE_50to59dt[i,t]
      E_60to69[i,t]<-E_60to69[i,t-1]+dE_60to69dt[i,t]
      E_70to79[i,t]<-E_70to79[i,t-1]+dE_70to79dt[i,t]
      E_80plus[i,t]<-E_80plus[i,t-1]+dE_80plusdt[i,t]
      #In[i,t]<-In[i,t-1]+dIdt[i,t]
      I_10to19[i,t]<-I_10to19[i,t-1]+dI_10to19dt[i,t]
      I_20to39[i,t]<-I_20to39[i,t-1]+dI_20to39dt[i,t]
      I_40to49[i,t]<-I_40to49[i,t-1]+dI_40to49dt[i,t]
      I_50to59[i,t]<-I_50to59[i,t-1]+dI_50to59dt[i,t]
      I_60to69[i,t]<-I_60to69[i,t-1]+dI_60to69dt[i,t]
      I_70to79[i,t]<-I_70to79[i,t-1]+dI_70to79dt[i,t]
      I_80plus[i,t]<-I_80plus[i,t-1]+dI_80plusdt[i,t]
      
      
      R[i,t]<-R[i,t-1]+dRdt[i,t]
      
      #H[i,t]<-H[i,t-1]+dHdt[i,t]
      G_10to19[i,t]<-G_10to19[i,t-1]+dG_10to19dt[i,t]
      G_20to39[i,t]<-G_20to39[i,t-1]+dG_20to39dt[i,t]
      G_40to49[i,t]<-G_40to49[i,t-1]+dG_40to49dt[i,t]
      G_50to59[i,t]<-G_50to59[i,t-1]+dG_50to59dt[i,t]
      G_60to69[i,t]<-G_60to69[i,t-1]+dG_60to69dt[i,t]
      G_70to79[i,t]<-G_70to79[i,t-1]+dG_70to79dt[i,t]
      G_80plus[i,t]<-G_80plus[i,t-1]+dG_80plusdt[i,t]
      
      Q_10to19[i,t]<-Q_10to19[i,t-1]+dQ_10to19dt[i,t]
      Q_20to39[i,t]<-Q_20to39[i,t-1]+dQ_20to39dt[i,t]
      Q_40to49[i,t]<-Q_40to49[i,t-1]+dQ_40to49dt[i,t]
      Q_50to59[i,t]<-Q_50to59[i,t-1]+dQ_50to59dt[i,t]
      Q_60to69[i,t]<-Q_60to69[i,t-1]+dQ_60to69dt[i,t]
      Q_70to79[i,t]<-Q_70to79[i,t-1]+dQ_70to79dt[i,t]
      Q_80plus[i,t]<-Q_80plus[i,t-1]+dQ_80plusdt[i,t]
      
      H_10to19[i,t]<-H_10to19[i,t-1]+dH_10to19dt[i,t]
      H_20to39[i,t]<-H_20to39[i,t-1]+dH_20to39dt[i,t]
      H_40to49[i,t]<-H_40to49[i,t-1]+dH_40to49dt[i,t]
      H_50to59[i,t]<-H_50to59[i,t-1]+dH_50to59dt[i,t]
      H_60to69[i,t]<-H_60to69[i,t-1]+dH_60to69dt[i,t]
      H_70to79[i,t]<-H_70to79[i,t-1]+dH_70to79dt[i,t]
      H_80plus[i,t]<-H_80plus[i,t-1]+dH_80plusdt[i,t]
      
      
      
      D[i,t]<-D[i,t-1]+dDdt[i,t]
      
      crit_10to19[i,t]<-crit_10to19[i,t-1]+dcrit_10to19dt[i,t]
      crit_20to39[i,t]<-crit_20to39[i,t-1]+dcrit_20to39dt[i,t]
      crit_40to49[i,t]<-crit_40to49[i,t-1]+dcrit_40to49dt[i,t]
      crit_50to59[i,t]<-crit_50to59[i,t-1]+dcrit_50to59dt[i,t]
      crit_60to69[i,t]<-crit_60to69[i,t-1]+dcrit_60to69dt[i,t]
      crit_70to79[i,t]<-crit_70to79[i,t-1]+dcrit_70to79dt[i,t]
      crit_80plus[i,t]<-crit_80plus[i,t-1]+dcrit_80plusdt[i,t]
      
      # Cumulative values
      Ecum_10to19[i,t]<-Ecum_10to19[i,t-1]-dS_10to19dt[i,t]
      Ecum_20to39[i,t]<-Ecum_20to39[i,t-1]-dS_20to39dt[i,t]
      Ecum_40to49[i,t]<-Ecum_40to49[i,t-1]-dS_40to49dt[i,t]
      Ecum_50to59[i,t]<-Ecum_50to59[i,t-1]-dS_50to59dt[i,t]
      Ecum_60to69[i,t]<-Ecum_60to69[i,t-1]-dS_60to69dt[i,t]
      Ecum_70to79[i,t]<-Ecum_70to79[i,t-1]-dS_70to79dt[i,t]
      Ecum_80plus[i,t]<-Ecum_80plus[i,t-1]-dS_80plusdt[i,t]}
    
      Icum_10to19[i,t]<-Icum_10to19[i,t-1]+E_10to19[i,t-1]/Tinc
      Icum_20to39[i,t]<-Icum_20to39[i,t-1]+E_20to39[i,t-1]/Tinc
      Icum_40to49[i,t]<-Icum_40to49[i,t-1]+E_40to49[i,t-1]/Tinc
      Icum_50to59[i,t]<-Icum_50to59[i,t-1]+E_50to59[i,t-1]/Tinc
      Icum_60to69[i,t]<-Icum_60to69[i,t-1]+E_60to69[i,t-1]/Tinc
      Icum_70to79[i,t]<-Icum_70to79[i,t-1]+E_70to79[i,t-1]/Tinc
      Icum_80plus[i,t]<-Icum_80plus[i,t-1]+E_80plus[i,t-1]/Tinc
      
      Hcum_10to19[i,t]<-Hcum_10to19[i,t-1]+(Rhosp_10to19)*comorbid_hosp[i]*I_10to19[i,t-1]/Tinf
      Hcum_20to39[i,t]<-Hcum_20to39[i,t-1]+(Rhosp_20to39)*comorbid_hosp[i]*I_20to39[i,t-1]/Tinf
      Hcum_40to49[i,t]<-Hcum_40to49[i,t-1]+(Rhosp_40to49)*comorbid_hosp[i]*I_40to49[i,t-1]/Tinf
      Hcum_50to59[i,t]<-Hcum_50to59[i,t-1]+(Rhosp_50to59)*comorbid_hosp[i]*I_50to59[i,t-1]/Tinf
      Hcum_60to69[i,t]<-Hcum_60to69[i,t-1]+(Rhosp_60to69)*comorbid_hosp[i]*I_60to69[i,t-1]/Tinf
      Hcum_70to79[i,t]<-Hcum_70to79[i,t-1]+(Rhosp_70to79)*comorbid_hosp[i]*I_70to79[i,t-1]/Tinf
      Hcum_80plus[i,t]<-Hcum_80plus[i,t-1]+(Rhosp_80plus)*comorbid_hosp[i]*I_80plus[i,t-1]/Tinf
      
      critcum_10to19[i,t]<-critcum_10to19[i,t-1]+Rcrit*comorbid_hosp[i]*I_10to19[i,t-1]/Tinf
      critcum_20to39[i,t]<-critcum_20to39[i,t-1]+Rcrit*comorbid_hosp[i]*I_20to39[i,t-1]/Tinf
      critcum_40to49[i,t]<-critcum_40to49[i,t-1]+Rcrit*comorbid_hosp[i]*I_40to49[i,t-1]/Tinf
      critcum_50to59[i,t]<-critcum_50to59[i,t-1]+Rcrit*comorbid_hosp[i]*I_50to59[i,t-1]/Tinf
      critcum_60to69[i,t]<-critcum_60to69[i,t-1]+Rcrit*comorbid_hosp[i]*I_60to69[i,t-1]/Tinf
      critcum_70to79[i,t]<-critcum_70to79[i,t-1]+Rcrit*comorbid_hosp[i]*I_70to79[i,t-1]/Tinf
      critcum_80plus[i,t]<-critcum_80plus[i,t-1]+Rcrit*comorbid_hosp[i]*I_80plus[i,t-1]/Tinf
  }
}  


if (model_by_age==1) {
  In=I_10to19+I_20to39+I_40to49+I_50to59+I_60to69+I_70to79+I_80plus
  E=E_10to19+E_20to39+E_40to49+E_50to59+E_60to69+E_70to79+E_80plus
  S=S_10to19+S_20to39+S_40to49+S_50to59+S_60to69+S_70to79+S_80plus
  crit=crit_10to19+crit_20to39+crit_40to49+crit_50to59+crit_60to69+crit_70to79+crit_80plus+
    Q_10to19+Q_20to39+Q_40to49+Q_50to59+Q_60to69+Q_70to79+Q_80plus+
    G_10to19+G_20to39+G_40to49+G_50to59+G_60to69+G_70to79+G_80plus
  H=H_10to19+H_20to39+H_40to49+H_50to59+H_60to69+H_70to79+H_80plus+crit
  Icum=Icum_10to19+Icum_20to39+Icum_40to49+Icum_50to59+Icum_60to69+Icum_70to79+Icum_80plus
  Hcum=Hcum_10to19+Hcum_20to39+Hcum_40to49+Hcum_50to59+Hcum_60to69+Hcum_70to79+Hcum_80plus
  Ecum=Ecum_10to19+Ecum_20to39+Ecum_40to49+Ecum_50to59+Ecum_60to69+Ecum_70to79+Ecum_80plus
  critcum=critcum_10to19+critcum_20to39+critcum_40to49+critcum_50to59+critcum_60to69+
    critcum_70to79+critcum_80plus
} else if (model_by_age==0) {
  crit<-crit+G+Q
  H<-H+crit
}

#------------------- this stuff is for plotting indiana counties, you'll probably
# want to comment it out -------------------------#
df.Marion<-data.frame("I"=In[49,],"H"=H[49,],"crit"=crit[49,],"D"=D[49,],"t"=(1:maxt))#"G"=G[49,],
                     # "Q"=Q[49,],"S"=S[49,],"E"=E[49,])
df.Monroe<-data.frame("I"=In[53,],"H"=H[53,],"crit"=crit[53,],"D"=D[53,],"t"=(1:maxt))
df.Lake<-data.frame("I"=In[45,],"H"=H[45,],"crit"=crit[45,],"D"=D[45,],"t"=(1:maxt))

print(ggplot(df.Marion,aes(x=t))+
        geom_line(aes(y=I,color="infected"))+
        geom_line(aes(y=H/0.95,color="hospitalized"))+
        geom_line(aes(y=crit,color="critical"))+
        geom_line(aes(y=D,color="deceased"))+
        #geom_line(aes(y=Q,color="Q"))+
        #geom_line(aes(y=G,color="G"))+
        ggtitle(tit1)+
        scale_x_continuous(breaks=seq(0,300,30),name="days")+
        scale_y_continuous(breaks=seq(0,190000,2000),name="number")+
        theme(axis.text.x=element_text(angle=90)) )
ggsave(paste(outdir,pic1,sep=""))

# print(ggplot(df.Monroe,aes(x=t))+
#         geom_line(aes(y=I,color="infected"))+
#         geom_line(aes(y=H/0.95,color="hospitalized"))+
#         geom_line(aes(y=crit,color="critical"))+
#         geom_line(aes(y=D,color="deceased"))+
#         ggtitle(tit2)+
#         scale_x_continuous(breaks=seq(0,300,30),name="days")+
#         scale_y_continuous(breaks=seq(0,80000,2000),name="number")+
#         theme(axis.text.x=element_text(angle=90)) )
# ggsave(pic2)
# 
# print(ggplot(df.Lake,aes(x=t))+
#         geom_line(aes(y=I,color="infected"))+
#         geom_line(aes(y=H/0.95,color="hospitalized"))+
#         geom_line(aes(y=crit,color="critical"))+
#         geom_line(aes(y=D,color="deceased"))+
#         ggtitle(tit3)+
#         scale_x_continuous(breaks=seq(0,300,30),name="days")+
#         scale_y_continuous(breaks=seq(0,80000,2000),name="number")+
#         theme(axis.text.x=element_text(angle=90)) )
# ggsave(pic3)


#######################################################
# output files
if (output==1) {

  countycol<-rep(county_names[1],maxt)
  daycol<-seq(1,maxt)

  df1<-data.frame("County"=countycol,"Day"=daycol,
                 "Susceptible"=round(S[1,]),
                 "Exposed"=round(E[1,]),
                 "ExposedCumulative"=round(Ecum[1,]),
                 "InfectedNotHospitalized"=round(In[1,]),
                 "InfectedCumulative"=round(Icum[1,]),
                 "Hospitalized"=round(H[1,]),
                 "HospitalizedCumulative"=round(Hcum[1,]),
                 "Critical"=round(crit[1,]),
                 "CriticalCumulative"=round(critcum[1,]),
                 "Deceased"=round(D[1,]))

  for (i in 2:ncounties) {
  
    countycol<-rep(county_names[i],maxt)
    df2<-data.frame("County"=countycol,"Day"=daycol,
                    "Susceptible"=round(S[i,]),
                    "Exposed"=round(E[i,]),
                    "ExposedCumulative"=round(Ecum[i,]),
                    "InfectedNotHospitalized"=round(In[i,]),
                    "InfectedCumulative"=round(Icum[i,]),
                    "Hospitalized"=round(H[i,]),
                    "HospitalizedCumulative"=round(Hcum[i,]),
                    "Critical"=round(crit[i,]),
                    "CriticalCumulative"=round(critcum[i,]),
                    "Deceased"=round(D[i,]))
    df1<-df1%>%bind_rows(df2)
  
  }

  write_csv(df1,paste(outdir,model_outfile,sep=""))

  sink(paste(outdir,param_outfile,sep=""))
  cat(paste("outfile=",model_outfile))
  cat("\n")
  cat(paste("R0=",R0))
  cat("\n")
  cat(paste("intervention_R_rdxn=",intervention_R_rdxn))
  cat("\n")
  cat(paste("intervention_time=",intervention_time))
  cat("\n")
  cat(paste("lift_time=",lift_time))
  cat("\n")
  cat(paste("Pinf=",Pinf))
  cat("\n")
  cat(paste("Rdeath=",Rdeath*Rcrit))
  cat("\n")
  cat(paste("Rhosp=",Rhosp))
  cat("\n")
  cat(paste("Trecov=",Trecov))
  cat("\n")
  #cat(paste("Thosp=",Thosp))
  cat("\n")
  cat(paste("Tdeath=",Tdeath))
  cat("\n")
  cat(paste("Rcrit=",Rcrit))
  cat("\n")
  cat(paste("Tinc=",Tinc))
  cat("\n")
  cat(paste("Tinf=",Tinf))
  cat("\n")
  cat(paste("maxt=",maxt))
  cat("\n")
  cat(paste("model_by_age=",model_by_age))
  cat("\n")
  cat(paste("Rdeath_10to19=",Rdeath_10to19*Rhosp_10to19))
  cat("\n")
  cat(paste("Rdeath_20to39=",Rdeath_20to39*Rhosp_20to39))
  cat("\n")
  cat(paste("Rdeath_40to49=",Rdeath_40to49*Rhosp_40to49))
  cat("\n")
  cat(paste("Rdeath_50to59=",Rdeath_50to59*Rhosp_50to59))
  cat("\n")
  cat(paste("Rdeath_60to69=",Rdeath_60to69*Rhosp_60to69))
  cat("\n")
  cat(paste("Rdeath_70to79=",Rdeath_70to79*Rhosp_70to79))
  cat("\n")
  cat(paste("Rdeath_80plus=",Rdeath_80plus*Rhosp_80plus))
  cat("\n")
  cat(paste("Rhosp_10to19=",Rhosp_10to19))
  cat("\n")
  cat(paste("Rhosp_20to39=",Rhosp_20to39))
  cat("\n")
  cat(paste("Rhosp_40to49=",Rhosp_40to49))
  cat("\n")
  cat(paste("Rhosp_50to59=",Rhosp_50to59))
  cat("\n")
  cat(paste("Rhosp_60to69=",Rhosp_60to69))
  cat("\n")
  cat(paste("Rhosp_70to79=",Rhosp_70to79))
  cat("\n")
  cat(paste("Rhosp_80plus=",Rhosp_80plus))
  cat("\n")
  cat(paste("model_comorbidities=",model_comorbidities))
  cat("\n")
  cat(paste("Rhosp_diabetes=",Rhosp_diabetes))
  cat("\n")
  cat(paste("Rhosp_heartdisease=",Rhosp_heartdisease))
  cat("\n")
  cat(paste("Rhosp_hypertension=",Rhosp_hypertension))
  cat("\n")
  cat(paste("Rhosp_maligneoplasm=",Rhosp_maligneoplasm))
  cat("\n")
  cat(paste("Rhosp_copd=",Rhosp_copd))
  cat("\n")
  cat(paste("Rdeath_diabetes=",Rdeath_diabetes))
  cat("\n")
  cat(paste("Rdeath_heartdisease=",Rdeath_heartdisease))
  cat("\n")
  cat(paste("Rdeath_hypertension=",Rdeath_hypertension))
  cat("\n")
  cat(paste("Rdeath_maligneoplasm=",Rdeath_maligneoplasm))
  cat("\n")
  cat(paste("Rdeath_copd=",Rdeath_copd))
  cat("\n")
  
  
  cat(paste(param_notes))
  sink()
  
}

# } # comment these out for not doing a mass scenario run
# }
# }
# }