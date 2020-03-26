###################################################################
# MODEL CONFIGURATION
#
# Configuration variables for running the model
###################################################################
#
datadir<-("../../COVID19Response/")
outdir<-datadir

# NPI timing
intervention_time<-50 # days after day 0
lift_time<-300 # days after day 0 that NPI is ceased

########## epidemiological variables
R0<-2.2 # base reproduction rate
intervention_R_rdxn<-0.25 # amount by which R0 is reduced by NPI
Tinc<-5 # incubation time (days)
Tinf<-3 # time of infection before either hospitalization or recovery
Thosp<-23 # time in hospitalization until recovery
Tdeath<-18 # time in hospital until death
Pinf<-1.0 # max proportion of population to get infected

nage=7 # number of age groups
age60<-5 # index of age group that is sixty
Rdeath<-c(seq(0,nage)) # set up Rdeath array
# Rdeath[1]<-0.02
Rdeath[1]<-0.002 # Death rate per infected for age groups...10-19 OR if nage==1 (no age groups) this is Rdeath
Rdeath[2]<-0.002  # 20 to 39
Rdeath[3]<-0.004  # 40 to 49
Rdeath[4]<-0.013  # 50 to 59
Rdeath[5]<-0.036  # 60 to 69
Rdeath[6]<-0.080  # 70 to 79
Rdeath[7]<-0.148  # 80 plus
Rhosp<-c(seq(0,nage))
# Rhosp[1]<-0.15
Rhosp[1]<-0.001 # Hospitalization rate per infected for age groups
Rhosp[2]<-0.100
Rhosp[3]<-0.100
Rhosp[4]<-0.100
Rhosp[5]<-0.223
Rhosp[6]<-0.223
Rhosp[7]<-0.223

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

Rcrit=0.05 # proportion to be critically hospitalized

maxt=301 # max time
ncounties=92 # number of counties

output<-1 # produce output? 0=no, 1=yes.  Output produces is a csv with all of
# the values as columns and a file containing all of the parameter settings
model_outfile<-"test.csv" # name of model output
param_outfile<-"test_params.txt" # name of parameter file
#notes to include in parameter text file
param_notes<-"No county-county transmission, all counties seeded with one case."
