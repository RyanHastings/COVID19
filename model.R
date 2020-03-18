rm(list=ls())

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(haven)

R0<-2.2 # rate of infection for exposed people
intervention_R_rdxn<-0.0 # R after intervention
intervention_time<-40 # intervention time (days)
lift_time<-300
Pinf<-1.0 # proportion we expect to be nonresistant

Rdeath<-0.02 # death rate per infected
Rhosp<-0.15 # hospitalization rate per infected
Trecov<-23; # time (days) between indeterminate hospitalization and recovery
Thosp=5; # time of indeterminate hospitalization
Tdeath=27; # time (days) between indeterminate hospitalization and death
crit_rate=0.05; # % of hospitalizations that are critical

Tinf<-2.9; # duration of infection (not including hospitalization)
Tinc<-5.2; # duration of incubation

model_by_age<-1;
Rdeath_10to19<-0.002
Rdeath_20to39<-0.002
Rdeath_40to49<-0.004
Rdeath_50to59<-0.013
Rdeath_60to69<-0.036
Rdeath_70to79<-0.080
Rdeath_80plus<-0.148
Rhosp_10to19<-0.001
Rhosp_20to39<-0.100
Rhosp_40to49<-0.100
Rhosp_50to59<-0.100
Rhosp_60to69<-0.223
Rhosp_70to79<-0.223
Rhosp_80plus<-0.223

maxt<-300

model_outfile<-"R0_2.2_Rdxn_0.5_Rd_2.csv"
param_outfile<-"R0_2.2_Rdxn_0.5_Rd_2_param.txt"
param_notes<-"No county-county transmission, all counties seeded with one case."
output<-0 # 1 for yes, 0 for no
pic1<-"Marion_R2.2_50p.png"
tit1<-"Marion, R0=2.2, 50% reduction"
pic2<-"Monre_R2.2_50p.png"
tit2<-"Monroe, R0=2.2, 50% reduction"
pic3<-"Lake_R2.2_no_50p.png"
tit3<-"Lake, R0=2.2, 50% reduction"
################################################
# set initial cases
if (model_by_age==0) {
  In<-matrix(0,nrow=92,ncol=maxt)
  for (i in 1:92) {
    In[i,1]=1
    In[i,2:maxt]=0
  }
} else if (model_by_age==1) {
  I_10to19<-matrix(0.0,nrow=92,ncol=maxt)
  I_20to39<-matrix(0.0,nrow=92,ncol=maxt)
  I_40to49<-matrix(0.0,nrow=92,ncol=maxt)
  I_50to59<-matrix(0.0,nrow=92,ncol=maxt)
  I_60to69<-matrix(0.0,nrow=92,ncol=maxt)
  I_70to79<-matrix(0.0,nrow=92,ncol=maxt)
  I_80plus<-matrix(0.0,nrow=92,ncol=maxt)
  for (i in 1:92) {
    
    I_10to19[i,1]=0
  
    
    I_20to39[i,1]=1
    
    
  }
}

################################################
# set county-to-county transmission rates
M<-matrix(0.0,nrow=92,ncol=92)
for (i in 1:92) { # v1: assume no transmission between counties
  for (j in 1:92) {
    if (i==j) {
      M[i,j]=1.0
    } else{
      M[i,j]=0.0
    }
  }
}

#################################################
# get county populations
US.pop.raw<-read_csv('PEP_2018_PEPAGESEX/PEP_2018_PEPAGESEX_with_ann.csv')
IN.pop.raw<-US.pop.raw%>%filter(grepl(", Indiana",`GEO.display-label`))
county_names<-IN.pop.raw$`GEO.display-label`

if (model_by_age==0) {
  S<-matrix(0,nrow=92,ncol=maxt)
  for (i in 1:92) {
    S[i,1]<-as.numeric(IN.pop.raw$est42010sex0_age999[i])*Pinf
    S[i,2:maxt]<-0
  }
} else if (model_by_age==1) {
  
  S_10to19<-matrix(0,nrow=92,ncol=maxt)
  S_20to39<-matrix(0,nrow=92,ncol=maxt)
  S_40to49<-matrix(0,nrow=92,ncol=maxt)
  S_50to59<-matrix(0,nrow=92,ncol=maxt)
  S_60to69<-matrix(0,nrow=92,ncol=maxt)
  S_70to79<-matrix(0,nrow=92,ncol=maxt)
  S_80plus<-matrix(0,nrow=92,ncol=maxt)
  
  for (i in 1:92) {
#    print(i)
    
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
# initialize other values
if (model_by_age==0) {
  dSdt<-matrix(0.0,nrow=92,ncol=maxt)

  E<-matrix(0,nrow=92,ncol=maxt)
  dEdt<-matrix(0.0,nrow=92,ncol=maxt)

  dIdt<-matrix(0.0,nrow=92,ncol=maxt)

  R<-matrix(0,nrow=92,ncol=maxt)
  dRdt<-matrix(0,nrow=92,ncol=maxt)

  H<-matrix(0,nrow=92,ncol=maxt)
  dHdt<-matrix(0,nrow=92,ncol=maxt)

  D<-matrix(0,nrow=92,ncol=maxt)
  dDdt<-matrix(0,nrow=92,ncol=maxt)

  Rdeath<-Rdeath/Rhosp

  crit<-matrix(0.0,nrow=92,ncol=maxt)

  Ecum<-matrix(0.0,nrow=92,ncol=maxt)
  Icum<-matrix(0.0,nrow=92,ncol=maxt)
  Hcum<-matrix(0.0,nrow=92,ncol=maxt)
  critcum<-matrix(0.0,nrow=92,ncol=maxt)

} else if (model_by_age==1) {
  
  dS_10to19dt<-matrix(0.0,nrow=92,ncol=maxt)
  dS_20to39dt<-matrix(0.0,nrow=92,ncol=maxt)
  dS_40to49dt<-matrix(0.0,nrow=92,ncol=maxt)
  dS_50to59dt<-matrix(0.0,nrow=92,ncol=maxt)
  dS_60to69dt<-matrix(0.0,nrow=92,ncol=maxt)
  dS_70to79dt<-matrix(0.0,nrow=92,ncol=maxt)
  dS_80plusdt<-matrix(0.0,nrow=92,ncol=maxt)
  
  E_10to19<-matrix(0.0,nrow=92,ncol=maxt)
  E_20to39<-matrix(0.0,nrow=92,ncol=maxt)
  E_40to49<-matrix(0.0,nrow=92,ncol=maxt)
  E_50to59<-matrix(0.0,nrow=92,ncol=maxt)
  E_60to69<-matrix(0.0,nrow=92,ncol=maxt)
  E_70to79<-matrix(0.0,nrow=92,ncol=maxt)
  E_80plus<-matrix(0.0,nrow=92,ncol=maxt)
  
  dE_10to19dt<-matrix(0.0,nrow=92,ncol=maxt)
  dE_20to39dt<-matrix(0.0,nrow=92,ncol=maxt)
  dE_40to49dt<-matrix(0.0,nrow=92,ncol=maxt)
  dE_50to59dt<-matrix(0.0,nrow=92,ncol=maxt)
  dE_60to69dt<-matrix(0.0,nrow=92,ncol=maxt)
  dE_70to79dt<-matrix(0.0,nrow=92,ncol=maxt)
  dE_80plusdt<-matrix(0.0,nrow=92,ncol=maxt)
  
  dI_10to19dt<-matrix(0.0,nrow=92,ncol=maxt)
  dI_20to39dt<-matrix(0.0,nrow=92,ncol=maxt)
  dI_40to49dt<-matrix(0.0,nrow=92,ncol=maxt)
  dI_50to59dt<-matrix(0.0,nrow=92,ncol=maxt)
  dI_60to69dt<-matrix(0.0,nrow=92,ncol=maxt)
  dI_70to79dt<-matrix(0.0,nrow=92,ncol=maxt)
  dI_80plusdt<-matrix(0.0,nrow=92,ncol=maxt)

  R<-matrix(0,nrow=92,ncol=maxt)
  dRdt<-matrix(0,nrow=92,ncol=maxt)
  
  H_10to19<-matrix(0.0,nrow=92,ncol=maxt)
  H_20to39<-matrix(0.0,nrow=92,ncol=maxt)
  H_40to49<-matrix(0.0,nrow=92,ncol=maxt)
  H_50to59<-matrix(0.0,nrow=92,ncol=maxt)
  H_60to69<-matrix(0.0,nrow=92,ncol=maxt)
  H_70to79<-matrix(0.0,nrow=92,ncol=maxt)
  H_80plus<-matrix(0.0,nrow=92,ncol=maxt)
  
  dH_10to19dt<-matrix(0.0,nrow=92,ncol=maxt)
  dH_20to39dt<-matrix(0.0,nrow=92,ncol=maxt)
  dH_40to49dt<-matrix(0.0,nrow=92,ncol=maxt)
  dH_50to59dt<-matrix(0.0,nrow=92,ncol=maxt)
  dH_60to69dt<-matrix(0.0,nrow=92,ncol=maxt)
  dH_70to79dt<-matrix(0.0,nrow=92,ncol=maxt)
  dH_80plusdt<-matrix(0.0,nrow=92,ncol=maxt)
  
  D<-matrix(0,nrow=92,ncol=maxt)
  dDdt<-matrix(0,nrow=92,ncol=maxt)
  
  Rdeath_10to19<-Rdeath_10to19/Rhosp_10to19
  Rdeath_20to39<-Rdeath_20to39/Rhosp_20to39
  Rdeath_40to49<-Rdeath_40to49/Rhosp_40to49
  Rdeath_50to59<-Rdeath_50to59/Rhosp_50to59
  Rdeath_60to69<-Rdeath_60to69/Rhosp_60to69
  Rdeath_70to79<-Rdeath_70to79/Rhosp_70to79
  Rdeath_80plus<-Rdeath_80plus/Rhosp_80plus

  crit_10to19<-matrix(0.0,nrow=92,ncol=maxt)
  crit_20to39<-matrix(0.0,nrow=92,ncol=maxt)
  crit_40to49<-matrix(0.0,nrow=92,ncol=maxt)
  crit_50to59<-matrix(0.0,nrow=92,ncol=maxt)
  crit_60to69<-matrix(0.0,nrow=92,ncol=maxt)
  crit_70to79<-matrix(0.0,nrow=92,ncol=maxt)
  crit_80plus<-matrix(0.0,nrow=92,ncol=maxt)
  
  Ecum_10to19<-matrix(0.0,nrow=92,ncol=maxt)
  Ecum_20to39<-matrix(0.0,nrow=92,ncol=maxt)
  Ecum_40to49<-matrix(0.0,nrow=92,ncol=maxt)
  Ecum_50to59<-matrix(0.0,nrow=92,ncol=maxt)
  Ecum_60to69<-matrix(0.0,nrow=92,ncol=maxt)
  Ecum_70to79<-matrix(0.0,nrow=92,ncol=maxt)
  Ecum_80plus<-matrix(0.0,nrow=92,ncol=maxt)
  
  Icum_10to19<-matrix(0.0,nrow=92,ncol=maxt)
  Icum_20to39<-matrix(0.0,nrow=92,ncol=maxt)
  Icum_40to49<-matrix(0.0,nrow=92,ncol=maxt)
  Icum_50to59<-matrix(0.0,nrow=92,ncol=maxt)
  Icum_60to69<-matrix(0.0,nrow=92,ncol=maxt)
  Icum_70to79<-matrix(0.0,nrow=92,ncol=maxt)
  Icum_80plus<-matrix(0.0,nrow=92,ncol=maxt)
  
  Hcum_10to19<-matrix(0.0,nrow=92,ncol=maxt)
  Hcum_20to39<-matrix(0.0,nrow=92,ncol=maxt)
  Hcum_40to49<-matrix(0.0,nrow=92,ncol=maxt)
  Hcum_50to59<-matrix(0.0,nrow=92,ncol=maxt)
  Hcum_60to69<-matrix(0.0,nrow=92,ncol=maxt)
  Hcum_70to79<-matrix(0.0,nrow=92,ncol=maxt)
  Hcum_80plus<-matrix(0.0,nrow=92,ncol=maxt)
  
  critcum_10to19<-matrix(0.0,nrow=92,ncol=maxt)
  critcum_20to39<-matrix(0.0,nrow=92,ncol=maxt)
  critcum_40to49<-matrix(0.0,nrow=92,ncol=maxt)
  critcum_50to59<-matrix(0.0,nrow=92,ncol=maxt)
  critcum_60to69<-matrix(0.0,nrow=92,ncol=maxt)
  critcum_70to79<-matrix(0.0,nrow=92,ncol=maxt)
  critcum_80plus<-matrix(0.0,nrow=92,ncol=maxt)
  
  dcrit_10to19dt<-matrix(0.0,nrow=92,ncol=maxt)
  dcrit_20to39dt<-matrix(0.0,nrow=92,ncol=maxt)
  dcrit_40to49dt<-matrix(0.0,nrow=92,ncol=maxt)
  dcrit_50to59dt<-matrix(0.0,nrow=92,ncol=maxt)
  dcrit_60to69dt<-matrix(0.0,nrow=92,ncol=maxt)
  dcrit_70to79dt<-matrix(0.0,nrow=92,ncol=maxt)
  dcrit_80plusdt<-matrix(0.0,nrow=92,ncol=maxt)
  
}
  
#################################################
# run the model

for (t in 2:maxt) {
  
  if (t<intervention_time & t>lift_time) {
    Rt<-R0
  } else {
    Rt<-(1-intervention_R_rdxn)*R0
  }
  
  if (model_by_age==0) {
    for (i in 1:92) {
    
      dSdt[i,t]<-0
      dEdt[i,t]<-0
      for (j in 1:92) {
        dSdt[i,t]<-dSdt[i,t]-Rt*M[i,j]*In[j,t-1]*S[i,t-1]/(S[i,1]*Tinf)
      #  dEdt[i,t]<-dEdt[i,t]+Rt*M[i,j]*In[j,t-1]*S[i,t-1]/(S[i,1]*Tinf)
      }
      if (dSdt[i,t]+S[i,t-1]<0) {
        dSdt[i,t]<--S[i,t-1]
      }
      dEdt[i,t]<--dSdt[i,t]-E[i,t-1]/Tinc
      #dIdt[i,t]<-E[i,t-1]/Tinc-In[i,t-1]/Tinf
      #total_infected[i]<-total_infected[i]+E[i,t-1]/Tinc
      #dRdt[i,t]<-In[i,t-1]/Tinf
    
      dIdt[i,t]<-E[i,t-1]/Tinc-(1-Rhosp)*In[i,t-1]/Tinf-Rhosp*In[i,t-1]/Thosp
      dHdt[i,t]<-Rhosp*In[i,t-1]/Thosp-(1-Rdeath)*H[i,t-1]/Trecov-Rdeath*H[i,t-1]/Tdeath
      dRdt[i,t]<-(1-Rhosp)*In[i,t-1]/Tinf+(1-Rdeath)*H[i,t-1]/Trecov
      dDdt[i,t]<-Rdeath*H[i,t-1]/Tdeath
    
    
      S[i,t]<-S[i,t-1]+dSdt[i,t]
      E[i,t]<-E[i,t-1]+dEdt[i,t]
      In[i,t]<-In[i,t-1]+dIdt[i,t]
      R[i,t]<-R[i,t-1]+dRdt[i,t]
    
      H[i,t]<-H[i,t-1]+dHdt[i,t]
      D[i,t]<-D[i,t-1]+dDdt[i,t]
      crit[i,t]<-crit_rate*In[i,t]
    
      # Cumulative values
      Ecum[i,t]<-Ecum[i,t-1]-dSdt[i,t]
      Icum[i,t]<-Icum[i,t-1]+E[i,t-1]/Tinc
      Hcum[i,t]<-Hcum[i,t-1]+Rhosp*In[i,t-1]/Thosp
    
    
      if (R[i,t]>S[i,1]) {
        print("Error: More recovered than original population.")
        stop()
      }
    }
  } else {
      
    for (i in 1:92) {
      
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
      for (j in 1:92) {
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
      
      #dIdt[i,t]<-E[i,t-1]/Tinc-(1-Rhosp)*In[i,t-1]/Tinf-Rhosp*In[i,t-1]/Thosp
      dI_10to19dt[i,t]<-E_10to19[i,t-1]/Tinc -
        (1-Rhosp_10to19)*I_10to19[i,t-1]/Tinf -
        Rhosp_10to19*I_10to19[i,t-1]/Thosp
      dI_20to39dt[i,t]<-E_20to39[i,t-1]/Tinc -
        (1-Rhosp_20to39)*I_20to39[i,t-1]/Tinf -
        Rhosp_20to39*I_20to39[i,t-1]/Thosp
      dI_40to49dt[i,t]<-E_40to49[i,t-1]/Tinc -
        (1-Rhosp_40to49)*I_40to49[i,t-1]/Tinf -
        Rhosp_40to49*I_40to49[i,t-1]/Thosp
      dI_50to59dt[i,t]<-E_50to59[i,t-1]/Tinc -
        (1-Rhosp_50to59)*I_50to59[i,t-1]/Tinf -
        Rhosp_50to59*I_50to59[i,t-1]/Thosp
      dI_60to69dt[i,t]<-E_60to69[i,t-1]/Tinc -
        (1-Rhosp_60to69)*I_60to69[i,t-1]/Tinf -
        Rhosp_60to69*I_60to69[i,t-1]/Thosp
      dI_70to79dt[i,t]<-E_70to79[i,t-1]/Tinc -
        (1-Rhosp_70to79)*I_70to79[i,t-1]/Tinf -
        Rhosp_70to79*I_70to79[i,t-1]/Thosp
      dI_80plusdt[i,t]<-E_80plus[i,t-1]/Tinc -
        (1-Rhosp_80plus)*I_80plus[i,t-1]/Tinf -
        Rhosp_80plus*I_80plus[i,t-1]/Thosp
      
      #dHdt[i,t]<-Rhosp*In[i,t-1]/Thosp-(1-Rdeath)*H[i,t-1]/Trecov-Rdeath*H[i,t-1]/Tdeath
      dH_10to19dt[i,t]<-Rhosp_10to19*I_10to19[i,t-1]/Thosp -
        (1-Rdeath_10to19)*H_10to19[i,t-1]/Trecov -
        Rdeath_10to19*H_10to19[i,t-1]/Tdeath
      dH_20to39dt[i,t]<-Rhosp_20to39*I_20to39[i,t-1]/Thosp -
        (1-Rdeath_20to39)*H_20to39[i,t-1]/Trecov -
        Rdeath_20to39*H_20to39[i,t-1]/Tdeath
      dH_40to49dt[i,t]<-Rhosp_40to49*I_40to49[i,t-1]/Thosp -
        (1-Rdeath_40to49)*H_40to49[i,t-1]/Trecov -
        Rdeath_40to49*H_40to49[i,t-1]/Tdeath
      dH_50to59dt[i,t]<-Rhosp_50to59*I_50to59[i,t-1]/Thosp -
        (1-Rdeath_50to59)*H_50to59[i,t-1]/Trecov -
        Rdeath_50to59*H_50to59[i,t-1]/Tdeath
      dH_60to69dt[i,t]<-Rhosp_60to69*I_60to69[i,t-1]/Thosp -
        (1-Rdeath_60to69)*H_60to69[i,t-1]/Trecov -
        Rdeath_60to69*H_60to69[i,t-1]/Tdeath
      dH_70to79dt[i,t]<-Rhosp_70to79*I_70to79[i,t-1]/Thosp -
        (1-Rdeath_70to79)*H_70to79[i,t-1]/Trecov -
        Rdeath_70to79*H_70to79[i,t-1]/Tdeath
      dH_80plusdt[i,t]<-Rhosp_80plus*I_80plus[i,t-1]/Thosp -
        (1-Rdeath_80plus)*H_80plus[i,t-1]/Trecov -
        Rdeath_80plus*H_80plus[i,t-1]/Tdeath
      
      dcrit_10to19dt[i,t]<-(crit_rate)*I_10to19[i,t-1]/Thosp -
        (1-Rdeath_10to19)*crit_10to19[i,t-1]/Trecov -
        Rdeath_10to19*crit_10to19[i,t-1]/Tdeath
      dcrit_20to39dt[i,t]<-(crit_rate)*I_20to39[i,t-1]/Thosp -
        (1-Rdeath_20to39)*crit_20to39[i,t-1]/Trecov -
        Rdeath_20to39*crit_20to39[i,t-1]/Tdeath
      dcrit_40to49dt[i,t]<-(crit_rate)*I_40to49[i,t-1]/Thosp -
        (1-Rdeath_40to49)*crit_40to49[i,t-1]/Trecov -
        Rdeath_40to49*crit_40to49[i,t-1]/Tdeath
      dcrit_50to59dt[i,t]<-(crit_rate)*I_50to59[i,t-1]/Thosp -
        (1-Rdeath_50to59)*crit_50to59[i,t-1]/Trecov -
        Rdeath_50to59*crit_50to59[i,t-1]/Tdeath
      dcrit_60to69dt[i,t]<-(crit_rate)*I_60to69[i,t-1]/Thosp -
        (1-Rdeath_60to69)*crit_60to69[i,t-1]/Trecov -
        Rdeath_60to69*crit_60to69[i,t-1]/Tdeath
      dcrit_70to79dt[i,t]<-Rhosp_70to79*(crit_rate)*I_70to79[i,t-1]/Thosp -
        (1-Rdeath_70to79)*crit_70to79[i,t-1]/Trecov -
        Rdeath_70to79*H_70to79[i,t-1]/Tdeath
      dcrit_80plusdt[i,t]<-Rhosp_80plus*(crit_rate)*I_80plus[i,t-1]/Thosp -
        (1-Rdeath_80plus)*crit_80plus[i,t-1]/Trecov -
        Rdeath_80plus*crit_80plus[i,t-1]/Tdeath
      
      #dRdt[i,t]<-(1-Rhosp)*In[i,t-1]/Tinf+(1-Rdeath)*H[i,t-1]/Trecov
      dRdt[i,t]<-(1-Rhosp_10to19)*I_10to19[i,t-1]/Tinf+(1-Rdeath_10to19)*H_10to19[i,t-1]/Trecov +
        (1-Rhosp_20to39)*I_20to39[i,t-1]/Tinf+(1-Rdeath_20to39)*H_20to39[i,t-1]/Trecov +
        (1-Rhosp_40to49)*I_40to49[i,t-1]/Tinf+(1-Rdeath_40to49)*H_40to49[i,t-1]/Trecov +
        (1-Rhosp_50to59)*I_50to59[i,t-1]/Tinf+(1-Rdeath_50to59)*H_50to59[i,t-1]/Trecov +
        (1-Rhosp_60to69)*I_60to69[i,t-1]/Tinf+(1-Rdeath_60to69)*H_60to69[i,t-1]/Trecov +
        (1-Rhosp_70to79)*I_70to79[i,t-1]/Tinf+(1-Rdeath_70to79)*H_70to79[i,t-1]/Trecov +
        (1-Rhosp_80plus)*I_80plus[i,t-1]/Tinf+(1-Rdeath_80plus)*H_80plus[i,t-1]/Trecov
      #dDdt[i,t]<-Rdeath*H[i,t-1]/Tdeath
      dDdt[i,t]<-Rdeath_10to19*H_10to19[i,t-1]/Tdeath +
        Rdeath_20to39*H_20to39[i,t-1]/Tdeath +
        Rdeath_40to49*H_40to49[i,t-1]/Tdeath +
        Rdeath_50to59*H_50to59[i,t-1]/Tdeath +
        Rdeath_60to69*H_60to69[i,t-1]/Tdeath +
        Rdeath_70to79*H_70to79[i,t-1]/Tdeath +
        Rdeath_80plus*H_80plus[i,t-1]/Tdeath
      
      
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
      H_10to19[i,t]<-H_10to19[i,t-1]+dH_10to19dt[i,t]
      H_20to39[i,t]<-H_20to39[i,t-1]+dH_20to39dt[i,t]
      H_40to49[i,t]<-H_40to49[i,t-1]+dH_40to49dt[i,t]
      H_50to59[i,t]<-H_50to59[i,t-1]+dH_50to59dt[i,t]
      H_60to69[i,t]<-H_60to69[i,t-1]+dH_60to69dt[i,t]
      H_70to79[i,t]<-H_70to79[i,t-1]+dH_70to79dt[i,t]
      H_80plus[i,t]<-H_80plus[i,t-1]+dH_80plusdt[i,t]
      
      
      
      D[i,t]<-D[i,t-1]+dDdt[i,t]
      
      #crit[i,t]<-crit_rate*In[i,t]
   
      crit_10to19[i,t]<-crit_10to19[i,t-1]+dcrit_10to19dt[i,t]
      crit_20to39[i,t]<-crit_20to39[i,t-1]+dcrit_20to39dt[i,t]
      crit_40to49[i,t]<-crit_40to49[i,t-1]+dcrit_40to49dt[i,t]
      crit_50to59[i,t]<-crit_50to59[i,t-1]+dcrit_50to59dt[i,t]
      crit_60to69[i,t]<-crit_60to69[i,t-1]+dcrit_60to69dt[i,t]
      crit_70to79[i,t]<-crit_70to79[i,t-1]+dcrit_70to79dt[i,t]
      crit_80plus[i,t]<-crit_80plus[i,t-1]+dcrit_80plusdt[i,t]
      
      # Cumulative values
      # Ecum[i,t]<-Ecum[i,t-1]-dSdt[i,t]
      # Icum[i,t]<-Icum[i,t-1]+E[i,t-1]/Tinc
      # Hcum[i,t]<-Hcum[i,t-1]+Rhosp*In[i,t-1]/Thosp
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
      
      Hcum_10to19[i,t]<-Hcum_10to19[i,t-1]+Rhosp_10to19*I_10to19[i,t-1]/Thosp
      Hcum_20to39[i,t]<-Hcum_20to39[i,t-1]+Rhosp_20to39*I_20to39[i,t-1]/Thosp
      Hcum_40to49[i,t]<-Hcum_40to49[i,t-1]+Rhosp_40to49*I_40to49[i,t-1]/Thosp
      Hcum_50to59[i,t]<-Hcum_50to59[i,t-1]+Rhosp_50to59*I_50to59[i,t-1]/Thosp
      Hcum_60to69[i,t]<-Hcum_60to69[i,t-1]+Rhosp_60to69*I_60to69[i,t-1]/Thosp
      Hcum_70to79[i,t]<-Hcum_70to79[i,t-1]+Rhosp_70to79*I_70to79[i,t-1]/Thosp
      Hcum_80plus[i,t]<-Hcum_80plus[i,t-1]+Rhosp_80plus*I_80plus[i,t-1]/Thosp
  }
}  


if (model_by_age==1) {
  In=I_10to19+I_20to39+I_40to49+I_50to59+I_60to69+I_70to79+I_80plus
  H=H_10to19+H_20to39+H_40to49+H_50to59+H_60to69+H_70to79+H_80plus
  crit=crit_10to19+crit_20to39+crit_40to49+crit_50to59+crit_60to69+crit_70to79+crit_80plus
  Icum=Icum_10to19+Icum_20to39+Icum_40to49+Icum_50to59+Icum_60to69+Icum_70to79+Icum_80plus
  Hcum=Hcum_10to19+Hcum_20to39+Hcum_40to49+Hcum_50to59+Hcum_60to69+Hcum_70to79+Hcum_80plus
  Ecum=Ecum_10to19+Ecum_20to39+Ecum_40to49+Ecum_50to59+Ecum_60to69+Ecum_70to79+Ecum_80plus
}

df.Marion<-data.frame("I"=In[49,],"H"=H[49,],"crit"=crit[49,],"D"=D[49,],"t"=(1:maxt))
df.Monroe<-data.frame("I"=In[53,],"H"=H[53,],"crit"=crit[53,],"D"=D[53,],"t"=(1:maxt))
df.Lake<-data.frame("I"=In[45,],"H"=H[45,],"crit"=crit[45,],"D"=D[45,],"t"=(1:maxt))

print(ggplot(df.Marion,aes(x=t))+
        geom_line(aes(y=I,color="infected"))+
        geom_line(aes(y=H/0.95,color="hospitalized"))+
        geom_line(aes(y=crit,color="critical"))+
        geom_line(aes(y=D,color="deceased"))+
        ggtitle(tit1)+
        scale_x_continuous(breaks=seq(0,300,30),name="days")+
        scale_y_continuous(breaks=seq(0,80000,2000),name="number")+
        theme(axis.text.x=element_text(angle=90)) )
ggsave(pic1)

print(ggplot(df.Monroe,aes(x=t))+
        geom_line(aes(y=I,color="infected"))+
        geom_line(aes(y=H/0.95,color="hospitalized"))+
        geom_line(aes(y=crit,color="critical"))+
        geom_line(aes(y=D,color="deceased"))+
        ggtitle(tit2)+
        scale_x_continuous(breaks=seq(0,300,30),name="days")+
        scale_y_continuous(breaks=seq(0,80000,2000),name="number")+
        theme(axis.text.x=element_text(angle=90)) )
ggsave(pic2)

print(ggplot(df.Lake,aes(x=t))+
        geom_line(aes(y=I,color="infected"))+
        geom_line(aes(y=H/0.95,color="hospitalized"))+
        geom_line(aes(y=crit,color="critical"))+
        geom_line(aes(y=D,color="deceased"))+
        ggtitle(tit3)+
        scale_x_continuous(breaks=seq(0,300,30),name="days")+
        scale_y_continuous(breaks=seq(0,80000,2000),name="number")+
        theme(axis.text.x=element_text(angle=90)) )
ggsave(pic3)


# colname1<-paste(county_names[1],"Susceptible",sep=",")
# df<-data.frame(colname1=S[1,])
# 
# colname1<-paste(county_names[1],"Exposed",sep=",")

if (output==1) {

  countycol<-rep(county_names[1],maxt)
  daycol<-seq(1,maxt)

  df<-data.frame("County"=countycol,"Day"=daycol,
                 "Susceptible"=S[1,],
                 "Exposed"=E[1,],
                 "ExposedCumulative"=Ecum[1,],
                 "InfectedNotHospitalized"=In[1,],
                 "InfectedCumulative"=Icum[1,],
                 "Hospitalized"=H[1,],
                 "HospitalizedCumulative"=Hcum[1,],
                 "Critical"=crit[1,],
                 "CriticalCumulative"=critcum[1,],
                 "Deceased"=D[1,])

  for (i in 2:92) {
  
    countycol<-rep(county_names[i],maxt)
    df2<-data.frame("County"=countycol,"Day"=daycol,
                    "Susceptible"=S[i,],
                    "Exposed"=E[i,],
                    "ExposedCumulative"=Ecum[i,],
                    "InfectedNotHospitalized"=In[i,],
                    "InfectedCumulative"=Icum[i,],
                    "Hospitalized"=H[i,],
                    "HospitalizedCumulative"=Hcum[i,],
                    "Critical"=crit[i,],
                    "CriticalCumulative"=critcum[i,],
                    "Deceased"=D[i,])
    df<-df%>%bind_rows(df2)
  
  }

  write_csv(df,model_outfile)

  sink(param_outfile)
  cat(paste("outfile=",model_outfile))
  cat(paste("R0=",R0))
  cat(paste("intervention_R_rdxn=",intervention_R_rdxn))
  cat(paste("intervention_time=",intervention_time))
  cat(paste("lift_time=",lift_time))
  cat(paste("Pinf=",Pinf))
  cat(paste("Rdeath=",Rdeath))
  cat(paste("Rhosp=",Rhosp))
  cat(paste("Trecov=",Trecov))
  cat(paste("Thosp=",Thosp))
  cat(paste("Tdeath=",Tdeath))
  cat(paste("crit_rate=",crit_rate))
  cat(paste("Tinc=",Tinc))
  cat(paste("Tinf=",Tinf))
  cat(paste("maxt=",maxt))
  cat(paste(param_notes))
  sink()
  
}

