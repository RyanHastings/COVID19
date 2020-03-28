########################################################
# model_dynamic_core.R
#
# Dynamic core for the model.  A system of linear differential
# equations solved.
########################################################
# Ryan Hastings, 26 Mar 2020
########################################################

# set up total N array
N<-array(0,dim=c(ncounties))

for (i in 1:ncounties) {
for (a in 1:nage) {
  N[i]<-N[i]+S[i,1,a]+In[i,1,a]
}
}

# the dynamic core
for (t in 2:maxt) {
  
  # change Rt based time
  if (t<intervention_time | t>=lift_time) {
    Rt<-R0
  } else if (t>=intervention_time & t<lift_time){
    Rt<-(1-intervention_R_rdxn)*R0
  }
  
  dSdt[i,t,a]<-0
  for (i in 1:ncounties) {
    
    dum1<-0
    dum2<-0
    for (a in 1:nage) {
      dum1<-dum1+S[i,t-1,a]
      dum2<-dum2+In[i,t-1,a]
    }

    term1<-Rt*dum1*dum2/(Tinf*N[i]) #S[i,t-1]*In[i,t-1]/(Tinf*N[i])
    
    for (a in 1:nage) {
      term2<-E[i,t-1,a]/Tinc
      term3<-In[i,t-1,a]/Tinf
      term4<-H[i,t-1,a]/Thosp
      term6<-G[i,t-1,a]/Tdeath
      term7<-Q[i,t-1,a]/Thosp
    
      dSdt[i,t,a]<--term1/nage
      dEdt[i,t,a]<-term1/nage-term2
      dIdt[i,t,a]<-term2-term3
    
      #SEIHCRD
      if (a<age60) {
        dGdt[i,t,a]<-Rdeath[a]*comorbid_death[i]*term3-term6
        dQdt[i,t,a]<-(Rcrit-Rdeath[a]*comorbid_death[i])*term3-term7
        dHdt[i,t,a]<-(Rhosp[a]*comorbid_hosp[i]-Rcrit)*term3-term4
        dDdt[i,t,a]<-term6
        dRdt[i,t,a]<-term4+term7+(1-Rhosp[a]*comorbid_hosp[i])*term3
      } else {
        dGdt[i,t,a]<-Rdeath[a]*comorbid_death_over60[i]*term3-term6
        dQdt[i,t,a]<-(Rcrit-Rdeath[a]*comorbid_death_over60[i])*term3-term7
        dHdt[i,t,a]<-(Rhosp[a]*comorbid_hosp_over60[i]-Rcrit)*term3-term4
        dDdt[i,t,a]<-term6
        dRdt[i,t,a]<-term4+term7+(1-Rhosp[a]*comorbid_hosp_over60[i])*term3
      }
    
      #SEIHRD
      #dHdt[i,t]<-Rhosp*term3-term4
      #dRdt[i,t]<-(1-Rhosp)*term3+(1-Rdeath/Rhosp)*term4
      #dDdt[i,t]<-(Rdeath/Rhosp)*term4
    
      #SEIRD
      #dRdt[i,t]<-(1-Rdeath)*term3
      #dDdt[i,t]<-Rdeath*term3
    
      #SEIR
      #dRdt[i,t]<-term3
    
      S[i,t,a]<-S[i,t-1,a]+dSdt[i,t,a]
      E[i,t,a]<-E[i,t-1,a]+dEdt[i,t,a]
      In[i,t,a]<-In[i,t-1,a]+dIdt[i,t,a]
      H[i,t,a]<-H[i,t-1,a]+dHdt[i,t,a]
      Q[i,t,a]<-Q[i,t-1,a]+dQdt[i,t,a]
      G[i,t,a]<-G[i,t-1,a]+dGdt[i,t,a]
      C[i,t,a]<-C[i,t-1,a]+dCdt[i,t,a]
      R[i,t,a]<-R[i,t-1,a]+dRdt[i,t,a]
      D[i,t,a]<-D[i,t-1,a]+dDdt[i,t,a]
    
      Ecum[i,t,a]=Ecum[i,t-1,a]+term1
      Icum[i,t,a]=Icum[i,t-1,a]+term2
      Hcum[i,t,a]=Hcum[i,t-1,a]+Rhosp[a]*term3
      Ccum[i,t,a]=Ccum[i,t-1,a]+Rcrit*term3
    }
  }
}

