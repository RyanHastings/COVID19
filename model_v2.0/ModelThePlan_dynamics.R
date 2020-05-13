###############################################################
# ModelThePlan_dynamics.R
#
# Dynamical core for ModelThePlan.R and ModelThePlanScenarios.R.
###############################################################
# 9 May 2020, Ryan Hastings
###############################################################

N<-S[1]+In[1]
# the dynamic core
for (t in 2:maxt) {
  
  if (t<PhaseOneT) {
    Rt<-R0
  } else if (t>=PhaseOneT & t<PhaseTwoT) {
    Rt<-R0*(1-PhaseOneReduction)
  } else if (t>=PhaseTwoT & t<PhaseThreeT) {
    Rt<-R0*(1-PhaseTwoReduction)
  } else if (t>=PhaseThreeT & t<PhaseFourT) {
    Rt<-R0*(1-PhaseThreeReduction)
  } else if (t>=PhaseFourT & t<PhaseFiveT) {
    Rt<-R0*(1-PhaseFourReduction)
  } else if (t>=PhaseFiveT) {
    Rt<-R0
  }
  
    
  term1<-Rt*S[t-1]*In[t-1]/(Tinf*N) #S[i,t-1]*In[i,t-1]/(Tinf*N[i])
    
  term2<-E[t-1]/Tinc
  term3<-In[t-1]/Tinf
  term4<-H[t-1]/Thosp
  term6<-G[t-1]/Tdeath
  term7<-Q[t-1]/Tcrit
      
  dSdt[t]<--term1
  dEdt[t]<-term1-term2
  dIdt[t]<-term2-term3
  dGdt[t]<-Rdeath*term3-term6
  dHdt[t]<-(Rhosp-Rcrit)*term3-term4
  dQdt[t]<-(Rcrit-Rdeath)*term3-term7
  dDdt[t]<-term6
  dRdt[t]<-term4+term7+(1-Rhosp)*term3
      
      
  S[t]<-S[t-1]+dSdt[t]
  E[t]<-E[t-1]+dEdt[t]
  In[t]<-In[t-1]+dIdt[t]
  H[t]<-H[t-1]+dHdt[t]
  Q[t]<-Q[t-1]+dQdt[t]
  G[t]<-G[t-1]+dGdt[t]
    #  C[t]<-C[t-1]+dCdt[t]
  R[t]<-R[t-1]+dRdt[t]
  D[t]<-D[t-1]+dDdt[t]
      
  Ecum[t]=Ecum[t-1]+term1
  Icum[t]=Icum[t-1]+term2
  Hcum[t]=Hcum[t-1]+Rhosp*term3
  Ccum[t]=Ccum[t-1]+Rcrit*term3
      
  Dday[t]=dDdt[t]

  }