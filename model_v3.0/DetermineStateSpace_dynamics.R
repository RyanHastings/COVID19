######################################################
# DetermineStateSpace_dynamics.R
#
# Dynamical core for DetermineStateSpace.
######################################################
# Ryan Hastings, 5 May 2020
######################################################

# set up total N array
N<-S[1]+In[1]

# the dynamic core
for (t in 2:maxt) {
  
  # change Rt based time
  if (t<intervention_time) {
    Rt<-R0
  } else if (t>=intervention_time & t<lift_time1){
    Rt<-(1-intervention_R_rdxn)*R0
  } else if (t>=lift_time1 & t<lift_time2) {
    Rt<-(1-lift_rdxn1)*R0
  } else if (t>=lift_time2) {
    Rt<-(1-lift_rdxn2)*R0
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
  dQdt[t]<-(Rcrit-Rdeath)*term3-term7
  dHdt[t]<-(Rhosp-Rcrit)*term3-term4
  dDdt[t]<-term6
  dRdt[t]<-term4+term7+(1-Rhosp)*term3
  
  S[t]<-S[t-1]+dSdt[t]*dt
  E[t]<-E[t-1]+dEdt[t]*dt
  In[t]<-In[t-1]+dIdt[t]*dt
  H[t]<-H[t-1]+dHdt[t]*dt
  Q[t]<-Q[t-1]+dQdt[t]*dt
  G[t]<-G[t-1]+dGdt[t]*dt
  R[t]<-R[t-1]+dRdt[t]*dt
  D[t]<-D[t-1]+dDdt[t]*dt
  Dday[t]<-term6*dt
      
  Ecum[t]=Ecum[t-1]+term1*dt
  Icum[t]=Icum[t-1]+term2*dt
  Hcum[t]=Hcum[t-1]+Rhosp*term3*dt
  Ccum[t]=Ccum[t-1]+Rcrit*term3*dt

}

