######################################################
# DetermineStateSpace_dynamics.R
#
# Dynamical core for DetermineStateSpace.
######################################################
# Ryan Hastings, 5 May 2020
######################################################
# 26 may
# set up total N array
# stage four 11 june
N<-S[1]+In[1]

# the dynamic core
for (t in 2:maxt) {
  
  # change Rt based time
  if (t<PhaseOneDay) {
    Rt<-R0
    Rhosp<-RhospPhaseOne
    Rcrit<-RcritPhaseOne
  } else if (t>=PhaseOneDay & t<PhaseTwoDayA){
    Rt<-(1-PhaseTwoReductionA)*R0
    Rhosp<-RhospPhaseOne
    Rcrit<-RcritPhaseOne
  } else if (t>=PhaseTwoDayB & t<PhaseThreeDayA) {
    Rt<-(1-PhaseTwoReductionB)*R0
    Rhosp<-RhospPhaseOne
    Rcrit<-RcritPhaseOne
  } else if (t>=PhaseThreeDayA & t<PhaseThreeDayB) {
    Rt<-(1-PhaseThreeReductionA)*R0
    Rhosp<-RhospPhaseThree
    Rcrit<-RcritPhaseThree
  } else if (t>=PhaseThreeDayB & t<PhaseFourDay) {
    Rt<-(1-PhaseThreeReductionB)*R0
    Rhosp<-RhospPhaseThree
    Rcrit<-RcritPhaseThree
  } else if (t>=PhaseFourDay & t<PhaseFiveDay) {
    Rt<-(1-PhaseFourReduction)*R0
    Rhosp<-RhospPhaseFour
    Rcrit<-RcritPhaseFour
  } else if (t>=PhaseFiveDay & t<PhaseFiveDay+stage5_weeks*7) {
    Rt<-R0*(1-PhaseFourReduction*(stage5_weeks*7-(t-PhaseFiveDay))/(stage5_weeks*7))
    Rhosp<-RhospPhaseFour
    Rcrit<-RcritPhaseFour
    # print(t-PhaseFiveDay)
    # print((stage5_weeks*7-(t-PhaseFiveDay))/(stage5_weeks*7))
    # print(Rt)
  } else if (t>=PhaseFiveDay+stage5_weeks*7) {
    Rt<-R0
  }
  
  # print(t)
  # print(Rt)
  
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

