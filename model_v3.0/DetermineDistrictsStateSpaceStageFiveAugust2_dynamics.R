###########################################################
# DetermineDistrictStateSpacePhaseTwo_dynamics.R
#
# Dynamical core for DetermineDistrictStateSpacePhaseTwo.R
###########################################################
# Ryan Hastings, 23 May 2020..29 june for stage five..31 july for august model
###########################################################

# total population
N<-S[1]+In[1]

# the dynamic core
for (t in 2:maxt) {
  
  # change Rt based time
  if (t<intervention_time) {
    Rt<-R0
    Rhosp<-RhospStageTwo[n]
    Rcrit<-RcritStageTwo[n]
  } else if (t>=intervention_time & t<lift_time1){
    Rt<-(1-StageOneReduction)*R0
    Rhosp<-RhospStageTwo[n]
    Rcrit<-RcritStageTwo[n]
  } else if (t>=lift_time1 & t<lift_time2) {
    Rt<-(1-StageTwoReductionA)*R0
    Rhosp<-RhospStageTwo[n]
    Rcrit<-RcritStageTwo[n]
  } else if (t>=lift_time2 & t<Stage3ADays[n]) {
    # print("here")
    Rt<-(1-StageTwoReductionB)*R0
    Rhosp<-RhospStageTwo[n]
    Rcrit<-RcritStageTwo[n]
  } else if (t>=Stage3ADays[n] & t<Stage3BDays[n]) {
    # print("here")
    Rt<-(1-StageThreeReductionA)*R0
    Rhosp<-RhospStageThree[n]
    Rcrit<-RcritStageThree[n]
  } else if (t>=Stage3BDays[n] & t<StageFourDay) {
    Rt<-(1-StageThreeReductionB)*R0
    Rhosp<-RhospStageThree[n]
    Rcrit<-RcritStageThree[n]
  } else if (t>=StageFourDay & t<StageFiveDay) {
    Rt<-(1-StageFourReduction)*R0
    Rhosp<-RhospStageFour[n]
    Rcrit<-RcritStageFour[n]
  } else if (t>=StageFiveDay & t<AugustDay) {
    Rt<-(1-JulyReduction)*R0
    Rhosp<-RhospJuly[n]
    Rcrit<-RcritJuly[n]
  } else if (t>=AugustDay & t<SeptemberDay) {
    Rt<-(1-AugReduction)*R0
    Rhosp<-RhospAugust
    Rcrit<-RcritAugust
  } else if (t>=SeptemberDay & t<OctoberDay) {
    Rt<-R0*(1-SepReduction)
    Rhosp<-RhospAugust
    Rcrit<-RcritAugust
    # print(t-PhaseFiveDay)
    # print((stage5_weeks*7-(t-PhaseFiveDay))/(stage5_weeks*7))
    # print(Rt)
  } else if (t>=OctoberDay) {
    Rt<-NA
    Rhosp<-RhospAugust
    Rcrit<-RcritAugust
  }
  
  # print(t)
  # print(Rt)
  # print(Rcrit)
  # print(RcritStageThree)
  # print(Rhosp)
  
  if (is.na(Rhosp)) { stop() }
  
  if (t==init_time+1) {
    In[t-1]<-1
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

