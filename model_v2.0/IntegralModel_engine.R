##################################################
# IntegralModel_engine.R
#
# Dynamical core for integral model.
##################################################
# Ryan Hastings, 5 May 2020
#################################################

for (t in 2:maxt) {
  
  if (t<intervention_time | t>=lift_time) {
    Rt<-R0
  } else if (t>=intervention_time & t<lift_time) {
    Rt<-R0*(1-intervention_R_rdxn)
  }
  
  
  t0<-max(1,t-Tinf)
  Itot<-sum(Inew[t0:t-1])
  Enew[t]<-Rt*S[t-1]*Itot/N
  if (t>Tinc) {
    Inew[t]<-Enew[t-Tinc]
    #print(Itot)
  }
  
  S[t]<-S[t-1]-Enew[t]
  
  if (t>Tinf) {
    Rnew[t]<-(1-Rhosp)*Inew[t-Tinf]
    Hnew[t]<-(Rhosp-Rcrit)*Inew[t-Tinf]
    Cnew[t]<-(Rcrit-Rdeath)*Inew[t-Tinf]
    Qnew[t]<-Rdeath*Inew[t-Tinf]
  }
  
  if (t>Trecov) {
    Rnew[t]<-Rnew[t]+Hnew[t-Trecov]+Cnew[t-Trecov]
  }
  
  if (t>Tdeath) {
    Dnew[t]<-Qnew[t-Tdeath]
  }
}