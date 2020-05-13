library(tidyverse)

rm(list=ls())

# initial values
R0<-2.9
intervention_R_rdxn<-0.65
Rdeath<-0.0066
Rhosp<-0.06
Rcrit<-0.024
intervention_time<-65
Tinc<-3
Tinf<-5
Thosp<-23
Tdeath<-18
Tcrit<-23
maxt<-300
Npop<-6.692e6
lift_time<-500

# resolution
dR0<-0.05
dintervention_R_rdxn<-0.05
dRdeath<-0#0.0001
dRhosp<-0#0.001
dRcrit<-0.001
dintervention_time<-1
dTinc<-0#1
dTinf<-0#1
dThosp<-0#1
dTdeath<-0#1
dTcrit<-0#1

# data files
DeathData<-"../model_v3.0/DeathCurves/Deaths0413.csv"
ICUData<-"../model_v3.0/DeathCurves/ICUBeds_200407.csv"

##################################################

dum<-read_csv(DeathData)
DeathPerDay<-dum$DeathCount

dum<-read_csv(ICUData)
ICUBedCount<-dum$ICUBedsCOVIDTotal


no_min<-TRUE
it<-1

while (no_min) {
  
  if (R0-dR0<=0) { R0<-R0+2*dR0}
  if (intervention_R_rdxn-dintervention_R_rdxn<0) {intervention_R_rdxn+2*dintervention_R_rdxn}
  if (Rdeath-dRdeath<=0) {Rdeath<-Rdeath+2*dRdeath}
  if (Rhosp-dRhosp<=0) {Rhosp<-Rhosp+2*dRhosp}
  if (Rcrit-dRcrit<=0) {Rcrit<-Rcrit+2*dRcrit}
  if (intervention_time-dintervention_time<=0) {intervention_time<-intervention_time+2*dintervention_time}
  if (Tinc-dTinc<=0) {Tinc<-Tinc+2*dTinc}
  if (Tinf-dTinf<=0) {Tinf<-Tinf+2*dTinf}
  if (Thosp-dThosp<=0) {Thosp<-Thosp+2*dThosp}
  if (Tdeath-dTdeath<=0) {Tdeath<-Tdeath+2*dTdeath}
  if (Tcrit-dTcrit<=0) {Tcrit<-Tcrit+2*dTcrit}
  
  source("GradientDescent_Engine.R")
  cdist<-distance
  print(paste("central distance=",cdist))
  
  R0<-R0+dR0
  source("GradientDescent_Engine.R")
  R0d2<-distance
  R0<-R0-2*dR0
  source("GradientDescent_Engine.R")
  R0d1<-distance
  
  intervention_R_rdxn<-intervention_R_rdxn+dintervention_R_rdxn
  source("GradientDescent_Engine.R")
  rdxnd2<-distance
  intervention_R_rdxn<-intervention_R_rdxn-2*dintervention_R_rdxn
  source("GradientDescent_Engine.R")
  rdxnd1<-distance
  
  Rdeath<-Rdeath+dRdeath
  source("GradientDescent_Engine.R")
  Rdeathd2<-distance
  Rdeath<-Rdeath-2*dRdeath
  source("GradientDescent_Engine.R")
  Rdeathd1<-distance
  
  Rhosp<-Rhosp+dRhosp
  source("GradientDescent_Engine.R")
  Rhospd2<-distance
  Rhosp<-Rhosp-2*dRhosp
  source("GradientDescent_Engine.R")
  Rhospd1<-distance
  
  Rcrit<-Rcrit+dRcrit
  source("GradientDescent_Engine.R")
  Rcritd2<-distance
  Rcrit<-Rcrit-2*dRcrit
  source("GradientDescent_Engine.R")
  Rcritd1<-distance
  
  intervention_time<-intervention_time+dintervention_time
  source("GradientDescent_Engine.R")
  intervention_timed2<-distance
  intervention_time<-intervention_time-2*dintervention_time
  source("GradientDescent_Engine.R")
  intervention_timed1<-distance
  
  Tinc<-Tinc+dTinc
  source("GradientDescent_Engine.R")
  Tincd2<-distance
  Tinc<-Tinc-2*dTinc
  source("GradientDescent_Engine.R")
  Tincd1<-distance

  Tinf<-Tinf+dTinf
  source("GradientDescent_Engine.R")
  Tinfd2<-distance
  Tinf<-Tinf-2*dTinf
  source("GradientDescent_Engine.R")
  Tinfd1<-distance
  
  Thosp<-Thosp+dThosp
  source("GradientDescent_Engine.R")
  Thospd2<-distance
  Thosp<-Thosp-2*dThosp
  source("GradientDescent_Engine.R")
  Thospd1<-distance
  
  Tdeath<-Tdeath+dTdeath
  source("GradientDescent_Engine.R")
  Tdeathd2<-distance
  Tdeath<-Tdeath-2*dTdeath
  source("GradientDescent_Engine.R")
  Tdeathd1<-distance
  
  Tcrit<-Tcrit+dTcrit
  source("GradientDescent_Engine.R")
  Tcritd2<-distance
  Tcrit<-Tcrit-2*dTcrit
  source("GradientDescent_Engine.R")
  Tcritd1<-distance
  
  gradR01<-R0d1-cdist
  gradR02<-cdist-R0d2
  
  if (gradR01>=0 & gradR02>=0) {
    tR0<-TRUE
  } else if (gradR01<0 & gradR02<0) {
    tR0<-FALSE
    if (gradR01>gradR02) {
      R0<-R0+dR0
      tR0<-FALSE
    } else {
      R0<-R0-dR0
      tR0<-FALSE
    }
  } else if (gradR01<0) {
    R0<-R0-dR0
    tR0<-FALSE
  } else if (gradR02<0) {
    R0<-R0+dR0
    tR0<-FALSE
  }
  
  gradrdxn1<-rdxnd1-cdist
  gradrdxn2<-cdist-rdxnd2
  
  if (gradrdxn1>=0 & gradrdxn2>=0) {
    trdxn<-TRUE
  } else if (gradrdxn1<0 & gradrdxn2<0) {
    trdxn<-FALSE
    if (gradrdxn1>gradrdxn2) {
      intervention_R_rdxn<-intervention_R_rdxn+dintervention_R_rdxn
      trdxn<-FALSE
    } else {
      intervention_R_rdxn<-intervention_R_rdxn-dintervention_R_rdxn
      trdxn<-FALSE
    }
  } else if (gradrdxn1<0) {
    intervention_R_rdxn<-intervention_R_rdxn-dintervention_R_rdxn
    trdxn<-FALSE
  } else if (gradrdxn2<0) {
    intervention_R_rdxn<-intervention_R_rdxn+dintervention_R_rdxn
    trdxn<-FALSE
  }

  gradRdeath1<-Rdeathd1-cdist
  gradRdeath2<--(Rdeathd2-cdist)
  
  if (gradRdeath1>=0 & gradRdeath2>=0) {
    tRdeath<-TRUE
  } else if (gradRdeath1<0 & gradRdeath2<0) {
    tRdeath<-FALSE
    if (gradRdeath1>gradRdeath2) {
      Rdeath<-Rdeath+dRdeath
      tRdeath<-FALSE
    } else {
      Rdeath<-Rdeath-dRdeath
      tRdeath<-FALSE
    }
  } else if (gradRdeath1<0) {
    Rdeath<-Rdeath-dRdeath
    tRdeath<-FALSE
  } else if (gradRdeath2<0) {
    Rdeath<-Rdeath+dRdeath
    tRdeath<-FALSE
  }

  gradRhosp1<--(cdist-Rhospd1)
  gradRhosp2<--(Rhospd2-cdist)  
  
  if (gradRhosp1>=0 & gradRhosp2>=0) {
    tRhosp<-TRUE
  } else if (gradRhosp1<0 & gradRhosp2<0) {
    tRhosp<-FALSE
    if (gradRhosp1>gradRhosp2) {
      Rhosp<-Rhosp+dRhosp
    } else {
      Rhosp<-Rhosp-dRhosp
    }
  } else if (gradRhosp1<0) {
    Rhosp<-Rhosp-dRhosp
    tRhosp<-FALSE
  } else if (gradRhosp2<0) {
    Rhosp<-Rhosp+dRhosp
    tRhosp<-FALSE
  }
  
  gradRcrit1<--(cdist-Rcritd1)
  gradRcrit2<--(Rcritd2-cdist) 
  
  if (gradRcrit1>=0 & gradRcrit2>=0) {
    tRcrit<-TRUE
  } else if (gradRcrit1<0 & gradRcrit2<0) {
    tRcrit<-FALSE
    if (gradRcrit1>gradRcrit2) {
      Rcrit<-Rcrit+dRcrit
    } else {
      Rcrit<-Rcrit-dRcrit
    }
  } else if (gradRcrit1<0) {
    Rcrit<-Rcrit-dRcrit
    tRcrit<-FALSE
  } else if (gradRcrit2<0) {
    Rcrit<-Rcrit+dRcrit
    tRcrit<-FALSE
  }
  
  gradintervention_time1<--(cdist-intervention_timed1)
  gradintervention_time2<--(intervention_timed2-cdist)  
  
  if (gradintervention_time1>=0 & gradintervention_time2>=0) {
    tintervention_time<-TRUE
  } else if (gradintervention_time1<0 & gradintervention_time2<0) {
    tintervention_time<-FALSE
    if (gradintervention_time1>gradintervention_time2) {
      intervention_time<-intervention_time+dintervention_time
    } else {
      intervention_time<-intervention_time-dintervention_time
    }
  } else if (gradintervention_time1<0) {
    intervention_time<-intervention_time-dintervention_time
    tintervention_time<-FALSE
  } else if (gradintervention_time2<0) {
    intervention_time<-intervention_time+dintervention_time
    tintervention_time<-FALSE
  }
  
  gradTinc1<--(cdist-Tincd1)
  gradTinc2<--(Tincd2-cdist)  
  
  if (gradTinc1>=0 & gradTinc2>=0) {
    tTinc<-TRUE
  } else if (gradTinc1<0 & gradTinc2<0) {
    tTinc<-FALSE
    if (gradTinc1>gradTinc2) {
      Tinc<-Tinc+dTinc
    } else {
      Tinc<-Tinc-dTinc
    }
  } else if (gradTinc1<0) {
    Tinc<-Tinc-dTinc
    tTinc<-FALSE
  } else if (gradTinc2<0) {
    Tinc<-Tinc+dTinc
    tTinc<-FALSE
  }
  
  gradTinf1<--(cdist-Tinfd1)
  gradTinf2<--(Tinfd2-cdist) 
  
  if (gradTinf1>=0 & gradTinf2>=0) {
    tTinf<-TRUE
  } else if (gradTinf1<0 & gradTinf2<0) {
    tTinf<-FALSE
    if (gradTinf1>gradTinf2) {
      Tinf<-Tinf+dTinf
    } else {
      Tinf<-Tinf-dTinf
    }
  } else if (gradTinf1<0) {
    Tinf<-Tinf-dTinf
    tTinf<-FALSE
  } else if (gradTinf2<0) {
    Tinf<-Tinf+dTinf
    tTinf<-FALSE
  }
  
  gradThosp1<--(cdist-Thospd1)
  gradThosp2<--(Thospd2-cdist)  
  
  if (gradThosp1>=0 & gradThosp2>=0) {
    tThosp<-TRUE
  } else if (gradThosp1<0 & gradThosp2<0) {
    tThosp<-FALSE
    if (gradThosp1>gradThosp2) {
      Thosp<-Thosp+dThosp
    } else {
      Thosp<-Thosp-dThosp
    }
  } else if (gradThosp1<0) {
    Thosp<-Thosp-dThosp
    tThosp<-FALSE
  } else if (gradThosp2<0) {
    Thosp<-Thosp+dThosp
    tThosp<-FALSE
  }
  
  gradTdeath1<--(cdist-Tdeathd1)
  gradTdeath2<--(Tdeathd2-cdist)  
  
  if (gradTdeath1>=0 & gradTdeath2>=0) {
    tTdeath<-TRUE
  } else if (gradTdeath1<0 & gradTdeath2<0) {
    tTdeath<-FALSE
    if (gradTdeath1>gradTdeath2) {
      Tdeath<-Tdeath+dTdeath
    } else {
      Tdeath<-Tdeath-dTdeath
    }
  } else if (gradTdeath1<0) {
    Tdeath<-Tdeath-dTdeath
    tTdeath<-FALSE
  } else if (gradTdeath2<0) {
    Tdeath<-Tdeath+dTdeath
    tTdeath<-FALSE
  }
  
  gradTcrit1<--(cdist-Tcritd1)
  gradTcrit2<--(Tcritd2-cdist)  
  
  if (gradTcrit1>=0 & gradTcrit2>=0) {
    tTcrit<-TRUE
  } else if (gradTcrit1<0 & gradTcrit2<0) {
    tTcrit<-FALSE
    if (gradTcrit1>gradTcrit2) {
      Tcrit<-Tcrit+dTcrit
    } else {
      Tcrit<-Tcrit-dTcrit
    }
  } else if (gradTcrit1<0) {
    Tcrit<-Tcrit-dTcrit
    tTcrit<-FALSE
  } else if (gradTcrit2<0) {
    Tcrit<-Tcrit+dTcrit
    tTcrit<-FALSE
  }
  
  no_min<-!(tR0 & trdxn & tRdeath & tRhosp & tRcrit & tintervention_time & tTinc & tTinf & tThosp & tTdeath & tTcrit)
  
  print(paste("R0=",R0))
  print(paste("intervention_R_rdxn=",intervention_R_rdxn))
  print(paste("Rdeath=",Rdeath))
  print(paste("Rhosp=",Rhosp))
  print(paste("Rcrit=",Rcrit))
  print(paste("intervention_time=",intervention_time))
  print(paste("Tinc=",Tinc))
  print(paste("Tinf=",Tinf))
  print(paste("Thosp=",Thosp))
  print(paste("Tdeath=",Tdeath))
  print(paste("Tcrit=",Tcrit))
  print(paste("interation",it))
  
  it<-it+1
}