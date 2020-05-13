library(tidyverse)

rm(list=ls())


# initial box edges
R0_sides<-c(2.0,3.0)
intervention_R_rdxn_sides<-c(0.0,1.0)
Rdeath_sides<-c(0.05,0.07)
Rhosp_sides<-c(0.0,0.1)
Rcrit_sides<-c(0.0,0.1)
intervention_time_sides<-c(50,80)
Tinf_sides<-c(1,5)
Thosp_sides<-c(5,35)
Tdeath_sides<-c(5,35)
Tcrit_sides<-c(5,35)

maxt=300
Npop=6.692e6
lift_time=500
Tinc=3

# smallest resolution
dR0min<-0.05
dintervention_R_rdxn_min<-0.05
dRdeathmin<-0.0001
dRhospmin<-0.001
dRcritmin<-0.001
dintervention_time_min<-1
dTinfmin<-1
dThospmin<-1
dTdeathmin<-1
dTcritmin<-1

# data files
DeathData<-"../model_v3.0/DeathCurves/Deaths0413.csv"
ICUData<-"../model_v3.0/DeathCurves/ICUBeds_200407.csv"

##################################################

dum<-read_csv(DeathData)
DeathPerDay<-dum$DeathCount

dum<-read_csv(ICUData)
ICUBedCount<-dum$ICUBedsCOVIDTotal

##################################################
no_min=TRUE

tR0<-FALSE
tintervention_R_rdxn<-FALSE
tRdeath<-FALSE
tRhosp<-FALSE
tRcrit<-FALSE
tintervention_time<-FALSE
tTinf<-FALSE
tThosp<-FALSE
tTdeath<-FALSE
tTcrit<-FALSE

while (no_min) {
  #print("here")
  dR0<-max(0.25*(R0_sides[2]-R0_sides[1]),dR0min)
  #print('here')
  if (dR0==dR0min) {tR0=TRUE}
  dintervention_R_rdxn<-max(0.25*(intervention_R_rdxn_sides[2]-intervention_R_rdxn_sides[1]),dintervention_R_rdxn_min)
  if (dintervention_R_rdxn==dintervention_R_rdxn_min) {tintervention_R_rdxn==TRUE}
  dRdeath<-max(0.25*(Rdeath_sides[2]-Rdeath_sides[1]),dRdeathmin)
  if (dRdeath==dRdeathmin) {tRdeath<-TRUE}
  #print('here')
  dRhosp<-max(0.25*(Rhosp_sides[2]-Rhosp_sides[1]),dRhospmin)
  if (dRhosp==dRhospmin) {tRhosp<-TRUE}
  dRcrit<-max(0.25*(Rcrit_sides[2]-Rcrit_sides[1]),dRcritmin)
  #print('here')
  if (dRcrit==dRcritmin) {tRcrit<-TRUE}
  dintervention_time<-max(0.25*(intervention_time_sides[2]-intervention_time_sides[1]),dintervention_time_min)
  if (dintervention_time==dintervention_time_min) {tintervention_time<-TRUE}
  dTinf<-max(0.25*(Tinf_sides[2]-Tinf_sides[1]),dTinfmin)
  if (dTinf==dTinfmin) {tTinf<-TRUE}
  #print('here')
  dThosp<-max(0.25*(Thosp_sides[2]-Thosp_sides[1]),dThospmin)
  if (dThosp==dThospmin) {tThosp<-TRUE}
  dTdeath<-max(0.25*(Tdeath_sides[2]-Tdeath_sides[1]),dTdeathmin)
  if (dTdeath==dTdeathmin) {tDeath<-TRUE}
  dTcrit<-max(0.25*(Tcrit_sides[2]-Tcrit_sides[1]),dTcritmin)
  #print('here')
  R0_vec<-seq(R0_sides[1],R0_sides[2],dR0)
  intervention_R_rdxn_vec<-seq(intervention_R_rdxn_sides[1],intervention_R_rdxn_sides[2],dintervention_R_rdxn)
  Rdeath_vec<-seq(Rdeath_sides[1],Rdeath_sides[2],dRdeath)
  Rhosp_vec<-seq(Rhosp_sides[1],Rhosp_sides[2],dRhosp)
  Rcrit_vec<-seq(Rcrit_sides[1],Rcrit_sides[2],dRcrit)
  intervention_time_vec<-seq(intervention_time_sides[1],intervention_time_sides[2],dintervention_time)
  Tinf_vec<-seq(Tinf_sides[1],Tinf_sides[2],dTinf)
  Thosp_vec<-seq(Thosp_sides[1],Thosp_sides[2],dThosp)
  Tdeath_vec<-seq(Tdeath_sides[1],Tdeath_sides[2],dTdeath)
  Tcrit_vec<-seq(Tcrit_sides[1],Tcrit_sides[2],dTcrit)
  #print('here')
  Dist<-array(0.0,dim=c(length(R0_vec),
                    length(intervention_R_rdxn_vec),
                    length(Rdeath_vec),
                    length(Rhosp_vec),
                    length(Rcrit_vec),
                    length(intervention_time_vec),
                    length(Tinf_vec),
                    length(Thosp_vec),
                    length(Tdeath_vec),
                    length(Tcrit_vec))
           )
  #print('here')
  for (i1 in 1:length(R0_vec)) {
    for (i2 in 1:length(intervention_R_rdxn_vec)) {
      for (i3 in 1:length(Rdeath_vec)) {
        for (i4 in 1:length(Rhosp_vec)) {
          for (i5 in 1:length(Rcrit_vec)) {
            for (i6 in 1:length(intervention_time_vec)) {
              for (i7 in 1:length(Tinf_vec)) {
                for (i8 in 1:length(Thosp_vec)) {
                  for (i9 in 1:length(Tdeath_vec)) {
                    for (i10 in 1:length(Tcrit_vec)) {
   #                   print('here')
                      R0<-R0_vec[i1]
                      intervention_R_rdxn<-intervention_R_rdxn_vec[i2]
                      Rdeath<-Rdeath_vec[i3]
                      Rhosp<-Rhosp_vec[i4]
                      Rcrit<-Rcrit_vec[i5]
                      intervention_time<-intervention_time_vec[i6]
                      Tinf<-Tinf_vec[i7]
                      Thosp<-Thosp_vec[i8]
                      Tdeath<-Tdeath_vec[i9]
                      Tcrit<-Tcrit_vec[i10]
                      #print('here')
                      # run the model
                      source("../model_v3.0/DetermineStateSpace_initalization.R")
                      source("../model_v3.0/DetermineStateSpace_dynamics.R")
                      
                      H=H+G+Q
                      C=G+Q
                      
                      percent_infected<-c(rep(0,maxt))
                      for (t in 1: maxt) {
                        percent_infected[t]=round(100*Icum[t]/N)
                      }
                      
                      # compute the distance
                      distance<-0
                      t1<-intervention_time-9
                      t2<-length(DeathPerDay)+t1-1
                      for (t in t1:t2) {
                        distance<-abs(Dday[t]-DeathPerDay[t-t1+1])+distance
                      }
                      
                      t1<-intervention_time+7
                      t2<-length(ICUBedCount)+t1-1
                      for (t in t1:t2) {
                        distance<-abs(C[t]-ICUBedCount[t-t1+1])+distance
                      }
                      
                      Dist[i1,i2,i3,i4,i5,i6,i7,i8,i9,i10]<-distance
                      print(paste("distance:",distance))
                      
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  
  Dmin<-which(D==min(D),arr.ind=TRUE)
  i1<-Dmin[1]
  print(paste("R0 min at",R0_vec[i1]))
  i2<-Dmin[2]
  print(paste("intervention_R_rdxn min at",intervention_R_rdxn_vec[i2]))
  i3<-Dmin[3]
  print(paste("Rdeath min at",Rdeath_vec[i3]))
  i4<-Dmin[4]
  print(paste("Rhosp min at",Rhosp_vec[i4]))
  i5<-Dmin[5]
  print(paste("Rcrit min at",Rcrit_vec[i5]))
  i6<-Dmin[6]
  print(paste("intervention_time min at",intervention_time_vec[i6]))
  i7<-Dmin[7]
  print(paste("Tinf min at",Tinf_vec[i7]))
  i8<-Dmin[8]
  print(paste("Thosp min at",Thosp_vec[i8]))
  i9<-Dmin[9]
  print(paste("Tdeath min at",Tdeath_vec[i9]))
  i10<Dmin[10]
  print(paste("Tcrit min at",Tcrit_vec[i10]))
  
  R0_sides[1]<-R0_vec[i1]-dR0
  R0_sides[2]<-R0_vec[i1]+dR0
  
  intervention_R_rdxn_sides[1]<-intervention_R_rdxn_vec[i2]-dintervention_R_rdxn
  intervention_R_rdxn_sides[2]<-intervention_R_rdxn_vec[i2]+dintervention_R_rdxn
  
  Rdeath_sides[1]<-Rdeath_vec[i3]-dRdeath
  Rdeath_sides[2]<-Rdeath_vec[i3]+dRdeath
  
  Rhosp_sides[1]<-Rhosp_vec[i4]-dRhosp
  Rhosp_sides[2]<-Rhosp_vec[i4]+dRhosp
  
  Rcrit_sides[1]<-Rcrit_vec[i5]-dRcrit
  Rcrit_sides[2]<-Rcrit_vec[i5]+dRcrit
  
  intervention_time_sides[1]<-intervention_time_vec[i6]-dintervention_time
  intervention_time_sides[2]<-intervention_time_vec[i6]+dintervention_time
  
  Tinf_sides[1]<-Tinf_vec[i7]-dTinf
  Tinf_sides[2]<-Tinf_vec[i7]+dTinf
  
  Thosp_sides[1]<-Thosp_vec[i8]-dThosp
  Thosp_sides[2]<-Thosp_vec[i8]+dThosp
  
  Tdeath_sides[1]<-Tdeath_vec[i9]-dTdeath
  Tdeath_sides[2]<-Tdeath_vec[i9]+dTdeath
  
  Tcrit_sides[1]<-Tcrit_vec[i10]-dTcrit
  Tcrit_sides[2]<-Tcrit_vec[i10]+dTcrit
  
  no_min<- tR0 & tintervention_R_rdxn & tRdeath & tRhosp & tRcrit & tintervention_time & tTinf & tThosp & tTdeath & tTcrit
  
}

# if it's on an edge, relocate the box

no_min<-FALSE



while (no_min) {

  if (Dmin[i1]==R0_sides[1] | Dmin[i1]==R0_sides[2]) {
    tR0<-FALSE
    R0_sides[1]<-R0_sides[i1]-dR0
    R0_sides[2]<-R0_sides[i1]+dR0
  } else {
    tR0<-TRUE
  }
  
  if (Dmin[i2]==intervention_R_rdxn_sides[1] | Dmin[i2]==intervention_R_rdxn_sides[2]) {
    tintervention_R_rdxn<-FALSE
    intervention_R_rdxn_sides[1]<-intervention_R_rdxn_sides[i2]-dintervention_R_rdxn
    intervention_R_rdxn_sides[2]<-intervention_R_rdxn_sides[i2]+dintervention_R_rdxn
  } else {
    tintervention_R_rdxn<-TRUE
  }
  
  if (Dmin[i3]==Rdeath_sides[1] | Dmin[i3]==Rdeath_sides[2]) {
    tRdeath<-FALSE
    Rdeath_sides[1]<-Rdeath_sides[i3]-dRdeath
    Rdeath_sides[2]<-Rdeath_sides[i3]+dRdeath
  } else {
    tRdeath<-TRUE
  }
  
  if (Dmin[i4]==Rhosp_sides[1] | Dmin[i4]==Rhosp_sides[2] ) {
    tRhosp<-FALSE
    Rhosp_sides[1]<-Rhosp_sides[i4]-dRhosp
    Rhosp_sides[2]<-Rhosp_sides[i4]+dRhosp
  } else {
    tRhosp<-TRUE
  }
  
  if (Dmin[i5]==Rcrit_sides[1] | Dmin[i5]==Rcrit_sides[2]) {
    tRcrit<-FALSE
    Rcrit_sides[1]<-Rcrit_sides[i5]-dRcrit
    Rcrit_sides[2]<-Rcrit_sides[i5]+dRcrit
  } else {
    tRcrit<-TRUE
  }
  
  if (Dmin[i6]==intervention_time_sides[1] | Dmin[i6]==intervention_time_sides[2]) {
    tintervention_time<-FALSE
    intervention_time_sides[1]<-intervention_time_sides[i6]-dintervention_time
    intervention_time_sides[2]<-intervention_time_sides[i6]+dintervention_time
  } else {
    tintervention_time<-TRUE
  }
  
  if (Dmin[i7]==Tinf_sides[1] | Dmin[i7]==Tinf_sides[2]) {
    tTinf<-FALSE
    Tinf_sides[1]<-Tinf_sides[i7]-dTinf
    Tinf_sides[2]<-Tinf_sides[i7]+dTinf
  } else {
    tTinf<-TRUE
  }
  
  if (Dmin[i8]==Thosp_sides[1] | Dmin[i8]==Thosp_sides[2]) {
    tThosp<-FALSE
    Thosp_sides[1]<-Thosp_sides[i8]-dThosp
    Thosp_sides[2]<-Thosp_sides[i8]+dThosp
  } else {
    tThosp<-TRUE
  }
  
  if (Dmin[i9]==Tdeath_side[1] | Dmin[i9]==Tdeath_sides[2]) {
    tTdeath<-FALSE
    Tdeath_sides[1]<-Tdeath_sides[i9]-dTdeath
    Tdeath_sides[2]<-Tdeath_sides[i9]+dTdeath
  } else {
    tTdeath<-TRUE
  }
  
  if (Dmin[i10]==Tcrit_side[1] | Dmin[i10]==Tcrit_side[2]) {
    tTcrit<-FALSE
    Tcrit_sides[1]<-Tcrit_sides[i10]-dTcrit
    Tcrit_sides[2]<-Tcrit_sides[i10]+dTcrit
  } else {
    tTcrit<-TRUE
  }
  
  R0_vec<-seq(R0_sides[1],R0_sides[2],dR0)
  intervention_R_rdxn_vec<-seq(intervention_R_rdxn_sides[1],intervention_R_rdxn_sides[2],dintervention_R_rdxn)
  Rdeath_vec<-seq(Rdeath_sides[1],Rdeath_sides[2],dRdeath)
  Rhosp_vec<-seq(Rhosp_sides[1],Rhosp_sides[2],dRhosp)
  Rcrit_vec<-seq(Rcrit_sides[1],Rcrit_sides[2],dRcrit)
  intervention_time_vec<-seq(intervention_time_sides[1],intervention_time_sides[2],dintervention_time)
  Tinf_vec<-seq(Tinf_sides[1],Tinf_sides[2],dTinf)
  Thosp_vec<-seq(Thosp_sides[1],Thosp_sides[2],dThosp)
  Tdeath_vec<-seq(Tdeath_sides[1],Tdeath_sides[2],dTdeath)
  Tcrit_vec<-seq(Tcrit_sides[1],Tcrit_sides[2],dTcrit)
  
  D<-array(0.0,dim=c(length(R0_vec),
                     length(intervention_R_rdxn_vec),
                     length(Rdeath_vec),
                     length(Rhosp_vec),
                     length(Rcrit_vec),
                     length(intervention_time_vec),
                     length(Tinf_vec),
                     length(Thosp_vec),
                     length(Tdeath_vec),
                     length(Tcrit_vec))
  )
  
  
  for (i1 in 1:length(R0_vec)) {
    for (i2 in 1:length(intervention_R_rdxn)) {
      for (i3 in 1:length(Rdeath_vec)) {
        for (i4 in 1:length(Rhosp_vec)) {
          for (i5 in 1:length(Rcrit_vec)) {
            for (i6 in 1:length(intervention_time_vec)) {
              for (i7 in 1:length(Tinf_vec)) {
                for (i8 in 1:length(Thosp_vec)) {
                  for (i9 in 1:length(Tdeath_vec)) {
                    for (i10 in 1:length(Tcrit_vec)) {
                      
                      R0<-R0_vec[i1]
                      intervention_R_rdxn<-intervention_R_rdxn_vec[i2]
                      Rdeath<-Rdeath_vec[i3]
                      Rhosp<-Rhosp_vec[i4]
                      Rcrit<-Rcrit_vec[i5]
                      intervention_time<-intervention_time_vec[i6]
                      Tinf<-Tinf_vec[i7]
                      Thosp<-Thosp_vec[i8]
                      Tdeath<-Tdeath_vec[i9]
                      Tcrit<-Tcrit_vec[i10]
                      
                      # run the model
                      source("../model_v3.0/DetermineStateSpace_initalization.R")
                      source("../model_v3.0/DetermineStateSpace_dynamics.R")
                      
                      H=H+G+Q
                      C=G+Q
                      
                      percent_infected<-c(rep(0,maxt))
                      for (t in 1: maxt) {
                        percent_infected[t]=round(100*Icum[t]/N)
                      }
                      
                      # compute the distance
                      distance<-0
                      t1<-intervention_date-9
                      t2<-length(DeathPerDay)-t1+1
                      for (t in t1:t2) {
                        distance<-abs(Dday[t]-DeathPerDay[t-t1+1])+distance
                      }
                      
                      t1<-intervention_date+7
                      t2<-length(ICUBedCount)-t1+1
                      for (t in t1:t2) {
                        distance<-abs(C[t]-ICUBedCount[t-t1+1])+distance
                      }
                      
                      D[i1,i2,i3,i4,i5,i6,i7,i8,i9,i10]<-distance
                      
                      print(paste("distance:",distance))
                      
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  
  Dmin<-which(D==min(D),arr.ind=TRUE)
  i1<-Dmin[1]
  print(paste("R0 min at",R0_vec[i1]))
  i2<-Dmin[2]
  print(paste("intervention_R_rdxn min at",intervention_R_rdxn_vec[i2]))
  i3<-Dmin[3]
  print(paste("Rdeath min at",Rdeath_vec[i3]))
  i4<-Dmin[4]
  print(paste("Rhosp min at",Rhosp_vec[i4]))
  i5<-Dmin[5]
  print(paste("Rcrit min at",Rcrit_vec[i5]))
  i6<-Dmin[6]
  print(paste("intervention_time min at",intervention_time_vec[i6]))
  i7<-Dmin[7]
  print(paste("Tinf min at",Tinf_vec[i7]))
  i8<-Dmin[8]
  print(paste("Thosp min at",Thosp_vec[i8]))
  i9<-Dmin[9]
  print(paste("Tdeath min at",Tdeath_vec[i9]))
  i10<Dmin[10]
  print(paste("Tcrit min at",Tcrit_vec[i10]))
  
  no_min<- tR0 & tintervention_R_rdxn & tRdeath & tRhosp & tRcrit & tintervention_time & tTinf & tThosp & tTdeath & tTcrit
}

