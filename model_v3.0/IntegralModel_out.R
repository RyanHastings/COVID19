Etot<-rep(0.0,maxt)
Itot<-rep(0.0,maxt)
Htot<-rep(0.0,maxt)
Ctot<-rep(0.0,maxt)
Rtot<-rep(0.0,maxt)
Dtot<-rep(0.0,maxt)


for (t in 1:maxt) {
  
  t0<-max(t,t-Tinc)
  Etot[t]<-sum(Enew[t0:t])
  
  t0<-max(t,t-Tinf)
  Itot[t]<-sum(Inew[t0:t])
  
  t0<-max(t,t-Trecov)
  t1<-max(t,t-Tdeath)
  Htot[t]<-sum(Hnew[t0:t])+sum(Cnew[t0:t])+sum(Qnew[t1:t])
  Ctot[t]<-sum(Cnew[t0:t])+sum(Qnew[t1:t])
  
  Rtot[t]<-sum(Rnew[1:t])
  Dtot[t]<-sum(Dnew[1:t])
  
}

StateSpace[i,j,k,l,1,]<-S
StateSpace[i,j,k,l,2,]<-Enew
StateSpace[i,j,k,l,3,]<-Etot
StateSpace[i,j,k,l,4,]<-Inew
StateSpace[i,j,k,l,5,]<-Itot
StateSpace[i,j,k,l,6,]<-Hnew+Cnew+Qnew
StateSpace[i,j,k,l,7,]<-Htot
StateSpace[i,j,k,l,8,]<-Cnew+Qnew
StateSpace[i,j,k,l,9,]<-Ctot
StateSpace[i,j,k,l,10,]<-Rnew
StateSpace[i,j,k,l,11,]<-Rtot
StateSpace[i,j,k,l,12,]<-Dnew
StateSpace[i,j,k,l,13,]<-Dtot

# df.SEIR<-data.frame(day=seq(1,maxt),Susceptible=S,NewExposed=Enew,TotalExposed=Etot,
#                     NewInfectious=Inew,TotalInfectious=Itot,
#                     NewHospitalized=Hnew+Cnew+Qnew,TotalHospitalized=Htot,
#                     NewCritical=Cnew+Qnew,TotalCritical=Ctot,
#                     NewRecovered=Rnew,TotalRecovered=Rtot,
#                     NewDeceased=Dnew,TotalDeceased=Dtot)
# 
# print( ggplot(df.SEIR,aes(x=day))+
#       #   geom_line(aes(y=Susceptible,color="S"))+
#       #   geom_line(aes(y=TotalExposed,color="E"))+
#       #   geom_line(aes(y=TotalInfectious,color="I"))+
#       #   geom_line(aes(y=TotalRecovered,color="R"))+
#          geom_line(aes(y=TotalDeceased,color="D"))+
#          geom_line(aes(y=TotalHospitalized,color="H"))+
#          geom_line(aes(y=TotalCritical,color="C"))+
#         geom_line(aes(y=NewDeceased,color="Dday"))+
#         ggtitle(paste(R0,",",intervention_R_rdxn))
# )