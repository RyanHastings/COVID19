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
