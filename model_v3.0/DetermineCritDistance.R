######################################################
# DetermineCritDistance.R
#
# Code to find K nearest neighbors of the observed ICU
# bed counts to simulated bed counts in the total statewide
# state space.  Depracated with the writing of Fit3curves.R.
######################################################
# Ryan Hastings, 5 May 2020
######################################################

# install packages
library(tidyverse)

# load data
ICU<-read_csv('DeathCurves/ICUBeds_200407.csv')
load('StateSpace_day65.Rdata')

# some configuration variables
maxt=300
R0=2.95 # determined by fitting death curve in separate code
Rcrit_vec<-seq(0.01,0.03,0.001)
intervention_R_rdxn_vec<-c(seq(0.0,0.95,0.05))
times<-c(seq(0,maxt))
nvars<-12

# set up time arrays
days<-c(seq(73,(nrow(ICU)+72)))
dates<-seq.Date(as.Date('2020-04-01'),as.Date(paste('2020-04',nrow(ICU),sep='-')),'days')
dates_dum<-seq.Date(as.Date('2020-01-20'),as.Date('2020-01-20')+maxt,'days')

#####################################################
# get distances
StateSpace.ndims<-dim(StateSpace)
D<-array(0.0,dim=c(StateSpace.ndims[1],StateSpace.ndims[2]))
for (t in days) {
  D<-D+abs(ICU$ICUBedsCOVIDTotal[t-72]-StateSpace[,,5,t])
}

# write out to array
Nlines<-length(Rcrit_vec)*length(intervention_R_rdxn_vec)*maxt
Rcrit<-rep(0,Nlines)
intervention_R_rdxn<-rep(0,Nlines)
Dflat<-rep(0,Nlines)
dim1<-rep(0,Nlines)
dim2<-rep(0,Nlines)
day<-rep(0,Nlines)
S<-rep(0,Nlines)
E<-rep(0,Nlines)
In<-rep(0,Nlines)
H<-rep(0,Nlines)
C<-rep(0,Nlines)
De<-rep(0,Nlines)
R<-rep(0,Nlines)
Ecum<-rep(0,Nlines)
Icum<-rep(0,Nlines)
Hcum<-rep(0,Nlines)
alltimes<-rep(0,Nlines)
Dates<-rep(0,Nlines)
percent_infected<-rep(0,Nlines)
n<-1
for (i in 1:length(Rcrit_vec)) {
  for (j in 1:length(intervention_R_rdxn_vec)) {
    for (t in 1:maxt) {
      dim1[n]<-i
      dim2[n]<-j
      day[n]<-t
      Rcrit[n]<-Rcrit_vec[i]
      intervention_R_rdxn[n]<-intervention_R_rdxn[j]
      Dflat[n]<-D[i,j]
      S[n]<-StateSpace[i,j,1,t]
      E[n]<-StateSpace[i,j,2,t]
      In[n]<-StateSpace[i,j,3,t]
      H[n]<-StateSpace[i,j,4,t]
      C[n]<-StateSpace[i,j,5,t]
      De[n]<-StateSpace[i,j,6,t]
      R[n]<-StateSpace[i,j,7,t]
      Ecum[n]<-StateSpace[i,j,8,t]
      Icum[n]<-StateSpace[i,j,9,t]
      Hcum[n]<-StateSpace[i,j,10,t]
      percent_infected<-StateSpace[i,j,11,t]
      alltimes[n]<-t
      Dates[n]<-dates_dum[t]
      n<-n+1
    }
  }
}
D.frame<-data.frame(dim1,dim2,day=alltimes,Rcrit=Rcrit,InterventionReduction=intervention_R_rdxn,Distance=Dflat,
                    Susceptible=S,Exposed=E,Infectious=In,Hospitalized=H,Critical=C,
                    Deceased=De,ExposedCumulative=Ecum,InfectiousCumulative=Icum,
                    HospitalizedCumulative=Hcum,PercentInfected=percent_infected,
                    Dates=Dates)
D.frame<-D.frame%>%arrange(Distance,day)

df.track<-data.frame(Critical=ICU$ICUBedsCOVIDTotal,Days=days,date=dates)

dum<-which(D==min(D),arr.ind=TRUE)
print(dum)
i=dum[1]
j=dum[2]
print(Rcrit_vec[i])
print(intervention_R_rdxn_vec[j])

print( ggplot()+
     #    geom_line(data=filter(D.frame,dim1==11,dim2==1),aes(x=day,y=Critical,color='no reduction, Rcrit=1.2%'))+
      #   geom_line(data=filter(D.frame,dim1==14,dim2==6),aes(x=day,y=Critical,color="25% reduction, Rcrit=1.6%"))+
      #   geom_line(data=filter(D.frame,dim1==18,dim2==11),aes(x=day,y=Critical,color='50% reduction, Rcrit=1.9%'))+
      #   geom_line(data=filter(D.frame,dim1==21,dim2==14),aes(x=day,y=Critical,color='60% reduction, Rcrit=2%'))+
         geom_line(data=filter(D.frame,dim1==i-1,dim2==j-2,),
                   aes(x=day,y=Critical))+
         geom_point(data=df.track,aes(x=Days,y=Critical))+
        # scale_x_date(limits=c(df.track$date[1],df.track$date[nrow(df.track)]))+
       xlim(c(days[1],days[nrow(df.track)]))+
         ylim(c(500,1000))
)
