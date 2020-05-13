#######################################################################
# Fit3curves.R
#
# Given a state space simulation from DetermineStateSpace.R, find the K-
# nearest neighbors to the observed death, hospitalization, and ICU counts.
#######################################################################
# Ryan Hastings, 5 May 2020
#######################################################################
rm(list=ls()) # clear out variables

# load in libraries
library(tidyverse)
library(viridis)

#--------------------- configuration variables -----------------------#
R0_vec<-c(seq(2.5,3.5,0.05))
intervention_R_rdxn_vec<-seq(0.0,0.95,0.05)
Rhosp_vec<-seq(0.025,0.09,0.005)#0.06 # hospitalization rate
Rcrit_vec<-seq(0.01,0.04,0.001)#0.024 # critical rate
nvars<-13 # number of variables for output (you probably don't want to change this unless you want a huge coding headache)
Npop<-6.692e6 # population of state

# NPI timing
intervention_time<-65#65#79 # days after day 0 NPI is introduced
StateSpace_filename<-"StateSpace_day65_detailRcrit_expanded.Rdata"#StateSpace_day65_detailRcrit.Rdata" #_expanded.Rdata"

# other vars
track_filename<-"DeathCurves/COVID_Deaths_Per_Day.csv" # deaths per day file
DayZero<-as.Date('2020-01-20') # Day Zero
maxt<-300 # maximum number of time steps
hosp_file<-"DeathCurves/EMResources_COVID_ICU_Snapshot.csv" # file with statewide hospitalization numbers
hosp_date1<-as.Date('2020-04-05') # first date of hospital bed records
K=5
dt=1.0

DeathByDayVar<-13
HospTotVar<-4
CritTotVar<-5

####################################################################
# load in data
print("loading in data")

# load in state space
load(StateSpace_filename)
StateSpace.ndims<-dim(StateSpace)
StateSpace.new<-array(0.0,dim=c(StateSpace.ndims[1],
                                StateSpace.ndims[2],
                                StateSpace.ndims[3],
                                StateSpace.ndims[4],
                                StateSpace.ndims[5],
                                StateSpace.ndims[6]*dt))


# read in death curve, arrange on timeline
deaths<-read_csv(track_filename)

CumDeathCount<-c(deaths$DeathCount[1])
for (i in 2:nrow(deaths)) {
  CumDeathCount<-append(CumDeathCount,CumDeathCount[i-1]+deaths$DeathCount[i])
}
deaths$CumDeathCount<-CumDeathCount

death_date1<-as.Date(deaths$Date_of_death[1],format='%m/%d/%y')
death_date2<-as.Date(deaths$Date_of_death[nrow(deaths)-1],format='%m/%d/%y')

d1<-as.numeric(as.Date(deaths$Date_of_death[1],format='%m/%d/%y')-DayZero+1)
d2<-as.numeric(as.Date(deaths$Date_of_death[nrow(deaths)-1],format="%m/%d/%y")-DayZero+1)

# set up general dates array
dates_exp<-seq.Date(DayZero,DayZero+maxt-1,"days")

# read in ICU and hospital curve, arrange on timeline
hosp.raw<-read_csv(hosp_file)

H.obs<-hosp.raw$`All Beds COVID Total`[5:nrow(hosp.raw)]
C.obs<-hosp.raw$`ICU Beds COVID Total`

crit_date1<-as.Date( paste(hosp.raw$Date[1],'2020',sep='-'), format='%d-%b-%Y' )
hosp_date2<-as.Date( paste(hosp.raw$Date[nrow(hosp.raw)],'2020',sep='-'), format='%d-%b-%Y')

h1<-as.numeric(hosp_date1-DayZero+2)
c1<-as.numeric(crit_date1-DayZero+2)
h2<-as.numeric(hosp_date2-DayZero+1)

#############################################################################
# find distances
print("finding distance")

# death distances
D1<-array(0.0,dim=c(StateSpace.ndims[1],StateSpace.ndims[2],StateSpace.ndims[3],StateSpace.ndims[4]))
tt<-1
#Dscale<-max(deaths$DeathCount)-min(deaths$DeathCount)
for (t in d1:d2) {
  if (!is.na(deaths$DeathCount[tt])) {
    D1<-D1+abs(StateSpace[,,,,DeathByDayVar,t]-(deaths$DeathCount[tt]+10))*20#/Dscale#*20
  }
  tt<-tt+1
}
#D1<-D1/max(D1)

# hospitalization distances
D2<-array(0.0,dim=c(StateSpace.ndims[1],StateSpace.ndims[2],StateSpace.ndims[3],StateSpace.ndims[4]))
tt<-1
Dscale<-max(H.obs)-min(H.obs)
for (t in h1:h2) {
  if (!is.na(H.obs[tt])) {
    D2<-D2+abs(StateSpace[,,,,HospTotVar,t]-H.obs[tt])#/Dscale
  }
  tt<-tt+1
}
#D2<-D2/max(D2)

# ICU distances
D3<-array(0.0,dim=c(StateSpace.ndims[1],StateSpace.ndims[2],StateSpace.ndims[3],StateSpace.ndims[4]))
tt<-1
#Dscale<-max(C.obs)-min(C.obs)
for (t in c1:h2) {
  if (!is.na(C.obs[tt])) {
    D3<-D3+abs(StateSpace[,,,,CritTotVar,t]-C.obs[tt])#/Dscale #*10
  }
  tt<-tt+1
}
#D3<-D3/max(D3)

# total distance
D<-D1+D2+D3

#######################################################################
# create response data frames

# create df.temp, which contains only indices and distances
print('write out prelim')
Nlines<-StateSpace.ndims[1]*StateSpace.ndims[2]*StateSpace.ndims[3]*StateSpace.ndims[4]
dum1<-rep(0,Nlines)
dum2<-rep(0,Nlines)
dum3<-rep(0,Nlines)
dum4<-rep(0,Nlines)
Dflat<-rep(0,Nlines)
rank<-rep(0,Nlines)
n<-1
for (i in 1:StateSpace.ndims[1]) {
  for (j in 1:StateSpace.ndims[2]) {
    for (k in 1:StateSpace.ndims[3]) {
      for (l in 1:StateSpace.ndims[4]) {
        dum1[n]<-i
        dum2[n]<-j
        dum3[n]<-k
        dum4[n]<-l
        Dflat[n]<-D[i,j,k,l]
        rank[n]<-n
        n<-n+1
      }
    }
  }
}

df.temp<-data.frame(dum1,dum2,dum3,dum4,Dflat)
df.temp<-df.temp%>%arrange(Dflat)

# set up D.frame, which contains all the data for the K nearest neighbors
print("the real output")

Nlines<-K*StateSpace.ndims[6]
R0<-rep(0,Nlines)
intervention_R_rdxn<-rep(0,Nlines)
Rhosp<-rep(0,Nlines)
Rcrit<-rep(0,Nlines)
Dflat<-rep(0,Nlines)
dim1<-rep(0,Nlines)
dim2<-rep(0,Nlines)
dim3<-rep(0,Nlines)
dim4<-rep(0,Nlines)
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
Ccum<-rep(0,Nlines)
Dday<-rep(0,Nlines)
Dates<-rep(dates_exp,Nlines/StateSpace.ndims[6])
percent_infected<-rep(0,Nlines)
rank<-rep(0,Nlines)
n<-1
kk<-1
while (kk<=K) {

  for (t in 1:maxt) {

    rank[n]<-kk

    i<-df.temp$dum1[kk]
    j<-df.temp$dum2[kk]
    k<-df.temp$dum3[kk]
    l<-df.temp$dum4[kk]

    dim1[n]<-i
    dim2[n]<-j
    dim3[n]<-k
    dim4[n]<-l

    R0[n]<-R0_vec[i]
    intervention_R_rdxn[n]<-intervention_R_rdxn_vec[j]
    Rhosp[n]<-Rhosp_vec[k]
    Rcrit[n]<-Rcrit_vec[l]
    Dflat[n]<-df.temp$Dflat[kk]

    S[n]<-StateSpace[i,j,k,l,1,t]
    E[n]<-StateSpace[i,j,k,l,2,t]
    In[n]<-StateSpace[i,j,k,l,3,t]
    H[n]<-StateSpace[i,j,k,l,4,t]
    C[n]<-StateSpace[i,j,k,l,5,t]
    De[n]<-StateSpace[i,j,k,l,6,t]
    R[n]<-StateSpace[i,j,k,l,7,t]
    Ecum[n]<-StateSpace[i,j,k,l,8,t]
    Icum[n]<-StateSpace[i,j,k,l,9,t]
    Hcum[n]<-StateSpace[i,j,k,l,10,t]
    Ccum[n]<-StateSpace[i,j,k,l,11,t]
    percent_infected<-StateSpace[i,j,k,l,12,t]
    Dday[n]<-StateSpace[i,j,k,l,13,t]
    n<-n+1
  }

  kk<-kk+1
}


D.frame<-data.frame(dim1,dim2,dim3,dim4,day,R0=R0,InterventionReduction=intervention_R_rdxn,
                    HospRate=Rhosp,CritRate=Rcrit,
                    Distance=Dflat,
                    Susceptible=S,Exposed=E,Infectious=In,Hospitalized=H,Critical=C,
                    Deceased=De,ExposedCumulative=Ecum,InfectiousCumulative=Icum,
                    HospitalizedCumulative=Hcum,PercentInfected=percent_infected,DeathPerDay=Dday,
                    Dates=Dates,rank=as.factor(rank))

##################################################################################
# plot k nearest neighbors
print('plotting')

# set up data frames for plotting obs
df.deaths<-data.frame( Dates=seq.Date(death_date1,death_date2,'days'),
                       DeathCount=deaths$DeathCount[1:(nrow(deaths)-1)],
                       CumDeathCount=CumDeathCount[1:(nrow(deaths)-1)])
df.hosp<-data.frame( Dates=seq.Date(hosp_date1,hosp_date2,'days'),
                     H<-H.obs )
df.crit<-data.frame( Dates=seq.Date(crit_date1,hosp_date2,'days'),
                     C<-C.obs)

#--------------------------------- plot fits ------------------------------------#
plt1<-ggplot()+geom_point(data=df.deaths,aes(x=Dates,y=DeathCount))+
  geom_line(data=D.frame,aes(x=Dates,y=DeathPerDay,group=rank,color=rank))+scale_color_viridis(discrete=TRUE)
plt2<-ggplot()+geom_point(data=df.hosp,aes(x=Dates,y=H))+
  geom_line(data=D.frame,aes(x=Dates,y=Hospitalized,group=rank,color=rank))+scale_color_viridis(discrete=TRUE)
plt3<-ggplot()+geom_point(data=df.crit,aes(x=Dates,y=C))+
  geom_line(data=D.frame,aes(x=Dates,y=Critical,group=rank,color=rank))+scale_color_viridis(discrete=TRUE)

# for (k in 1:K) {
# 
#   plt1<-plt1+geom_line(data=D.frame%>%filter(rank==k),aes(x=Dates,y=DeathPerDay))
#   plt2<-plt2+geom_line(data=D.frame%>%filter(rank==k),aes(x=Dates,y=Hospitalized))
#   plt3<-plt3+geom_line(data=D.frame%>%filter(rank==k),aes(x=Dates,y=Critical))
# 
# }

plt1<-plt1+xlim(death_date1,death_date2)+ylab("Deceased")
plt2<-plt2+xlim(hosp_date1,hosp_date2)+ylab("Hospital beds")
plt3<-plt3+xlim(crit_date1,hosp_date2)+ylab("ICU beds")

print(plt1)
print(plt2)
print(plt3)

#------------------------------ plot projections ------------------------------------------#
plt4<-ggplot()+geom_point(data=df.deaths,aes(x=Dates,y=DeathCount))+ylab("Deaths per day")+
  geom_line(data=D.frame,aes(x=Dates,y=DeathPerDay,group=rank,color=rank))+scale_color_viridis(discrete=TRUE)
plt5<-ggplot()+geom_point(data=df.hosp,aes(x=Dates,y=H))+ylab("Hospitalizations")+
  geom_line(data=D.frame,aes(x=Dates,y=Hospitalized,group=rank,color=rank))+scale_color_viridis(discrete=TRUE)
plt6<-ggplot()+geom_point(data=df.crit,aes(x=Dates,y=C))+ylab("ICU beds")+
  geom_line(data=D.frame,aes(x=Dates,y=Critical,group=rank,color=rank))+scale_color_viridis(discrete=TRUE)
plt7<-ggplot()+geom_point(data=df.deaths,aes(x=Dates,y=CumDeathCount))+ylab("Cumulative death count")+
  geom_line(data=D.frame,aes(x=Dates,y=Deceased,group=rank,color=rank))+scale_color_viridis(discrete=TRUE)

# for (k in 1:K) {
#   plt4<-plt4+geom_line(data=D.frame%>%filter(rank==k),aes(x=Dates,y=DeathPerDay))
#   plt5<-plt5+geom_line(data=D.frame%>%filter(rank==k),aes(x=Dates,y=Hospitalized))
#   plt6<-plt6+geom_line(data=D.frame%>%filter(rank==k),aes(x=Dates,y=Critical))
#   plt7<-plt7+geom_line(data=D.frame%>%filter(rank==k),aes(x=Dates,y=Deceased))
# }

print(plt4)
print(plt5)
print(plt6)
print(plt7)

#---------------------- plot histograms of parameters --------------------------------------#
print( ggplot(D.frame,aes(x=R0))+geom_histogram(binwidth=0.05)+xlim(R0_vec[1],R0_vec[length(R0_vec)]) )
print( ggplot(D.frame,aes(x=InterventionReduction))+geom_histogram(binwidth=0.05)+xlim(0.0,0.95) )
print( ggplot(D.frame,aes(x=HospRate))+geom_histogram(binwidth=0.005)+xlim(Rhosp_vec[1],Rhosp_vec[length(Rhosp_vec)]))
print( ggplot(D.frame,aes(x=CritRate))+geom_histogram(binwidth=0.001)+xlim(Rcrit_vec[1],Rcrit_vec[length(Rcrit_vec)]) )