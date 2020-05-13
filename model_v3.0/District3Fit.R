###############################################################
# District3Fit.R
#
# For Phase One of NPI in Indiana, fit all district state space
# simulations to death, hospitalization, and ICU bed curves.
###############################################################
# Ryan Hastings, 4 May 2020
###############################################################
rm(list=ls()) # clear out variables

# load in libraries
library(tidyverse)
library(viridis)

#-------------------- configuration variables ---------------#
ndistricts<-10 # number of districts
DayZero<-as.Date('2020-01-20')
K<-50 # number of neighbors to find
maxt=300 # maximum number of time steps
R0_vec<-c(seq(2.0,3.0,0.05))#c(seq(2.25,3.35,0.05))#c(seq(2.0,3.5,0.05)) # field of R0 over which has been simulated
intervention_R_rdxn_vec<-c(seq(0.1,0.8,0.05))#c(seq(0.5,0.8,0.05))#c(seq(0.0,0.95,0.05)) # field of NPI reduction rate over which has been simulated
Rhosp_vec<-seq(0.01,0.1,0.005)#seq(0.01,0.09,0.005) # hospitalization rate
Rcrit_vec<-seq(0.01,0.03,0.001) # critical rate
plt_outdir<-"out/20200507/"

#---------------- read in district crosswalk ---------------#
DistrictCounty.crosswalk<-read_csv("../CountyDistrict.csv")

# read and process death by county data
DeathByCounty.raw<-read_csv("DeathCurves/COVID_Deaths_Per_Day_Per_County.csv")

DeathByCounty.raw.colnames<-colnames(DeathByCounty.raw)

dum<-DeathByCounty.raw.colnames[3]
death_date1<-as.Date( dum, format='%m/%d/%y' )
dum<-DeathByCounty.raw.colnames[length(DeathByCounty.raw.colnames)]
death_date2<-as.Date( dum, format='%m/%d/%y' )
DeathDates<-seq.Date(death_date1,death_date2,'days')

DeathByCounty.colnames<-DeathByCounty.raw$FIPS

M<-data.matrix(DeathByCounty.raw[,3:ncol(DeathByCounty.raw)])
Mt<-t(M)

DeathByCounty<-as_tibble(Mt)
DeathByCounty<-DeathByCounty%>%bind_cols(DeathDates=DeathDates)

DeathByCounty.colnames<-append(DeathByCounty.colnames,"Dates")
colnames(DeathByCounty)<-DeathByCounty.colnames

DeathByDistrict<-array(0,dim=c(ndistricts,length(DeathDates)))

for (n in 1:92) {
    dum<-DistrictCounty.crosswalk%>%filter(FIPS==DeathByCounty.colnames[n])
    ndistrict<-dum$DISTRICT
    dum<-DeathByCounty%>%select(DeathByCounty.colnames[n])
    for (d in 1:length(DeathDates)) {
      DeathByDistrict[ndistrict,d]<-DeathByDistrict[ndistrict,d]+as.numeric(dum[d,])
  }
}

d1<-as.numeric(death_date1-DayZero+1)
d2<-as.numeric(death_date2-DayZero+1)

#---------------- hospitalizations and ICU by county --------------------#
HospByRegion<-read_csv("DeathCurves/COVID_EMResources_By_Region_By_Date_Fix_0427.csv")

dum<-as.Date(HospByRegion$Date,format='%m/%d/%y')
hosp_date1<-min(dum)
hosp_date2<-max(dum)

h1<-as.numeric(hosp_date1-DayZero+1)
h2<-as.numeric(hosp_date2-DayZero+1)

#------------------------- total date field -----------------------------#
dates_exp<-seq.Date(DayZero,DayZero+maxt-1,"days")

#----------------------- loop through districts -------------------------#

for (n in 7) {
  
  load(paste('StateSpacePhaseOne_District',n,'.RData',sep=""))
  StateSpace.ndims<-dim(StateSpace)
  print(paste("district",n))
  #------------------------- find distances -----------------------------#
  print("finding distance")
  
  # death distance
  D1<-array(0.0,dim=c(StateSpace.ndims[1],StateSpace.ndims[2],StateSpace.ndims[3],StateSpace.ndims[4]))
  tt<-1
  #Dscale<-max(DeathByDistrict[n,])-min(DeathByDistrict[n,])
  for (t in d1:d2) {
    if (!is.na(DeathByDistrict[n,tt])) {
      D1<-D1+abs(StateSpace[,,,,13,t]-(DeathByDistrict[n,tt]*(1+0.25)))*20#/Dscale#*20
    }
    tt<-tt+1
  }
  D1<-D1/max(D1,na.rm=TRUE)
  
  # hospitalization distance
  D2<-array(0.0,dim=c(StateSpace.ndims[1],StateSpace.ndims[2],StateSpace.ndims[3],StateSpace.ndims[4]))
  tt<-1
  dum<-HospByRegion%>%filter(Region==n)
  #Dscale<-max(dum$Hosp,na.rm=TRUE)-min(dum$Hosp,na.rm=TRUE)
  for (t in h1:h2) {
    if (!is.na(dum$`Beds COVID Total`[tt])) {
      D2<-D2+abs(StateSpace[,,,,4,t]-dum$`Beds COVID Total`[tt])#/Dscale
    }
    tt<-tt+1
  }
  D2<-D2/max(D2,na.rm=TRUE)
  
  # ICU distance
  D3<-array(0.0,dim=c(StateSpace.ndims[1],StateSpace.ndims[2],StateSpace.ndims[3],StateSpace.ndims[4]))
  tt<-1
  #Dscale<-max(dum$Crit,na.rm=TRUE)-min(dum$Crit,na.rm=TRUE)
  for (t in h1:h2) {
    if (!is.na(dum$`ICU Beds COVID Total`[tt])) {
      D3<-D3+abs(StateSpace[,,,,5,t]-dum$`ICU Beds COVID Total`[tt])#/Dscale #*10
    }
    tt<-tt+1
  }
  D3<-D3/max(D3,na.rm=TRUE)
  
  # total distance
  D<-D1+D2+D3
  
  #----------------- create output data frames ----------------------# 
  
  # create df.temp
  print('write out prelim')
  Nlines<-StateSpace.ndims[1]*StateSpace.ndims[2]*StateSpace.ndims[3]*StateSpace.ndims[4]
  dum1<-rep(0,Nlines)
  dum2<-rep(0,Nlines)
  dum3<-rep(0,Nlines)
  dum4<-rep(0,Nlines)
  Dflat<-rep(0,Nlines)
  rank<-rep(0,Nlines)
  nn<-1
  for (i in 1:StateSpace.ndims[1]) {
    for (j in 1:StateSpace.ndims[2]) {
      for (k in 1:StateSpace.ndims[3]) {
        for (l in 1:StateSpace.ndims[4]) {
          dum1[nn]<-i
          dum2[nn]<-j
          dum3[nn]<-k
          dum4[nn]<-l
          Dflat[nn]<-D[i,j,k,l]
          rank[nn]<-nn
          nn<-nn+1
        }
      }
    }
  }
  
  df.temp<-data.frame(dum1,dum2,dum3,dum4,Dflat)
  df.temp<-df.temp%>%arrange(Dflat)
  
  # create D.frame
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
  nn<-1
  kk<-1
  while (kk<=K) {
    
    for (t in 1:maxt) {
      
      rank[nn]<-kk
      
      i<-df.temp$dum1[kk]
      j<-df.temp$dum2[kk]
      k<-df.temp$dum3[kk]
      l<-df.temp$dum4[kk]
      
      dim1[nn]<-i
      dim2[nn]<-j
      dim3[nn]<-k
      dim4[nn]<-l
      
      R0[nn]<-R0_vec[i]
      intervention_R_rdxn[nn]<-intervention_R_rdxn_vec[j]
      Rhosp[nn]<-Rhosp_vec[k]
      Rcrit[nn]<-Rcrit_vec[l]
      Dflat[nn]<-df.temp$Dflat[kk]
      
      S[nn]<-StateSpace[i,j,k,l,1,t]
      E[nn]<-StateSpace[i,j,k,l,2,t]
      In[nn]<-StateSpace[i,j,k,l,3,t]
      H[nn]<-StateSpace[i,j,k,l,4,t]
      C[nn]<-StateSpace[i,j,k,l,5,t]
      De[nn]<-StateSpace[i,j,k,l,6,t]
      R[nn]<-StateSpace[i,j,k,l,7,t]
      Ecum[nn]<-StateSpace[i,j,k,l,8,t]
      Icum[nn]<-StateSpace[i,j,k,l,9,t]
      Hcum[nn]<-StateSpace[i,j,k,l,10,t]
      Ccum[nn]<-StateSpace[i,j,k,l,11,t]
      percent_infected[nn]<-StateSpace[i,j,k,l,12,t]
      Dday[nn]<-StateSpace[i,j,k,l,13,t]
      nn<-nn+1
    }
    
    kk<-kk+1
  }
  
  
  
  D.frame<-data.frame(rank=as.factor(rank),day,R0=R0,InterventionReduction=intervention_R_rdxn,
                      HospRate=Rhosp,CritRate=Rcrit,Dates=Dates,
                      Susceptible=S,Exposed=E,Infectious=In,Hospitalized=H,Critical=C,
                      Deceased=De,DeathPerDay=Dday,ExposedCumulative=Ecum,InfectiousCumulative=Icum,
                      HospitalizedCumulative=Hcum,PercentInfected=percent_infected,Distance=Dflat)
 
  
  write_csv(D.frame,path=paste(plt_outdir,'District',n,'_simulation.csv',sep=''))
  
  #------------------ plotting -----------------------------------#
  print('plotting')
  
  # set up data frames for plotting
  df.deaths<-data.frame( Dates=seq.Date(death_date1,death_date2,'days'),
                         DeathCount=DeathByDistrict[n,])#deaths$DeathCount[1:(nrow(deaths)-1)],
  write_csv(df.deaths,path=paste(plt_outdir,'District',n,'_deaths.csv',sep=''))
                        # CumDeathCount=CumDeathCount[1:(nrow(deaths)-1)])
  dum<-HospByRegion%>%filter(Region==n)
  df.hosp<-data.frame( Dates=seq.Date(hosp_date1,hosp_date2,'days'),
                       H=dum$`Beds COVID Total` )
  write_csv(df.hosp,path=paste(plt_outdir,'District',n,'_hosp.csv',sep=''))
  df.crit<-data.frame( Dates=seq.Date(hosp_date1,hosp_date2,'days'),
                       C=dum$`ICU Beds COVID Total`)
  write_csv(df.crit,path=paste(plt_outdir,'District',n,'_ICU.csv',sep=''))
  
  # # plot fits
  # plt1<-ggplot()+geom_point(data=df.deaths,aes(x=Dates,y=DeathCount))+ggtitle(paste('district',n))+
  #   geom_line(data=D.frame,aes(x=Dates,y=DeathPerDay,group=rank,color=rank))+scale_color_viridis(discrete=TRUE)
  # plt2<-ggplot()+geom_point(data=df.hosp,aes(x=Dates,y=H))+ggtitle(paste('district',n))+
  #   geom_line(data=D.frame,aes(x=Dates,y=Hospitalized,group=rank,color=rank))+scale_color_viridis(discrete=TRUE)
  # plt3<-ggplot()+geom_point(data=df.crit,aes(x=Dates,y=C))+ggtitle(paste('district',n))+
  #   geom_line(data=D.frame,aes(x=Dates,y=Critical,group=rank,color=rank))+scale_color_viridis(discrete=TRUE)
  # 
  # # for (k in 1:K) {
  # #   
  # #   plt1<-plt1+geom_line(data=D.frame%>%filter(rank==k),aes(x=Dates,y=DeathPerDay))
  # #   plt2<-plt2+geom_line(data=D.frame%>%filter(rank==k),aes(x=Dates,y=Hospitalized))
  # #   plt3<-plt3+geom_line(data=D.frame%>%filter(rank==k),aes(x=Dates,y=Critical))
  # #   
  # # }
  # 
  # plt1<-plt1+xlim(death_date1,death_date2)+ylab("Deceased")
  # plt2<-plt2+xlim(hosp_date1,hosp_date2)+ylab("Hospital beds")
  # plt3<-plt3+xlim(hosp_date1,hosp_date2)+ylab("ICU beds")
  # 
  # 
  # print(plt1)
  # ggsave(paste(plt_outdir,"district",n,"_plt1.png",sep=""),plot=plt1)
  # print(plt2)
  # ggsave(paste(plt_outdir,"district",n,"_plt2.png",sep=""),plot=plt2)
  # print(plt3)
  # ggsave(paste(plt_outdir,"district",n,"_plt3.png",sep=""),plot=plt3)
  # 
  # plot projections
  plt4<-ggplot()+geom_point(data=df.deaths,aes(x=Dates,y=DeathCount))+ylab("Deaths per day")+ggtitle(paste('district',n))+
    geom_line(data=D.frame,aes(x=Dates,y=DeathPerDay,group=rank,color=rank))+scale_color_viridis(discrete=TRUE)+
    scale_x_date(limits=c(death_date1-7,death_date2+14))
  plt5<-ggplot()+geom_point(data=df.hosp,aes(x=Dates,y=H))+ylab("Hospitalizations")+ggtitle(paste('district',n))+
    geom_line(data=D.frame,aes(x=Dates,y=Hospitalized,group=rank,color=rank))+scale_color_viridis(discrete=TRUE)+
    scale_x_date(limits=c(hosp_date1-7,hosp_date2+14))
  plt6<-ggplot()+geom_point(data=df.crit,aes(x=Dates,y=C))+ylab("ICU beds")+ggtitle(paste('district',n))+
    geom_line(data=D.frame,aes(x=Dates,y=Critical,group=rank,color=rank))+scale_color_viridis(discrete=TRUE)+
    scale_x_date(limits=c(hosp_date1-7,hosp_date2+14))
  # plt7<-ggplot()+geom_point(data=df.deaths,aes(x=Dates,y=CumDeathCount))+ylab("Cumulative death count")+
  #   geom_line(data=D.frame,aes(x=Dates,y=Deceased,group=rank,color=rank))+scale_color_viridis(discrete=TRUE)
  
  # for (k in 1:K) {
  #   plt4<-plt4+geom_line(data=D.frame%>%filter(rank==k),aes(x=Dates,y=DeathPerDay))
  #   plt5<-plt5+geom_line(data=D.frame%>%filter(rank==k),aes(x=Dates,y=Hospitalized))
  #   plt6<-plt6+geom_line(data=D.frame%>%filter(rank==k),aes(x=Dates,y=Critical))
  #   plt7<-plt7+geom_line(data=D.frame%>%filter(rank==k),aes(x=Dates,y=Deceased))
  # }
  
  print(plt4)
  ggsave(paste(plt_outdir,"district",n,"_plt4.png",sep=""),plot=plt4)
  print(plt5)
  ggsave(paste(plt_outdir,"district",n,"_plt5.png",sep=""),plot=plt5)
  print(plt6)
  ggsave(paste(plt_outdir,"district",n,"_plt6.png",sep=""),plot=plt6)
  # print(plt7)
  
  # plot histograms
  print( ggplot(D.frame,aes(x=R0))+geom_histogram(binwidth=0.05)+xlim(R0_vec[1],R0_vec[length(R0_vec)]) )+ggtitle(paste('district',n))
  ggsave(paste(plt_outdir,"district",n,"_plt7.png",sep=""))
  print( ggplot(D.frame,aes(x=InterventionReduction))+geom_histogram(binwidth=0.05)+xlim(0.0,0.95) )+ggtitle(paste('district',n))
  ggsave(paste(plt_outdir,"district",n,"_plt8.png",sep=""))
  print( ggplot(D.frame,aes(x=HospRate))+geom_histogram(binwidth=0.005)+xlim(Rhosp_vec[1],Rhosp_vec[length(Rhosp_vec)]))+ggtitle(paste('district',n))
  ggsave(paste(plt_outdir,"district",n,"_plt9.png",sep=""))
  print( ggplot(D.frame,aes(x=CritRate))+geom_histogram(binwidth=0.001)+xlim(Rcrit_vec[1],Rcrit_vec[length(Rcrit_vec)]) )+ggtitle(paste('district',n))
  ggsave(paste(plt_outdir,"district",n,"_plt10.png",sep=""))
  
  print(paste("done with district",n))
  
}