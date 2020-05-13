###############################################################
# FitUrbanCountyCurves.R
#
# Try to fit curves separately for urban and rural counties.  This
# turned out to be impractical and was abandoned.
###############################################################
# 12 May 2020, Ryan Hastings
###############################################################

DeathByCounty.raw<-read_csv("DeathCurves/COVID_Deaths_Per_Day_Per_County.csv")

DeathByCounty.raw.colnames<-colnames(DeathByCounty.raw)

dum<-DeathByCounty.raw.colnames[2]
date1<-as.Date( dum, format='%m/%d/%y' )
dum<-DeathByCounty.raw.colnames[length(DeathByCounty.raw.colnames)]
date2<-as.Date( dum, format='%m/%d/%y' )
DeathDates<-seq.Date(date1,date2,'days')

DeathByCounty.colnames<-DeathByCounty.raw$COUNTY

M<-data.matrix(DeathByCounty.raw[,2:ncol(DeathByCounty.raw)])
Mt<-t(M)

DeathByCounty<-as_tibble(Mt)
DeathByCounty<-DeathByCounty%>%bind_cols(DeathDates=DeathDates)

colnames(DeathByCounty)<-DeathByCounty.colnames

dum<-read_csv("../CountyUrbanRural.csv")
dum<-dum%>%bind_cols(idx=1:92)
dum2<-dum%>%filter(Urban_Rural_Custom=="Urban")
urban_county_idx<-dum2$idx
dum2<-dum%>%filter(Urban_Rural_Custom=="Rural")
rural_county_idx<-dum2$idx

DeathByCounty.Rural<-DeathByCounty[,rural_county_idx]
DeathByCounty.Urban<-DeathByCounty[,urban_county_idx]

DeathByCounty.Rural<-DeathByCounty.Rural%>%mutate(TotalDeaths=rowSums(.))
DeathByCounty.Urban<-DeathByCounty.Urban%>%mutate(TotalDeaths=rowSums(.))

load("StateSpace_day65.Rdata")
# get distances
StateSpace.ndims<-dim(StateSpace)
trackdate<-seq.Date(as.Date('2020-01-20'),DeathDates[length(DeathDates)],'days')
Rural.track<-c(rep(0,as.numeric(date1-as.Date('2020-01-20'))),DeathByCounty.Rural$TotalDeaths)
Rural.D<-array(0.0,dim=c(StateSpace.ndims[1],StateSpace.ndims[2]))
for (t in 1:length(Rural.track)) {
  Rural.D<-Rural.D+abs(Rural.track[t]-StateSpace[,,13,t])
}

Rural.mins<-which(Rural.D==min(Rural.D),arr.ind=TRUE)

Urban.track<-c(rep(0,as.numeric(date1-as.Date('2020-01-20'))),DeathByCounty.Urban$TotalDeaths)
Urban.D<-array(0.0,dim=c(StateSpace.ndims[1],StateSpace.ndims[2]))
for (t in 1:length(Urban.track)) {
  Urban.D<-Urban.D+abs(Urban.track[t]-StateSpace[,,13,t])
}

Urban.mins<-which(Urban.D==min(Urban.D),arr.ind=TRUE)