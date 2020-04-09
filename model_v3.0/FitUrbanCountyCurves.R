DeathByCounty.raw<-read_xlsx("DeathCurves/COVID-19 Cumulative Death Counts by County and Date of Death.xlsx")

DeathByCounty.raw.colnames<-colnames(DeathByCounty.raw)

dum<-DeathByCounty.raw.colnames[2]
date1<-as.Date( paste('2020',substr(dum,2,3),substr(dum,4,5),sep='-') )
dum<-DeathByCounty.raw.colnames[length(DeathByCounty.raw.colnames)]
date2<-as.Date( paste('2020',substr(dum,2,3),substr(dum,4,5),sep='-') )

DeathDates<-seq.Date(date1,date2,'days')

DeathByCounty.colnames<-DeathByCounty.raw$County

M<-data.matrix(DeathByCounty.raw[,2:(length(DeathDates)+1)])
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
trackdate<-seq.Date('2020-01-20',DeathDates[length(DeathDates)],'days')
track<-c(rep(0,55),DeathByCounty.Rural$TotalDeaths)
D<-array(0.0,dim=c(StateSpace.ndims[1],StateSpace.ndims[2]))
for (t in 1:length(track)) {
  D<-D+abs(track[t]-StateSpace[,,6,t])
}
