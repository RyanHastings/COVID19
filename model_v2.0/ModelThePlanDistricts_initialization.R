###############################################################
# ModelThePlanDistricts_initialization.R
#
# Initialize model for ModelThePlanDistricts.R and ModelThePlanDistrictsScenarios.R.
###############################################################
# 11 May 2020
###############################################################

############ get county and state populations #################
US.pop.raw<-read_csv('../PEP_2018_PEPAGESEX/PEP_2018_PEPAGESEX_with_ann.csv')
#state_name<-paste(",",state_name)
IN.pop.raw<-US.pop.raw%>%filter(grepl(state_name,`GEO.display-label`))
county_names<-IN.pop.raw$`GEO.display-label`
county_names<-append(county_names,state_name)
fips<-IN.pop.raw$GEO.id2
county_pops<-IN.pop.raw$est42010sex0_age999

#------------- crosswalk districts and counties --------------#
DistrictCounty.crosswalk<-read_csv("../CountyDistrict.csv")

#---------------- get district populations -------------------#
Npops<-rep(0.0,10)
for (i in 1:92) {
  dum<-DistrictCounty.crosswalk%>%filter(FIPS==fips[i])
  ndistrict<-dum$DISTRICT
  Npops[ndistrict]<-Npops[ndistrict]+as.numeric(county_pops[i])
}

#-------- find date of first death, substract 55 -------------#
In<-array(0.0,dim=c(10,maxt))
init_t<-rep(300,10)
init_date<-rep(as.Date('2020-12-31'),10)

init_data<-read_csv(init_filename,col_types=cols(
  FIPS=col_integer(),
  County=col_character(),
  DateFirstConfirmed=col_date(format="%m/%d/%Y"),
  DateFirstDeath=col_date(format="%m/%d/%Y"),
  X5=col_character()
))
null_start_date<-Sys.Date()
init_data$DateFirstDeath[is.na(init_data$DateFirstDeath)]<-null_start_date

for (i in 1:92) {
  dum<-DistrictCounty.crosswalk%>%filter(FIPS==init_data$FIPS[i])
  ndistrict<-dum$DISTRICT
  if (init_date[ndistrict]>init_data$DateFirstDeath[i]-DayZeroOffset) {
    init_date[ndistrict]<-init_data$DateFirstDeath[i]-DayZeroOffset
  }
}
for (i in 1:10) {
  init_t[i]<-as.numeric(init_date-as.Date('2020-01-20'))+1
}

############### initialize other variables ####################

S<-rep(0.0,maxt)
S[1]<-Npops[n]
dSdt<-rep(0.0,maxt)

E<-rep(0.0,maxt)
dEdt<-rep(0.0,maxt)

In<-rep(0.0,maxt)
#In[1]<-1
dIdt<-rep(0.0,maxt)

H<-rep(0.0,maxt)
dHdt<-rep(0.0,maxt)

C<-rep(0.0,maxt)
dCdt<-rep(0.0,maxt)

R<-rep(0.0,maxt)
dRdt<-rep(0.0,maxt)

Q<-rep(0.0,maxt)
dQdt<-rep(0.0,maxt)

G<-rep(0.0,maxt)
dGdt<-rep(0.0,maxt)

D<-rep(0.0,maxt)
dDdt<-rep(0.0,maxt)
Dday<-rep(0.0,maxt)

Ecum<-rep(0.0,maxt)
Icum<-rep(0.0,maxt)
Hcum<-rep(0.0,maxt)
Ccum<-rep(0.0,maxt)
Qcum<-rep(0.0,maxt)
Gcum<-rep(0.0,maxt)

day<-seq(1,maxt)
dates<-c(seq.Date(DayZero,DayZero+maxt-1,"days"))

PhaseOneT<-as.numeric(PhaseOneDate-DayZero+1)
PhaseTwoT<-as.numeric(PhaseTwoDate-DayZero+1)
PhaseThreeT<-as.numeric(PhaseThreeDate-DayZero+1)
PhaseFourT<-as.numeric(PhaseFourDate-DayZero+1)
PhaseFiveT<-as.numeric(PhaseFiveDate-DayZero+1)