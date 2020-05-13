###############################################################
# ModelThePlan_initialization.R
#
# Initialize model for ModelThePlan.R and ModelThePlanScenarios.R.
###############################################################
# 9 May 2020, Ryan Hastings
###############################################################
S<-rep(0.0,maxt)
S[1]<-Npop
dSdt<-rep(0.0,maxt)

E<-rep(0.0,maxt)
dEdt<-rep(0.0,maxt)

In<-rep(0.0,maxt)
In[1]<-1
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