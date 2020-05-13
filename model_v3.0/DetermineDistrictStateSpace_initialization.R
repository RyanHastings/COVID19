##########################################################
# DetermineDistrictStateSpace_initialization.R
#
# Initialize the model for determining state space for districts.
##########################################################
# Ryan Hastings, 5 May 2020
##########################################################

S<-rep(0,maxt)
S[1]<-Npop
dSdt<-rep(0,maxt)

E<-rep(0,maxt)
dEdt<-rep(0,maxt)

In<-rep(0,maxt)
dIdt<-rep(0,maxt)

H<-rep(0,maxt)
dHdt<-rep(0,maxt)

C<-rep(0,maxt)
dCdt<-rep(0,maxt)

R<-rep(0,maxt)
dRdt<-rep(0,maxt)

Q<-rep(0,maxt)
dQdt<-rep(0,maxt)

G<-rep(0,maxt)
dGdt<-rep(0,maxt)

D<-rep(0,maxt)
dDdt<-rep(0,maxt)
Dday<-rep(0,maxt)

Ecum<-rep(0,maxt)
Icum<-rep(0,maxt)
Hcum<-rep(0,maxt)
Ccum<-rep(0,maxt)
Qcum<-rep(0,maxt)
Gcum<-rep(0,maxt)

