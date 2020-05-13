##############################################
# IntegralModel_initialization.R
#
# Initialization of integral model.
##############################################
# Ryan Hastings, 5 May 2020
##############################################

S<-rep(0.0,maxt)
Enew<-rep(0.0,maxt)
Inew<-rep(0.0,maxt)
Hnew<-rep(0.0,maxt)
Cnew<-rep(0.0,maxt)
Qnew<-rep(0.0,maxt)
Rnew<-rep(0.0,maxt)
Dnew<-rep(0.0,maxt)

Inew[1]<-1
S[1]<-Npop

N<-S[1]+Inew[1]