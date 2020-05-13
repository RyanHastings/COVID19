###########################################################
# DetermineStateSpace_initialization.R
#
# Initialize variables for DetermineStateSpace model runs.
###########################################################
# Ryan Hastings, 5 May 2020
###########################################################

In<-c(rep(0,maxt))
In[1]<-1

S<-c(rep(0,maxt))
S[1]<-Npop
dSdt<-c(rep(0,maxt))

E<-c(rep(0,maxt))
dEdt<-c(rep(0,maxt))

dIdt<-c(rep(0,maxt))

H<-c(rep(0,maxt))
dHdt<-c(rep(0,maxt))

C<-c(rep(0,maxt))
dCdt<-c(rep(0,maxt))

R<-c(rep(0,maxt))
dRdt<-c(rep(0,maxt))

Q<-c(rep(0,maxt))
dQdt<-c(rep(0,maxt))

G<-c(rep(0,maxt))
dGdt<-c(rep(0,maxt))

D<-c(rep(0,maxt))
dDdt<-c(rep(0,maxt))
Dday<-c(rep(0,maxt))

Ecum<-c(rep(0,maxt))
Icum<-c(rep(0,maxt))
Hcum<-c(rep(0,maxt))
Ccum<-c(rep(0,maxt))
Qcum<-c(rep(0,maxt))
Gcum<-c(rep(0,maxt))

