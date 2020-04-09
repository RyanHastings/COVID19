########################################################
# model_dynamic_core.R
#
# Dynamic core for the model.  A system of linear differential
# equations solved.
########################################################
# Ryan Hastings, 26 Mar 2020
########################################################

# set up total N array
N<-array(0,dim=c(ncounties))

for (i in 1:ncounties) {
for (a in 1:nage) {
  N[i]<-N[i]+S[i,1,a]+In[i,1,a]
}
}

# the dynamic core
for (t in 2:maxt) {
  
  In[,t-1,1]<-In[,t-1,1]+seed_I[,t-1,1]
  
  # change Rt based time
  if (t<intervention_time | t>=lift_time) {
    Rt<-R0
  } else if (t>=intervention_time & t<lift_time){
    Rt<-(1-intervention_R_rdxn)*R0
  }
  
  #dSdt[i,t,a]<-0
  for (i in 1:ncounties-1) {
    source("model_engine.R")
  }
  
  if (statewide_method==1) {
    for (a in 1:nage) {
      S[ncounties,t,a]<-sum(S[1:ncounties-1,t,a])
      E[ncounties,t,a]<-sum(E[1:ncounties-1,t,a])
      In[ncounties,t,a]<-sum(In[1:ncounties-1,t,a])
      H[ncounties,t,a]<-sum(H[1:ncounties-1,t,a])
      Q[ncounties,t,a]<-sum(Q[1:ncounties-1,t,a])
      G[ncounties,t,a]<-sum(G[1:ncounties-1,t,a])
      C[ncounties,t,a]<-sum(C[1:ncounties-1,t,a])
      R[ncounties,t,a]<-sum(R[1:ncounties-1,t,a])
      D[ncounties,t,a]<-sum(D[1:ncounties-1,t,a])
      Ecum[ncounties,t,a]<-sum(Ecum[1:ncounties-1,t,a])
      Icum[ncounties,t,a]<-sum(Icum[1:ncounties-1,t,a])
      Hcum[ncounties,t,a]<-sum(Hcum[1:ncounties-1,t,a])
      Ccum[ncounties,t,a]<-sum(Ccum[1:ncounties-1,t,a])
    }
  } else if (statewide_method==0) {
    i<-ncounties
    source("model_engine.R")
  } else if (statewide_method==2) {
    source("model_dynamic_core_statewide2.R")
  }
}


