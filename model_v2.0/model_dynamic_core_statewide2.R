
#print("opening statewide2")
tempS<-S
tempE<-E
tempI<-In
tempH<-H
tempG<-G
tempQ<-Q
tempD<-D
tempR<-R
tempEcum<-Ecum
tempIcum<-Icum
tempHcum<-Hcum
tempCcum<-Ccum

S<-urbrurS
E<-urbrurE
In<-urbrurI
H<-urbrurH
G<-urbrurG
Q<-urbrurQ
D<-urbrurD
R<-urbrurR
Ecum<-urbrurEcum
Icum<-urbrurIcum
Hcum<-urbrurHcum
Ccum<-urbrurCcum

In[,t-1,1]<-In[,t-1,1]+seed_I[,t-1,1]

if (t<intervention_time | t>=lift_time) {
  Rt<-R0urban
} else if (t>=intervention_time & t<lift_time) {
  Rt<-R0urban*intervention_R_rdxn_urban
}

for (n in 1:length(urban_county_idx)) {
  i<-urban_county_idx[n]
  source("model_engine.R")
}
#print("done with urban, starting rural")
#print(t)
if(t<intervention_time | t>=lift_time) {
  Rt<-R0rural
} else if (t>=intervention_time & t<lift_time) {
  Rt<-R0rural*intervention_R_rdxn_rural
}
#print(t)
for (n in 1:length(rural_county_idx)) {
  i<-rural_county_idx[n]
  #print(t)
  source("model_engine.R")
}

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

urbrurS<-S
urbrurE<-E
urbrurI<-In
urbrurH<-H
urbrurG<-G
urbrurQ<-Q
urbrurD<-D
urbrurR<-R
urbrurEcum<-Ecum
urbrurIcum<-Icum
urbrurHcum<-Hcum
urbrurCcum<-Ccum

S<-tempS
E<-tempE
In<-tempI
H<-tempH
G<-tempG
Q<-tempQ
D<-tempD
R<-tempR
Ecum<-tempEcum
Icum<-tempIcum
Hcum<-tempHcum
Ccum<-tempCcum

for (a in 1:nage) {
  S[ncounties,t,a]<-urbrurS[ncounties,t,a]
  E[ncounties,t,a]<-urbrurE[ncounties,t,a]
  In[ncounties,t,a]<-urbrurI[ncounties,t,a]
  H[ncounties,t,a]<-urbrurH[ncounties,t,a]
  G[ncounties,t,a]<-urbrurG[ncounties,t,a]
  Q[ncounties,t,a]<-urbrurQ[ncounties,t,a]
  D[ncounties,t,a]<-urbrurD[ncounties,t,a]
  R[ncounties,t,a]<-urbrurR[ncounties,t,a]
  Ecum[ncounties,t,a]<-urbrurEcum[ncounties,t,a]
  Icum[ncounties,t,a]<-urbrurIcum[ncounties,t,a]
  Hcum[ncounties,t,a]<-urbrurHcum[ncounties,t,a]
  Ccum[ncounties,t,a]<-urbrurCcum[ncounties,t,a]
}