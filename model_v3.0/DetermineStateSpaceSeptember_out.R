###############################################################
# DetermineStateSpace_out.R
#
# Organizes output for DetermineStateSpace.R
###############################################################
# Ryan Hastings, 26 May 2020..stage four 11 jun
###############################################################

H=H+G+Q
C=G+Q

percent_infected<-c(rep(0,maxt))
for (t in 1: maxt) {
  percent_infected[t]=round(100*Icum[t]/N)
}

for (t in 1:maxt) {
  
  # StateSpace[Rcrit_vec_i,Rhosp_vec_i,j,1,t]=round(S[t])
  # StateSpace[Rcrit_vec_i,Rhosp_vec_i,j,2,t]=round(E[t])
  # StateSpace[Rcrit_vec_i,Rhosp_vec_i,j,3,t]=round(In[t])
  # StateSpace[Rcrit_vec_i,Rhosp_vec_i,j,4,t]=round(H[t])
  # StateSpace[Rcrit_vec_i,Rhosp_vec_i,j,5,t]=round(C[t])
  # StateSpace[Rcrit_vec_i,Rhosp_vec_i,j,6,t]=round(D[t])
  # StateSpace[Rcrit_vec_i,Rhosp_vec_i,j,7,t]=round(R[t])
  # StateSpace[Rcrit_vec_i,Rhosp_vec_i,j,8,t]=round(Ecum[t])
  # StateSpace[Rcrit_vec_i,Rhosp_vec_i,j,9,t]=round(Icum[t])
  # StateSpace[Rcrit_vec_i,Rhosp_vec_i,j,10,t]=round(Hcum[t])
  # StateSpace[Rcrit_vec_i,Rhosp_vec_i,j,11,t]=round(Ccum[t])
  # StateSpace[Rcrit_vec_i,Rhosp_vec_i,j,12,t]=round(percent_infected[t])
  # StateSpace[Rcrit_vec_i,Rhosp_vec_i,j,13,t]=round(Dday[t])
  
  
  StateSpace[Rcrit_vec_i,Rhosp_vec_i,j,1,t]=round(H[t])
  StateSpace[Rcrit_vec_i,Rhosp_vec_i,j,2,t]=round(C[t])
  StateSpace[Rcrit_vec_i,Rhosp_vec_i,j,3,t]=round(D[t])
  StateSpace[Rcrit_vec_i,Rhosp_vec_i,j,4,t]=round(Dday[t])
  StateSpace[Rcrit_vec_i,Rhosp_vec_i,j,5,t]=SepReduction
  StateSpace[Rcrit_vec_i,Rhosp_vec_i,j,6,t]=SepReduction
  
}