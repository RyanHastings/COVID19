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
  
  # StateSpace[i,j,Rhosp_vec_i,Rcrit_vec_i,1,t]=round(S[t])
  # StateSpace[i,j,Rhosp_vec_i,Rcrit_vec_i,2,t]=round(E[t])
  # StateSpace[i,j,Rhosp_vec_i,Rcrit_vec_i,3,t]=round(In[t])
  # StateSpace[i,j,Rhosp_vec_i,Rcrit_vec_i,4,t]=round(H[t])
  # StateSpace[i,j,Rhosp_vec_i,Rcrit_vec_i,5,t]=round(C[t])
  # StateSpace[i,j,Rhosp_vec_i,Rcrit_vec_i,6,t]=round(D[t])
  # StateSpace[i,j,Rhosp_vec_i,Rcrit_vec_i,7,t]=round(R[t])
  # StateSpace[i,j,Rhosp_vec_i,Rcrit_vec_i,8,t]=round(Ecum[t])
  # StateSpace[i,j,Rhosp_vec_i,Rcrit_vec_i,9,t]=round(Icum[t])
  # StateSpace[i,j,Rhosp_vec_i,Rcrit_vec_i,10,t]=round(Hcum[t])
  # StateSpace[i,j,Rhosp_vec_i,Rcrit_vec_i,11,t]=round(Ccum[t])
  # StateSpace[i,j,Rhosp_vec_i,Rcrit_vec_i,12,t]=round(percent_infected[t])
  # StateSpace[i,j,Rhosp_vec_i,Rcrit_vec_i,13,t]=round(Dday[t])
  
  
  StateSpace[i,j,Rhosp_vec_i,Rcrit_vec_i,1,t]=round(H[t])
  StateSpace[i,j,Rhosp_vec_i,Rcrit_vec_i,2,t]=round(C[t])
  StateSpace[i,j,Rhosp_vec_i,Rcrit_vec_i,3,t]=round(D[t])
  StateSpace[i,j,Rhosp_vec_i,Rcrit_vec_i,4,t]=round(Dday[t])
  StateSpace[i,j,Rhosp_vec_i,Rcrit_vec_i,5,t]=PhaseFourReduction
  StateSpace[i,j,Rhosp_vec_i,Rcrit_vec_i,6,t]=stage5_weeks
  
}