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
  
  # StateSpace[Rcrit_vec_i,Rhosp_vec_i,j,i,1,t]=round(S[t])
  # StateSpace[Rcrit_vec_i,Rhosp_vec_i,j,i,2,t]=round(E[t])
  # StateSpace[Rcrit_vec_i,Rhosp_vec_i,j,i,3,t]=round(In[t])
  # StateSpace[Rcrit_vec_i,Rhosp_vec_i,j,i,4,t]=round(H[t])
  # StateSpace[Rcrit_vec_i,Rhosp_vec_i,j,i,5,t]=round(C[t])
  # StateSpace[Rcrit_vec_i,Rhosp_vec_i,j,i,6,t]=round(D[t])
  # StateSpace[Rcrit_vec_i,Rhosp_vec_i,j,i,7,t]=round(R[t])
  # StateSpace[Rcrit_vec_i,Rhosp_vec_i,j,i,8,t]=round(Ecum[t])
  # StateSpace[Rcrit_vec_i,Rhosp_vec_i,j,i,9,t]=round(Icum[t])
  # StateSpace[Rcrit_vec_i,Rhosp_vec_i,j,i,10,t]=round(Hcum[t])
  # StateSpace[Rcrit_vec_i,Rhosp_vec_i,j,i,11,t]=round(Ccum[t])
  # StateSpace[Rcrit_vec_i,Rhosp_vec_i,j,i,12,t]=round(percent_infected[t])
  # StateSpace[Rcrit_vec_i,Rhosp_vec_i,j,i,13,t]=round(Dday[t])
  
  
  StateSpace[Rcrit_vec_i,Rhosp_vec_i,j,i,1,t]=round(H[t])
  StateSpace[Rcrit_vec_i,Rhosp_vec_i,j,i,2,t]=round(C[t])
  StateSpace[Rcrit_vec_i,Rhosp_vec_i,j,i,3,t]=round(D[t])
  StateSpace[Rcrit_vec_i,Rhosp_vec_i,j,i,4,t]=round(Dday[t])
  StateSpace[Rcrit_vec_i,Rhosp_vec_i,j,i,5,t]=PhaseFiveReduction
  StateSpace[Rcrit_vec_i,Rhosp_vec_i,j,i,6,t]=stage5_weeks
  
}