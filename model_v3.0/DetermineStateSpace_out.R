###############################################################
# DetermineStateSpace_out.R
#
# Organizes output for DetermineStateSpace.R
###############################################################
# Ryan Hastings, 5 May 2020
###############################################################

H=H+G+Q
C=G+Q

percent_infected<-c(rep(0,maxt))
for (t in 1: maxt) {
  percent_infected[t]=round(100*Icum[t]/N)
}

for (t in 1:maxt) {
  
  # StateSpace[intervention_R_rdxn_i,intervention_R_rdxn_j,Rhosp_vec_i,Rcrit_vec_i,1,t]=round(S[t])
  # StateSpace[intervention_R_rdxn_i,intervention_R_rdxn_j,Rhosp_vec_i,Rcrit_vec_i,2,t]=round(E[t])
  # StateSpace[intervention_R_rdxn_i,intervention_R_rdxn_j,Rhosp_vec_i,Rcrit_vec_i,3,t]=round(In[t])
  # StateSpace[intervention_R_rdxn_i,intervention_R_rdxn_j,Rhosp_vec_i,Rcrit_vec_i,4,t]=round(H[t])
  # StateSpace[intervention_R_rdxn_i,intervention_R_rdxn_j,Rhosp_vec_i,Rcrit_vec_i,5,t]=round(C[t])
  # StateSpace[intervention_R_rdxn_i,intervention_R_rdxn_j,Rhosp_vec_i,Rcrit_vec_i,6,t]=round(D[t])
  # StateSpace[intervention_R_rdxn_i,intervention_R_rdxn_j,Rhosp_vec_i,Rcrit_vec_i,7,t]=round(R[t])
  # StateSpace[intervention_R_rdxn_i,intervention_R_rdxn_j,Rhosp_vec_i,Rcrit_vec_i,8,t]=round(Ecum[t])
  # StateSpace[intervention_R_rdxn_i,intervention_R_rdxn_j,Rhosp_vec_i,Rcrit_vec_i,9,t]=round(Icum[t])
  # StateSpace[intervention_R_rdxn_i,intervention_R_rdxn_j,Rhosp_vec_i,Rcrit_vec_i,10,t]=round(Hcum[t])
  # StateSpace[intervention_R_rdxn_i,intervention_R_rdxn_j,Rhosp_vec_i,Rcrit_vec_i,11,t]=round(Ccum[t])
  # StateSpace[intervention_R_rdxn_i,intervention_R_rdxn_j,Rhosp_vec_i,Rcrit_vec_i,12,t]=round(percent_infected[t])
  # StateSpace[intervention_R_rdxn_i,intervention_R_rdxn_j,Rhosp_vec_i,Rcrit_vec_i,13,t]=round(Dday[t])
  
  
  StateSpace[intervention_R_rdxn_i,intervention_R_rdxn_j,Rhosp_vec_i,Rcrit_vec_i,1,t]=round(H[t])
  StateSpace[intervention_R_rdxn_i,intervention_R_rdxn_j,Rhosp_vec_i,Rcrit_vec_i,2,t]=round(C[t])
  StateSpace[intervention_R_rdxn_i,intervention_R_rdxn_j,Rhosp_vec_i,Rcrit_vec_i,3,t]=round(D[t])
  StateSpace[intervention_R_rdxn_i,intervention_R_rdxn_j,Rhosp_vec_i,Rcrit_vec_i,4,t]=round(Dday[t])
  
}