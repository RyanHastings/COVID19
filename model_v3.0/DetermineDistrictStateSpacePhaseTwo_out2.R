##################################################################
# DetermineDistrictStateSpacePhaseTwo_out2.R
#
# Output for DetermineDistrictStateSpacePhaseTwo.R for disticts
# 1 and 5.
#################################################################
# Ryan Hastings, 5 May 2020
#################################################################

H=H+G+Q
C=G+Q

# percent_infected<-array(0.0,dim=c(2,maxt))
# for (t in 1: maxt) {
#   percent_infected[t]=round(100*Icum[t]/N)
# }

for (t in 1:maxt) {
  
  #StateSpace[lift_rdxn_i,lift_rdxn_j,Rhosp_vec_i,Rcrit_vec_i,1,t]=round(S[t])
  #StateSpace[lift_rdxn_i,lift_rdxn_j,Rhosp_vec_i,Rcrit_vec_i,2,t]=round(E[t])
  #StateSpace[lift_rdxn_i,lift_rdxn_j,Rhosp_vec_i,Rcrit_vec_i,3,t]=round(In[t])
  StateSpace[lift_rdxn_i,lift_rdxn_j,Rhosp_vec_i,Rcrit_vec_i,1,t]=round(H[t])
  StateSpace[lift_rdxn_i,lift_rdxn_j,Rhosp_vec_i,Rcrit_vec_i,2,t]=round(C[t])
  StateSpace[lift_rdxn_i,lift_rdxn_j,Rhosp_vec_i,Rcrit_vec_i,3,t]=round(D[t])
  S#tateSpace[lift_rdxn_i,lift_rdxn_j,Rhosp_vec_i,Rcrit_vec_i,7,t]=round(R[t])
  #StateSpace[lift_rdxn_i,lift_rdxn_j,Rhosp_vec_i,Rcrit_vec_i,8,t]=round(Ecum[t])
  #StateSpace[lift_rdxn_i,lift_rdxn_j,Rhosp_vec_i,Rcrit_vec_i,9,t]=round(Icum[t])
  StateSpace[lift_rdxn_i,lift_rdxn_j,Rhosp_vec_i,Rcrit_vec_i,5,t]=round(Hcum[t])
  StateSpace[lift_rdxn_i,lift_rdxn_j,Rhosp_vec_i,Rcrit_vec_i,6,t]=round(Ccum[t])
  #StateSpace[lift_rdxn_i,lift_rdxn_j,Rhosp_vec_i,Rcrit_vec_i,12,t]=round(percent_infected[t])
  StateSpace[lift_rdxn_i,lift_rdxn_j,Rhosp_vec_i,Rcrit_vec_i,4,t]=round(Dday[t])
}
