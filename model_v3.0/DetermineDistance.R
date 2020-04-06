
MatchVar<-6

# load in state space
StateSpace_filename<-"StateSpace.Rdata"

load(StateSpace_filename)

maxt=300
R0_vec<-c(seq(2.0,3.0,0.05))
intervention_R_rdxn_vec<-c(seq(0.0,0.95,0.05))

# open up whatever file I get for the death track
# this should give me a ntimes and track[ntimes]

# get distances
StateSpace.ndims<-dims(StateSpace)
D<-array(0.0,dims=c(StateSpace.ndims[1],StateSpace.ndims[2]))
for (t in 1:ntimes) {
  D<-D+abs(track[t]-StateSpace[,,MatchVar,t])
}

# write out to array
Nlines<-length(R0_vec)*length(intervention_R_rdxn_vec)*maxt
R0<-rep(0,Nlines)
intervention_R_rdxn<-rep(0,Nlines)
Dflat<-rep(0,Nlines)
dim1<-rep(0,Nlines)
dim2<-rep(0,Nlines)
day<-rep(0,Nlines)
S<-rep(0,Nlines)
E<-rep(0,Nlines)
In<-rep(0,Nlines)
H<-rep(0,Nlines)
C<-rep(0,Nlines)
D<-rep(0,Nlines)
R<-rep(0,Nlines)
Ecum<-rep(0,Nlines)
Icum<-rep(0,Nlines)
Hcum<-rep(0,Nlines)
percent_infected<-rep(0,Nlines)
n<-1
for (i in 1:length(R0_vec)) {
  for (j in 1:length(intervention_R_rdxn_vec)) {
    for (t in 1:maxt) {
      dim1[n]<-i
      dim2[n]<-j
      day[n]<-t
      R0[n]<-R0_vec[i]
      intervention_R_rdxn[n]<-intervention_R_rdxn[j]
      Dflat[n]<-D[i,j]
      S[n]<-StateSpace[i,j,1,t]
      E[n]<-StateSpace[i,j,2,t]
      In[n]<-StateSpace[i,j,3,t]
      H[n]<-StateSpace[i,j,4,t]
      C[n]<-StateSpace[i,j,5,t]
      D[n]<-StateSpace[i,j,6,t]
      R[n]<-StateSpace[i,j,7,t]
      Ecum[n]<-StateSpace[i,j,8,t]
      Icum[n]<-StateSpace[i,j,9,t]
      Hcum[n]<-StateSpace[i,j,10,t]
      percent_infected<-StateSpace[i,j,11,t]
      n<-n+1
    }
  }
}
D.frame<-data.frame(dim1,dim2,day,R0=R0,InterventionReduction=intervention_R_rdxn,Distance=Dflat,
                    Susceptible=S,Exposed=E,Infectious=In,Hospitalized<-H,Critical<-C,
                    Deceased<-D,ExposedCumulative=Ecum,InfectiousCumulative=Icum,
                    HospitalizedCumulative=Hcum,PercentInfected=percent_infected)
D.frame<-D.frame%>%arrange(Distance,day)