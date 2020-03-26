####################################################################
# model_out.R
#
# Write out results of model.
####################################################################
# Ryan Hastings, 26 Mar 2020
####################################################################


# Set up output arrays
Sout<-array(0,dim=c(ncounties,maxt))
Eout<-array(0,dim=c(ncounties,maxt))
Iout<-array(0,dim=c(ncounties,maxt))
Hout<-array(0,dim=c(ncounties,maxt))
Cout<-array(0,dim=c(ncounties,maxt))
Rout<-array(0,dim=c(ncounties,maxt))
Dout<-array(0,dim=c(ncounties,maxt))
Ecumout<-array(0,dim=c(ncounties,maxt))
Icumout<-array(0,dim=c(ncounties,maxt))
Hcumout<-array(0,dim=c(ncounties,maxt))
Ccumout<-array(0,dim=c(ncounties,maxt))

for (a in 1:nage) {
  Sout<-Sout+S[,,a]
  Eout<-Eout+E[,,a]
  Iout<-Iout+In[,,a]
  Hout<-Hout+H[,,a]+G[,,a]+Q[,,a]
  Cout<-Cout+G[,,a]+Q[,,a]
  Rout<-Rout+R[,,a]
  Dout<-Dout+D[,,a]
  Ecumout<-Ecumout+Ecum[,,a]
  Icumout<-Icumout+Icum[,,a]
  Hcumout<-Hcumout+Hcum[,,a]
  Ccumout<-Ccumout+Ccum[,,a]
}

# compute percent infected
Nout<-Sout+Eout+Iout+Hout+Cout+Rout+Dout
percent_infected<-array(0,dim=c(ncounties,maxt))
for (i in 1:ncounties) {
  for (t in 1: maxt) {
    percent_infected[i,t]=round(100*Icumout[i,t]/Nout[i,1])
  }
}

# make a plot of Marion county
df.Marion<-data.frame("t"=(seq(0,maxt-1)),"S"=Sout[49,],"E"=Eout[49,],
                      "I"=Iout[49,],"R"=Rout[49,],
                      "D"=Dout[49,],"H"=Hout[49,],"C"=Cout[49,])
df.Marion<-df.Marion%>% mutate(N=S+E+I+R+C+H+D)
print(ggplot(df.Marion,aes(x=t))+
        scale_x_continuous(breaks=seq(0,300,30),name="days")+
        geom_line(aes(y=S,color="susceptible"))+
        geom_line(aes(y=E,color="exposed"))+
        geom_line(aes(y=I,color="infectious"))+
        geom_line(aes(y=R,color="removed"))+
        geom_line(aes(y=D,color="deceased"))+
        geom_line(aes(y=N,color="total"))+
        geom_line(aes(y=H,color="hospitalized"))+
        geom_line(aes(y=C,color="critical"))
)

#######################################################
# output files
if (output==1) {
  
  countycol<-rep(county_names[1],maxt)
  daycol<-seq(1,maxt)
  
  df1<-data.frame("County"=countycol,"Day"=daycol,
                  "Susceptible"=round(Sout[1,]),
                  "Exposed"=round(Eout[1,]),
                  "ExposedCumulative"=round(Ecumout[1,]),
                  "InfectedNotHospitalized"=round(Iout[1,]),
                  "InfectedCumulative"=round(Icumout[1,]),
                  "Hospitalized"=round(Hout[1,]),
                  "HospitalizedCumulative"=round(Hout[1,]),
                  "Critical"=round(Cout[1,]),
                  "CriticalCumulative"=round(Ccumout[1,]),
                  "Deceased"=round(Dout[1,]),
                  "PercentInfected"=round(percent_infected[1,]))
  
  for (i in 2:ncounties) {
    
    countycol<-rep(county_names[i],maxt)
    df2<-data.frame("County"=countycol,"Day"=daycol,
                    "Susceptible"=round(Sout[i,]),
                    "Exposed"=round(Eout[i,]),
                    "ExposedCumulative"=round(Ecumout[i,]),
                    "InfectedNotHospitalized"=round(Iout[i,]),
                    "InfectedCumulative"=round(Icumout[i,]),
                    "Hospitalized"=round(Hout[i,]),
                    "HospitalizedCumulative"=round(Hcumout[i,]),
                    "Critical"=round(Cout[i,]),
                    "CriticalCumulative"=round(Ccumout[i,]),
                    "Deceased"=round(Dout[i,]),
                    "PercentInfected"=round(percent_infected[i,]))
    df1<-df1%>%bind_rows(df2)
    
  }
  
  write_csv(df1,paste(outdir,model_outfile,sep=""))
  
  sink(paste(outdir,param_outfile,sep=""))
  cat(paste("outfile=",model_outfile))
  cat("\n")
  cat(paste("R0=",R0))
  cat("\n")
  cat(paste("intervention_R_rdxn=",intervention_R_rdxn))
  cat("\n")
  cat(paste("intervention_time=",intervention_time))
  cat("\n")
  cat(paste("lift_time=",lift_time))
  cat("\n")
  cat(paste("Pinf=",Pinf))
  cat("\n")
  cat(paste("Thosp=",Thosp))
  cat("\n")
  cat(paste("Tdeath=",Tdeath))
  cat("\n")
  cat(paste("crit_rate=",Rcrit))
  cat("\n")
  cat(paste("Tinc=",Tinc))
  cat("\n")
  cat(paste("Tinf=",Tinf))
  cat("\n")
  cat(paste("maxt=",maxt))
  cat("\n")
  cat(paste("nage=",nage))
  cat("\n")
  for (a in 1:nage) {
    cat(paste("Rdeath[",a,"]=",Rdeath[a]))
    cat("\n")
  }
  for (a in 1:nage) {
    cat(paste("Rhosp[",a,"]=",Rhosp[a]))
    cat("\n")
  }
  cat(paste("model_comorbidities=",model_comorbidities))
  cat("\n")
  cat(paste("Rhosp_diabetes=",Rhosp_diabetes))
  cat("\n")
  cat(paste("Rhosp_heartdisease=",Rhosp_heartdisease))
  cat("\n")
  cat(paste("Rhosp_hypertension=",Rhosp_hypertension))
  cat("\n")
  cat(paste("Rhosp_maligneoplasm=",Rhosp_maligneoplasm))
  cat("\n")
  cat(paste("Rhosp_copd=",Rhosp_copd))
  cat("\n")
  cat(paste("Rdeath_diabetes=",Rdeath_diabetes))
  cat("\n")
  cat(paste("Rdeath_heartdisease=",Rdeath_heartdisease))
  cat("\n")
  cat(paste("Rdeath_hypertension=",Rdeath_hypertension))
  cat("\n")
  cat(paste("Rdeath_maligneoplasm=",Rdeath_maligneoplasm))
  cat("\n")
  cat(paste("Rdeath_copd=",Rdeath_copd))
  cat("\n")
  
  
  cat(paste(param_notes))
  sink()
  
}


