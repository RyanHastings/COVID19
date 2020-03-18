rm(list=ls())

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(haven)

R0<-2.2 # rate of infection for exposed people
intervention_R_rdxn<-0.5 # R after intervention
intervention_time<-40 # intervention time (days)
lift_time<-300
Pinf<-1.0 # proportion we expect to be nonresistant

Rdeath<-0.02 # death rate per infected
Rhosp<-0.15 # hospitalization rate per infected
Trecov<-23; # time (days) between indeterminate hospitalization and recovery
Thosp=5; # time of indeterminate hospitalization
Tdeath=27; # time (days) between indeterminate hospitalization and death
crit_rate=0.05; # % of hospitalizations that are critical

Tinf<-2.9; # duration of infection (not including hospitalization)
Tinc<-5.2; # duration of incubation

maxt<-300;

outfile<-"R0_2.6_Rt_1.5_Rd_2.csv"
output<-0 # 1 for yes, 0 for no
pic1<-"Marion_R2.2_50p.png"
tit1<-"Marion, R0=2.2, 50% reduction"
pic2<-"Monre_R2.2_50p.png"
tit2<-"Monroe, R0=2.2, 50% reduction"
pic3<-"Lake_R2.2_no_50p.png"
tit3<-"Lake, R0=2.2, 50% reduction"
################################################
# set initial cases
In<-matrix(0,nrow=92,ncol=maxt)
for (i in 1:92) {
  In[i,1]=1
  In[i,2:maxt]=0
}

################################################
# set county-to-county transmission rates
M<-matrix(0.0,nrow=92,ncol=92)
for (i in 1:92) { # v1: assume no transmission between counties
  for (j in 1:92) {
    if (i==j) {
      M[i,j]=1.0
    } else{
      M[i,j]=0.0
    }
  }
}

#################################################
# get county populations
US.pop.raw<-read_csv('PEP_2018_PEPAGESEX/PEP_2018_PEPAGESEX_with_ann.csv')
IN.pop.raw<-US.pop.raw%>%filter(grepl(", Indiana",`GEO.display-label`))
county_names<-IN.pop.raw$`GEO.display-label`

S<-matrix(0,nrow=92,ncol=maxt)
for (i in 1:92) {
  S[i,1]<-as.numeric(IN.pop.raw$est42010sex0_age999[i])*Pinf
  S[i,2:maxt]<-0
}


#################################################
# initialize other values
dSdt<-matrix(0.0,nrow=92,ncol=maxt)

E<-matrix(0,nrow=92,ncol=maxt)
dEdt<-matrix(0.0,nrow=92,ncol=maxt)

dIdt<-matrix(0.0,nrow=92,ncol=maxt)

R<-matrix(0,nrow=92,ncol=maxt)
dRdt<-matrix(0,nrow=92,ncol=maxt)

H<-matrix(0,nrow=92,ncol=maxt)
dHdt<-matrix(0,nrow=92,ncol=maxt)

D<-matrix(0,nrow=92,ncol=maxt)
dDdt<-matrix(0,nrow=92,ncol=maxt)

Rdeath<-Rdeath/Rhosp

Nhosp<-matrix(0.0,nrow=92,ncol=maxt)
Ndead<-matrix(0.0,nrow=92,ncol=maxt)

new_hosp<-matrix(0.0,nrow=92,ncol=maxt)

total_infected<-rep(0,92)

crit<-matrix(0.0,nrow=92,ncol=maxt)

#################################################
# run the model

for (t in 2:maxt) {
  
  if (t<intervention_time & t>lift_time) {
    Rt<-R0
  } else {
    Rt<-(1-intervention_R_rdxn)*R0
  }
  
  for (i in 1:92) {
    
    dSdt[i,t]<-0
    dEdt[i,t]<-0
    for (j in 1:92) {
      dSdt[i,t]<-dSdt[i,t]-Rt*M[i,j]*In[j,t-1]*S[i,t-1]/(S[i,1]*Tinf)
    #  dEdt[i,t]<-dEdt[i,t]+Rt*M[i,j]*In[j,t-1]*S[i,t-1]/(S[i,1]*Tinf)
    }
    if (dSdt[i,t]+S[i,t-1]<0) {
      dSdt[i,t]<--S[i,t-1]
    }
    dEdt[i,t]<--dSdt[i,t]-E[i,t-1]/Tinc
    #dIdt[i,t]<-E[i,t-1]/Tinc-In[i,t-1]/Tinf
    #total_infected[i]<-total_infected[i]+E[i,t-1]/Tinc
    #dRdt[i,t]<-In[i,t-1]/Tinf
    
    dIdt[i,t]<-E[i,t-1]/Tinc-(1-Rhosp)*In[i,t-1]/Tinf-Rhosp*In[i,t-1]/Thosp
    dHdt[i,t]<-Rhosp*In[i,t-1]/Thosp-(1-Rdeath)*H[i,t-1]/Trecov-Rdeath*H[i,t-1]/Tdeath
    dRdt[i,t]<-(1-Rhosp)*In[i,t-1]/Tinf+(1-Rdeath)*H[i,t-1]/Trecov
    dDdt[i,t]<-Rdeath*H[i,t-1]/Tdeath
    
    
    S[i,t]<-S[i,t-1]+dSdt[i,t]
    E[i,t]<-E[i,t-1]+dEdt[i,t]
    In[i,t]<-In[i,t-1]+dIdt[i,t]
    R[i,t]<-R[i,t-1]+dRdt[i,t]
    
    H[i,t]<-H[i,t-1]+dHdt[i,t]
    D[i,t]<-D[i,t-1]+dDdt[i,t]
    crit[i,t]<-crit_rate*In[i,t]
    
    if (R[i,t]>S[i,1]) {
      print("Error: More recovered than original population.")
      stop()
    }
    
    
    
    # if (t > Thosp) {
    #   new_hosp[i,t]<-max(c(0,Rhosp*dIdt[i,t-Thosp]))
    # } else {
    #   new_hosp[i,t]<-0
    # }
    # In[i,t]<-In[i,t]-new_hosp[i,t]
    # if (t > Thosp+Trecov) {
    #   recovered<-(1-Rdeath)*new_hosp[i,t-Trecov]
    # } else {
    #   recovered<-0
    # }
    # R[i,t]<-R[i,t]+recovered
    # if (t > Thosp+Tdeath) {
    #   dead<-Rdeath*new_hosp[i,t-Tdeath]
    # } else {
    #   dead<-0
    # }
    # Nhosp[i,t]<-Nhosp[i,t-1]+
    #   new_hosp[i,t]-
    #   recovered-
    #   dead
    # 
    # Ndead[i,t]<-Ndead[i,t-1]+dead
    # 
  }
  
}

# dum<-data.frame(S=round(S[1,]),
#                 E=round(E[1,]),
#                 I=round(In[1,]),
#                 R=round(R[1,]),
#                 H=round(H[1,]),
#                 D=round(D[1,]),
#                 t=(1:300))
# print( ggplot(dum,aes(x=t))+
#          geom_line(aes(y=S,color='S'))+
#          geom_line(aes(y=E,color='E'))+
#          geom_line(aes(y=I,color='I'))+
#          geom_line(aes(y=R,color='R'))+
#          geom_line(aes(y=H,color='H'))+
#          geom_line(aes(y=D,color='D')))

df.Marion<-data.frame("I"=In[49,],"H"=H[49,],"crit"=crit[49,],"D"=D[49,],"t"=(1:maxt))
df.Monroe<-data.frame("I"=In[53,],"H"=H[53,],"crit"=crit[53,],"D"=D[53,],"t"=(1:maxt))
df.Lake<-data.frame("I"=In[45,],"H"=H[45,],"crit"=crit[45,],"D"=D[45,],"t"=(1:maxt))

print(ggplot(df.Marion,aes(x=t))+
        geom_line(aes(y=I,color="infected"))+
        geom_line(aes(y=H/0.95,color="hospitalized"))+
        geom_line(aes(y=crit,color="critical"))+
        geom_line(aes(y=D,color="deceased"))+
        ggtitle(tit1)+
        scale_x_continuous(breaks=seq(0,300,30),name="days")+
        #scale_y_continuous(breaks=seq(0,80000,2000),name="number")+
        theme(axis.text.x=element_text(angle=90)) )
ggsave(pic1)

print(ggplot(df.Monroe,aes(x=t))+
        geom_line(aes(y=I,color="infected"))+
        geom_line(aes(y=H/0.95,color="hospitalized"))+
        geom_line(aes(y=crit,color="critical"))+
        geom_line(aes(y=D,color="deceased"))+
        ggtitle(tit2)+
        scale_x_continuous(breaks=seq(0,300,30),name="days")+
       # scale_y_continuous(breaks=seq(0,80000,2000),name="number")+
        theme(axis.text.x=element_text(angle=90)) )
ggsave(pic2)

print(ggplot(df.Lake,aes(x=t))+
        geom_line(aes(y=I,color="infected"))+
        geom_line(aes(y=H/0.95,color="hospitalized"))+
        geom_line(aes(y=crit,color="critical"))+
        geom_line(aes(y=D,color="deceased"))+
        ggtitle(tit3)+
        scale_x_continuous(breaks=seq(0,300,30),name="days")+
      #  scale_y_continuous(breaks=seq(0,80000,2000),name="number")+
        theme(axis.text.x=element_text(angle=90)) )
ggsave(pic3)


# colname1<-paste(county_names[1],"Susceptible",sep=",")
# df<-data.frame(colname1=S[1,])
# 
# colname1<-paste(county_names[1],"Exposed",sep=",")


if (output==1) {

df<-data.frame("County Names"=county_names[1],"Variable"="Susceptible")
for (t in 1:maxt) {
  #colname<-paste("Day",toString(t))
  df<-df%>%bind_cols(day=round(S[1,t]))
}
i<-1

df2<-data.frame("County Names"=county_names[1],"Variable"="Exposed")
for (t in 1:maxt){
  #colname<-paste("Day",toString(t))
  df2<-df2%>%bind_cols(day=round(E[1,t]))
}
df<-df%>%bind_rows(df2)

df2<-data.frame("County Names"=county_names[1],"Variable"="Infected, not Hospitalized")
for (t in 1:maxt){
  #colname<-paste("Day",toString(t))
  df2<-df2%>%bind_cols(day=round(In[1,t]))
}
df<-df%>%bind_rows(df2)

df2<-data.frame("County Names"=county_names[1],"Variable"="Hospitalized, Non-critical")
for (t in 1:maxt){
  #colname<-paste("Day",toString(t))
  df2<-df2%>%bind_cols(day=round(0.95*H[1,t]))
}
df<-df%>%bind_rows(df2)

df2<-data.frame("County Names"=county_names[1],"Variable"="Hospitalized, Critical")
for (t in 1:maxt){
  #colname<-paste("Day",toString(t))
  df2<-df2%>%bind_cols(day=round(crit[1,t]))
}
df<-df%>%bind_rows(df2)

df2<-data.frame("County Names"=county_names[1],"Variable"="Recovered")
for (t in 1:maxt){
  #colname<-paste("Day",toString(t))
  df2<-df2%>%bind_cols(day=round(R[1,t]))
}
df<-df%>%bind_rows(df2)

df2<-data.frame("County Names"=county_names[1],"Variable"="Deceased")
for (t in 1:maxt){
  #colname<-paste("Day",toString(t))
  df2<-df2%>%bind_cols(day=round(D[1,t]))
}
df<-df%>%bind_rows(df2)

for (i in 2:92) {
  df2<-data.frame("County Names"=county_names[i],"Variable"="Susceptible")
  for (t in 1:maxt) {
    #colname<-paste("Day",toString(t))
    df2<-df2%>%bind_cols(day=round(S[i,t]))
  }
  df<-df%>%bind_rows(df2)
  
  df2<-data.frame("County Names"=county_names[i],"Variable"="Exposed")
  for (t in 1:maxt){
    #colname<-paste("Day",toString(t))
    df2<-df2%>%bind_cols(day=round(E[i,t]))
  }
  df<-df%>%bind_rows(df2)
  
  df2<-data.frame("County Names"=county_names[i],"Variable"="Infected, not Hospitalized")
  for (t in 1:maxt){
    #colname<-paste("Day",toString(t))
    df2<-df2%>%bind_cols(day=round(In[i,t]))
  }
  df<-df%>%bind_rows(df2)
  
  df2<-data.frame("County Names"=county_names[i],"Variable"="Hospitalized, Not critical")
  for (t in 1:maxt){
    #colname<-paste("Day",toString(t))
    df2<-df2%>%bind_cols(day=round(0.95*H[i,t]))
  }
  df<-df%>%bind_rows(df2)
  
  df2<-data.frame("County Names"=county_names[i],"Variable"="Hospitalized, Critical")
  for (t in 1:maxt){
    #colname<-paste("Day",toString(t))
    df2<-df2%>%bind_cols(day=round(crit[i,t]))
  }
  df<-df%>%bind_rows(df2)
  
  
  df2<-data.frame("County Names"=county_names[i],"Variable"="Recovered")
  for (t in 1:maxt){
    #colname<-paste("Day",toString(t))
    df2<-df2%>%bind_cols(day=round(R[i,t]))
  }
  df<-df%>%bind_rows(df2)
  
  df2<-data.frame("County Names"=county_names[i],"Variable"="Deceased")
  for (t in 1:maxt){
    #colname<-paste("Day",toString(t))
    df2<-df2%>%bind_cols(day=round(D[i,t]))
  }
  df<-df%>%bind_rows(df2)
  
  
  
  
  
  
}

  write_csv(df,outfile)
}