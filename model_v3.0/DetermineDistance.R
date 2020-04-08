############################################################
# DetermineDistance.R
#
# Using the output from DetermineStateSpace.R, find the nearest
# neighbor to the observed deceased curve.  Because of the 
# configuration particular to Indiana, the current technique 
# is to use the closest match for no intervention, because 
# at the time of writing there hasn't been enough time for
# our NPI to have an effect on the deceased curve.
###########################################################
# Ryan Hastings, 9 Apr 2020
###########################################################
rm(list=ls()) # clear out the memory

###########################################################
############# CONFIGURATION VARIABLES #####################

MatchVar<-6 # which variable to match on (i.e. which one is the deceased curve)
plot<-1 # make plots? 0=no, 1=yes

maxt=300
R0_vec<-c(seq(2.0,3.0,0.05))
intervention_R_rdxn_vec<-c(seq(0.0,0.95,0.05))

StateSpace_filename<-"StateSpace_day65.Rdata"
track_filename<-"DeathCurves/DeathsCurve_200407.csv"

##########################################################
################### INITIALIZATION #######################
# load in state space
load(StateSpace_filename)

# load in deceased track
dum<-read_csv(track_filename)

DayZero<-as.Date('2020-01-10')+10#as.Date('2020-01-05')#as.Date("2020-02-05")

i1<-as.numeric(as.Date(paste(dum$Date[1],"2020",sep="-"),format="%d-%b-%Y")-DayZero+1)
i2<-as.numeric(as.Date(paste(dum$Date[nrow(dum)],"2020",sep="-"),format="%d-%b-%Y")-DayZero+1)

dates<-seq.Date(DayZero,as.Date(paste(dum$Date[nrow(dum)],"2020",sep="-"),format="%d-%b-%Y"),"days")
dates_exp<-seq.Date(DayZero,DayZero+maxt-1,"days")


track<-c(rep(0,length(dates)))
track[i1:i2]<-dum$CumulativeDeathsByDateofDeath
ntimes<-length(track)

# get distances
StateSpace.ndims<-dim(StateSpace)
D<-array(0.0,dim=c(StateSpace.ndims[1],StateSpace.ndims[2]))
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
De<-rep(0,Nlines)
R<-rep(0,Nlines)
Ecum<-rep(0,Nlines)
Icum<-rep(0,Nlines)
Hcum<-rep(0,Nlines)
Dates<-rep(dates_exp,length(R0_vec)*length(intervention_R_rdxn_vec))
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
      De[n]<-StateSpace[i,j,6,t]
      R[n]<-StateSpace[i,j,7,t]
      Ecum[n]<-StateSpace[i,j,8,t]
      Icum[n]<-StateSpace[i,j,9,t]
      Hcum[n]<-StateSpace[i,j,10,t]
      percent_infected<-StateSpace[i,j,11,t]
    #  Dates[n]<-dates_exp[t]
      n<-n+1
    }
  }
}
D.frame<-data.frame(dim1,dim2,day,R0=R0,InterventionReduction=intervention_R_rdxn,Distance=Dflat,
                    Susceptible=S,Exposed=E,Infectious=In,Hospitalized=H,Critical=C,
                    Deceased=De,ExposedCumulative=Ecum,InfectiousCumulative=Icum,
                    HospitalizedCumulative=Hcum,PercentInfected=percent_infected,
                    Dates=Dates)
D.frame<-D.frame%>%arrange(Distance,day)

df.track<-data.frame(Dates=dates,Deceased=track,Days=(1:ntimes))

# find the row with the minimum
dum23<-which(D==min(D),arr.ind=TRUE)
dumi1<-dum23[1]

if (plot==1) {
print(ggplot()+geom_point(data=df.track,aes(x=Dates,y=Deceased))+
        geom_line(data=D.frame%>%filter(dim1==dumi1,dim2==1),aes(x=Dates,y=Deceased))+
        xlim(DayZero,DayZero+100)+ylim(0,150)+ggtitle(paste("R0=",R0_vec[dumi1])) )
print(ggplot()+geom_point(data=df.track,aes(x=Dates,y=Deceased))+
        geom_line(data=D.frame%>%filter(dim1==dumi1+1,dim2==1),aes(x=Dates,y=Deceased))+
        xlim(DayZero,DayZero+100)+ylim(0,150)+ggtitle(paste("R0=",R0_vec[dumi1+1])) )
print(ggplot()+geom_point(data=df.track,aes(x=Dates,y=Deceased))+
        geom_line(data=D.frame%>%filter(dim1==dumi1+1,dim2==1),aes(x=Dates,y=Deceased,color='no NPI'))+
        geom_line(data=D.frame%>%filter(dim1==dumi1+1,dim2==11),aes(x=Dates,y=Deceased,color='50% reduction'))+
        geom_line(data=D.frame%>%filter(dim1==dumi1+1,dim2==17),aes(x=Dates,y=Deceased,color='80% reduction'))+
        geom_line(data=D.frame%>%filter(dim1==dumi1+1,dim2==19),aes(x=Dates,y=Deceased,color='90% reduction')) )


##############################################################################################
print(ggplot(data=D.frame%>%filter(dim1==dumi1+1,dim2==1),aes(x=Dates))+
        geom_line(aes(y=Infectious,color="Infectious"))+
        geom_line(aes(y=Hospitalized,color="Hospitalized"))+
        geom_line(aes(y=Critical,color="Critical"))+
        geom_line(aes(y=Deceased,color='Deceased'))+
        scale_y_continuous(breaks=seq(0,1e6,1e5),#(0,6e5,5e4),
                           labels=c('0','100K','200K','300K','400K','500K','600K','700K','800K','900K','1M'),
                           #labels=c('0','50K','100K','150K','200K','250K','300K','350K','400K','450K','500K','550K','600K'),
                           name='population',
                          limits=c(0,8e5))+
        ggtitle("No NPI")
)
print(ggplot(data=D.frame%>%filter(dim1==dumi1+1,dim2==11),aes(x=Dates))+
        geom_line(aes(y=Infectious,color="Infectious"))+
        geom_line(aes(y=Hospitalized,color="Hospitalized"))+
        geom_line(aes(y=Critical,color="Critical"))+
        geom_line(aes(y=Deceased,color='Deceased'))+
        scale_y_continuous(breaks=seq(0,125e3,25e3),
                           labels=c('0','25K','50K','75K','100K','125K'),
                           name='population')+
        # scale_y_continuous(breaks=seq(0,1e6,1e5),#(0,6e5,5e4),
        #                    labels=c('0','100K','200K','300K','400K','500K','600K','700K','800K','900K','1M'),
        #                    #labels=c('0','50K','100K','150K','200K','250K','300K','350K','400K','450K','500K','550K','600K'),
        #                    name='population',limits=c(0,8e5))+
        ggtitle("NPI, 50% reduction")
)
print(ggplot(data=D.frame%>%filter(dim1==dumi1+1,dim2==17),aes(x=Dates))+
        geom_line(aes(y=Infectious,color="Infectious"))+
        geom_line(aes(y=Hospitalized,color="Hospitalized"))+
        geom_line(aes(y=Critical,color="Critical"))+
        geom_line(aes(y=Deceased,color='Deceased'))+
       # scale_y_continuous(breaks=seq(0,6e5,5e4),
      #                     labels=c('0','50K','100K','150K','200K','250K','300K','350K','400K','450K','500K','550K','600K'),
      #                     name='population')
        # scale_y_continuous(breaks=seq(0,12e3,1e3),
        #                    labels=c('0','1K','2K','3K','4K','5K','6K','7K','8K','9K','10K','11K','12K'),
        #                    name='population',
        #                    limits=c(0,8e5))+
               # scale_y_continuous(breaks=seq(0,1e6,1e5),#(0,6e5,5e4),
               #             labels=c('0','100K','200K','300K','400K','500K','600K','700K','800K','900K','1M'),
               #             #labels=c('0','50K','100K','150K','200K','250K','300K','350K','400K','450K','500K','550K','600K'),
               #             name='population',
               #            limits=c(0,8e5))+
      ylab('population')+
        ggtitle("NPI, 80% reduction")
)

print(ggplot(data=D.frame%>%filter(dim1==dumi1+1,dim2==19),aes(x=Dates))+
        geom_line(aes(y=Infectious,color="Infectious"))+
        geom_line(aes(y=Hospitalized,color="Hospitalized"))+
        geom_line(aes(y=Critical,color="Critical"))+
        geom_line(aes(y=Deceased,color='Deceased'))+
        # scale_y_continuous(breaks=seq(0,12e3,1e3),
        #                    labels=c('0','1K','2K','3K','4K','5K','6K','7K','8K','9K','10K','11K','12K'),
        #                    name='population',
        #                    limits=c(0,8e5))+
        # scale_y_continuous(breaks=seq(0,1e6,1e5),#(0,6e5,5e4),
        #             labels=c('0','100K','200K','300K','400K','500K','600K','700K','800K','900K','1M'),
        #             #labels=c('0','50K','100K','150K','200K','250K','300K','350K','400K','450K','500K','550K','600K'),
        #             name='population',
      #            limits=c(0,8e5))+
        ylab('population')+
      ggtitle("NPI, 90% reduction")
)


print(ggplot(data=D.frame%>%filter(dim1==dumi1+1,dim2==1),aes(x=Dates))+
        geom_line(aes(y=Infectious,color="Infectious"))+
        geom_line(aes(y=Hospitalized,color="Hospitalized"))+
        geom_line(aes(y=Critical,color="Critical"))+
        geom_line(aes(y=Deceased,color='Deceased'))+
        scale_y_continuous(breaks=seq(0,1e6,1e5),#(0,6e5,5e4),
                           labels=c('0','100K','200K','300K','400K','500K','600K','700K','800K','900K','1M'),
                           #labels=c('0','50K','100K','150K','200K','250K','300K','350K','400K','450K','500K','550K','600K'),
                           name='population',
                           limits=c(0,7.5e5))+
        ggtitle("No NPI")
)
print(ggplot(data=D.frame%>%filter(dim1==dumi1+1,dim2==11),aes(x=Dates))+
        geom_line(aes(y=Infectious,color="Infectious"))+
        geom_line(aes(y=Hospitalized,color="Hospitalized"))+
        geom_line(aes(y=Critical,color="Critical"))+
        geom_line(aes(y=Deceased,color='Deceased'))+
        scale_y_continuous(breaks=seq(0,1e6,1e5),#(0,6e5,5e4),
                           labels=c('0','100K','200K','300K','400K','500K','600K','700K','800K','900K','1M'),
                           #labels=c('0','50K','100K','150K','200K','250K','300K','350K','400K','450K','500K','550K','600K'),
                           name='population',limits=c(0,7.5e5))+
        ggtitle("NPI, 50% reduction")
)
print(ggplot(data=D.frame%>%filter(dim1==dumi1+1,dim2==17),aes(x=Dates))+
        geom_line(aes(y=Infectious,color="Infectious"))+
        geom_line(aes(y=Hospitalized,color="Hospitalized"))+
        geom_line(aes(y=Critical,color="Critical"))+
        geom_line(aes(y=Deceased,color='Deceased'))+
      scale_y_continuous(breaks=seq(0,1e6,1e5),#(0,6e5,5e4),
                  labels=c('0','100K','200K','300K','400K','500K','600K','700K','800K','900K','1M'),
                  #labels=c('0','50K','100K','150K','200K','250K','300K','350K','400K','450K','500K','550K','600K'),
                  name='population',
                 limits=c(0,7.5e5))+
      ggtitle("NPI, 80% reduction")
)

print(ggplot(data=D.frame%>%filter(dim1==dumi1+1,dim2==19),aes(x=Dates))+
        geom_line(aes(y=Infectious,color="Infectious"))+
        geom_line(aes(y=Hospitalized,color="Hospitalized"))+
        geom_line(aes(y=Critical,color="Critical"))+
        geom_line(aes(y=Deceased,color='Deceased'))+
        scale_y_continuous(breaks=seq(0,1e6,1e5),#(0,6e5,5e4),
                    labels=c('0','100K','200K','300K','400K','500K','600K','700K','800K','900K','1M'),
                    #labels=c('0','50K','100K','150K','200K','250K','300K','350K','400K','450K','500K','550K','600K'),
                    name='population',
                   limits=c(0,7.5e5))+
        ggtitle("NPI, 90% reduction")
)
}