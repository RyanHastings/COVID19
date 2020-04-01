rm(list=ls()) # clear out variables

library(tidyverse)

us_states<-read_csv("us-states.csv")


us_states<-us_states%>%arrange(state,date)
state_names<-unique(us_states$state)


dum2<-c()
for (state_name in state_names) {
  
  dum<-us_states%>%filter(state==state_name)%>%arrange(date)
  
  change_week<-c(rep(NA,nrow(dum)))
  if (nrow(dum)>7) {
    
    for (i in 8:nrow(dum)) {
      nprev<-dum$cases[i-7]
      change_week[i]<-dum$cases[i]-nprev
    }
  }
  dum2<-append(dum2,change_week)

  print(paste(state_name,length(dum2)))
}

us_states<-us_states%>%add_column(week_change=dum2)

print( ggplot(us_states,aes(x=cases))+
         geom_line(aes(group=state,y=week_change))+
         scale_x_log10()+scale_y_log10()+
         geom_line(data=us_states%>%filter(state=="Indiana"),aes(x=cases,y=week_change,color="Indiana"))
)