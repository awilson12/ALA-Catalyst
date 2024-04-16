#Dose-response

#Gather doses per matrix

risk.student.inhale<-rep(NA,length(frame.dose$avg.dose.student.inhale))
risk.student.face<-rep(NA,length(frame.dose$avg.dose.student.inhale))
risk.student.total<-rep(NA,length(frame.dose$avg.dose.student.inhale))

risk.teacher.inhale<-rep(NA,length(frame.dose$avg.dose.teacher.inhale))
risk.teacher.face<-rep(NA,length(frame.dose$avg.dose.teacher.inhale))
risk.teacher.total<-rep(NA,length(frame.dose$avg.dose.teacher.inhale))

if (pathogen=="Common Cold"){
    k<-rtriangle(iterations,a=0.484,b=1,c=1)
    risk.student.inhale<-1-exp(-frame.dose$avg.dose.student.inhale*k)
    risk.student.face<-1-exp(-frame.dose$avg.dose.student.face*k)
    risk.student.total<-1-exp(-frame.dose$total.student*k)
    
    risk.teacher.inhale<-1-exp(-frame.dose$dose.teacher.inhale*k)
    risk.teacher.face<-1-exp(-frame.dose$dose.teacher.face*k)
    risk.teacher.total<-1-exp(-frame.dose$total.teacher*k)
    
}else if (pathogen=="COVID-19"){
  #placeholder from Watanabe and Pitol & Julian
    k<-2.46E-3
    
    risk.student.inhale<-1-exp(-frame.dose$avg.dose.student.inhale*k)
    risk.student.face<-1-exp(-frame.dose$avg.dose.student.face*k)
    risk.student.total<-1-exp(-frame.dose$total.student*k)
    
    risk.teacher.inhale<-1-exp(-frame.dose$dose.teacher.inhale*k)
    risk.teacher.face<-1-exp(-frame.dose$dose.teacher.face*k)
    risk.teacher.total<-1-exp(-frame.dose$total.teacher*k)
      
}else{
  #from QMRA wiki for now: https://qmrawiki.org/pathogens/influenza
    alpha.adults<-exp(-1.22)
    N50.adults<-exp(13.0)
    
    alpha.children<-exp(-1.71)
    N50.children<-exp(10.3)
    
    risk.student.inhale<-1-(1+(frame.dose$avg.dose.student.inhale*(2^(1/alpha.children)-1)/N50.children))^-alpha.children
    risk.student.face<-1-(1+(frame.dose$avg.dose.student.face*(2^(1/alpha.children)-1)/N50.children))^-alpha.children
    risk.student.total<-1-(1+(frame.dose$total.student*(2^(1/alpha.children)-1)/N50.children))^-alpha.children
    
    risk.teacher.inhale<-1-(1+(frame.dose$dose.teacher.inhale*(2^(1/alpha.adults)-1)/N50.adults))^-alpha.adults
    risk.teacher.face<-1-(1+(frame.dose$dose.teacher.face*(2^(1/alpha.adults)-1)/N50.adults))^-alpha.adults
    risk.teacher.total<-1-(1+(frame.dose$total.teacher*(2^(1/alpha.adults)-1)/N50.adults))^-alpha.adults
}

require(ggplot2)