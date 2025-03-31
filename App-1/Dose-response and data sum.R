#Dose-response

#Gather doses per matrix

risk.student.inhale<-rep(NA,length(frame.dose$avg.dose.student.inhale))
risk.student.face<-rep(NA,length(frame.dose$avg.dose.student.inhale))
risk.student.total<-rep(NA,length(frame.dose$avg.dose.student.inhale))

if (pathogen=="Common Cold"){
  
    k<-rtriangle(iterations,a=0.484,b=1,c=1)
    
    risk.student.inhale<-1-exp(-frame.dose$avg.dose.student.inhale*k)
    risk.student.face<-1-exp(-frame.dose$avg.dose.student.face*k)
    risk.student.total<-1-exp(-frame.dose$total.student*k)
    
}else if (pathogen=="COVID-19"){

    k<-0.054
    
    risk.student.inhale<-1-exp(-frame.dose$avg.dose.student.inhale*k)
    risk.student.face<-1-exp(-frame.dose$avg.dose.student.face*k)
    risk.student.total<-1-exp(-frame.dose$total.student*k)
      
}else{
  #from Watanabe et al. H1N1 numbers for kids
    alpha.children<-exp(-1.24)
    N50.children<-exp(5.6)
    
    risk.student.inhale<-1-(1+(frame.dose$avg.dose.student.inhale*(2^(1/alpha.children)-1)/N50.children))^-alpha.children
    risk.student.face<-1-(1+(frame.dose$avg.dose.student.face*(2^(1/alpha.children)-1)/N50.children))^-alpha.children
    risk.student.total<-1-(1+(frame.dose$total.student*(2^(1/alpha.children)-1)/N50.children))^-alpha.children
}  