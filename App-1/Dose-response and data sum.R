#Dose-response

#Gather doses per matrix

risk.student.inhale<-rep(NA,length(frame.dose$avg.dose.student.inhale))
risk.student.face<-rep(NA,length(frame.dose$avg.dose.student.inhale))
risk.student.total<-rep(NA,length(frame.dose$avg.dose.student.inhale))

#risk.teacher.inhale<-rep(NA,length(frame.dose$dose.teacher.inhale))
#risk.teacher.face<-rep(NA,length(frame.dose$dose.teacher.inhale))
#risk.teacher.total<-rep(NA,length(frame.dose$dose.teacher.inhale))

if (pathogen=="Common Cold"){
    k<-rtriangle(iterations,a=0.484,b=1,c=1)
    risk.student.inhale<-1-exp(-frame.dose$avg.dose.student.inhale*k)
    risk.student.face<-1-exp(-frame.dose$avg.dose.student.face*k)
    risk.student.total<-1-exp(-frame.dose$total.student*k)
    
    #risk.teacher.inhale<-1-exp(-frame.dose$dose.teacher.inhale*k)
    #risk.teacher.face<-1-exp(-frame.dose$dose.teacher.face*k)
    #risk.teacher.total<-1-exp(-frame.dose$total.teacher*k)
    
}else if (pathogen=="COVID-19"){
  #placeholder from Watanabe and Pitol & Julian
    k<-2.46E-3
    
    risk.student.inhale<-1-exp(-frame.dose$avg.dose.student.inhale*k)
    risk.student.face<-1-exp(-frame.dose$avg.dose.student.face*k)
    risk.student.total<-1-exp(-frame.dose$total.student*k)
    
    #risk.teacher.inhale<-1-exp(-frame.dose$dose.teacher.inhale*k)
    #risk.teacher.face<-1-exp(-frame.dose$dose.teacher.face*k)
    #risk.teacher.total<-1-exp(-frame.dose$total.teacher*k)
      
}else{
  #from QMRA wiki for now: https://qmrawiki.org/pathogens/influenza
    alpha.adults<-exp(-1.22)
    N50.adults<-exp(13.0)
    
    risk.student.inhale<-1-(1+(frame.dose$avg.dose.student.inhale*(2^(1/alpha.children)-1)/N50.children))^-alpha.children
    risk.student.face<-1-(1+(frame.dose$avg.dose.student.face*(2^(1/alpha.children)-1)/N50.children))^-alpha.children
    risk.student.total<-1-(1+(frame.dose$total.student*(2^(1/alpha.children)-1)/N50.children))^-alpha.children
    
    #risk.teacher.inhale<-1-(1+(frame.dose$dose.teacher.inhale*(2^(1/alpha)-1)/N50))^-alpha
    #risk.teacher.face<-1-(1+(frame.dose$dose.teacher.face*(2^(1/alpha)-1)/N50))^-alpha
    #risk.teacher.total<-1-(1+(frame.dose$total.teacher*(2^(1/alpha)-1)/N50))^-alpha
#}else{
  #RSV
  #alpha<-0.217
  #beta<-3180
    
  #risk.student.inhale<-1-(1+(frame.dose$avg.dose.student.inhale/beta))^-alpha
  #risk.student.face<-1-(1+(frame.dose$avg.dose.student.face/beta))^-alpha
  #risk.student.total<-1-(1+(frame.dose$total.student/beta))^-alpha
    
  #risk.teacher.inhale<-1-(1+(frame.dose$dose.teacher.inhale/beta))^-alpha
  #risk.teacher.face<-1-(1+(frame.dose$dose.teacher.face/beta))^-alpha
  #risk.teacher.total<-1-(1+(frame.dose$total.teacher/beta))^-alpha
}

# P(illness|infection) - age dependent? Can see what data I can find...

# Rhinovirus

P.illness.infection.child<-(38/(14+38)) #from Table II in van der Zalm et al. (2009) (for non asthamtic children, ages 0-7)
P.illness.student<-P.illness.infection.child*risk.student.total


#will look for data on this topic.. increased risk of infection and/or illness for asthamtic children?
#Need to determine more from reading... 
#P.infection.asthma.exac<-runif(1,0.6,0.8) #from Kennedy et al. (2020)
#from Table III in Johnston et al. (1995), 9-11 year olds
#P.asthma.exacerbation.child<-(P.asthma.exac.infection*risk.student.total)/P.infection.asthma.exac
#major limitation here is that risk.student.total is being used - increased risk for those who are asthmatic but by how much?

  
#P.illness.infection.teacher<-(8/(8+2)) #from Table II in van der Zalm et al. (2009) (for healthcare workers, assuming similar to teachers? high contact but not that of a parent?)
#P.illness.teacher<-

# SARS-CoV-2

# Influenza A virus

# RSV
