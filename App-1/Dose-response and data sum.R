#Dose-response

#Gather doses per matrix

risk.student.inhale<-rep(NA,length(frame.dose$avg.dose.student.inhale))
risk.student.face<-rep(NA,length(frame.dose$avg.dose.student.inhale))
risk.student.total<-rep(NA,length(frame.dose$avg.dose.student.inhale))

risk.teacher.inhale<-rep(NA,length(frame.dose$avg.dose.student.inhale))
risk.teacher.face<-rep(NA,length(frame.dose$avg.dose.student.inhale))
risk.teacher.total<-rep(NA,length(frame.dose$avg.dose.student.inhale))

if (pathogen=="Rhinovirus"){
    k<-rtriangle(iterations,a=0.484,b=1,c=1)
    risk.student.inhale<-1-exp(-frame.dose$avg.dose.student.inhale*k)
    risk.student.face<-1-exp(-frame.dose$avg.dose.student.face*k)
    risk.student.total<-1-exp(-frame.dose$total.student*k)
    
    risk.teacher.inhale<-1-exp(-frame.dose$dose.teacher.inhale*k)
    risk.teacher.face<-1-exp(-frame.dose$dose.teacher.face*k)
    risk.teacher.total<-1-exp(-frame.dose$total.teacher*k)
    
}else if (pathogen=="SARS-CoV-2"){
  #placeholder from Watanabe and Pitol & Julian
    k<-2.46E-3
    
    risk.student.inhale<-1-exp(-frame.dose$avg.dose.student.inhale*k)
    risk.student.face<-1-exp(-frame.dose$avg.dose.student.face*k)
    risk.student.total<-1-exp(-frame.dose$total.student*k)
    
    risk.teacher.inhale<-1-exp(-frame.dose$dose.teacher.inhale*k)
    risk.teacher.face<-1-exp(-frame.dose$dose.teacher.face*k)
    risk.teacher.total<-1-exp(-frame.dose$total.teacher*k)
      
}else if (pathogen=="Influenza"){
  #from QMRA wiki for now: https://qmrawiki.org/pathogens/influenza
    alpha<-5.81E-1
    N50<-9.45E5
    
    risk.student.inhale<-1-(1+(frame.dose$avg.dose.student.inhale*(2^(1/alpha)-1)/N50))^-alpha
    risk.student.face<-1-(1+(frame.dose$avg.dose.student.face*(2^(1/alpha)-1)/N50))^-alpha
    risk.student.total<-1-(1+(frame.dose$total.student*(2^(1/alpha)-1)/N50))^-alpha
    
    risk.teacher.inhale<-1-(1+(frame.dose$avg.dose.teacher.inhale*(2^(1/alpha)-1)/N50))^-alpha
    risk.teacher.face<-1-(1+(frame.dose$avg.dose.teacher.face*(2^(1/alpha)-1)/N50))^-alpha
    risk.teacher.total<-1-(1+(frame.dose$total.teacher*(2^(1/alpha)-1)/N50))^-alpha
}else{
  #RSV
  alpha<-0.217
  beta<-3180
    
  risk.student.inhale<-1-(1+(frame.dose$avg.dose.student.inhale/beta))^-alpha
  risk.student.face<-1-(1+(frame.dose$avg.dose.student.face/beta))^-alpha
  risk.student.total<-1-(1+(frame.dose$total.student/beta))^-alpha
    
  risk.teacher.inhale<-1-(1+(frame.dose$avg.dose.teacher.inhale/beta))^-alpha
  risk.teacher.face<-1-(1+(frame.dose$avg.dose.teacher.face/beta))^-alpha
  risk.teacher.total<-1-(1+(frame.dose$total.teacher/beta))^-alpha
}

require(ggplot2)