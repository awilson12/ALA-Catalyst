
setwd('C:/Users/amwilson2/Documents/Github repositories/ALA-Catalyst/App-1')

  
  timestep<-0.1 #fraction of a minute
  iterations<-100
  
  #-----------------DEFINING PARAMETERS
  source('defining_parameters.R')
  source('defining_rates.R')
  
  exposure.frames<-list()
  avg.dose.student.inhale<-rep(0,iterations)
  avg.dose.student.face<-rep(0,iterations)
  dose.teacher.inhale<-rep(0,iterations)
  dose.teacher.face<-rep(0,iterations)
  
  for(i in 1:iterations){
    
    #keep this in iterations loop since we use a new set of probabilities per iteration,
    #informed by newly selected transfer efficiencies, fraction of hand in contact, etc.
    source('defining_probabilities.R')
    
    #defining dimensions for exposure frame
    times=length(1:(class.duration*1/timestep))
    statesnum=10
    
    #generating new frame to save # of viruses over time across all states
    sim.mat<-matrix(nrow=statesnum,ncol=times)
    
    #set initial concentrations
    #if(previous.sick==FALSE){
      #assume no contamination from previous class
      sim.mat[,1]<-0
    #}else{
      #assume contamination in air and on surfaces from previous class
    #  sim.mat[1,1]<-100 #place holder
    #  sim.mat[2,1]<-0
    #  sim.mat[3,1]<-100 #place holder
    #  sim.mat[4:10,1]<-0
    #}
    
    for(k in 2:(class.duration*(1/timestep))){
      
       sim.mat[,k]<-sim.mat[,k-1]%*%Ptemp
       
       sim.mat[1,k]<-sim.mat[1,k]+emissions[i]
       #print(k)

    }       #end of exposure model
    
    exposure.frames[[i]]<-sim.mat
    
    avg.dose.student.inhale[i]<-sim.mat[4,length(2:(class.duration*(1/timestep)))]/(num.student.female+num.student.male)
    dose.teacher.inhale[i]<-sim.mat[5,length(2:(class.duration*(1/timestep)))]
    avg.dose.student.face[i]<-sim.mat[6,length(2:(class.duration*(1/timestep)))]/(num.student.female+num.student.male)
    dose.teacher.face[i]<-sim.mat[7,length(2:(class.duration*(1/timestep)))]
  } #end of iterations through exposure frames
  exposure.frame.final<-exposure.frames
  frame.dose<-data.frame(avg.dose.student.inhale,avg.dose.student.face,total.student=avg.dose.student.inhale+avg.dose.student.face,
                         dose.teacher.inhale,dose.teacher.face,total.teacher=dose.teacher.inhale+dose.teacher.face)




source("Dose-response and data sum.R")
