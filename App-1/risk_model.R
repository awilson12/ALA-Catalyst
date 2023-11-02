
setwd('C:/Users/amwilson2/Documents/Github repositories/ALA-Catalyst/App-1')

classroom.model<-function(volume=9000,pathogen="Rhinovirus",num.student.male=4,num.student.female=9,num.infect=1,
                          AER=0.35,student.age=12,teacher.gender="male",teacher.age=35,
                          student.mask=FALSE,teacher.mask=FALSE,desk.material="wood",class.duration=30,
                          percent.sick.student=0.1,previous.sick=FALSE){
  
  timestep<-0.1 #fraction of a minute
  iterations<-100
  
  source('defining_parameters.R')
  source('defining_rates.R')
  
  exposure.frames<-list()
  
  for(i in 1:iterations){
    
    #keep this in iterations loop since we use a new set of probabilities per iteration,
    #informed by newly selected transfer efficiencies, fraction of hand in contact, etc.
    source('defining_probabilities.R')
    
    #defining dimensions for exposure frame
    columns=length(1:(class.duration*1/timestep))
    
    #generating new frame to save # of viruses over time across all states
    sim.mat<-matrix(nrow=10,ncol=columns)
    
    #set initial concentrations
    if(previous.sick==FALSE){
      #assume no contamination from previous class
      sim.mat[,1]<-0
    }else{
      #assume contamination in air and on surfaces from previous class
      sim.mat[1,1]<-100 #place holder
      sim.mat[2,1]<-0
      sim.mat[3,1]<-100 #place holder
      sim.mat[4:10,1]<-0
    }
    
    for(k in 2:class.duration*(1/timestep)){
      
       sim.mat[,i]<-sim.mat[,i-1]%*%P
       sim.mat[1,i]<-sim.mat[1,i]+emissions[i]

    } #end of exposure model
    
    exposure.frames[[i]]<-sim.mat
    
  } #end of iterations through exposure frames
  
} #end of function

