require(truncdist)
require(triangle)


#inhalation rates----------------------------------------------------------------

#student inhalation rate

  inhalation.student<-rtrunc(iterations,"norm",a=7.31,b=13.87,mean=10.59,sd=1.64)
  #inhalation.student.female<-rtrunc(iterations,"norm",a=7.06,b=12.61,mean=9.84,sd=1.39)
  
#teacher inhalation rate
   
if(teacher.age<31){
  if(teacher.gender=="male"){
    inhalation.teacher<-rtrunc(iterations,"norm",a=12.06,b=22.66,mean=17.36,sd=2.65)
  }else{
    inhalation.teacher<-rtrunc(iterations,"norm",a=9.33,b=17.57,mean=13.45,sd=2.06)
  }
}else if (teacher.age>=31 & teacher.age<41){
  if(teacher.gender=="male"){
    inhalation.teacher<-rtrunc(iterations,"norm",a=12.76,b=21.00,mean=16.88,sd=2.06)
  }else{
    inhalation.teacher<-rtrunc(iterations,"norm",a=10.78,b=16.58,mean=13.68,sd=1.45)
  }
}else{
  if(teacher.gender=="male"){
    inhalation.teacher<-rtrunc(iterations,"norm",a=11.84,b=20.65,mean=16.24,sd=2.2)
  }else{
    inhalation.teacher<-rtrunc(iterations,"norm",a=8.91,b=15.71,mean=12.31,sd=1.7)
  }
}

#Parameters related to fomite contacts---------------------------------------------

if (pathogen!="Rhinovirus"){
  if(desk.material=="steel"){
    TE.HS<-rtrunc(iterations,"norm",a=0,b=1,mean=0.18,sd=0.20)
    TE.SH>-rtrunc(iterations,"norm",a=0,b=1,mean=0.23,sd=0.19)
  }else if (desk.material=="plastic"){
    TE.HS<-rtrunc(iterations,"norm",a=0,b=1,mean=0.17,sd=0.19)
    TE.SH<-rtrunc(iterations,"norm",a=0,b=1,mean=0.28,sd=0.23)
  }else{
    TE.HS<-rtrunc(iterations,"norm",a=0,b=1,mean=0.13,sd=0.14)
    TE.SH<-rtrunc(iterations,"norm",a=0,b=1,mean=0.05,sd=0.07)
  }
  
 
}else{
  if(desk.material=="steel"){
    TE.HS<-rtrunc(iterations,"norm",a=0,b=1,mean=0.13,sd=0.12)
    TE.SH<-rtrunc(iterations,"norm",a=0,b=1,mean=0.34,sd=0.12)
  }else if (desk.material=="plastic"){
    TE.HS<-rtrunc(iterations,"norm",a=0,b=1,mean=0.16,sd=0.16)
    TE.SH<-rtrunc(iterations,"norm",a=0,b=1,mean=0.37,sd=0.14)
  }else{
    TE.HS<-rtrunc(iterations,"norm",a=0,b=1,mean=0.22,sd=0.17)
    TE.SH<-rtrunc(iterations,"norm",a=0,b=1,mean=0.30,sd=0.18)  
  }
  
}
  
TE.HF<-rtrunc(iterations,"norm",a=0,b=1,mean=0.3390,sd=0.1318)

#Adjusted for fraction of total hand surface area--------------------
S.H.student<-runif(iterations,0.008,0.25)/2

S.H.teacher<-runif(iterations,0.008,0.31)/2

S.F.student<-runif(iterations,0.008,0.010)/2 #assumed for now
S.F.teacher<-runif(iterations,0.008,0.010)/2 #assumed for now


A.surface<-100 #assumed for now

#-----frequency of hand-to-face contacts
H.teacher<-rtriangle(iterations,a=2.7,b=8.3,c=5.00) #for now assuming same as student
H.mask.teacher<-rtriangle(iterations,a=2.7,b=8.3,c=5.00)*0.3 # assuming for now reduced by 30% & assuming same as student for now

H.student<-rtriangle(iterations,a=2.7,b=8.3,c=5.00)
H.mask.student<-rtriangle(iterations,a=2.7,b=8.3,c=5.00)*0.3 # assuming for now reduced by 30%

#----------mask efficacies
mask.student<-0.20 #placeholder
mask.teacher<-0.20 #placeholder

#------settling rate
settle<-0.0003 #place holder

#inactivation rates--------
inactiv.hands<-0.03 #place holder
inactiv.air<-0.03 #place holder
inactiv.surf<-0.03 #place holder

#emissions-------------------
RNAinfective<-1/1000 #placeholder
emissions<-numstudents*fractinfect*(rtriangle(iterations,a=10^0,b=10^5,c=10^0)/30)*RNAinfective*timestep #placeholder