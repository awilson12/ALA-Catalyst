require(truncdist)
require(triangle)


#inhalation rates----------------------------------------------------------------

class.duration<-8 # assume 8 hr (daily risk)


   
if(activitylevel=="Sedentary or passive activity"){
    inhalation.teacher<-
    inhalation.student<-
      
}else if (activitylevel=="moderate intensity"){
    inhalation.teacher<-
    inhalation.student<-
  
}else{
    inhalation.teacher<-
    inhalation.student<-
 
}

#Parameters related to fomite contacts---------------------------------------------

if (pathogen!="Rhinovirus"){
    TE.HS<-rtrunc(iterations,"norm",a=0,b=1,mean=0.13,sd=0.14) #NOW ASSUMING WOOD MATERIAL FOR DESKS
    TE.SH<-rtrunc(iterations,"norm",a=0,b=1,mean=0.05,sd=0.07)
}else{
    TE.HS<-rtrunc(iterations,"norm",a=0,b=1,mean=0.22,sd=0.17) #NOW ASSUMING WOOD MATERIAL FOR DESKS
    TE.SH<-rtrunc(iterations,"norm",a=0,b=1,mean=0.30,sd=0.18)  
}
  
TE.HF<-rtrunc(iterations,"norm",a=0,b=1,mean=0.3390,sd=0.1318)

#Adjusted for fraction of total hand surface area--------------------
S.H.student<-runif(iterations,0.008,0.25)/2

S.H.teacher<-runif(iterations,0.008,0.31)/2

S.F.student<-runif(iterations,0.008,0.010)/2 #assumed for now
S.F.teacher<-runif(iterations,0.008,0.010)/2 #assumed for now

if(tabletype=="kidney"){ #NEED TO ADd
  A.surface<-111630
}else{
  A.surface<-57240
}



A.surface<-100 #assumed for now

percent.total.body<-iterations(runif,0.047,0.057)

if(student.age<6){
  total.body.SA<-rtriangle(iterations,a=0.675, b=0.918,c=0.779)
  
}else if (student.age>=6 & student.age<7){
  total.body.SA<-rtriangle(iterations,a=0.723,b=1.06,c=0.843)
  
}else if (student.age>=7 & student.age<8){
  total.body.SA<-rtriangle(iterations,a=0.792,b=1.11,c=0.917)
  
}else if (student.age>=8 & student.age<9){
  total.body.SA<-rtriangle(iterations,a=0.863,b=1.24,c=0.779)
  
}else if (student.age>=9 & student.age<10){
  total.body.SA<-rtriangle(iterations,a=0.897,b=1.29,c=1.06)
  
}else{
  total.body.SA<-rtriangle(iterations,a=0.981,b=1.48,c=1.17)
  
}

A.student.hand<-percent.total.body*total.body.SA

A.teacher.hand<-runif(iterations,445,535)

#-----frequency of hand-to-face contacts
H.teacher<-rtrunc(iterations,"norm",mean=14,sd=5.4,a=0,b=30)
H.mask.teacher<-H.teacher*rtriangle(iterations,a=0.10,b=1.39,c=0.27)

H.student<-rtriangle(iterations,a=2.7,b=8.3,c=5.00)
if (studentmaskpercent!=0){
  H.student<-rtriangle(iterations,a=2.7,b=8.3,c=5.00)*0.3 # assuming for now reduced by 30%
}

#----------mask efficacies
mask.student<-runif(iterations,0.70,0.995)
mask.teacher<-runif(iterations,0.63,0.78)

if(teachermasktype=="Surgical"){
  mask.teacher<-runif(iterations,0.63,0.82)
}else if (teachermasktype=="KN95"){
  mask.teacher<-runif(iterations,0.57,0.78)
}

#------settling rate
settle<-0.0003 #place holder

#inactivation rates--------

if(pathogen=="Common Cold"){
  inactiv.hands<-runif(iterations,3.45,9.2)*(1/60) #convert to minutes
  inactiv.air<-3*(1/60) #convert to minutes
  inactiv.surf<-0.92*(1/60) #convert to minutes
}else if (pathogen=="Flu"){
  inactiv.hands<-rtriangle(iterations,a=0.83,b=0.98,c=0.90)*(1/60) #convert to minutes
  inactiv.air<-1.22*(1/24)*(1/60) #convert to minutes
  inactiv.surf<-0.13*(1/60) #convert to minutes
}else{
  inactiv.hands<-rtriangle(iterations,a=0.15,b=0.18,c=0.17)*(1/60) #convert to minutes
  inactiv.air<-rtrunc(iterations,"norm",mean=0.006,sd=0.006,a=0,b=10)
  inactiv.surf<-rtriangle(iterations,a=0.08,b=0.12,c=0.10)
}
inactiv.air<-0.03 #place holder
inactiv.surf<-0.03 #place holder

#emissions-------------------
RNAinfective<-1/1000 #placeholder
emissions<-numstudents*fractinfect*(rtriangle(iterations,a=10^0,b=10^5,c=10^0)/30)*RNAinfective*timestep #placeholder