require(truncdist)
require(triangle)



#processing inputs from tool-----------------------------------------------------

if(size!="Small" & size!="Medium" & size!="Large"){
  volume<-(size*9)*(0.3048)^3 #assuming height of 9 ft and then convert cubic ft to cubic m
}else{
  if(size=="Small"){
    volume<-numstudents*28*9*(0.3048)^3 #assuming 28 ft^2/student and then 9 ft height and convert to m^3
  }else if (size=="Medium"){
    volume<-numstudents*32*9*(0.3048)^3
  }else{
    volume<-numstudents*45*9*(0.3048)^3
  }
}

if (actlevel=="PE"){
  volume<-numstudents*45*9*(0.3048)^3
}



if (airexchange=="Poor"){
  AER.outdoor<-2 #https://onlinelibrary.wiley.com/doi/10.1111/ina.12384
}else if (airexchange=="Fair"){
  AER.outdoor<-4 #https://onlinelibrary.wiley.com/doi/10.1111/ina.12384
}else if (airexchange=="Good"){
  AER.outdoor<-5 #ASHRAE minimum, chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://www.ashrae.org/file%20library/technical%20resources/free%20resources/design-guidance-for-education-facilities.pdf
}else{
  AER.outdoor<-6 #ASHRAE upper range, on page 24: chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://www.ashrae.org/file%20library/technical%20resources/free%20resources/design-guidance-for-education-facilities.pdf
}



if(studentage=="Kindergarten"){
  student.age<-5
}else if (studentage=="1st"){
  student.age<-6
}else if (studentage=="2nd"){
  student.age<-7
}else if (studentage=="3rd"){
  student.age<-8
}else if (studentage=="4th"){
  student.age<-9
}else{
  student.age<-10
}

if(actlevel=="General Ed" | actlevel=="SPED"){
  activitylevel<- "Sedentary or passive activity" #singing somewhat similar to breathing/speaking
}else if(actlevel=="PE"){
  activitylevel<-"moderate intensity"
  volume<-60*38*18*(0.3048)^3 #chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://www.a2schools.org/site/handlers/filedownload.ashx?moduleinstanceid=5760&dataid=1158&FileName=gym_dimensions.pdf
  
  #assumptions about air exchange rate and classroom volume?
}else {
  #Music
  activitylevel<-"light intensity"
}




studentmaskpercent<-as.numeric(studentmaskpercent)/100

if(openwindows=="Yes"){
  W<-runif(iterations,2,3)
}else{
  W<-1
}

if(hepa=="Yes"){
  AER.portable<-(runif(iterations,114,823)*0.028*60)/volume
}else{
  AER.portable<-0
}

AER.indoor<-runif(iterations,1,3)

if(filtertype=="HEPA"){
  Filter<-0.9997
}else if (filtertype=="MERV 14"){
  Filter<-0.75
}else if (filtertype=="MERV 13"){
  Filter<-0.5
}else{
  Filter<-0.2 
}

AER<-(AER.outdoor*W)+(AER.indoor*Filter)+AER.portable


#inhalation rates----------------------------------------------------------------

class.duration<-8 # assume 8 hr (daily risk)

if(activitylevel=="Sedentary or passive activity"){
  inhalation.student<-rtrunc(iterations,"norm",mean=4.8E-3,sd=8E-4,a=3.2E-3,b=6.4E-3)*timestep
  inhalation.teacher<-rtrunc(iterations,"norm",mean=4.3E-3,sd=1.15E-3,a=2E-3,b=6.6E-3)*timestep
  
}else if (activitylevel=="moderate intensity"){
  inhalation.student<-rtrunc(iterations,"norm",mean=1.1E-2,sd=2E-3,a=7E-3,b=1.5E-2)*timestep
  inhalation.teacher<-rtrunc(iterations,"norm",mean=1.2E-2,sd=2E-3,a=8E-3,b=1.6E-2)*timestep
  
}else{
  inhalation.student<-rtrunc(iterations,"norm",mean=2.2E-2,sd=3.5E-3,a=1.5E-2,b=2.9E-2)*timestep
  inhalation.teacher<-rtrunc(iterations,"norm",mean=2.7E-2,sd=5E-3,a=1.7E-2,b=3.7E-2)*timestep
}

#Parameters related to fomite contacts---------------------------------------------

if (pathogen!="Common Cold"){
  TE.HS<-rtrunc(iterations,"norm",a=0,b=1,mean=0.13,sd=0.14) 
  TE.SH<-rtrunc(iterations,"norm",a=0,b=1,mean=0.05,sd=0.07)
}else{
  TE.HS<-rtrunc(iterations,"norm",a=0,b=1,mean=0.22,sd=0.17) 
  TE.SH<-rtrunc(iterations,"norm",a=0,b=1,mean=0.30,sd=0.18)  
}

TE.HF<-rtrunc(iterations,"norm",a=0,b=1,mean=0.3390,sd=0.1318)

#Adjusted for fraction of total hand surface area--------------------
S.H.student<-runif(iterations,0.008,0.25)/2
S.H.teacher<-runif(iterations,0.008,0.31)/2

S.F.student<-runif(iterations,0.008,0.010)/2 
S.F.teacher<-runif(iterations,0.008,0.010)/2 

A.surface<-runif(iterations,57240,111630)
percent.total.body<-runif(iterations,0.047,0.066)

if(student.age<6){
  total.body.SA<-rtriangle(iterations,a=0.61, b=0.95,c=0.76)*100*100
  #converting to cm^2
}else{
  total.body.SA<-rtriangle(iterations,a=0.81,b=1.48,c=1.08)*100*100
  #converting to cm^2
} 

A.student.hand<-percent.total.body*total.body.SA/2 #single hand SA

A.teacher.hand<-runif(iterations,445,535) #single hand SA

#-----frequency of hand-to-face contacts
H.teacher.face<-rtrunc(iterations,"norm",mean=14,sd=5.4,a=0,b=30)
H.teacher<-rlnorm(iterations,meanlog=log(4.1),sdlog=log(1.6))

H.student<-rtriangle(iterations,a=2.7,b=8.3,c=5.00)
H.student.face<-rtrunc(iterations,"norm",mean=18.9,sd=5.6)

if (studentmaskpercent!=0){
  factor.reduce<-rtriangle(iterations,a=0.07,b=0.21,c=0.12)
  H.student.face<-(H.student.face*factor.reduce*studentmaskpercent)
}

#----------mask filtration efficiencies
mask.student<-runif(iterations,0.70,0.995)
#mask.teacher<-runif(iterations,0.63,0.78)

#if(teachermasktype=="Surgical"){

#  mask.teacher<-runif(iterations,0.63,0.82)

#}else if (teachermasktype=="KN95"){

#  mask.teacher<-runif(iterations,0.57,0.78)

#}

#------settling rate
settle<-rtriangle(a=21.60,b=36,c=28.80)/(24*60)*timestep

#------hand sanitizer

if (pathogen=="Common Cold"){
  reduce.hands<-1-1/(10^1.5)
}else{
  reduce.hands<-1-1/(10^3)
}
if(handsanitizer=="Yes"){
  R<-(-log(reduce.hands/1))/(class.duration*60*timestep)*5 #0.999 reduction per event and assuming 5 hand sanitizer events in a whole day - amount of reduction assumed over a whole day,
  #and assuming first order decay
}else{
  R<-1
}

#inactivation rates--------

if(pathogen=="Common Cold"){
  inactiv.hands<-(runif(iterations,3.45,9.2)*(1/60)*timestep)+R #convert to minutes and then per timestep
  inactiv.air<-rep(3*(1/60)*timestep,iterations) #convert to minutes and then per timestep
  inactiv.surf<-rep(0.92*(1/60)*timestep,iterations) #convert to minutes and then per timestep
}else if (pathogen=="Flu"){
  inactiv.hands<-(rtriangle(iterations,a=0.83,b=0.98,c=0.90)*(1/60)*timestep)+R #convert to minutes and then per timestep
  inactiv.air<-rep(1.22*(1/24)*(1/60)*timestep,iterations) #convert to minutes
  inactiv.surf<-rep(0.13*(1/60)*timestep,iterations) #convert to minutes
}else{
  inactiv.hands<-(rtriangle(iterations,a=0.15,b=0.18,c=0.17)*(1/60)*timestep)+R #convert to minutes and then per timestep
  inactiv.air<-rtrunc(iterations,"norm",mean=0.006,sd=0.006,a=0,b=10)*timestep
  inactiv.surf<-rtriangle(iterations,a=0.08,b=0.12,c=0.10)*timestep
}

#emissions-------------------
RNAinfective<-runif(iterations,1/1000,1/100)
if (studentmaskpercent!=0){
  
  if(pathogen=="Flu"){
    air.emissions<-(numstudents*fractinfect)*
      ((studentmaskpercent)*(1-mask.student) + (1-(studentmaskpercent)))*
      (10^rtriangle(iterations,a=0.3,b=3,c=0.3)/30)*RNAinfective*timestep 
  }else if (pathogen=="COVID-19"){
    air.emissions<-(numstudents*fractinfect)*
      ((studentmaskpercent)*(1-mask.student) + (1-(studentmaskpercent)))*
      (10^rtriangle(iterations,a=0.3,b=3.3,c=0.3)/30)*RNAinfective*timestep 
  }else{
    air.emissions<-(numstudents*fractinfect)*
      ((studentmaskpercent)*(1-mask.student) + (1-(studentmaskpercent)))*
      (10^rtriangle(iterations,a=0.3,b=2.8,c=1.8)/30)*RNAinfective*timestep 
  }
  
  droplet.emissions<-rep(0,iterations)
  
}else{
  
  if(pathogen=="Flu"){
    
    air.emissions<-numstudents*fractinfect*(10^rtriangle(iterations,a=0.3,b=3,c=0.3)/30)*RNAinfective*timestep 
    droplet.emissions<-numstudents*fractinfect*(10^rtriangle(iterations,a=0.3,b=1.1,c=0.3)/30)*RNAinfective*timestep 
    
  }else if (pathogen=="COVID-19"){
    
    air.emissions<-numstudents*fractinfect*(10^rtriangle(iterations,a=0.3,b=3.3,c=0.3)/30)*RNAinfective*timestep 
    droplet.emissions<-numstudents*fractinfect*(10^rtriangle(iterations,a=0.3,b=1.2,c=0.3)/30)*RNAinfective*timestep 
    
  }else{
    air.emissions<-numstudents*fractinfect*(10^rtriangle(iterations,a=0.3,b=2.8,c=1.8)/30)*RNAinfective*timestep 
    droplet.emissions<-numstudents*fractinfect*(10^rtriangle(iterations,a=0.3,b=1.3,c=0.3)/30)*RNAinfective*timestep 
  }
}