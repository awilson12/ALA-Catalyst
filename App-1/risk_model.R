
  risk_model<-function(timestep=0.1,iterations=3000){
    
    require(shiny)
    require(shinydashboard)
    require(waffle)
    require(extrafont)
    require(showtext)
    require(tidyverse)
    require(hrbrthemes)
    require(echarts4r.assets)
    require(echarts4r)
    require(devtools)
    require(flexdashboard)
    require(magrittr)
    require(shinycssloaders)
    require(truncdist)
    require(triangle)
    
    set.seed(18)
  
  #-----------------DEFINING PARAMETERS
    
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
    
    class.duration<-3 # assume 8 hr (daily risk)
    
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
    H.teacher.face<-rtrunc(iterations,"norm",mean=14,sd=5.4,a=0,b=30)/60 #per hour and convert to per minute
    H.teacher<-rlnorm(iterations,meanlog=log(4.1),sdlog=log(1.6)) #per minute
    
    H.student<-rtriangle(iterations,a=328.7,b=999.4,c=599.6)/60 #per hour and convert to per minute
    H.student.face<-rtrunc(iterations,"norm",mean=18.9,sd=5.6)/60 #per hour and convert to per minute
    
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
    
    if(handsanitizer=="Yes"){
      if (pathogen=="Common Cold"){
        reduce.hands<-1.5
      }else{
        reduce.hands<-3
      }
      R<-(-log(1/10^reduce.hands))/(class.duration*60*timestep)
      #and assuming first order decay
    }else{
      R<-0
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
    
    
    #states
    
    states<-c("room air","exhaust","surfaces","student respiratory tracts","teacher respiratory tract","student mucosal membranes",
              "teacher mucosal membrane","student hands","teacher hands","inactivation")
    
    #1---->2, room air to exhaust, first convert to minutes then adjust for timestep (fraction of a minute)
    lambda.1.2<-AER*(1/60)*timestep
    
    #1---->3, room air to surfaces
    lambda.1.3<-settle
    
    #3---->1, surfaces to room air (resuspension)
    lambda.3.1<-0 #assume no resuspension for now... 
    
    #3--->8, surfaces to student hands
    lambda.3.8<-(A.student.hand/A.surface)*numstudents*S.H.student*H.student*TE.SH*timestep
    
    #8--->3, student hands to surfaces
    lambda.8.3<- S.H.student*TE.HS*H.student*numstudents
    
    #3--->9, surfaces to teacher hands
    lambda.3.9<-(A.teacher.hand/A.surface)*S.H.teacher*TE.SH*timestep
    
    #9--->8, teacher hands to surfaces
    lambda.9.3<-S.H.teacher*TE.HS*H.teacher*timestep
    
    #1---->4, room air to student respiratory tract
    if(studentmaskpercent!=0){
      lambda.1.4<-(1/volume)*(inhalation.student*numstudents)*
        ((1-(studentmaskpercent))+((studentmaskpercent)*(1-mask.student)))
      #print('true')
    }else{
      lambda.1.4<-(1/volume)*(inhalation.student*numstudents)
    }
    
    #1---->5, room air to teacher respiratory tract
    
    lambda.1.5<-(1/volume)*inhalation.teacher
    
    
    #8---->6, hands to student mucosal membrane
    lambda.8.6<-S.F.student*TE.HF*numstudents*H.student.face
    
    #9---->7, hands to teacher mucosal membrane
    lambda.9.7<-S.F.teacher*TE.HF*H.teacher.face*timestep
    
    #1---->8,9 assume no settling on hands
    lambda.1.8<-0
    lambda.1.9<-0
    
    #1---->10, room air to inactivation
    lambda.1.10<-inactiv.air
    
    #8,9-->10, hands to inactivation
    lambda.8.10<-inactiv.hands
    lambda.9.10<-inactiv.hands
    
    #3--->10, surfaces to inactivation
    lambda.3.10<-inactiv.surf
    
  
  exposure.frames<-list()
  avg.dose.student.inhale<-rep(0,iterations)
  avg.dose.student.face<-rep(0,iterations)
  dose.teacher.inhale<-rep(0,iterations)
  dose.teacher.face<-rep(0,iterations)
  
  for(i in 1:iterations){
    
    #keep this in iterations loop since we use a new set of probabilities per iteration,
    #informed by newly selected transfer efficiencies, fraction of hand in contact, etc.
    
    
    lambdas.1<-c(lambda.1.2[i], lambda.1.3, lambda.1.4[i],lambda.1.5[i],0,
                 0,lambda.1.8,lambda.1.9,lambda.1.10[i])
    
    
    lambda.1.T<-sum(lambdas.1)
    
    P.1.1<-exp(-lambda.1.T)
    
    P.1<-(1-P.1.1)*lambdas.1/lambda.1.T
    
    P.1.total<-c(P.1.1,P.1)
    
    #Row 2 (exhaust)------------------------
    P.2.total<-c(0,1,rep(0,8))
    
    #Row 3 (surfaces)------------------------
    
    lambdas.3<-c(0,0,0,0,0,0,lambda.3.8[i],lambda.3.9[i],lambda.3.10)
    lambda.3.T<-sum(lambdas.3)
    P.3.3<-exp(-lambda.3.T)
    P.3<-(1-P.3.3)*(lambdas.3/lambda.3.T)
    P.3.total<-c(P.3[1:2],P.3.3,P.3[3:9])
    
    #Row 4 (student respiratory tracts)------------------------
    
    P.4.total<-c(0,0,0,1,0,0,0,0,0,0)
    
    #Row 5 (teacher respiratory tract)------------------------
    
    P.5.total<-c(0,0,0,0,1,0,0,0,0,0)
    
    #Row 6 (student mucosal membranes)------------------------
    
    P.6.total<-c(0,0,0,0,0,1,0,0,0,0)
    
    #Row 7 (teacher mucosal membranes)------------------------
    
    P.7.total<-c(0,0,0,0,0,0,1,0,0,0)
    
    #Row 8 (student hands)------------------------
    
    lambdas.8<-c(0,0,lambda.8.3[i],0,0,lambda.8.6[i],0,0,lambda.8.10[i])
    lambda.8.T<-sum(lambdas.8)
    P.8.8<-exp(-lambda.8.T)
    P.8<-(1-P.8.8)*(lambdas.8/lambda.8.T)
    P.8.total<-c(P.8[1:7],P.8.8,P.8[8:9])
    
    #Row 9 (teacher hands)------------------------
    
    lambdas.9<-c(0,0,lambda.9.3[i],0,0,0,lambda.9.7[i],0,lambda.9.10[i])
    lambda.9.T<-sum(lambdas.9)
    P.9.9<-exp(-lambda.9.T)
    P.9<-(1-P.9.9)*(lambdas.9/lambda.9.T)
    P.9.total<-c(P.9[1:8],P.9.9,P.9[9])
    
    
    #Row 10 (inactivation)-----------------------
    P.10.total<-c(rep(0,9),1)
    
    
    matrix.all<-matrix(nrow=10,ncol=10)
    matrix.all[1,]<-P.1.total
    matrix.all[2,]<-P.2.total
    matrix.all[3,]<-P.3.total
    matrix.all[4,]<-P.4.total
    matrix.all[5,]<-P.5.total
    matrix.all[6,]<-P.6.total
    matrix.all[7,]<-P.7.total
    matrix.all[8,]<-P.8.total
    matrix.all[9,]<-P.9.total
    matrix.all[10,]<-P.10.total
    Ptemp<-matrix.all
    
    
    
    #defining dimensions for exposure frame
    times=length(1:(class.duration*60*1/timestep))
    statesnum=10
    
    #generating new frame to save # of viruses over time across all states
    sim.mat<-matrix(nrow=statesnum,ncol=times)
    sim.mat[,1]<-0

    
    for(k in 2:(class.duration*60*(1/timestep))){
      
       sim.mat[,k]<-sim.mat[,k-1]%*%Ptemp
       sim.mat[1,k]<-sim.mat[1,k]+air.emissions[i]
       #if(!is.na(droplet.emissions[i])){
      #    sim.mat[3,k]<-sim.mat[3,k]+droplet.emissions[i]
      # }else{
      #    sim.mat[3,k]<-sim.mat[3,k]
      # }
       

    }       #end of exposure model
    
    exposure.frames[[i]]<-sim.mat
    
    avg.dose.student.inhale[i]<-sim.mat[4,length(2:(class.duration*60*(1/timestep)))]/(numstudents)
    #dose.teacher.inhale[i]<-sim.mat[5,length(2:(class.duration*60*(1/timestep)))]
    avg.dose.student.face[i]<-sim.mat[6,length(2:(class.duration*60*(1/timestep)))]/(numstudents)
    #dose.teacher.face[i]<-sim.mat[7,length(2:(class.duration*60*(1/timestep)))]
    
  } #end of iterations through exposure frames
  
  exposure.frame.final<-exposure.frames
  frame.dose<-data.frame(avg.dose.student.inhale,avg.dose.student.face,total.student=avg.dose.student.inhale+avg.dose.student.face)

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
  
  frame.all<-data.frame(risks=c(risk.student.inhale,risk.student.face,risk.student.total),
                        type=c(rep("Inhalation",length(risk.student.inhale)),rep("Ingestion",length(risk.student.face)),rep("Total",length(risk.student.total))),
                        person=c(rep("Student",length(c(risk.student.inhale,risk.student.face,risk.student.total)))))
  
  
  type<-c("Inhalation","Ingestion","Total")
  person<-c("Student")
  type.all<-rep(NA,6)
  person.all<-rep(NA,6)
  risk<-rep(NA,6)
  
  for (i in 1:3){
    for (j in 1:2){
      if (i==1 & j==1){
        risk<-mean(frame.all$risks[frame.all$type==type[i] & frame.all$person==person[j]])
        type.all<-type[i]
        person.all<-person[j]
      }else{
        risktemp<-mean(frame.all$risks[frame.all$type==type[i] & frame.all$person==person[j]])
        typetemp<-type[i]
        persontemp<-person[j]
        risk<-c(risk,risktemp)
        type.all<-c(type.all,typetemp)
        person.all<-c(person.all,persontemp)
      }
      
    }
  }
  
  
  df22<-data.frame(y=risk,type.all,x=person.all)
  df22<-df22[df22$type.all=="Total" & df22$x=="Student" & !is.na(df22$type.all),]
  risk.output<<-df22$y[!is.na(df22$y)]
  
}