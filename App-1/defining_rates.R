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
#mean(lambda.1.4)
#1---->5, room air to teacher respiratory tract
  #if(teacher.mask==TRUE){
  #  lambda.1.5<-(1/volume)*(1-mask.teacher)*inhalation.teacher
  #}else{
    lambda.1.5<-(1/volume)*inhalation.teacher
  #}

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
