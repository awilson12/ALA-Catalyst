#states

states<-c("room air","exhaust","surfaces","student respiratory tracts","teacher respiratory tracts","student mucosal membrane",
          "teacher mucosal membrane","student hands","teacher hands","inactivation")

#1---->2, room air to exhaust
  lambda.1.2<-AER*timestep

#1---->3, room air to surfaces
  lambda.1.3<-settle*timestep
  
#3---->1, surfaces to room air (resuspension)
  lambda.3.1<-
    
#3--->8, surfaces to student hands
  lambda.3.8<-A.surface*0.5*S.H*TE.HS*timestep
  
#3--->9, surfaces to teacher hands
  lambda.3.9<-A.surface*0.5*S.H*TE.HS*timestep
  
#1---->4, room air to student respiratory tract
  if(student.mask==TRUE){
    lambda.1.4<-(1/volume)*inhalation.student*(1-mask.student)*num.student*timestep
  
  }else{
    lambda.1.4<-(1/volume)*inhalation.student*num.student*timestep
  
  }

#1---->5, room air to teacher respiratory tract
  if(teacher.mask==TRUE){
    lambda.1.5<-(1/volume)*(1-mask.teacher)*inhalation.teacher*timestep
  }else{
    lambda.1.5<-(1/volume)*inhalation.teacher*timestep
  }

#1---->6, room air to student mucosal membrane
  if(student.mask==TRUE){
    lambda.1.6<-0.5*S.F*TE.HF*H.mask.student*timestep
  }else{
    lambda.1.6<-0.5*S.F*TE.HF*H.student*timestep
  }

#1---->7, room air to teacher mucosal membrane
  if(teacher.mask==TRUE){
    lambda.1.7<-0.5*S.F*TE.HF*H.mask.teacher*timestep
  }else{
    lambda.1.7<-0.5*S.F*TE.HF*H.teacher*timestep
  }

#1---->8,9 assume no settling on hands
lambda.1.8<-0
lambda.1.9<-0

#1---->10, room air to inactivation
lambda.1.10<-inactiv.air*timestep

#8,9-->10, hands to inactivation
lambda.8.10<-inactiv.hands*timestep
lambda.9.10<-inactiv.hands*timestep
