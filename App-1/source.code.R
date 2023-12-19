# Test inputs
#volume<-2000
#pathogen<-"Rhinovirus"
#num.student.male<-15
#num.student.female<-20
#num.infect<-1
#AER<-0.35
#student.age<-12
#teacher.gender<-"Male"
#teacher.age<-40
#studentmask=TRUE
#teachermask=FALSE
#deskmaterial<-"wood"
#class.duration<-30
#previous.sick<-FALSE

source('risk_model.R')


#source('Dose-response and data sum.R')

summary(risk.teacher.inhale)
summary(risk.student.inhale)
