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

require(shiny)
require(shinydashboard)
require(waffle)
require(extrafont)
require(showtext)
require(tidyverse)
require(hrbrthemes)
require(echarts4r)
require(devtools)
require(echarts4r.assets)


source('risk_model.R')





#summary(risk.teacher.inhale)
#summary(risk.student.inhale)
