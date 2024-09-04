#Test model
rm(list = ls())
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

#iterations<-1
size<-"Medium"
studentmaskpercent<-0
pathogen<-"Common Cold"
numstudents<-20
fractinfect<-5
teachermasktype<-"Cloth"
airexchange<-"Great"
actlevel<-"General Ed"
studentage<-"1st"
openwindows<-"Yes"
opendoor<-"No"
hepa<-"Yes"
filtertype<-"MERV 13"
handsanitizer<-"No"

source("C:/Users/wilso/Documents/ALA-Catalyst/App-1/risk_model.R")

View(exposure.frames[[500]])

#row 1 = air, row 2 = exhaust, row 3 = surfaces,
#row 4 = student respiratory tract,
#row 5 = teacher respiratory tract,
#row 6 = student mucosal membrane
#row 7 = teacher mucosal membrane
#row 8 = student hands
#row 9 = teacher hands
#row 10 = inactivation

#Rhinovirus

#checked conservation of virus when emissions are set to zero
#checked approaching steady state in air across simulated 1 hour if holding emissions to once at beginning
#takes longer to approach steady state for surfaces
data.temp<-data.frame(conc=exposure.frames[[1]][3,],time=c(1:length(exposure.frames[[1]][1,]))*0.1)

ggplot(data.temp)+geom_line(aes(x=time,y=conc))

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
View(df22)
df22<-df22[df22$type.all=="Total" & df22$x=="Student" & !is.na(df22$type.all),]
#df22teacher<-subset(df22teacher,select=-c(type.all))
risk.output<-df22$y[!is.na(df22$y)]

#checking iterations and time step (500 iterations, 0.1 min timestep / 1,000 iterations, 0.1 min timestep
#500 reasonable for computational issues for the app, but 1,000 appears superior in terms of consistency in
#precision
#run 1 risk output = 	
0.007029778 / 0.006115967
#run 2 risk output = 
0.006277753 / 0.006189188
#run 3 risk output = 
0.006304982 / 0.006154562
#run 4 risk output = 
0.005627256 / 0.006112971
#run 5 risk output = 
0.006340685 / 0.006601055
#same order of magnitude, and rougly +/- 0.001. Happy with this outcome.
