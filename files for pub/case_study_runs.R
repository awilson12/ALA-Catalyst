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

#----CS1-----------------

#Case Study 1 (CS1): Rhinovirus transmission in a med
#5% of students infected. Risk reductions offered by 
#replacement of a MERV8 filter with a MERV13 filter.

size<-"Medium"
pathogen<-"Common Cold"
studentmaskpercent<-0
numstudents<-25
fractinfect<-5
airexchange<-"Poor"
actlevel<-"General Ed"
studentage<-"5th"
openwindows<-"No"
hepa<-"No"
filtertype<-"MERV 13"
handsanitizer<-"Yes"
class.duration<-8


source("C:/Users/wilso/Documents/ALA-Catalyst/files for pub/risk_model_v2.R")

type<-c("Inhalation","Ingestion","Total")
person<-c("Student")


frame.all<-data.frame(risks=c(risk.student.inhale,risk.student.face,risk.student.total),
                      type=c(rep("Inhalation",length(risk.student.inhale)),rep("Ingestion",length(risk.student.face)),rep("Total",length(risk.student.total))),
                      person=c(rep("Student",length(c(risk.student.inhale,risk.student.face,risk.student.total)))))

  
for (i in 1:3){
  for (j in 1:1){
    if (i==1 & j==1){
      risk.mean<-mean(frame.all$risks[frame.all$type==type[i] & frame.all$person==person[j]])
      risk.sd<-sd(frame.all$risks[frame.all$type==type[i] & frame.all$person==person[j]])
      type.all<-type[i]
      person.all<-person[j]
    }else{
      risktemp.mean<-mean(frame.all$risks[frame.all$type==type[i] & frame.all$person==person[j]])
      risktemp.sd<-sd(frame.all$risks[frame.all$type==type[i] & frame.all$person==person[j]])
      typetemp<-type[i]
      persontemp<-person[j]
      risk.mean<-c(risk.mean,risktemp.mean)
      risk.sd<-c(risk.sd,risktemp.sd)
      type.all<-c(type.all,typetemp)
      person.all<-c(person.all,persontemp)
    }
    
  }
}


df22<-data.frame(meanall=risk.mean,sdall=risk.sd,type.all,x=person.all)
View(df22)


airexchange<-"Great"

source("C:/Users/wilso/Documents/ALA-Catalyst/files for pub/risk_model_v2.R")

type<-c("Inhalation","Ingestion","Total")
person<-c("Student")


frame.all<-data.frame(risks=c(risk.student.inhale,risk.student.face,risk.student.total),
                      type=c(rep("Inhalation",length(risk.student.inhale)),rep("Ingestion",length(risk.student.face)),rep("Total",length(risk.student.total))),
                      person=c(rep("Student",length(c(risk.student.inhale,risk.student.face,risk.student.total)))))


for (i in 1:3){
  for (j in 1:1){
    if (i==1 & j==1){
      risk.mean<-mean(frame.all$risks[frame.all$type==type[i] & frame.all$person==person[j]])
      risk.sd<-sd(frame.all$risks[frame.all$type==type[i] & frame.all$person==person[j]])
      type.all<-type[i]
      person.all<-person[j]
    }else{
      risktemp.mean<-mean(frame.all$risks[frame.all$type==type[i] & frame.all$person==person[j]])
      risktemp.sd<-sd(frame.all$risks[frame.all$type==type[i] & frame.all$person==person[j]])
      typetemp<-type[i]
      persontemp<-person[j]
      risk.mean<-c(risk.mean,risktemp.mean)
      risk.sd<-c(risk.sd,risktemp.sd)
      type.all<-c(type.all,typetemp)
      person.all<-c(person.all,persontemp)
    }
    
  }
}


df22<-data.frame(meanall=risk.mean,sdall=risk.sd,type.all,x=person.all)
View(df22)


#--------CS 2----------------------------------------
#Case Study 2 (CS2): Influenza transmission in a 3rd grade 
#music classroom, 10% of students infected. Risk reductions 
#with portable air purifier.

size<-"Small"
pathogen<-"Flu"
studentmaskpercent<-0
numstudents<-30
fractinfect<-10
airexchange<-"Fair"
actlevel<-"Music"
studentage<-"3rd"
openwindows<-"No"
hepa<-"No"
filtertype<-"MERV 8"
handsanitizer<-"No"
class.duration<-1


source("C:/Users/wilso/Documents/ALA-Catalyst/files for pub/risk_model_v2.R")

type<-c("Inhalation","Ingestion","Total")
person<-c("Student")


frame.all<-data.frame(risks=c(risk.student.inhale,risk.student.face,risk.student.total),
                      type=c(rep("Inhalation",length(risk.student.inhale)),rep("Ingestion",length(risk.student.face)),rep("Total",length(risk.student.total))),
                      person=c(rep("Student",length(c(risk.student.inhale,risk.student.face,risk.student.total)))))


for (i in 1:3){
  for (j in 1:1){
    if (i==1 & j==1){
      risk.mean<-mean(frame.all$risks[frame.all$type==type[i] & frame.all$person==person[j]])
      risk.sd<-sd(frame.all$risks[frame.all$type==type[i] & frame.all$person==person[j]])
      type.all<-type[i]
      person.all<-person[j]
    }else{
      risktemp.mean<-mean(frame.all$risks[frame.all$type==type[i] & frame.all$person==person[j]])
      risktemp.sd<-sd(frame.all$risks[frame.all$type==type[i] & frame.all$person==person[j]])
      typetemp<-type[i]
      persontemp<-person[j]
      risk.mean<-c(risk.mean,risktemp.mean)
      risk.sd<-c(risk.sd,risktemp.sd)
      type.all<-c(type.all,typetemp)
      person.all<-c(person.all,persontemp)
    }
    
  }
}


df22<-data.frame(meanall=risk.mean,sdall=risk.sd,type.all,x=person.all)
View(df22)

hepa<-"Yes"

source("C:/Users/wilso/Documents/ALA-Catalyst/files for pub/risk_model_v2.R")

type<-c("Inhalation","Ingestion","Total")
person<-c("Student")


frame.all<-data.frame(risks=c(risk.student.inhale,risk.student.face,risk.student.total),
                      type=c(rep("Inhalation",length(risk.student.inhale)),rep("Ingestion",length(risk.student.face)),rep("Total",length(risk.student.total))),
                      person=c(rep("Student",length(c(risk.student.inhale,risk.student.face,risk.student.total)))))


for (i in 1:3){
  for (j in 1:1){
    if (i==1 & j==1){
      risk.mean<-mean(frame.all$risks[frame.all$type==type[i] & frame.all$person==person[j]])
      risk.sd<-sd(frame.all$risks[frame.all$type==type[i] & frame.all$person==person[j]])
      type.all<-type[i]
      person.all<-person[j]
    }else{
      risktemp.mean<-mean(frame.all$risks[frame.all$type==type[i] & frame.all$person==person[j]])
      risktemp.sd<-sd(frame.all$risks[frame.all$type==type[i] & frame.all$person==person[j]])
      typetemp<-type[i]
      persontemp<-person[j]
      risk.mean<-c(risk.mean,risktemp.mean)
      risk.sd<-c(risk.sd,risktemp.sd)
      type.all<-c(type.all,typetemp)
      person.all<-c(person.all,persontemp)
    }
    
  }
}


df22<-data.frame(meanall=risk.mean,sdall=risk.sd,type.all,x=person.all)
View(df22)

#-------CS 3----------------------------------------
#Case Study 3 (CS3): COVID-19 transmission in a small 
#special education classroom with 5% of students infected. 
#Risk reductions with open doors and windows.


size<-"Small"
pathogen<-"COVID-19"
studentmaskpercent<-0
numstudents<-10
fractinfect<-5
airexchange<-"Poor"
actlevel<-"Special ed"
studentage<-"3rd"
openwindows<-"No"
hepa<-"No"
filtertype<-"MERV 8"
handsanitizer<-"No"
class.duration<-1


source("C:/Users/wilso/Documents/ALA-Catalyst/files for pub/risk_model_v2.R")
summary(AER)

type<-c("Inhalation","Ingestion","Total")
person<-c("Student")


frame.all<-data.frame(risks=c(risk.student.inhale,risk.student.face,risk.student.total),
                      type=c(rep("Inhalation",length(risk.student.inhale)),rep("Ingestion",length(risk.student.face)),rep("Total",length(risk.student.total))),
                      person=c(rep("Student",length(c(risk.student.inhale,risk.student.face,risk.student.total)))))


for (i in 1:3){
  for (j in 1:1){
    if (i==1 & j==1){
      risk.mean<-mean(frame.all$risks[frame.all$type==type[i] & frame.all$person==person[j]])
      risk.sd<-sd(frame.all$risks[frame.all$type==type[i] & frame.all$person==person[j]])
      type.all<-type[i]
      person.all<-person[j]
    }else{
      risktemp.mean<-mean(frame.all$risks[frame.all$type==type[i] & frame.all$person==person[j]])
      risktemp.sd<-sd(frame.all$risks[frame.all$type==type[i] & frame.all$person==person[j]])
      typetemp<-type[i]
      persontemp<-person[j]
      risk.mean<-c(risk.mean,risktemp.mean)
      risk.sd<-c(risk.sd,risktemp.sd)
      type.all<-c(type.all,typetemp)
      person.all<-c(person.all,persontemp)
    }
    
  }
}


df22<-data.frame(meanall=risk.mean,sdall=risk.sd,type.all,x=person.all)
View(df22)


size<-"Small"
pathogen<-"COVID-19"
studentmaskpercent<-0
numstudents<-10
fractinfect<-5
airexchange<-"Poor"
actlevel<-"Special ed"
studentage<-"3rd"
openwindows<-"Yes"
hepa<-"No"
filtertype<-"MERV 8"
handsanitizer<-"No"


source("C:/Users/wilso/Documents/ALA-Catalyst/files for pub/risk_model_v2.R")
summary(AER)

type<-c("Inhalation","Ingestion","Total")
person<-c("Student")

frame.all<-data.frame(risks=c(risk.student.inhale,risk.student.face,risk.student.total),
                      type=c(rep("Inhalation",length(risk.student.inhale)),rep("Ingestion",length(risk.student.face)),rep("Total",length(risk.student.total))),
                      person=c(rep("Student",length(c(risk.student.inhale,risk.student.face,risk.student.total)))))




for (i in 1:3){
  for (j in 1:1){
    if (i==1 & j==1){
      risk.mean<-mean(frame.all$risks[frame.all$type==type[i] & frame.all$person==person[j]])
      risk.sd<-sd(frame.all$risks[frame.all$type==type[i] & frame.all$person==person[j]])
      type.all<-type[i]
      person.all<-person[j]
    }else{
      risktemp.mean<-mean(frame.all$risks[frame.all$type==type[i] & frame.all$person==person[j]])
      risktemp.sd<-sd(frame.all$risks[frame.all$type==type[i] & frame.all$person==person[j]])
      typetemp<-type[i]
      persontemp<-person[j]
      risk.mean<-c(risk.mean,risktemp.mean)
      risk.sd<-c(risk.sd,risktemp.sd)
      type.all<-c(type.all,typetemp)
      person.all<-c(person.all,persontemp)
    }
    
  }
}


df22<-data.frame(meanall=risk.mean,sdall=risk.sd,type.all,x=person.all)
View(df22)

#----------exploring an ideal case


size<-"Medium"
pathogen<-"Common cold"
studentmaskpercent<-30
numstudents<-30
fractinfect<-5
airexchange<-"Great"
actlevel<-"General ed"
studentage<-"3rd"
openwindows<-"Yes"
hepa<-"Yes"
filtertype<-"HEPA"
handsanitizer<-"Yes"
class.duration<-8

source("C:/Users/wilso/Documents/ALA-Catalyst/files for pub/risk_model_v2.R")
summary(AER)

type<-c("Inhalation","Ingestion","Total")
person<-c("Student")

frame.all<-data.frame(risks=c(risk.student.inhale,risk.student.face,risk.student.total),
                      type=c(rep("Inhalation",length(risk.student.inhale)),rep("Ingestion",length(risk.student.face)),rep("Total",length(risk.student.total))),
                      person=c(rep("Student",length(c(risk.student.inhale,risk.student.face,risk.student.total)))))




for (i in 1:3){
  for (j in 1:1){
    if (i==1 & j==1){
      risk.mean<-mean(frame.all$risks[frame.all$type==type[i] & frame.all$person==person[j]])
      risk.sd<-sd(frame.all$risks[frame.all$type==type[i] & frame.all$person==person[j]])
      type.all<-type[i]
      person.all<-person[j]
    }else{
      risktemp.mean<-mean(frame.all$risks[frame.all$type==type[i] & frame.all$person==person[j]])
      risktemp.sd<-sd(frame.all$risks[frame.all$type==type[i] & frame.all$person==person[j]])
      typetemp<-type[i]
      persontemp<-person[j]
      risk.mean<-c(risk.mean,risktemp.mean)
      risk.sd<-c(risk.sd,risktemp.sd)
      type.all<-c(type.all,typetemp)
      person.all<-c(person.all,persontemp)
    }
    
  }
}


df22<-data.frame(meanall=risk.mean,sdall=risk.sd,type.all,x=person.all)
View(df22)





