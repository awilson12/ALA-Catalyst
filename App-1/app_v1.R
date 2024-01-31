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
require(magrittr)

sidebar<-dashboardSidebar(
  sidebarMenu(
    menuItem("Welcome", tabName = "welcome", icon = icon("school")),
    menuItem("About", icon = icon("book"), tabName = "about"),
    menuItem("Calculator",icon=icon("th"),tabName="calc")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "welcome",
            h2("Welcome!"),
            p("Welcome to the school health tool.")
    ),
    
    tabItem(tabName = "about",
            h2("What is this app all about?"),
            p("This application is meant to guide decision-making regarding interventions in schools meant to reduce 
              the spread of respiratory viral diseases, such as flu or rhinovirus, the virus responsible for
              the common cold. This application is also useful for educational purposes by demonstrating in
              real time how single or bundled interventions can reduce risks for students and teachers.")
    ),
    tabItem(tabName= "calc",
            h2("Click through choices and see changes in risk in real time"),
            column(width=12,
                   fluidRow(
                     box(
                       title="Infection Risk output",status="primary",solidHeader=TRUE,
                       collabpsible=TRUE,
                       echarts4rOutput("plot")
                     ),
                     box(
                       title="Human Inputs",solidHeader=TRUE,
                       "Student Settings",
                       sliderInput("numstudents","Number of Students",min=10,max=45,value=1),
                       #sliderInput("numstudentfemale","Number of Female Students",1,20,1),
                       sliderInput("fractinfect","% of Infected Students",1,100,1),
                       sliderInput("studentage","Average Student Age",5,10,1),
                       "Teacher Settings",
                       selectInput("teachergender","Teacher Gender",choices=c("male","female")),
                       sliderInput("teacherage","Teacher Age",20,65,1),
                       selectInput("previoussick","Classroom Previously Occupied by Sick Student",choices=c("TRUE","FALSE"))
                     ),
                     box(
                       title="Environmental Intputs",solidHeader=TRUE,
                       sliderInput("airexchange","Air Exchange Rate (1/hr)",min=0.2,max=3,value=0.2),
                       selectInput("deskmaterial","Student Desk Material",choices=c("wood","steel","plastic")),
                       selectInput("pathogen","Pathogen",choices=c("Rhinovirus","SARS-CoV-2","Influenza","RSV")),
                       sliderInput("volume","Classroom Size (m^3)",min=1000,max=5000,value=1000),
                       sliderInput("classduration","Class Duration (min)",min=10,max=60,value=10)
                     ),
                     box(
                       title="Intervention Settings",solidHeader=TRUE,
                       "Student Interventions",
                       selectInput("studentmask","Students Masked",choices=c("TRUE","FALSE")),
                       sliderInput("studentmaskpercent","Percent of Students Masked",min=1,max=100,value=10),
                       selectInput("masktype","Mask Type",choices=c("cloth","surgical","KN95")),
                       "Teacher Settings",
                       selectInput("teachermask","Teacher Masked",choices=c("TRUE","FALSE")),
                       selectInput("masktype","Mask Type",choices=c("cloth","surgical","KN95","N95")),
                       "Air Settings",
                       sliderInput("increasedvent","Increase Ventilation by %",min=10,max=100,value=10),
                       selectInput("portablehepa","Portable HEPA Filter",choices=c("TRUE","FALSE")),
                       selectInput("openwindows","Open Windows",choices=c("TRUE","FALSE")),
                       selectInput("opendoor","Open Door",choices=c("TRUE","FALSE"))
                     )
                   )
                ))
            
  )
)



shinyApp(
   ui=dashboardPage(
     dashboardHeader(title = "Menu"),
     sidebar,
     body
   ), #end of ui
  
   server=function(input,output){
     output$plot<-renderEcharts4r({
       
       rm(list = ls())
       
       volume<<-as.numeric(input$volume)
       pathogen<<-input$pathogen
       numstudents<<-as.numeric(input$numstudents)
       fractinfect<<-as.numeric(input$fractinfect)
       AER<<-as.numeric(input$airexchange)
       student.age<<-as.numeric(input$studentage)
       teacher.gender<<-input$teachergender
       teacher.age<<-as.numeric(input$teacherage)
       student.mask<<-input$studentmask
       teacher.mask<<-input$teachermask
       desk.material<<-input$deskmaterial
       class.duration<<-as.numeric(input$classduration)
       previous.sick<<-input$previoussick
       
       
       source('risk_model.R')
       
       #print(summary(risk.student.inhale))
       
       #print(as.numeric(input$numstudentmale))
       
       frame.all<-data.frame(risks=c(risk.student.inhale,risk.student.face,risk.student.total,
                                     risk.teacher.inhale,risk.teacher.face,risk.teacher.total),
                             type=rep(c(rep("Inhalation",length(risk.student.inhale)),rep("Ingestion",length(risk.student.face)),rep("Total",length(risk.student.total))),2),
                             person=c(rep("Student",length(c(risk.student.inhale,risk.student.face,risk.student.total))),
                                      rep("Teacher",length(c(risk.teacher.inhale,risk.teacher.face,risk.teacher.total)))))
       print(summary(risk.student.total))
       print(summary(risk.teacher.total))
       
       #print(frame.all$risks)
       
       
       type<-c("Inhalation","Ingestion","Total")
       person<-c("Student","Teacher")
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
       
       print(risk)
       #print(type.all)
       #print(person.all)
       
       #print(df22)
       df22<-data.frame(y=risk*1000,type.all,x=person.all)
       print(df22)
       df22<-data.frame(y=round(risk*1000,digits=0),type.all,x=person.all)
       df22teacher<-df22[type.all=="Total",]
       df22teacher<-subset(df22teacher,select=-c(type.all))
       
       #print(summary(df22teacher$risk))
       #print(df22teacher)
       
         df22teacher |> 
           e_charts(x) |>
           e_pictorial(y, symbol = ea_icons("user"), 
                       symbolRepeat = TRUE,
                       symbolSize = c(15, 15)) %>% 
           e_theme("westeros") %>%
           e_title("Number of Infections per 1,000 People") %>% 
           e_flip_coords() %>%
           # Hide Legend
           e_legend(show = FALSE) %>%
           # Remove Gridlines
           e_x_axis(splitLine=list(show = TRUE)) %>%
           e_y_axis(splitLine=list(show = TRUE)) %>%
           # Format Label
           e_labels(fontSize = 16, fontWeight ='bold', position = "right",offset=c(10,0)) 

     })
   }
)


