require(shiny)
require(shinydashboard)

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
            p("Here is information about our app")
    ),
    tabItem(tabName= "calc",
            h2("Click through choices and see changes in risk in real time"),
            column(width=12,
                   fluidRow(
                     box(
                       title="Infection Risk output",status="primary",solidHeader=TRUE,
                       collabpsible=TRUE,
                       plotOutput("infection",height=600),
                       "Test test test",br(),
                       "Test test test",br(),
                       "test",br(),
                       "test"
                     ),
                     box(
                       title="Human Inputs",solidHeader=TRUE,
                       "Student Settings",
                       sliderInput("numstudentmale","Number of Male Students",min=1,max=20,value=1),
                       sliderInput("numstudentfemale","Number of Female Students",1,20,1),
                       sliderInput("numstudentinfect","Number of Infected Students",1,100,1),
                       sliderInput("studentage","Average Student Age",1,10,1),
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
     output$infection<-renderPlot({
       
       volume<<-as.numeric(input$volume)
       pathogen<<-input$pathogen
       num.student.male<<-as.numeric(input$numstudentmale)
       num.student.female<<-as.numeric(input$numstudentfemale)
       num.infect<<-as.numeric(input$numstudentinfect)
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
       
       #print(as.numeric(input$numstudentmale))
       
       frame.all<-data.frame(risks=c(risk.student.inhale,risk.student.face,risk.student.total,
                                     risk.teacher.inhale,risk.teacher.face,risk.teacher.total),
                             type=rep(c(rep("Inhalation",length(risk.student.inhale)),rep("Ingestion",length(risk.student.face)),rep("Total",length(risk.student.total))),2),
                             person=c(rep("student",length(c(risk.student.inhale,risk.student.face,risk.student.total))),
                                      rep("teacher",length(c(risk.teacher.inhale,risk.teacher.face,risk.teacher.total)))))
       require(ggplot2)
       
       A<-ggplot(frame.all)+geom_boxplot(aes(x=type,y=risks))+facet_wrap(~person,ncol=1)+
         scale_y_continuous(trans="log10")
       A
     })
   }
)


