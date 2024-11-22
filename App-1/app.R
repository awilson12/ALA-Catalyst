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




sidebar<-dashboardSidebar(
  sidebarMenu(
    menuItem("Welcome", tabName = "welcome", icon = icon("school")),
    menuItem("About", icon = icon("book"), tabName = "about"),
    menuItem("Calculator Instructions",tabName="How",icon=icon("cog")),
    menuItem("Calculator",icon=icon("th"),tabName="calc")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "welcome",
            h2("Welcome!"),
            p("Welcome to the school health tool. The following tabs
              will provide information about the tool, how to use it, and access to the tool itself. A summary of the project at large can be seen in a digital
              flyer below."),
            img(src="infosheet.png",width=900)
    ),
    
    tabItem(tabName = "about",
            h2("What is this app all about?"),
            p("This application is meant to guide decision-making regarding interventions in schools meant to reduce 
              the spread of respiratory viral diseases, such as flu or rhinovirus, the virus responsible for
              the common cold. This application is also useful for educational purposes by demonstrating in
              real time how single or bundled interventions can reduce risks for students and teachers.",style="font-size:18px;"),
            p("This tool cannot connect to attendance data systems and is not for contact tracing or detecting
              outbreaks. Rather, it is for exploring hypothetical scenarios and seeing, quantitatively, how single or
              bundled interventions could reduce disease burden and maintain infection risk levels below 10%. In other words,
              the tool is designed for planning and prevention. New features in development include the ability to download reports
              with information on tested scenarios and guidance based on the risk calculator tool output.
              A paper describing the tool in full detail is under review in a
              peer reviewed journal. A link to that paper will be made available here upon publication.",style="font-size:18px;"),
            p("The development of this tool is funded by the American Lung Association, and Drs. Amanda Wilson and Ashley Lowe
              are working with Arizona-based public, private, and charter schools to inform and improve the tool's develpoment so that it is as
              accurate as possible and useful to school health professionals in making decisions regarding controlling the spread of
              respiratory viral disease in school settings. This tool and the assumptions in developing it are not reflective of official
              views of the American Lung Associaton and are soley the responsiblity of the research investigators.
              Questions about the tool or the project can be directed to Dr. Amanda 
              Wilson at amwilson2@arizona.edu.",style="font-size:18px;")
    ),
    tabItem(tabName="How",
            h2("How to Use the Tool"),
              p("This calculator tool estimates the average infection risk per student and for the teacher for a simulation of 8-hr of class. This means that risks
                for some scenarios (like specials classes that may only be for an hour) are likely over estimated. The 
                output is given in the form of a percent chance any given student gets infected. These numbers update as you change
                settings in the model with the menu below the figure. Not every student who gets infected will become ill (i.e., have symptoms), but those who are infectious
                can still infect others without symptoms.
                The available parameters and their descriptions are provided below.",style="font-size:18px;"),
              p("We use a framework called 'Quantitative Microbial Risk Assessment' (QMRA), in which we use data from published scientific literature and assumptions
                based on the inputs that to select to quantify an infection risk for a given scenario. More information on what QMRA is and how it's used can be found below.",style="font-size:18px;"),
              tags$a(href="https://qmrawiki.org/about", "What is QMRA?",style="font-size:18px;"),
            h2("Illness-related Settings"),  
              h3("Illness Type"),
                p("We included pathogens for which there is published information about how the amount of virus someone inhales/ingests relates to 
                  risk of infection. This is called a 'dose response' relationship, where a curve informed by data estimates an infection risk based on
                  an estimate of how much virus enters a student's or teacher's body through the lungs or through hand-to-face contacts. To learn more
                  about dose response and its use in quantitative microbial risk assessment, please use the link below.",style="font-size:18px;"),
            tags$a(href="https://qmrawiki.org/framework/dose-response/dose-response-assessment","Dose Response Information",style="font-size:18px;"),
              h3("% of Students Infected"),
               p("This allows you to select what percent of the students are assumed to be infected (asymptomatic or not). You may have a number to inform this setting depending on the number of students who are absent due to illnes, for example.
                  Or, you  may choose a value just to see how it impacts the estimated infection risk. The estimated infection risk will be compared to a
                 threshold of 10%, which would trigger communication about an outbreak.",style="font-size:18px;"),
            h2("Classrom-related Settings"),
              h3("Number of Students"),
                p("This setting allows you to select the number of students in the classroom.",style="font-size:18px;"),
              h3("Grade Level"),
                p("This setting allows you to choose kindergarten through 5th grade. This input is used to estimate the average
                  age of a child and the volume of air they would inhale over time.",style="font-size:18px;"),
              h3("Class Type"),
                p("You  may be interested in exploring risks for general education vs.
                  physical education or music class. This setting influences assumptions about
                  the activity level of students and their respiration rates. For PE, it also
                  affects assumption about the volume of the room.",style="font-size:18px;"),
              h3("Classroom Size"),
                p("You can select general classroom sizes informed by the amount of space required or recommended per student and common
                  heights in U.S. classrooms.",style="font-size:18px;"),
              h3("Advanced Classroom Options"),
                p("This is a special setting if you want to explore results for a specific classroom volume.",style="font-size:18px;"),
            h2("Air Quality Settings"),
              p("The basic air quality setting allows you to select from 'Poor' to 'Great,' based on
              ranges of recommended fresh air exchange values in classrooms and reported air exchange
              values in real-world settings. For more specific air quality settings, see 'Advanced
              Air Quality Settings.'",style="font-size:18px;"),
              h2("Advanced Air Quality Settings"),
                p("Air exchange rate is a measure of how quickly the air in a space is replaced with fresh air. A higher air exchange rate means more
                  fresh air replaces 'old' air over a shorter period of time. Some tradeoffs to consider include increased cost of cooling or heating air
                  if more fresh air is being circulated. For more information on air exchange rates and recommended air exchange
                  rates for different spaces, please see this information from the American Society of Heating, Refrigerating, and Air Conditioning
                  Engineers (ASHRAE)",style="font-size:18px;"),
                   tags$a(href="https://www.ashrae.org/file%20library/technical%20resources/free%20resources/design-guidance-for-education-facilities.pdf", 
                         "ASHRAE information on Air Changes for Classrooms",style="font-size:18px;"),
                p("The portable air purifier option allows you to select whether one of these devices is assumed to be on in the classroom and working effectively. These devices can be very useful in reducing not only the amount of virus in the air but also dust that can exacerbate students
                  with asthma. Tradeoffs to consider include maintenance (changing out of filters) and being able to safely plug it into an outlet. Some
                  may be noisy, and the amount of air the device filters per time should be considered when purshasing one or more for a given space.",style="font-size:18px;"),
                p("You can also decide whether to assume windows and/or doors are open or closed in the classroom. Open windows and doors can increase the amount of
                  fresh air that is circulated in the room, diluting the amount of virus that is exhaled by infected students. We assume that opening doors or
                  windows will double the current fresh air exchange. The pros of this approach is it is inexpensive since it doesn't require new equipment.
                  However, the tradeoffs to consider include potential distractions for students, safety concerns, weather, or sources of air pollution
                  that could be nearby the school, such as for schools in high vehicle traffic areas.",style="font-size:18px;"),
              h3("Percent of Student Masks"),
                p("In this setting, you can select the percent of students that are wearing masks (including 0% as an option). The mask effectiveness
                  values used in the model assume that the mask is being worn propertly (i.e., sealed securly to the chin and nose). So reductions in risk
                  that are estimated are optimistic, and barriers to feasibility in proper use should be considered. Tradeoffs to consider include
                  safety issues, such as choking hazards with the straps, for small students or students with special educational needs and the
                  abilitiy to enforce compliance with wearing them properly. It may also interfere with certain subjects, such as phonics or
                  English as a Second Language (ESL) education.",style="font-size:18px;")
            ),
    tabItem(tabName= "calc",
            #h2("Click through choices and see changes in risk in real time"),
            fluidRow(
                       column(width=12, offset=0, style='padding:0px;', style="background-color:#ffffff;",
                              
                                title="Percent Chance of Infection",status="primary",solidHeader=TRUE,
                                collabpsible=TRUE,
                                box(flexdashboard::gaugeOutput("plot")%>% withSpinner(color="#0dc5c1"),width=200,
                                  title="Percent Chance of Infection per Student") 
                              
                              )), #end of top left
                
            fluidRow(
                column(3,offset=0, style='padding:0px;',
                    selectInput("pathogen",tags$span(style="color: #009999;","Illness Type"),choices=c("Common Cold","COVID-19","Flu")),
                    sliderInput("fractinfect",tags$span(style="color: #009999;","% of Students Infected"),1,100,0,step=10,ticks=FALSE),
                    sliderInput("numstudents",tags$span(style="color: #003399;","Number of Students"),min=1,max=40,value=20,step=5,ticks=FALSE),
                    selectInput("studentage",tags$span(style="color: #003399;","Grade Level"),choices=c("Kindergarten","1st","2nd","3rd","4th","5th")),
                    selectInput("actlevel",label=tags$span(style="color: #003399;","Class Type"),choices=c("General Ed","PE","SPED","Music")),
                    conditionalPanel(
                      condition=("input.actlevel!='PE'"),
                      selectInput("size",tags$span(style="color: #003399;","Classroom Size"),choices=c("Small","Medium","Large")),
                      actionButton(inputId="advclass",label=tags$span(style="color: #003399;","Advanced Classroom Size Options")),
                      conditionalPanel(
                        condition=("input.advclass%2>0"),
                        sliderInput(inputId="size",label=tags$span(style="color: #003399;","Classroom Sq. Ft."),500,2000,value=1000,100,ticks=FALSE)
                      )
                    )
                    
           
              ), #end of top right
                   column(width=3,offset=1, style='padding:0px;',
                          selectInput("handsanitizer","Hand Sanitizer Used by Students",choices=c("Yes","No"),selected="No"),
                         conditionalPanel(
                           condition=("input.actlevelspace!='Outdoors'"),
                           selectInput("airexchange",choices=c("Poor","Fair","Good","Great"),label=tags$span(style="color: #6699FF;","Air Quality Settings")),
                           actionButton(inputId = "adv", label=tags$span(style="color: #6699FF;","Advanced Air Quality Options")),
                         ),
                            
                          
                            # If advanced variables are selected, open sample type and dose response.
                             conditionalPanel(
                              condition = ("input.adv%2>0"),
                              #sliderInput(inputId = "airexchange",
                               #           label=tags$span(style="color: #6699FF;","Air Exchange Rate"),
                                #        0.3, 4, 1,ticks=FALSE),
                              selectInput(inputId = "filtertype",label=tags$span(style="color: #6699FF;","Filter Type"),
                                          choices=c("HEPA","MERV 13","MERV 11","MERV 8"),selected="MERV 8"),
                              selectInput("portablehepa",label=tags$span(style="color: #6699FF;","Portable Air Purifier"),choices=c("Yes","No"),selected="No"),
                              selectInput("openwindows",label=tags$span(style="color: #6699FF;","Are windows and/or doors open?"),choices=c("Yes","No"),selected="No"))
                              #selectInput("opendoor",label=tags$span(style="color: #6699FF;","Are doors open?"),choices=c("Yes","No")),selected="No"),

                     ), #end of column
                   column(width=3,offset=1, style='padding:0px;',

  
                            sliderInput("studentmaskpercent",label=tags$span(style="color: #660000;","Percent of Students Masked"),min=0,max=100,value=0,ticks=FALSE),
                            #selectInput("teachermask",label=tags$span(style="color: #660000;","Is the teacher masked?"),choices=c("Yes","No")),
                            #actionButton(inputId = "advmask", label=tags$span(style="color: #660000;","Advanced Teacher Mask Options")),
                          
                          # If advanced variables are selected, open sample type and dose response.
                          #conditionalPanel(
                          #  condition = ("input.advmask%2>0"),
                          #  selectInput(inputId = "teachermasktype",
                           #             label = "Mask Type",
                          #              choices=c("Cloth","Surgical","KN95")))
                        ) #end of column
            ) #end of second row
            
    ) #end of tabItem
  ) #end of tab Items
) #end of dashboard body
             


shinyApp(
   ui=dashboardPage(
     dashboardHeader(title = "Menu"),
     sidebar,
     body
   ), #end of ui
  
   server=function(input,output){

     output$plot<-renderGauge({
       
       rm(list = ls())
       
       size<<-input$size
       studentmaskpercent<<-input$studentmaskpercent
       pathogen<<-input$pathogen 
       numstudents<<-as.numeric(input$numstudents)
       fractinfect<<-as.numeric(input$fractinfect)
       #teachermasktype<<-input$teachermasktype
       airexchange<<-input$airexchange
       actlevel<<-input$actlevel
       studentage<<-input$studentage
       #teachermask<<-input$teachermask
       openwindows<<-input$openwindows
       handsanitizer<<-input$handsanitizer
       #opendoor<<-input$opendoor
       hepa<<-input$portablehepa
       filtertype<<-input$filtertype
       
       source('risk_model.R')
       
       
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
       risk.output<-df22$y[!is.na(df22$y)]
       
       
         gauge(risk.output*100,
               min = 0, 
               max = 10, 
               symbol="%",
               sectors = gaugeSectors(success = c(0, 0.1), 
                                      warning = c(0.1, 1),
                                      danger = c(1, 25)))
     
     })
   }
)


