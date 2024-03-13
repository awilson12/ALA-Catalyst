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
    menuItem("Calculator Instructions",tabName="How",icon=icon("cog")),
    menuItem("Calculator",icon=icon("th"),tabName="calc")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "welcome",
            h2("Welcome!"),
            p("Welcome to the school health tool. This tool is a part of a larger resource for school health professionals, called ____________. The following tabs
              will provide information about the tool, how to use it, and access to the tool itself. A summary of the project at large can be seen in a digital
              flyer below."),
            img(src="infosheet.png",width=900)
    ),
    
    tabItem(tabName = "about",
            h2("What is this app all about?"),
            p("This application is meant to guide decision-making regarding interventions in schools meant to reduce 
              the spread of respiratory viral diseases, such as flu or rhinovirus, the virus responsible for
              the common cold. This application is also useful for educational purposes by demonstrating in
              real time how single or bundled interventions can reduce risks for students and teachers."),
            p("The development of this tool is funded by the American Lung Association, and Drs. Amanda Wilson and Ashley Lowe
              are working with Arizona-based public, private, and charter schools to inform and improve the tool's develpoment so that it is as
              accurate as possible and useful to school health professionals in making decisions regarding controlling the spread of
              respiratory viral disease in school settings. Questions about the tool or the project can be directed to Dr. Amanda 
              Wilson at amwilson2@arizona.edu.")
    ),
    tabItem(tabName="How",
            h2("How to Use the Tool"),
              p("This calculator tool estimates the average infection risk per student and for the teacher for a simulation of a 1-hr class. The 
                output is given in the form of a number of infections expected per 1,000 students or teachers. These numbers update as you change
                settings in the model with the menu below the figure.
                The available parameters and their descriptions are provided below. For more detailed information on model assumptions and
                sources, please click here. (link to be entered!!!)"),
              p("We use a framework called 'Quantitative Microbial Risk Assessment' (QMRA), in which we use data from published scientific literature and assumptions
                based on the inputs that to select to quantify an infection risk for a given scenario. More information on what QMRA is and how it's used can be found below."),
              tags$a(href="https://qmrawiki.org/about", "What is QMRA?"),
              h3("Number of Students"),
                p("This setting allows you to select the number of students in the classroom."),
              h3("% Infected Students"),
                p("This allows you to select what percent of the students are assumed to be infected (asymptomatic or not)."),
              h3("Student Grade Level"),
                p("This setting allows you to choose kindergarten through 5th grade. This input is used to estimate the average
                  age of a child and the volume of air they would inhale over 1-hr in a classroom."),
              h3("Teacher Age"),
                p("This allows you to select the assumed teacher's age so that the model can estimate the volume of air that a teacher
                  would inhale over a 1-hr class. The model considers differences in inhalation rates not only by age but also by gender and additionally
                  incorporates the average proportions of male vs. female teachers in K-5 classrooms across the U.S."),
              h3("Pathogen"),
                p("We included pathogens for which there is published information about how the amount of virus someone inhales/ingests relates to 
                  risk of infection. This is called a 'dose response' relationship, where a curve informed by data estimates an infection risk based on
                  an estimate of how much virus enters a student's or teacher's body through the lungs or through hand-to-face contacts. To learn more
                  about dose response and its use in quantitative microbial risk assessment, please use the link below."),
                  tags$a(href="https://qmrawiki.org/content/dose-response-assessment","Dose Response Information"),
              h3("Air Exchange Rate"),
                p("Air exchange rate is a measure of how quickly the air in a space is replaced with fresh air. A higher air exchange rate means more
                  fresh air replaces 'old' air over a shorter period of time. For more information on air exchange rates and recommended air exchange
                  rates for different spaces, please see this information from the American Society of Heating, Refrigerating, and Air Conditioning
                  Engineers (ASHRAE)"),
                   tags$a(href="chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://www.ashrae.org/file%20library/technical%20resources/free%20resources/design-guidance-for-education-facilities.pdf", 
                         "ASHRAE information on Air Changes for Classrooms"),
              h3("Portable HEPA Filter"),
                p("This allows you to select whether a portable HEPA filter is assumed to be on in the classroom and working effectively. Portable
                  HEPA filters can be very useful in reducing not only the amount of virus in the air but also dust that can exacerbate students
                  with asthma."),
              h3("Open Windows & Doors"),
                p("These options allow you to select whether to assume windows and/or doors are open or closed in the classroom. Open windows and doors can increase the amount of
                  fresh air that is circulated in the room, diluting the amount of virus that is exhaled by infected students. We assume an average size
                  of _______ per window and ________ windows and an average size of ______ per door and 1 door to the classroom. While the amount of fresh air can depend greatly 
                  on the weather and whether the classroom door exits to outdoors or indoors, we assume _________."),
              h3("Classroom Volume"),
                p("You can select the assumed volume of the classroom. This is used in the model to estimate how virus will accumulate in the air, given other
                  assumptions, such as the air exchange rate, percent of sick students, etc."),
              h3("Student and Teacher Masks"),
                p("In these settings, you can select the percent of students that are wearing masks (including 0% as an option) and whether the
                  teacher is wearing a maks. Because the effectiveness of masks depends upon the material type, you can also select the type of mask
                  being used. This is assumed to be used by the teacher and the students, if any students are assumed to be wearing masks. The mask effectiveness
                  values used in the model assume that the mask is being worn propertly (i.e., sealed securly to the chin and nose). So reductions in risk
                  that are estimated are optimistic, and barriers to feasibility in proper use should be considered.")
            ),
    tabItem(tabName= "calc",
            #h2("Click through choices and see changes in risk in real time"),
            fluidRow(
                       column(width=12, offset=0, style='padding:0px;', style="background-color:#ffffff;",
                              
                                title="Infection Risk for a 1-hr Class",status="primary",solidHeader=TRUE,
                                collabpsible=TRUE,
                                echarts4rOutput("plot")
                              
                              )), #end of top left
                
            fluidRow(
                column(3,offset=0, style='padding:0px;',
                    selectInput("pathogen",tags$span(style="color: Purple;","Illness Type"),choices=c("Common Cold","COVID-19","Flu")),
                    sliderInput("fractinfect",tags$span(style="color: Purple;","% of Students Infected"),0,100,0,step=10,ticks=FALSE),
                       
                    sliderInput("numstudents",tags$span(style="color: Blue;","Number of Students"),min=10,max=40,value=10,step=5,ticks=FALSE),
                    selectInput("studentage",tags$span(style="color: Blue;","Grade Level"),choices=c("Kindergarten","1st","2nd","3rd","4th","5th")),
                    sliderInput("teacherage",tags$span(style="color: Blue;","Teacher Age"),20,65,5,ticks=FALSE),
                    selectInput("actlevel",label=tags$span(style="color: blue;","Class Type"),choices=c("General Ed","PE","SPED","Music")),
                    selectInput("size",tags$span(style="color: blue;","Classroom Size"),choices=c("Small","Medium","Large")),
                     actionButton(inputId="advclass",label=tags$span(style="color: blue;","Advanced Classroom Size")),
                     conditionalPanel(
                       condition=("input.advclass%2>0"),
                       sliderInput(inputId="size",label=tags$span(style="color: blue;","Classroom Sq. Ft."),500,1000,100,ticks=FALSE)
                     )
                     #sliderInput("numstudentfemale","Number of Female Students",1,20,1),
                     
                     
              ), #end of top right
                   column(width=3,offset=1, style='padding:0px;',
                         
                            selectInput("airexchange","HVAC Settings",choices=c("Poor","Fair","Good","Great")),
                            actionButton(inputId = "adv", label = "Advanced HVAC Options"),
                          
                            # If advanced variables are selected, open sample type and dose response.
                             conditionalPanel(
                              condition = ("input.adv%2>0"),
                              sliderInput(inputId = "airexchange",
                                        label = "Air Exchange Rate",
                                        0.3, 4, 1,ticks=FALSE),
                              selectInput(inputId = "filter type",label="Filter Type",
                                          choices=c("HEPA","MERV 13","MERV 12"))),
                            
              
                          
                          #sliderInput("airexchange","Air Exchange Rate (1/hr)",min=0.2,max=3,value=0.2,step=0.2,ticks=FALSE),
                            #selectInput("deskmaterial","Student Desk Material",choices=c("wood","steel","plastic")),
                            #sliderInput("increasedvent","Increase Ventilation by %",min=10,max=100,value=10,ticks=FALSE),
                            selectInput("portablehepa","Portable Air Purifier?",choices=c("Yes","No")),
                            selectInput("openwindows","Are windows open?",choices=c("Yes","No")),
                            selectInput("opendoor","Are doors open?",choices=c("Yes","No"))
                     ), #end of column
                   column(width=3,offset=1, style='padding:0px;',

  
                            sliderInput("studentmaskpercent","% Students Masked",min=0,max=100,value=10,ticks=FALSE),
                            selectInput("teachermask","Is the teacher masked?",choices=c("Yes","No")),
                            actionButton(inputId = "advmask", label = "Advanced Teacher Mask Options"),
                          
                          # If advanced variables are selected, open sample type and dose response.
                          conditionalPanel(
                            condition = ("input.advmask%2>0"),
                            selectInput(inputId = "teachermasktype",
                                        label = "Mask Type",
                                        choices=c("Cloth","Surgical","KN95","N95")))
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

    # observeEvent(input$adv, {
    #   if(input$airexchange>0){
    #     shinyjs::show(id = c("adv"))
    #   }else{
    #     shinyjs::hide(id = c("adv"))
    #   }
    
    # })
     
    # print(input$adv)
     
     output$plot<-renderEcharts4r({
       
       rm(list = ls())
       
       volume<<-as.numeric(input$volume)
       pathogen<<-input$pathogen
       numstudents<<-as.numeric(input$numstudents)
       fractinfect<<-as.numeric(input$fractinfect)
       AER<<-as.numeric(input$airexchange)
       
       if(input$studentage=="Kindergarten"){
         student.age<<-5
       }else if (input$studentage=="1st"){
         student.age<<-6
       }else if (input$studentage=="2nd"){
         student.age<<-7
       }else if (input$studentage=="3rd"){
         student.age<<-8
       }else if (input$studentage=="4th"){
         student.age<<-9
       }else{
         student.age<<-10
       }
       
       
       
       student.age<<-as.numeric(input$studentage)
       #teacher.gender<<-input$teachergender
       teacher.age<<-as.numeric(input$teacherage)
       #student.mask<<-input$studentmask
       if(input$teachermask=="Yes"){
         teacher.mask<<-TRUE
       }else{
         teacher.mask<<-FALSE
       }
       #desk.material<<-input$deskmaterial
       #class.duration<<-as.numeric(input$classduration)
       #previous.sick<<-input$previoussick
       studentmaskpercent<<-as.numeric(input$studentmaskpercent)
       if(input$openwindows=="Yes"){
         openwindows<<-TRUE
       }else{
         openwindows<<-FALSE
       }
       if(input$opendoor=="Yes"){
         opendoor<<-TRUE
       }else{
         opendoor<<-FALSE
       }
       if(input$portablehepa=="Yes"){
         hepa<<-TRUE
       }else{
         hepa<<-FALSE
       }
       
       source('risk_model.R')
       
       #print(summary(risk.student.inhale))
       
       #print(as.numeric(input$numstudentmale))
       
       frame.all<-data.frame(risks=c(risk.student.inhale,risk.student.face,risk.student.total,
                                     risk.teacher.inhale,risk.teacher.face,risk.teacher.total),
                             type=rep(c(rep("Inhalation",length(risk.student.inhale)),rep("Ingestion",length(risk.student.face)),rep("Total",length(risk.student.total))),2),
                             person=c(rep("Student",length(c(risk.student.inhale,risk.student.face,risk.student.total))),
                                      rep("Teacher",length(c(risk.teacher.inhale,risk.teacher.face,risk.teacher.total)))))
       #print(summary(risk.student.total))
       #print(summary(risk.teacher.total))
       
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
           e_title("Daily Risk: Infections/1,000 People") %>% 
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


