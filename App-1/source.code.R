source('risk_model.R')
classroom.model(volume=as.numeric(input$volume),
                pathogen=input$pathogen,
                num.student.male=as.numeric(input$numstudentmale),
                num.student.female=as.numeric(input$numstudentfemale),
                num.infect=as.numeric(input$numstudentinfect),
                AER=as.numeric(input$airexchange),
                student.age=as.numeric(input$studentage),
                teacher.gender=input$teachergender,
                teacher.age=as.numeric(input$teacherage),
                input$studentmask,
                input$teachermask,
                input$deskmaterial,
                class.duration=as.numeric(input$classduration),
                previous.sick=input$previoussick)


source('Dose-response and data sum.R')