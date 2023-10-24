require(shiny)
require(shinydashboard)

shinyApp(
  ui=dashboardPage(
    dashboardHeader(title="School Health Risk Calculator",titleWidth=320),
    dashboardSidebar(width=320,
      sidebarMenu(
        menuItem("Welcome",tabName="Welcome",icon=icon("school")),
        menuItem("How to Use",tabname="How to Use",icon=icon("book")),
        menuItem("Risk Tool",tabname="Risk Tool",icon=icon("calculator")),
        menuItem("Support",tabname="Supporting Information",icon=icon("question mark"))
      ) #end of sidebarMenu
    ), #end of dashbboardSidebar
    dashboardBody(
      tabItems(
        tabItem(tabName="Welcome",
                mainPanel(
                  h1("Welcome!"),
                  p("Welcome to the School Health Risk Calculator, a tool to 
                    guide you through decisions to prevent the transmission of respiratory
                    diseases in the classroom. This calculator can also serve as an educational
                    tool to explore how single or combined interventions can work together
                    to reduce risk.",style="text-align:justify;"),
                  p("While this tool can help inform intervention decision making quickly
                    (and for free), this tool utilizes what are called 'exposure models', or
                    mathematical representations of reality. Therefore, there is uncertainty in 
                    the true estimated infection risks, and our models continually improve
                    as we gain more data to inform them. For more information on the specific
                    assumptions in these models, please see our Supporting Information tab.",
                    style="text-align:justify;")
                )),
        tabItem(tabName="How to Use"),
        tabItem(tabName="Risk Tool"),
        tabItem(tabName="Supporting Information")
      ) #end of tabItems
    ) #end of dashboardBody
  ), #end of ui
  server=function(input,output){
    
  }
)


