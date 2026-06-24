require(shiny)
require(bslib)
require(flexdashboard)
require(shinycssloaders)
require(truncdist)
require(triangle)
require(magrittr)

# The risk model only DEFINES risk_model(); source it once at startup
# instead of on every gauge render (faster + avoids re-parsing each click).
source("risk_model.R")

# ---- Theme -------------------------------------------------------------------
# Clean, modern Bootstrap 5 theme in American Lung Association-style blues.
app_theme <- bs_theme(
  version = 5,
  bootswatch = "cosmo",
  primary = "#0a6ebd",
  secondary = "#5a7184",
  success = "#1a9e6f",
  warning = "#e8a33d",
  danger = "#d6455d",
  base_font = font_collection(
    font_google("Inter", local = FALSE), "system-ui", "sans-serif"
  ),
  heading_font = font_collection(
    font_google("Inter", local = FALSE), "system-ui", "sans-serif"
  )
)

extra_css <- tags$head(tags$style(HTML("
  .app-hero {
    background: linear-gradient(120deg, #0a6ebd 0%, #1192c9 100%);
    color: #fff; border-radius: 1rem; padding: 1.75rem 2rem; margin-bottom: 1.25rem;
  }
  .app-hero h2 { color:#fff; font-weight:700; margin-bottom:.35rem; }
  .app-hero p  { color:#eaf4fb; margin-bottom:0; max-width:60ch; }
  .gauge-card .card-body { display:flex; flex-direction:column; align-items:center; }
  .gauge-caption { color:#5a7184; font-size:.9rem; text-align:center; max-width:46ch; margin-top:.25rem; }
  .legend-pill { display:inline-flex; align-items:center; gap:.4rem; font-size:.82rem;
    padding:.2rem .6rem; border-radius:999px; background:#f2f5f8; margin:.15rem; }
  .legend-dot  { width:.7rem; height:.7rem; border-radius:50%; display:inline-block; }
  .prose p { font-size: 1.02rem; line-height: 1.6; }
  .prose h3 { margin-top: 1.4rem; font-weight: 600; }
  .card { border:none; box-shadow:0 1px 3px rgba(16,42,67,.08), 0 1px 2px rgba(16,42,67,.06); }
  .accordion-button { font-weight: 600; }
")))

# ---- Reusable input groups (sidebar) -----------------------------------------
calc_sidebar <- sidebar(
  width = 340, class = "p-0",
  accordion(
    open = c("Illness", "Classroom"),
    accordion_panel(
      "Illness", icon = icon("virus"),
      selectInput("pathogen", "Illness type",
                  choices = c("Common Cold", "COVID-19", "Flu")),
      sliderInput("fractinfect", "% of students infected",
                  min = 0, max = 100, value = 0, step = 10, ticks = FALSE)
    ),
    accordion_panel(
      "Classroom", icon = icon("chalkboard"),
      sliderInput("numstudents", "Number of students",
                  min = 1, max = 40, value = 20, step = 5, ticks = FALSE),
      selectInput("studentage", "Grade level",
                  choices = c("Kindergarten", "1st", "2nd", "3rd", "4th", "5th")),
      selectInput("actlevel", "Class type",
                  choices = c("General Ed", "PE", "SPED", "Music")),
      conditionalPanel(
        condition = "input.actlevel != 'PE'",
        selectInput("size", "Classroom size",
                    choices = c("Small", "Medium", "Large")),
        checkboxInput("advclass", "Advanced: set exact square footage", FALSE),
        conditionalPanel(
          condition = "input.advclass == true",
          sliderInput("size_sqft", "Classroom sq. ft.",
                      min = 500, max = 2000, value = 1000, step = 100, ticks = FALSE)
        )
      )
    ),
    accordion_panel(
      "Air quality", icon = icon("wind"),
      selectInput("airexchange", "Air quality",
                  choices = c("Poor", "Fair", "Good", "Great")),
      checkboxInput("adv", "Advanced air quality options", FALSE),
      conditionalPanel(
        condition = "input.adv == true",
        selectInput("filtertype", "Filter type",
                    choices = c("HEPA", "MERV 14", "MERV 13", "MERV 8"),
                    selected = "MERV 8"),
        selectInput("portablehepa", "Portable air purifier",
                    choices = c("Yes", "No"), selected = "No"),
        selectInput("openwindows", "Windows and/or doors open?",
                    choices = c("Yes", "No"), selected = "No")
      )
    ),
    accordion_panel(
      "Masks & hygiene", icon = icon("droplet"),
      sliderInput("studentmaskpercent", "Percent of students masked",
                  min = 0, max = 100, value = 0, ticks = FALSE),
      selectInput("handsanitizer", "Hand sanitizer used by students",
                  choices = c("Yes", "No"), selected = "No"),
      checkboxInput("advsurface",
                    "Advanced: explore shared surface area", FALSE),
      conditionalPanel(
        condition = "input.advsurface == true",
        sliderInput("surfacearea", "Total shared surface area (cm²)",
                    min = 2000, max = 120000, value = 84435, step = 2000,
                    ticks = FALSE),
        helpText("Default model uses a range of 57,240–111,630 cm². ",
                 "Smaller shared surface areas concentrate virus onto hands, ",
                 "strengthening the fomite pathway — the route hand sanitizer ",
                 "acts on. Use this to explore how surface area drives the ",
                 "impact of hand hygiene.")
      )
    )
  )
)

# ---- Pages -------------------------------------------------------------------
welcome_tab <- nav_panel(
  title = "Welcome", icon = icon("house"),
  div(
    class = "app-hero",
    h2("School Respiratory Risk Tool"),
    p("Explore how single or bundled interventions change the estimated infection
       risk for students and teachers during class time.")
  ),
  card(
    card_body(
      class = "prose",
      p("Welcome to the school health tool. The tabs above provide information
         about the tool, how to use it, and access to the calculator itself.
         A summary of the project is shown in the digital flyer below."),
      img(src = "infosheet.png", style = "max-width:900px;width:100%;border-radius:.5rem;")
    )
  )
)

about_tab <- nav_panel(
  title = "About", icon = icon("book"),
  card(card_header("What is this app all about?"), card_body(
    class = "prose",
    p("This application is meant to guide decision-making in schools with regards
       to interventions meant to reduce the spread of respiratory viral diseases,
       such as flu or rhinovirus (the virus responsible for the common cold). This
       application is also useful for educational purposes by demonstrating in real
       time how single or bundled interventions can reduce risks for students and
       teachers."),
    p("This tool cannot connect to attendance data systems and is not for contact
       tracing or detecting outbreaks. Rather, it is for exploring hypothetical
       scenarios and seeing, quantitatively, how single or bundled interventions
       could reduce disease burden and maintain infection risk levels below 10%. In
       other words, the tool is designed for planning and prevention. New features
       in development include the ability to download reports with information on
       tested scenarios and guidance based on the risk calculator tool output. A
       paper describing the tool in full detail is under review in a peer reviewed
       journal. A link to that paper will be made available here upon publication."),
    p("The development of this tool is funded by the American Lung Association, and
       Drs. Amanda Wilson and Ashley Lowe are working with Arizona-based public,
       private, and charter schools to inform and improve the tool's development so
       that it is as accurate as possible and useful to school health professionals
       in making decisions regarding controlling the spread of respiratory viral
       disease in school settings. This tool and the assumptions in developing it
       are not reflective of official views of the American Lung Association and are
       solely the responsibility of the research investigators. Questions about the
       tool or the project can be directed to Dr. Amanda Wilson at amwilson2@arizona.edu.")
  ))
)

how_tab <- nav_panel(
  title = "Instructions", icon = icon("gear"),
  card(card_header("How to use the tool"), card_body(
    class = "prose",
    p("This calculator estimates the average infection risk per student and for the
       teacher for a simulation of 3 hours of class time. This means risks for some
       scenarios (like specials classes that may only be for an hour) are likely
       over-estimated. The output is given as a percent chance any given student gets
       infected, and updates as you change settings in the sidebar. Not every student
       who gets infected will become ill (i.e., have symptoms), but those who are
       infectious can still infect others without symptoms."),
    p("We use a framework called 'Quantitative Microbial Risk Assessment' (QMRA), in
       which we use data from published scientific literature and assumptions based on
       the inputs you select to quantify an infection risk for a given scenario."),
    tags$a(href = "https://qmrawiki.org/about", target = "_blank", "What is QMRA?"),

    h3("Illness type"),
    p("We included pathogens for which there is published information about how the
       amount of virus someone inhales/ingests relates to risk of infection. This is
       called a 'dose response' relationship."),
    tags$a(href = "https://qmrawiki.org/framework/dose-response/dose-response-assessment",
           target = "_blank", "Dose response information"),

    h3("% of students infected"),
    p("Select what percent of the students are assumed to be infected (asymptomatic or
       not). You may have a number to inform this depending on how many students are
       absent due to illness, or you may choose a value just to see how it impacts the
       estimated infection risk. The estimated risk is compared to a threshold of 10%,
       which would trigger communication about an outbreak."),

    h3("Classroom settings"),
    p("Number of students, grade level, class type, and classroom size influence the
       activity level, respiration rates, average student age, and room volume used in
       the model. The advanced option lets you set an exact square footage."),

    h3("Air quality settings"),
    p("The basic air quality setting ranges from 'Poor' to 'Great,' based on
       recommended fresh-air exchange values in classrooms. Advanced options let you
       choose a filter type, a portable air purifier, and whether windows/doors are
       open (assumed to double the fresh-air exchange)."),
    tags$a(href = "https://www.ashrae.org/file%20library/technical%20resources/free%20resources/design-guidance-for-education-facilities.pdf",
           target = "_blank", "ASHRAE information on air changes for classrooms"),

    h3("Masks & hygiene"),
    p("Select the percent of students wearing masks and whether students use hand
       sanitizer. Mask effectiveness values assume masks are worn properly, so
       estimated reductions are optimistic.")
  ))
)

risk_legend <- div(
  span(class = "legend-pill",
       span(class = "legend-dot", style = "background:#1a9e6f;"), "Safe (< 0.10%)"),
  span(class = "legend-pill",
       span(class = "legend-dot", style = "background:#e8a33d;"), "Elevated (0.10–5%)"),
  span(class = "legend-pill",
       span(class = "legend-dot", style = "background:#d6455d;"), "High (> 5%)")
)

calc_tab <- nav_panel(
  title = "Calculator", icon = icon("calculator"),
  layout_sidebar(
    sidebar = calc_sidebar,
    card(
      class = "gauge-card",
      card_header("Percent chance of infection per student"),
      card_body(
        withSpinner(flexdashboard::gaugeOutput("plot", height = "260px"),
                    color = "#0a6ebd"),
        div(class = "gauge-caption",
            "Estimated average infection risk for one student over a 3-hour class.
             Adjust the settings in the sidebar to see how interventions change the
             risk in real time."),
        div(class = "mt-2", risk_legend)
      )
    )
  )
)

ui <- page_navbar(
  title = "School Respiratory Risk Tool",
  theme = app_theme,
  fillable = FALSE,
  header = extra_css,
  welcome_tab,
  about_tab,
  how_tab,
  calc_tab
)

# ---- Server (logic unchanged from original) ----------------------------------
server <- function(input, output) {
  output$plot <- renderGauge({

    # Use the exact sq-ft slider when advanced is checked, else the size category.
    # (These are separate inputs now to avoid a duplicate-ID conflict that made
    #  risk_model() error and freeze the gauge at its last value.)
    size <<- if (isTRUE(input$advclass)) as.numeric(input$size_sqft) else input$size
    studentmaskpercent <<- input$studentmaskpercent
    pathogen <<- input$pathogen
    numstudents <<- as.numeric(input$numstudents)
    fractinfect <<- as.numeric(input$fractinfect)
    airexchange <<- input$airexchange
    actlevel <<- input$actlevel
    studentage <<- input$studentage
    openwindows <<- input$openwindows
    handsanitizer <<- input$handsanitizer
    hepa <<- input$portablehepa
    filtertype <<- input$filtertype
    # Optional fixed shared surface area; NA -> model uses its default range.
    surfacearea <<- if (isTRUE(input$advsurface)) as.numeric(input$surfacearea) else NA

    risk_model()
    print(risk.output)

    gauge(risk.output * 100,
          min = 0,
          max = 10.0,
          symbol = "%",
          abbreviateDecimals = 2,
          sectors = gaugeSectors(success = c(0, 0.10),
                                 warning = c(0.10, 5.0),
                                 danger  = c(5.0, 25.0)))
  })
}

shinyApp(ui = ui, server = server)
