# =============================================================================
# Contact-Network Risk Explorer — draggable seating chart (Stage-1 prototype)
#
# Desks are draggable nodes. Close-contact time between two students DECAYS WITH
# DISTANCE, so rearranging the room changes who is exposed:
#
#   contact_min(i,j) = Tmax * exp( -(distance_m / scale)^2 )
#   lambda_i = far-field (shared) + sum_{infected j} contact_h(i,j)*q*p / Q.near
#   risk_i   = 1 - exp(-lambda_i)
#
# Drag a desk next to an infected classmate and its risk rises; move it away and
# it falls. Illustrative (Wells-Riley surrogate), not calibrated.
# =============================================================================
require(shiny)
require(bslib)
require(visNetwork)

SCALE_PX <- 55   # vis-units per metre (links on-screen distance to the model)

# ---- model -----------------------------------------------------------------
make_layout <- function(P, preset) {
  N <- P$N; s <- SCALE_PX
  if (preset == "Clusters (pods)") {
    ps <- 5; npod <- ceiling(N / ps); cols <- ceiling(sqrt(npod))
    pos <- matrix(NA_real_, N, 2); k <- 0
    for (pd in seq_len(npod)) {
      cx <- ((pd - 1) %% cols) * 3.3 * s; cy <- ((pd - 1) %/% cols) * 3.3 * s
      mm <- min(ps, N - k); a <- seq(0, 2 * pi, length.out = mm + 1)[seq_len(mm)]
      for (t in seq_len(mm)) { k <- k + 1
        pos[k, ] <- c(cx + 0.5 * s * cos(a[t]), cy + 0.5 * s * sin(a[t])) }
    }
  } else {
    sp <- if (preset == "Spaced (distanced)") 1.9 else 1.05
    cols <- ceiling(sqrt(N * 1.5)); pos <- matrix(NA_real_, N, 2)
    for (k in seq_len(N)) pos[k, ] <- c(((k - 1) %% cols) * sp * s,
                                        ((k - 1) %/% cols) * sp * s)
  }
  pos
}

contact_from_pos <- function(pos, P) {
  dpx <- as.matrix(dist(pos)); dm <- dpx / SCALE_PX
  C <- P$Tmax * exp(-(dm / P$scale)^2); diag(C) <- 0; C
}

compute_risk <- function(C, infected, P) {
  Q.far <- P$V * P$AER
  lambda.far <- P$n.infected * P$q * P$p.breath * P$duration.h / Q.far
  contact.h <- rowSums(C[, infected, drop = FALSE]) / 60
  lambda <- lambda.far + contact.h * P$q * P$p.breath / P$Q.near
  risk <- 1 - exp(-lambda); risk[infected] <- NA
  list(risk = risk, p.far = 1 - exp(-lambda.far), contact.h = contact.h)
}

risk_hex <- function(r, cap = 0.35) {
  pal <- colorRamp(c("#1a9e6f", "#e8a33d", "#d6455d"))
  out <- rep("#444444", length(r)); ok <- !is.na(r)
  out[ok] <- grDevices::rgb(pal(pmin(r[ok] / cap, 1)), maxColorValue = 255); out
}

build_nodes <- function(pos, infected, st, P) {
  N <- P$N; isInf <- seq_len(N) %in% infected
  bg <- risk_hex(st$risk)
  ttl <- vapply(seq_len(N), function(i) if (isInf[i])
    sprintf("<b>Student %d</b><br>Infected (index case)", i) else
    sprintf("<b>Student %d</b><br>Susceptible<br>Risk: %.1f%%<br>%d min close contact w/ infected",
            i, 100 * st$risk[i], round(60 * st$contact.h[i])), character(1))
  # "circle" puts the student number INSIDE the node (dot/star put it below).
  # Infected are marked by a thick red ring on a dark fill instead of a shape.
  data.frame(id = seq_len(N), label = as.character(seq_len(N)),
             x = pos[, 1], y = pos[, 2],
             color.background = bg,
             color.border = ifelse(isInf, "#e23b3b", "#ffffff"),
             borderWidth = ifelse(isInf, 4, 2),
             shape = "circle", title = ttl, stringsAsFactors = FALSE)
}

pos_from_input <- function(lst, N) {
  m <- matrix(NA_real_, N, 2)
  for (i in seq_len(N)) { p <- lst[[as.character(i)]]
    if (!is.null(p)) m[i, ] <- c(p$x, p$y) }
  m
}

plot_distribution <- function(st, infected, P) {
  sus <- setdiff(seq_len(P$N), infected); r <- st$risk[sus]
  op <- par(mar = c(4, 4, 1, 1)); on.exit(par(op))
  bp <- barplot(100 * sort(r), col = "#0a6ebd", border = NA,
                ylim = c(0, max(100 * r, 12) * 1.12),
                ylab = "Infection risk (%)", xlab = "Students (sorted)")
  abline(h = 100 * st$p.far, lty = 2, lwd = 2, col = "#333333")
  text(mean(bp), 100 * st$p.far, pos = 3, cex = 0.9, col = "#333333",
       labels = sprintf("well-mixed average = %.1f%%", 100 * st$p.far))
}

# ---- ui --------------------------------------------------------------------
app_theme <- bs_theme(version = 5, bootswatch = "cosmo", primary = "#0a6ebd")
disclaimer <- div(
  style = "background:#fff8e6;border:1px solid #f0d48a;color:#7a5b00;border-radius:.5rem;
           padding:.55rem .9rem;margin-bottom:1rem;font-size:.88rem;",
  icon("triangle-exclamation"),
  HTML("&nbsp;<strong>Educational prototype — under active development.</strong>
        Illustrative seating/contact model; not calibrated for decision-making."))

controls <- sidebar(
  width = 320, title = "Classroom",
  radioButtons("preset", "Starting arrangement",
               c("Rows", "Clusters (pods)", "Spaced (distanced)")),
  sliderInput("N", "Number of students", 10, 40, 24, 1),
  sliderInput("ninf", "Number infected", 1, 6, 2, 1),
  sliderInput("dur", "Class length (hours)", 1, 8, 3, 1),
  hr(),
  sliderInput("aer", "Air exchange rate (per hour)", 1, 10, 3, 1),
  sliderInput("scale", "Close-contact distance (m)", 0.5, 2.5, 1.2, 0.1),
  actionButton("reshuffle", "Reshuffle infected", icon = icon("shuffle"),
               class = "btn-primary w-100"))

ui <- page_sidebar(
  title = "Contact-Network Risk Explorer", theme = app_theme, sidebar = controls,
  tags$head(tags$style(HTML(".card{border:none;box-shadow:0 1px 3px rgba(16,42,67,.1);}"))),
  disclaimer,
  layout_columns(col_widths = c(3, 3, 3, 3),
    value_box("Well-mixed average", textOutput("v_far"),  showcase = icon("wind"),       theme = "secondary"),
    value_box("Network mean",       textOutput("v_mean"), showcase = icon("users"),      theme = "primary"),
    value_box("Highest-risk student", textOutput("v_max"), showcase = icon("user-xmark"), theme = "danger"),
    value_box("Students above 10%", textOutput("v_over"), showcase = icon("triangle-exclamation"), theme = "warning")),
  layout_columns(col_widths = c(7, 5),
    card(card_header("Seating chart — drag desks to rearrange the room (red ring = infected)"),
         visNetworkOutput("seat", height = "470px"),
         card_footer(class = "text-muted small",
           "Drag any desk; risk recomputes from how close each student sits to an infected classmate. Hover for details.")),
    card(card_header("Per-student risk vs. the room average"),
         plotOutput("dist", height = "470px"),
         card_footer(class = "text-muted small",
           "The dashed line is the single number a well-mixed model reports."))))

# ---- server ----------------------------------------------------------------
server <- function(input, output, session) {
  rv <- reactiveValues(struct = 0, infected = NULL, pos = NULL)

  params <- reactive(list(
    N = input$N, n.infected = min(input$ninf, input$N - 1), duration.h = input$dur,
    q = 10, p.breath = 0.5, V = input$N * 8, AER = input$aer, Q.near = 30,
    Tmax = 90, scale = input$scale))

  # Structural changes -> new layout + new infected + re-render
  observeEvent(list(input$N, input$ninf, input$preset, input$reshuffle), {
    P <- params()
    rv$infected <- sort(sample(P$N, P$n.infected))
    rv$pos <- make_layout(P, input$preset)
    rv$struct <- rv$struct + 1
  }, ignoreInit = FALSE)

  state <- reactive({ req(rv$pos)
    compute_risk(contact_from_pos(rv$pos, params()), rv$infected, params()) })

  output$seat <- renderVisNetwork({ rv$struct
    isolate({ req(rv$pos); P <- params()
      visNetwork(build_nodes(rv$pos, rv$infected, state(), P)) |>
        visNodes(font = list(size = 16, color = "#111111",
                             strokeWidth = 3, strokeColor = "#ffffff")) |>
        visPhysics(enabled = FALSE) |>
        visInteraction(dragNodes = TRUE, dragView = TRUE, zoomView = TRUE,
                       hover = TRUE, tooltipDelay = 60) |>
        visEvents(dragEnd = "function(p){ Shiny.setInputValue('seat_positions', this.getPositions(), {priority:'event'}); }")
    })
  })

  # Drag -> update positions (triggers recolor + plots)
  observeEvent(input$seat_positions, {
    rv$pos <- pos_from_input(input$seat_positions, params()$N)
  })

  # Recolor nodes in place whenever risk changes (drag or slider), no re-render
  observe({ req(rv$pos); st <- state(); P <- params()
    nd <- build_nodes(rv$pos, rv$infected, st, P)
    visNetworkProxy("seat") |>
      visUpdateNodes(nd[, c("id", "color.background", "title")])
  })

  output$dist <- renderPlot({ req(rv$pos); plot_distribution(state(), rv$infected, params()) })

  f <- function(x) sprintf("%.1f%%", 100 * x)
  output$v_far  <- renderText({ req(rv$pos); f(state()$p.far) })
  output$v_mean <- renderText({ req(rv$pos); f(mean(state()$risk, na.rm = TRUE)) })
  output$v_max  <- renderText({ req(rv$pos); f(max(state()$risk, na.rm = TRUE)) })
  output$v_over <- renderText({ req(rv$pos); r <- state()$risk
    sprintf("%d of %d", sum(r > 0.10, na.rm = TRUE), sum(!is.na(r))) })
}

shinyApp(ui, server)
