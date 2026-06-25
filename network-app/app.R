# =============================================================================
# Contact-Network Risk Explorer (Stage-1 prototype, visual)
# Companion to the School Respiratory Risk Tool: shows how a sensor-derived
# contact network turns a single room-average risk into a per-student
# distribution, and how cohorting reshapes it.
#
# Model (illustrative Wells-Riley surrogate, see network_risk_prototype.R):
#   lambda_i = far-field (shared)  +  near-field (close contact w/ infected)
#   risk_i   = 1 - exp(-lambda_i)
# Near-field is the INCREMENT above the room average (small Q.near), so the
# emitter is not double-counted across the two routes.
# =============================================================================
require(shiny)
require(bslib)

# ---- model -----------------------------------------------------------------
make_network <- function(P) {
  N <- P$N
  pod <- rep(seq_len(ceiling(N / P$pod.size)), each = P$pod.size)[1:N]
  C <- matrix(0, N, N)
  for (i in 1:(N - 1)) for (j in (i + 1):N) {
    rng <- if (pod[i] == pod[j]) P$within else P$between
    m <- runif(1, rng[1], rng[2])
    C[i, j] <- C[j, i] <- m
  }
  list(contact = C, pod = pod)
}

compute_risk <- function(net, infected, P) {
  C <- net$contact
  Q.far <- P$V * P$AER
  lambda.far <- P$n.infected * P$q * P$p.breath * P$duration.h / Q.far
  contact.h <- rowSums(C[, infected, drop = FALSE]) / 60
  lambda <- lambda.far + contact.h * P$q * P$p.breath / P$Q.near
  risk <- 1 - exp(-lambda)
  risk[infected] <- NA
  list(risk = risk, p.far = 1 - exp(-lambda.far), contact.h = contact.h)
}

risk_cols <- function(r, cap = 0.35) {
  pal <- colorRamp(c("#1a9e6f", "#e8a33d", "#d6455d"))
  out <- rep("#777777", length(r))
  ok <- !is.na(r)
  out[ok] <- grDevices::rgb(pal(pmin(r[ok] / cap, 1)), maxColorValue = 255)
  out
}

node_xy <- function(net, P) {
  pod <- net$pod; pods <- sort(unique(pod)); np <- length(pods)
  pa <- seq(0, 2 * pi, length.out = np + 1)[seq_len(np)]
  pc <- cbind(cos(pa), sin(pa))
  xy <- matrix(NA_real_, P$N, 2)
  for (k in seq_along(pods)) {
    idx <- which(pod == pods[k]); m <- length(idx)
    a <- seq(0, 2 * pi, length.out = m + 1)[seq_len(m)]
    rr <- ifelse(m == 1, 0, 0.30)
    xy[idx, 1] <- pc[k, 1] * 1.15 + rr * cos(a)
    xy[idx, 2] <- pc[k, 2] * 1.15 + rr * sin(a)
  }
  xy
}

plot_network <- function(net, res, infected, P) {
  N <- P$N; C <- net$contact; xy <- node_xy(net, P)
  op <- par(mar = c(0, 0, 0, 0)); on.exit(par(op))
  plot(xy, type = "n", axes = FALSE, xlab = "", ylab = "", asp = 1,
       xlim = range(xy[, 1]) + c(-.35, .35), ylim = range(xy[, 2]) + c(-.35, .35))
  maxc <- max(C)
  if (maxc > 0) for (i in 1:(N - 1)) for (j in (i + 1):N) if (C[i, j] > 1) {
    w <- C[i, j] / maxc
    segments(xy[i, 1], xy[i, 2], xy[j, 1], xy[j, 2],
             col = adjustcolor("#8aa0b2", 0.12 + 0.45 * w), lwd = 0.4 + 3 * w)
  }
  cols <- risk_cols(res$risk)
  points(xy, pch = 21, bg = cols, col = "white", cex = 3.1, lwd = 1.6)
  if (length(infected))
    points(xy[infected, , drop = FALSE], pch = 4, col = "#111111", cex = 1.9, lwd = 3)
}

plot_distribution <- function(net, res, infected, P) {
  sus <- setdiff(seq_len(P$N), infected)
  r <- res$risk[sus]; pods <- net$pod[sus]
  pal <- c("#0a6ebd", "#1a9e6f", "#e8a33d", "#d6455d", "#7a5b9c",
           "#3aa0a0", "#b5651d", "#577590")
  o <- order(r)
  op <- par(mar = c(4, 4, 1, 1)); on.exit(par(op))
  bp <- barplot(100 * r[o], col = pal[((pods[o] - 1) %% length(pal)) + 1],
                border = NA, ylim = c(0, max(100 * r, 12) * 1.12),
                ylab = "Infection risk (%)", xlab = "Students (sorted)")
  abline(h = 100 * res$p.far, lty = 2, lwd = 2, col = "#333333")
  text(mean(bp), 100 * res$p.far, pos = 3, cex = 0.9, col = "#333333",
       labels = sprintf("well-mixed average = %.1f%%", 100 * res$p.far))
}

# ---- theme / ui ------------------------------------------------------------
app_theme <- bs_theme(version = 5, bootswatch = "cosmo", primary = "#0a6ebd")

disclaimer <- div(
  style = "background:#fff8e6;border:1px solid #f0d48a;color:#7a5b00;
           border-radius:.5rem;padding:.55rem .9rem;margin-bottom:1rem;font-size:.88rem;",
  icon("triangle-exclamation"),
  HTML("&nbsp;<strong>Educational prototype — under active development.</strong>
        Illustrative contact-network model for exploring how person-to-person
        contact reshapes risk; not calibrated for decision-making."))

controls <- sidebar(
  width = 320, title = "Classroom",
  sliderInput("N", "Number of students", 10, 40, 25, 1),
  sliderInput("podsize", "Students per group (pod)", 2, 8, 5, 1),
  sliderInput("ninf", "Number infected", 1, 6, 2, 1),
  sliderInput("dur", "Class length (hours)", 1, 8, 3, 1),
  hr(),
  sliderInput("aer", "Air exchange rate (per hour)", 1, 10, 3, 1),
  sliderInput("near", "Close-contact intensity", 1, 10, 3, 1),
  sliderInput("within", "Avg within-group contact (min)", 10, 90, 50, 5),
  checkboxInput("cohort", "Cohorting: cut between-group mixing", FALSE),
  actionButton("reshuffle", "Reshuffle classroom", icon = icon("shuffle"),
               class = "btn-primary w-100 mt-1")
)

ui <- page_sidebar(
  title = "Contact-Network Risk Explorer",
  theme = app_theme,
  sidebar = controls,
  tags$head(tags$style(HTML(".card{border:none;box-shadow:0 1px 3px rgba(16,42,67,.1);}"))),
  disclaimer,
  layout_columns(
    col_widths = c(3, 3, 3, 3),
    value_box("Well-mixed average", textOutput("v_far"), showcase = icon("wind"),
              theme = "secondary"),
    value_box("Network mean", textOutput("v_mean"), showcase = icon("users"),
              theme = "primary"),
    value_box("Highest-risk student", textOutput("v_max"), showcase = icon("user-xmark"),
              theme = "danger"),
    value_box("Students above 10%", textOutput("v_over"), showcase = icon("triangle-exclamation"),
              theme = "warning")
  ),
  layout_columns(
    col_widths = c(7, 5),
    card(card_header("Contact network (node colour = infection risk, ✕ = infected)"),
         plotOutput("net", height = "460px"),
         card_footer(class = "text-muted small",
           HTML("Green → red = low → high risk. Edges = close-contact time. "),
           "Clusters are seating groups/pods.")),
    card(card_header("Per-student risk vs. the room average"),
         plotOutput("dist", height = "460px"),
         card_footer(class = "text-muted small",
           "The dashed line is the single number a well-mixed model reports."))
  )
)

# ---- server ----------------------------------------------------------------
server <- function(input, output) {
  seedval <- reactiveVal(20)
  observeEvent(input$reshuffle, seedval(seedval() + 1))

  model <- reactive({
    set.seed(seedval())
    ninf <- min(input$ninf, input$N - 1)
    P <- list(
      N = input$N, pod.size = input$podsize, n.infected = ninf,
      duration.h = input$dur, q = 10, p.breath = 0.5,
      V = input$N * 8, AER = input$aer, Q.near = 90 / input$near,
      within = c(max(2, input$within - 25), input$within + 25),
      between = if (input$cohort) c(0, 1) else c(0, 8)
    )
    net <- make_network(P)
    infected <- sample(P$N, P$n.infected)
    list(P = P, net = net, infected = infected,
         res = compute_risk(net, infected, P))
  })

  output$net  <- renderPlot(plot_network(model()$net, model()$res,
                                         model()$infected, model()$P))
  output$dist <- renderPlot(plot_distribution(model()$net, model()$res,
                                              model()$infected, model()$P))

  fmt <- function(x) sprintf("%.1f%%", 100 * x)
  output$v_far  <- renderText(fmt(model()$res$p.far))
  output$v_mean <- renderText({
    r <- model()$res$risk; fmt(mean(r, na.rm = TRUE)) })
  output$v_max  <- renderText({
    r <- model()$res$risk; fmt(max(r, na.rm = TRUE)) })
  output$v_over <- renderText({
    r <- model()$res$risk
    sprintf("%d of %d", sum(r > 0.10, na.rm = TRUE), sum(!is.na(r))) })
}

shinyApp(ui, server)
