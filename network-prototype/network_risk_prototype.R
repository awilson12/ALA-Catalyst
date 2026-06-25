# =============================================================================
# Stage-1 prototype: adding a sensor/contact-network NEAR-FIELD layer on top of
# a well-mixed FAR-FIELD room risk.
#
# Purpose: illustrate how per-student risk SPREADS OUT once you account for
# who-contacts-whom, versus the single room-average the current tool reports.
#
# This is a CONCEPTUAL prototype, not a calibrated model. It uses a simplified
# Wells-Riley-style quanta formulation as a stand-in for the full QMRA compartment
# model, because it cleanly separates the two exposure routes:
#
#   total exposure_i (lambda_i) = far-field (shared by all)  +  near-field (from contacts)
#   P_infection_i = 1 - exp(-lambda_i)
#
# Far-field  = everyone breathes the same diluted room air  -> identical for all.
# Near-field = extra dose from close proximity to infected peers, weighted by the
#              contact network (sensor-derived "close-contact minutes").
#
# IMPORTANT: near-field is modeled as the INCREMENT above the room average
# (a much smaller effective dilution volume Q.near), so the emitter is not
# double-counted between the two layers.
# =============================================================================

set.seed(20)   # reproducible illustration; swap/remove to explore variability

# ---- Parameters (all illustrative & documented; tune freely) ----------------
P <- list(
  N            = 25,     # students
  pod.size     = 5,      # seating groups / pods (network structure)
  duration.h   = 3,      # class length (hours) — matches the app default
  n.infected   = 2,      # index cases in the room
  q            = 10,     # quanta emission rate per infected (per hour) [illustrative]
  p.breath     = 0.5,    # student breathing rate (m^3/h), light activity
  V            = 180,    # room volume (m^3) ~ 25-student classroom
  AER          = 3,      # air exchanges per hour -> far-field dilution
  Q.near       = 30,     # near-zone effective clearance (m^3/h); SMALL -> close
                         #   contact = high local concentration. Key NF knob.
  within.pod.min = c(20, 80),  # cumulative close-contact minutes within a pod (range)
  between.pod.min= c(0, 8)     # cumulative close-contact minutes across pods (range)
)

# ---- Build a structured classroom contact network ---------------------------
# Sensor data would supply this matrix directly (close-contact minutes per pair).
# Here we synthesize realistic structure: dense within-pod, sparse between-pod.
make_network <- function(P) {
  N   <- P$N
  pod <- rep(seq_len(ceiling(N / P$pod.size)), each = P$pod.size)[1:N]
  C   <- matrix(0, N, N)
  for (i in 1:(N - 1)) for (j in (i + 1):N) {
    rng <- if (pod[i] == pod[j]) P$within.pod.min else P$between.pod.min
    m   <- runif(1, rng[1], rng[2])
    C[i, j] <- C[j, i] <- m            # symmetric, minutes of close contact
  }
  list(contact = C, pod = pod)
}

# ---- Risk model: far-field (shared) + near-field (network) ------------------
compute_risk <- function(net, infected, P) {
  C <- net$contact; N <- P$N
  # Far-field: identical for everyone (Wells-Riley shared-air term)
  Q.far     <- P$V * P$AER
  lambda.far <- P$n.infected * P$q * P$p.breath * P$duration.h / Q.far
  # Near-field: extra dose from close contact with INFECTED peers (network-driven)
  contact.h.with.inf <- rowSums(C[, infected, drop = FALSE]) / 60   # hours
  lambda.near <- contact.h.with.inf * P$q * P$p.breath / P$Q.near
  lambda <- lambda.far + lambda.near
  risk   <- 1 - exp(-lambda)
  risk[infected] <- NA                                    # already infected
  list(risk = risk, lambda.far = lambda.far,
       p.far = 1 - exp(-lambda.far),
       contact.h.with.inf = contact.h.with.inf)
}

# ---- One illustrative scenario ----------------------------------------------
net <- make_network(P)
infected <- c(2, 13)                       # two index cases (pod 1 and pod 3)
res <- compute_risk(net, infected, P)
sus <- setdiff(seq_len(P$N), infected)     # susceptibles
risk.sus <- res$risk[sus]

cat("================ STAGE-1 NETWORK PROTOTYPE ================\n")
cat(sprintf("Classroom: %d students in %d pods, %d infected, %.0f-h class\n\n",
            P$N, length(unique(net$pod)), P$n.infected, P$duration.h))
cat(sprintf("CURRENT (well-mixed) model -> ONE number for everyone: %.1f%%\n",
            100 * res$p.far))
cat("NETWORK model -> a DISTRIBUTION across students:\n")
cat(sprintf("   mean   %.1f%%   median %.1f%%   min %.1f%%   MAX %.1f%%\n",
            100*mean(risk.sus), 100*median(risk.sus),
            100*min(risk.sus), 100*max(risk.sus)))
cat(sprintf("   students above 10%%: %d of %d   (the average hides them)\n\n",
            sum(risk.sus > 0.10), length(sus)))
ord <- order(risk.sus, decreasing = TRUE)
cat("Highest-risk students (pod-mates of an infected child):\n")
for (k in ord[1:5]) {
  s <- sus[k]
  cat(sprintf("   student %2d (pod %d): %.1f%%   close-contact w/ infected = %.0f min\n",
              s, net$pod[s], 100*risk.sus[k], 60*res$contact.h.with.inf[s]))
}

# ---- Plot: per-student risk vs the well-mixed average -----------------------
png_path <- file.path(getwd(), "network_risk_example.png")
png(png_path, width = 1000, height = 600, res = 120)
cols <- c("#0a6ebd","#1a9e6f","#e8a33d","#d6455d","#7a5b9c")[net$pod[sus]]
o <- order(risk.sus)
bp <- barplot(100*risk.sus[o], col = cols[o], border = NA,
              ylim = c(0, max(100*risk.sus, 12) * 1.1),
              ylab = "Infection risk (%)", xlab = "Students (sorted)",
              main = "Per-student risk: network model vs. well-mixed average")
abline(h = 100*res$p.far, lty = 2, lwd = 2, col = "#333333")
text(x = max(bp)*0.5, y = 100*res$p.far, pos = 3,
     labels = sprintf("well-mixed average = %.1f%% (what the tool reports now)",
                      100*res$p.far), col = "#333333", cex = 0.9)
legend("topleft", legend = paste("pod", sort(unique(net$pod[sus]))),
       fill = c("#0a6ebd","#1a9e6f","#e8a33d","#d6455d","#7a5b9c"),
       border = NA, bty = "n", cex = 0.85)
dev.off()
cat(sprintf("\nPlot saved: %s\n", png_path))

# ---- Quick Monte Carlo: how big is the hidden tail? -------------------------
# Average over random index-case placements -> the room mean barely moves, but
# the per-child tail (max) is consistently far above it.
set.seed(7)
nrep <- 2000
maxv <- meanv <- numeric(nrep)
for (r in seq_len(nrep)) {
  net.r <- make_network(P)
  inf.r <- sample(P$N, P$n.infected)
  rr <- compute_risk(net.r, inf.r, P)$risk
  rr <- rr[!is.na(rr)]
  meanv[r] <- mean(rr); maxv[r] <- max(rr)
}
cat("\n---- Monte Carlo over random infected placements (2000 classrooms) ----\n")
cat(sprintf("Typical room-average risk:        %.1f%%\n", 100*mean(meanv)))
cat(sprintf("Typical HIGHEST-risk student:     %.1f%%  (95th pct of max: %.1f%%)\n",
            100*mean(maxv), 100*quantile(maxv, 0.95)))
cat("Takeaway: the network layer leaves the room AVERAGE ~unchanged but reveals\n")
cat("a heavy upper tail of high-risk children that a single average cannot show.\n")
