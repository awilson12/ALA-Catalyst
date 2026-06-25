# Contact-Network Risk Explorer

A **separate version of the school risk calculator** that adds person-to-person
(contact-network) transmission on top of the environmental, well-mixed risk.

This is an exploratory **Stage-1 prototype** (illustrative, not calibrated): it
shows how seating arrangement and close contact with infected classmates turn a
single room-average risk into a per-student distribution.

## Run it
```r
shiny::runApp("network-app")
```
Requires: `shiny`, `bslib`, `visNetwork` (all on CRAN).

## What it does
- **Draggable seating chart** — desks are nodes; close-contact time decays with
  physical distance, so rearranging the room changes who is exposed.
- **Live risk** — dragging a desk recomputes each student's risk (node colour),
  the value boxes, and the risk distribution.
- **Starting arrangements** — rows, clusters (pods), or spaced (distanced).
- **Hover** a desk for infected vs. susceptible status and risk.

## Model (summary)
```
contact_min(i,j) = Tmax * exp( -(distance_m / scale)^2 )
lambda_i = far-field (shared)  +  sum_{infected j} contact_h(i,j) * q * p / Q.near
risk_i   = 1 - exp(-lambda_i)
```
A Wells-Riley surrogate is used for the far-field so the near-field (contact)
term separates cleanly. See `../network-prototype/network_risk_prototype.R` for
the non-interactive version and a Monte-Carlo of the high-risk tail.
