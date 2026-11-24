# Input checks and checks of the first order decay implementation
source("scripts/setup.R")

problems <- validate_inputs(inputs)
if (nrow(problems)) {
  print(problems)
  stop("input tables have problems")
}

p <- default_params(inputs)
lines <- c("# Validation", "", "Input tables: no problems found.", "")

# 1. mass balance: a single deposit eventually releases W * DOC * DOCf * MCF * F * 16/12
w <- 1000
dep <- c(w, rep(0, 399))
total <- sum(fod_ch4(dep, doc = 0.15, k = 0.185, mcf = 1))
expected <- w * 0.15 * 0.5 * 1 * 0.5 * 16 / 12
lines <- c(lines, "## Mass balance of a single deposit (1000 t food waste, temperate wet)", "",
           sprintf("- CH4 released over 400 years: %.3f t", total),
           sprintf("- Expected from DOC, DOCf, MCF and F: %.3f t", expected),
           sprintf("- Difference: %.2e t", total - expected), "")

# 2. half-life: half of the decomposable carbon is gone after ln(2)/k years after the start of decay
k <- 0.06
gen <- fod_ch4(dep, doc = 0.40, k = k, mcf = 1)
t_half <- log(2) / k
share <- sum(gen[1:(1 + round(t_half))]) / sum(gen)
lines <- c(lines, "## Half-life (paper, k = 0.06/yr)", "",
           sprintf("- ln(2)/k = %.1f years; share released in the first %d years of decay: %.3f",
                   t_half, round(t_half), share), "")

# 3. hand calculation of the first years for a constant deposit
dep <- rep(100, 5)
gen <- fod_ch4(dep, doc = 0.2, k = 0.1, mcf = 0.8)
d <- 100 * 0.2 * 0.5 * 0.8
stock <- 0
hand <- numeric(5)
for (t in 1:5) {
  hand[t] <- stock * (1 - exp(-0.1)) * 0.5 * 16 / 12
  stock <- stock * exp(-0.1) + d
}
lines <- c(lines, "## Constant deposit, first five years (100 t/yr garden waste, MCF 0.8)", "",
           "| year | model (t CH4) | hand calculation (t CH4) |", "|---|---|---|",
           sprintf("| %d | %.4f | %.4f |", 1:5, gen, hand), "")

# 4. wastewater: worked example
act <- list(years = 2020, climate = "temperate_wet", population = 1e6,
            bod_t = 1e6 * 60 * 365 / 1e6,
            ww_share = c(aerobic_good = 0, aerobic_poor = 0, anaerobic_reactor = 0,
                         lagoon_deep = 1, septic = 0, latrine = 0, discharge = 0))
ww <- wastewater_emissions(act, p)
lines <- c(lines, "## Wastewater worked example", "",
           "1 million people, 60 g BOD per person and day, all to deep anaerobic lagoons:",
           sprintf("21,900 t BOD x 1.25 x 0.6 x 0.8 = %.0f t CH4; model: %.0f t CH4", 21900 * 1.25 * 0.48, ww$emitted), "")

writeLines(lines, "results/validation.md")
cat(lines, sep = "\n")
