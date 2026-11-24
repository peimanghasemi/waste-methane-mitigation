# Monte Carlo uncertainty and global sensitivity (standardised rank regression)
source("scripts/setup.R")

n <- as.integer(Sys.getenv("N_DRAWS", "1000"))
draws <- sample_params(inputs, n = n, seed = 42)
policy <- scenarios(inputs)$combined
mc <- monte_carlo(inputs, draws, policy, years_out = c(2030, 2040, 2050))
write_res(mc, "results/monte_carlo.csv")

central <- run_model(inputs, list(), default_params(inputs), 1960:2050)
central <- aggregate(total_kt_co2e ~ year, data = central[central$year %in% c(2030, 2040, 2050), ], FUN = sum)

q <- function(x) stats::quantile(x, c(0.05, 0.5, 0.95))
summ <- do.call(rbind, lapply(split(mc, mc$year), function(d) {
  data.frame(year = d$year[1],
             baseline_p05 = q(d$baseline)[1], baseline_p50 = q(d$baseline)[2], baseline_p95 = q(d$baseline)[3],
             policy_p05 = q(d$policy)[1], policy_p50 = q(d$policy)[2], policy_p95 = q(d$policy)[3],
             reduction_p05 = q(d$reduction)[1], reduction_p50 = q(d$reduction)[2], reduction_p95 = q(d$reduction)[3])
}))
summ$baseline_central <- central$total_kt_co2e[match(summ$year, central$year)]
rownames(summ) <- NULL
write_res(summ, "results/uncertainty_summary.csv")
print(summ, digits = 3)

d40 <- mc[mc$year == 2040, ]
s_base <- srrc(draws$design, d40$baseline)
s_red <- srrc(draws$design, d40$reduction)
write_res(s_base, "results/sensitivity_baseline_2040.csv")
write_res(s_red, "results/sensitivity_reduction_2040.csv")
cat("\nBaseline 2040, R2 =", round(s_base$r2[1], 3), "\n"); print(head(s_base[, 1:2], 8), row.names = FALSE)
cat("\nReduction 2040, R2 =", round(s_red$r2[1], 3), "\n"); print(head(s_red[, 1:2], 8), row.names = FALSE)

long <- rbind(data.frame(case = "Baseline", value = d40$baseline / 1000),
              data.frame(case = "All measures", value = d40$policy / 1000))
long$case <- factor(long$case, levels = c("Baseline", "All measures"))
p1 <- ggplot(long, aes(value, fill = case)) +
  geom_histogram(bins = 50, alpha = 0.75, position = "identity") +
  labs(x = expression("Mt CO"[2] * "e in 2040"), y = "Draws", fill = NULL,
       title = sprintf("Uncertainty in 2040 emissions (%d Monte Carlo draws)", n))
save_fig(p1, "uncertainty_2040.png")

pretty_input <- function(x) {
  lab <- c(f = "CH4 share in landfill gas (F)", docf = "Decomposable share of DOC (DOCf)",
           bo = "Max. CH4 from BOD (Bo)", msw_scale = "Waste generation per person",
           bod_scale = "BOD per person", i_collected = "Industrial co-discharge factor (I)",
           ox_managed = "Oxidation, managed sites", ox_unmanaged = "Oxidation, unmanaged sites")
  out <- ifelse(x %in% names(lab), lab[x], x)
  out <- sub("^doc_(.*)$", "DOC, \\1", out)
  out <- sub("^k_(.*)$", "Decay rate k, \\1", out)
  out <- sub("^mcf_(.*)$", "MCF, \\1", out)
  out <- sub("^ww_mcf_(.*)$", "Wastewater MCF, \\1", out)
  out <- sub("^eff_(.*)$", "Efficiency, \\1", out)
  out <- gsub("_", " ", out)
  out <- sub("lfg", "landfill gas,", out)
  sub("ww ", "", out)
}
top <- rbind(cbind(output = "Baseline emissions 2040", head(s_base, 10)),
             cbind(output = "Relative reduction 2040", head(s_red, 10)))
top$label <- pretty_input(top$input)
# order bars within each panel
top$key <- paste(top$output, top$label, sep = "::")
top$key <- factor(top$key, levels = top$key[order(top$output, abs(top$srrc))])
p2 <- ggplot(top, aes(srrc, key, fill = srrc > 0)) +
  geom_col(show.legend = FALSE) +
  scale_y_discrete(labels = function(x) sub("^.*::", "", x)) +
  facet_wrap(~output, scales = "free_y") +
  geom_vline(xintercept = 0, colour = "grey30") +
  labs(x = "Standardised rank regression coefficient", y = NULL,
       title = "What drives the uncertainty")
save_fig(p2, "sensitivity_2040.png", w = 9, h = 4.5)
