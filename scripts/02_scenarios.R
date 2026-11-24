# Baseline and mitigation scenarios, 2000-2050
source("scripts/setup.R")

res <- run_scenarios(inputs)
summ <- summarise_scenarios(res, from = 2000)
write_res(summ, "results/scenarios.csv")

by_region <- res[res$year %in% c(2020, 2030, 2040, 2050), c("scenario", "region", "year",
                                                            "landfill_ch4", "wastewater_ch4",
                                                            "total_kt_co2e", "cost_meur")]
write_res(by_region, "results/scenarios_by_region.csv")

labels <- c(baseline = "Baseline", circular = "Organics diversion", gas_capture = "Landfill gas capture",
            biocover = "Biocovers", wastewater = "Wastewater measures", combined = "All measures")
summ$scenario_label <- factor(labels[summ$scenario], levels = labels)

p1 <- ggplot(summ, aes(year, total_kt_co2e / 1000, colour = scenario_label)) +
  geom_vline(xintercept = 2025, linetype = 3, colour = "grey50") +
  geom_line(linewidth = 0.9) +
  labs(x = NULL, y = expression("Mt CO"[2] * "e per year"), colour = NULL,
       title = "Methane from solid waste and domestic wastewater, four example regions",
       subtitle = "Measures start in 2025 and reach full applicability in 2035") +
  guides(colour = guide_legend(nrow = 2))
save_fig(p1, "scenarios.png")

# how fast each landfill measure takes effect: reduction relative to the baseline
base <- summ[summ$scenario == "baseline", c("year", "landfill_ch4")]
lag <- merge(summ[summ$scenario %in% c("circular", "gas_capture", "biocover"),
                  c("scenario_label", "year", "landfill_ch4")],
             base, by = "year", suffixes = c("", "_base"))
lag$reduction <- 1 - lag$landfill_ch4 / lag$landfill_ch4_base
lag <- lag[lag$year >= 2020, ]
p2 <- ggplot(lag, aes(year, reduction, colour = scenario_label)) +
  annotate("rect", xmin = 2025, xmax = 2035, ymin = -Inf, ymax = Inf, alpha = 0.08) +
  geom_line(linewidth = 0.9) +
  scale_y_continuous(labels = function(x) paste0(round(100 * x), "%")) +
  labs(x = NULL, y = "Reduction of landfill CH4 vs baseline", colour = NULL,
       title = "Capture and cover act at once, diversion works through the landfill stock",
       subtitle = "Shaded: ramp-up of measures (2025-2035)")
save_fig(p2, "landfill_response.png")

pick <- summ[summ$year %in% c(2030, 2040, 2050), c("scenario", "year", "total_kt_co2e", "cost_meur")]
b <- pick[pick$scenario == "baseline", c("year", "total_kt_co2e")]
pick <- merge(pick, b, by = "year", suffixes = c("", "_base"))
pick$reduction_pct <- round(100 * (1 - pick$total_kt_co2e / pick$total_kt_co2e_base), 1)
pick <- pick[order(pick$year, -pick$reduction_pct), c("year", "scenario", "total_kt_co2e", "reduction_pct", "cost_meur")]
print(pick, row.names = FALSE, digits = 4)
write_res(pick, "results/scenario_reductions.csv")
