# Marginal abatement cost curve for 2040 and the effect of the target year on organics diversion
source("scripts/setup.R")

mac <- mac_curve(inputs, year = 2040, start = 2025)
write_res(mac, "results/macc_2040.csv")

mac$xmin <- mac$cum_abatement_kt_co2e - mac$abatement_kt_co2e
nice <- c(organics_diversion = "Organics diversion", lfg_energy = "LFG to energy",
          lfg_flaring = "LFG flaring", biocover = "Biocover",
          ww_aerobic_upgrade = "Aerobic plant upgrade", ww_lagoon_cover = "Covered lagoons",
          ww_septic_to_sewer = "Septic to sewer")
mac$measure_label <- nice[mac$measure]

p <- ggplot(mac) +
  geom_rect(aes(xmin = xmin / 1000, xmax = cum_abatement_kt_co2e / 1000,
                ymin = 0, ymax = eur_per_t_co2e, fill = measure_label),
            colour = "white", linewidth = 0.2) +
  geom_hline(yintercept = 0, colour = "grey30") +
  labs(x = expression("Abatement in 2040 (Mt CO"[2] * "e)"),
       y = expression("EUR per t CO"[2] * "e"), fill = NULL,
       title = "Marginal abatement cost curve, 2040",
       subtitle = "One bar per region and measure; measures added in order of cost") +
  guides(fill = guide_legend(nrow = 2))
save_fig(p, "macc_2040.png")

# organics diversion pays off slowly: cost per tonne in different target years
div <- do.call(rbind, lapply(c(2030, 2035, 2040, 2050), function(y) {
  m <- mac_curve(inputs, year = y, start = 2025)
  m <- m[m$measure == "organics_diversion", ]
  data.frame(year = y,
             abatement_kt_co2e = sum(m$abatement_kt_co2e),
             eur_per_t_co2e = sum(m$cost_meur) * 1e3 / sum(m$abatement_kt_co2e))
}))
write_res(div, "results/diversion_by_target_year.csv")
print(div, row.names = FALSE, digits = 4)

summary_tab <- aggregate(cbind(abatement_kt_co2e, cost_meur) ~ measure, data = mac, FUN = sum)
summary_tab$eur_per_t_co2e <- summary_tab$cost_meur * 1e3 / summary_tab$abatement_kt_co2e
summary_tab <- summary_tab[order(summary_tab$eur_per_t_co2e), ]
write_res(summary_tab, "results/macc_2040_by_measure.csv")
print(summary_tab, row.names = FALSE, digits = 4)
