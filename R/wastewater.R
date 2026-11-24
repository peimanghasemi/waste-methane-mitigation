#' Domestic wastewater methane for one region
#'
#' IPCC 2006 Vol. 5 Ch. 6, Eq. 6.1 to 6.3, without sludge removal or
#' recovery in the baseline. Measures change the pathway shares (aerobic
#' upgrade, septic to sewer) or capture part of the lagoon methane.
#'
#' @inheritParams landfill_emissions
#' @export
wastewater_emissions <- function(act, params, policy = list()) {
  years <- act$years
  a_up <- ramp_value(policy$ww_aerobic_upgrade, years)
  a_lag <- ramp_value(policy$ww_lagoon_cover, years)
  a_sep <- ramp_value(policy$ww_septic_to_sewer, years)

  sh <- matrix(act$ww_share, nrow = length(years), ncol = length(WW_PATHWAYS),
               byrow = TRUE, dimnames = list(NULL, WW_PATHWAYS))
  moved_sep <- sh[, "septic"] * a_sep
  sh[, "septic"] <- sh[, "septic"] - moved_sep
  sh[, "aerobic_good"] <- sh[, "aerobic_good"] + moved_sep
  moved_up <- sh[, "aerobic_poor"] * a_up * params$eff[["ww_aerobic_upgrade"]]
  sh[, "aerobic_poor"] <- sh[, "aerobic_poor"] - moved_up
  sh[, "aerobic_good"] <- sh[, "aerobic_good"] + moved_up

  latrine <- if (grepl("wet", act$climate)) "latrine_wet" else "latrine_dry"
  mcf <- params$ww_mcf[c(WW_PATHWAYS[WW_PATHWAYS != "latrine"], latrine)]
  names(mcf)[names(mcf) == latrine] <- "latrine"
  mcf <- mcf[WW_PATHWAYS]
  i_fac <- ifelse(WW_PATHWAYS %in% WW_COLLECTED, params$i_collected, 1)

  tow <- act$bod_t * sh %*% diag(i_fac)                    # t BOD per pathway
  colnames(tow) <- WW_PATHWAYS
  ch4 <- sweep(tow, 2, params$bo * mcf, "*")
  captured <- ch4[, "lagoon_deep"] * a_lag * params$eff[["ww_lagoon_cover"]]
  ch4[, "lagoon_deep"] <- ch4[, "lagoon_deep"] - captured

  data.frame(
    year = years,
    emitted = rowSums(ch4),
    captured = captured,
    bod_upgraded_t = act$bod_t * moved_up * params$i_collected,
    people_connected = act$population * moved_sep
  )
}
