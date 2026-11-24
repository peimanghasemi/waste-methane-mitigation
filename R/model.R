#' Linear ramp from 0 in `start` to `target` in `full`
#' @param target applicability reached in `full`
#' @param start first year of the measure
#' @param full year of full applicability
#' @return function of years
#' @export
ramp <- function(target, start = 2025, full = 2035) {
  force(target); force(start); force(full)
  function(years) target * pmin(1, pmax(0, (years - start) / max(1, full - start)))
}

#' Scenario definitions used in the analysis
#'
#' Each scenario is a named list of applicability paths. Targets are the
#' maximum applicability in measures.csv, so the scenarios show the full
#' technical potential of each group of measures.
#' @param inputs list of input tables from [load_inputs()]
#' @param start,full ramp-up period, see [ramp()]
#' @export
scenarios <- function(inputs, start = 2025, full = 2035) {
  amax <- stats::setNames(inputs$measures$max_applicability, inputs$measures$measure)
  r <- function(m) ramp(amax[[m]], start, full)
  circular <- list(organics_diversion = r("organics_diversion"))
  capture <- list(lfg_energy = r("lfg_energy"), lfg_flaring = r("lfg_flaring"))
  cover <- list(biocover = r("biocover"))
  water <- list(ww_aerobic_upgrade = r("ww_aerobic_upgrade"),
                ww_lagoon_cover = r("ww_lagoon_cover"),
                ww_septic_to_sewer = r("ww_septic_to_sewer"))
  list(
    baseline = list(),
    circular = circular,
    gas_capture = capture,
    biocover = cover,
    wastewater = water,
    combined = c(circular, capture, cover, water)
  )
}

#' Run the model for all regions under one policy
#'
#' @param inputs list of input tables from [load_inputs()]
#' @param policy named list of applicability paths, e.g. from [scenarios()]
#' @param params parameter list, see [default_params()]
#' @param years years to model
#' @return data frame with one row per region and year: landfill and
#'   wastewater CH4 (t), CO2-equivalent (kt) and the annual cost of the
#'   measures (million EUR)
#' @export
run_model <- function(inputs, policy = list(), params = default_params(inputs),
                      years = 1960:2050) {
  rows <- lapply(seq_len(nrow(inputs$regions)), function(i) {
    act <- region_activity(inputs$regions[i, ], years, params)
    lf <- landfill_emissions(act, params, policy)
    ww <- wastewater_emissions(act, params, policy)
    lf_y <- stats::aggregate(lf[, c("generated", "recovered_energy", "recovered_flared",
                                    "emitted", "to_biocover")],
                             by = list(year = lf$year), FUN = sum)
    cost <- params$cost
    measure_cost <- cbind(
      organics_diversion = attr(lf, "diverted_t") * cost[["organics_diversion"]],
      lfg_energy = lf_y$recovered_energy * cost[["lfg_energy"]],
      lfg_flaring = lf_y$recovered_flared * cost[["lfg_flaring"]],
      biocover = lf_y$to_biocover * cost[["biocover"]],
      ww_aerobic_upgrade = ww$bod_upgraded_t * cost[["ww_aerobic_upgrade"]],
      ww_lagoon_cover = ww$captured * cost[["ww_lagoon_cover"]],
      ww_septic_to_sewer = ww$people_connected * cost[["ww_septic_to_sewer"]]
    ) / 1e6
    data.frame(
      region = act$region, year = years,
      landfill_ch4 = lf_y$emitted, wastewater_ch4 = ww$emitted,
      total_ch4 = lf_y$emitted + ww$emitted,
      total_kt_co2e = (lf_y$emitted + ww$emitted) * GWP_CH4 / 1000,
      cost_meur = rowSums(measure_cost),
      measure_cost
    )
  })
  do.call(rbind, rows)
}

#' Run every scenario and stack the results
#' @param inputs list of input tables from [load_inputs()]
#' @param params parameter list
#' @param years years to model
#' @param ... passed to [scenarios()]
#' @export
run_scenarios <- function(inputs, params = default_params(inputs), years = 1960:2050, ...) {
  sc <- scenarios(inputs, ...)
  res <- lapply(names(sc), function(n) cbind(scenario = n, run_model(inputs, sc[[n]], params, years)))
  do.call(rbind, res)
}

#' Totals over regions by scenario and year
#' @param res output of [run_scenarios()]
#' @param from first year to keep
#' @export
summarise_scenarios <- function(res, from = 2000) {
  res <- res[res$year >= from, ]
  out <- stats::aggregate(res[, c("landfill_ch4", "wastewater_ch4", "total_ch4",
                                  "total_kt_co2e", "cost_meur")],
                          by = list(scenario = res$scenario, year = res$year), FUN = sum)
  out[order(out$scenario, out$year), ]
}
