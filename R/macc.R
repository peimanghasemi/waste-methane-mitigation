#' Marginal abatement cost curve for one target year
#'
#' For each region, measures are first ranked by their stand-alone cost per
#' tonne CO2e and then added one by one, so every step is evaluated with the
#' cheaper measures already in place. This matters for measures that act on
#' the same methane (gas capture, biocovers and organics diversion all reduce
#' the same landfill emissions). The region-measure steps are then sorted by
#' cost into a single curve.
#'
#' Costs and abatement are annual values in the target year. Organics
#' diversion only reaches its full effect after the landfill stock has
#' adjusted, so its cost per tonne falls the later the target year.
#'
#' @param inputs list of input tables from [load_inputs()]
#' @param year target year
#' @param params parameter list
#' @param start year from which all measures apply at full applicability
#' @export
mac_curve <- function(inputs, year = 2040, start = 2025, params = default_params(inputs)) {
  amax <- stats::setNames(inputs$measures$max_applicability, inputs$measures$measure)
  years <- 1960:year
  step_fun <- function(a) function(y) ifelse(y >= start, a, 0)

  steps <- list()
  for (i in seq_len(nrow(inputs$regions))) {
    reg <- inputs$regions[i, , drop = FALSE]
    one <- inputs
    one$regions <- reg
    eval_policy <- function(pol) {
      r <- run_model(one, pol, params, years)
      r[r$year == year, ]
    }
    base <- eval_policy(list())

    alone <- sapply(MEASURES, function(m) {
      r <- eval_policy(stats::setNames(list(step_fun(amax[[m]])), m))
      ab <- base$total_kt_co2e - r$total_kt_co2e
      if (ab <= 1e-9) return(NA_real_)
      r$cost_meur * 1e3 / ab                               # EUR per t CO2e
    })
    order_m <- names(sort(alone[!is.na(alone)]))

    pol <- list()
    prev <- base
    for (m in order_m) {
      pol[[m]] <- step_fun(amax[[m]])
      cur <- eval_policy(pol)
      ab <- prev$total_kt_co2e - cur$total_kt_co2e
      dc <- cur$cost_meur - prev$cost_meur
      if (ab > 1e-9) {
        steps[[length(steps) + 1]] <- data.frame(
          region = reg$region, measure = m, abatement_kt_co2e = ab,
          cost_meur = dc, eur_per_t_co2e = dc * 1e3 / ab,
          baseline_kt_co2e = base$total_kt_co2e
        )
      }
      prev <- cur
    }
  }
  out <- do.call(rbind, steps)
  out <- out[order(out$eur_per_t_co2e), ]
  out$cum_abatement_kt_co2e <- cumsum(out$abatement_kt_co2e)
  rownames(out) <- NULL
  out
}
