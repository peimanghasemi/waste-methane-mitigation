qtri <- function(u, low, mode, high) {
  if (high <= low) return(rep(mode, length(u)))
  fc <- (mode - low) / (high - low)
  ifelse(u < fc,
         low + sqrt(u * (high - low) * (mode - low)),
         high - sqrt((1 - u) * (high - low) * (high - mode)))
}

#' Draw parameter sets for Monte Carlo analysis
#'
#' All parameters get triangular distributions over the ranges in the input
#' tables. Decay rates and DOC share one draw per waste type across climates
#' (a fast-decaying food fraction is fast everywhere), which also keeps the
#' number of inputs in the sensitivity analysis manageable. Activity data get
#' +-15% (waste generation) and +-20% (BOD per person).
#'
#' @param inputs list of input tables from [load_inputs()]
#' @param n number of draws
#' @param seed random seed
#' @return list with `params` (list of parameter lists) and `design`
#'   (data frame of the underlying draws, one column per uncertain input)
#' @export
sample_params <- function(inputs, n = 1000, seed = 1) {
  set.seed(seed)
  lf <- inputs$landfill
  ww <- inputs$wastewater
  m <- inputs$measures
  base <- default_params(inputs)
  row_of <- function(d, cond) d[cond, , drop = FALSE]
  tri_from <- function(row, u) qtri(u, row$low, row$value, row$high)

  design <- list()
  params <- vector("list", n)
  u <- function() stats::runif(n)

  u_doc <- sapply(WASTE_TYPES, function(w) u())
  u_k <- sapply(WASTE_TYPES, function(w) u())
  u_mcf <- sapply(SITE_TYPES, function(s) u())
  u_misc <- sapply(c("docf", "f", "ox_managed", "ox_unmanaged", "bo", "i_collected",
                     "msw_scale", "bod_scale"), function(x) u())
  ww_paths <- names(base$ww_mcf)
  u_ww <- sapply(ww_paths, function(p) u())
  u_eff <- sapply(MEASURES, function(x) u())

  climates <- rownames(base$k)
  for (j in seq_len(n)) {
    p <- base
    for (w in WASTE_TYPES) {
      p$doc[[w]] <- tri_from(row_of(lf, lf$parameter == "doc" & lf$waste_type == w), u_doc[j, w])
      for (cl in climates) {
        p$k[cl, w] <- tri_from(row_of(lf, lf$parameter == "k" & lf$waste_type == w & lf$climate == cl),
                               u_k[j, w])
      }
    }
    for (s in SITE_TYPES) {
      p$mcf[[s]] <- tri_from(row_of(lf, lf$parameter == "mcf" & lf$climate == s), u_mcf[j, s])
    }
    p$docf <- tri_from(row_of(lf, lf$parameter == "docf"), u_misc[j, "docf"])
    p$f <- tri_from(row_of(lf, lf$parameter == "f"), u_misc[j, "f"])
    p$ox_managed <- tri_from(row_of(lf, lf$parameter == "ox" & lf$climate == "managed"), u_misc[j, "ox_managed"])
    p$ox_unmanaged <- tri_from(row_of(lf, lf$parameter == "ox" & lf$climate == "unmanaged"), u_misc[j, "ox_unmanaged"])
    p$bo <- tri_from(row_of(ww, ww$parameter == "bo"), u_misc[j, "bo"])
    p$i_collected <- tri_from(row_of(ww, ww$parameter == "correction_collected"), u_misc[j, "i_collected"])
    for (pw in ww_paths) {
      p$ww_mcf[[pw]] <- tri_from(row_of(ww, ww$parameter == "mcf" & ww$pathway == pw), u_ww[j, pw])
    }
    for (ms in MEASURES) {
      r <- m[m$measure == ms, ]
      p$eff[[ms]] <- qtri(u_eff[j, ms], r$eff_low, r$efficiency, r$eff_high)
    }
    p$msw_scale <- qtri(u_misc[j, "msw_scale"], 0.85, 1, 1.15)
    p$bod_scale <- qtri(u_misc[j, "bod_scale"], 0.8, 1, 1.2)
    params[[j]] <- p
  }

  design <- data.frame(
    stats::setNames(as.data.frame(u_doc), paste0("doc_", WASTE_TYPES)),
    stats::setNames(as.data.frame(u_k), paste0("k_", WASTE_TYPES)),
    stats::setNames(as.data.frame(u_mcf), paste0("mcf_", SITE_TYPES)),
    as.data.frame(u_misc),
    stats::setNames(as.data.frame(u_ww), paste0("ww_mcf_", ww_paths)),
    stats::setNames(as.data.frame(u_eff), paste0("eff_", MEASURES)),
    check.names = FALSE
  )
  list(params = params, design = design)
}

#' Monte Carlo run of the baseline and one policy scenario
#'
#' @param inputs list of input tables from [load_inputs()]
#' @param draws output of [sample_params()]
#' @param policy policy to compare with the baseline
#' @param years_out years to keep in the output
#' @return data frame with one row per draw and year: baseline and policy
#'   emissions (kt CO2e) and the relative reduction
#' @export
monte_carlo <- function(inputs, draws, policy, years_out = c(2030, 2040, 2050)) {
  yrs <- 1960:max(years_out)
  res <- lapply(seq_along(draws$params), function(j) {
    p <- draws$params[[j]]
    b <- run_model(inputs, list(), p, yrs)
    s <- run_model(inputs, policy, p, yrs)
    b <- stats::aggregate(total_kt_co2e ~ year, data = b[b$year %in% years_out, ], FUN = sum)
    s <- stats::aggregate(total_kt_co2e ~ year, data = s[s$year %in% years_out, ], FUN = sum)
    data.frame(draw = j, year = b$year, baseline = b$total_kt_co2e, policy = s$total_kt_co2e)
  })
  out <- do.call(rbind, res)
  out$reduction <- 1 - out$policy / out$baseline
  out
}

#' Standardised rank regression coefficients
#'
#' Global sensitivity measure for monotonic models: regress the ranks of the
#' output on the ranks of all inputs. With R2 close to 1 the squared SRRC
#' roughly gives each input's share of the output variance.
#'
#' @param design data frame of inputs (one row per draw)
#' @param y output vector
#' @return data frame with input, srrc and the model R2, sorted by |srrc|
#' @export
srrc <- function(design, y) {
  X <- as.data.frame(lapply(design, rank))
  keep <- vapply(X, function(v) stats::sd(v) > 0, logical(1))
  X <- scale(X[, keep, drop = FALSE])
  ry <- as.numeric(scale(rank(y)))
  fit <- stats::lm(ry ~ X)
  co <- stats::coef(fit)[-1]
  names(co) <- colnames(X)
  out <- data.frame(input = names(co), srrc = unname(co), r2 = summary(fit)$r.squared)
  out[order(-abs(out$srrc)), ]
}
