WASTE_TYPES <- c("food", "garden", "paper", "wood", "textiles", "nappies")
SITE_TYPES <- c("managed_anaerobic", "managed_semi_aerobic", "unmanaged_deep", "unmanaged_shallow")
WW_PATHWAYS <- c("aerobic_good", "aerobic_poor", "anaerobic_reactor", "lagoon_deep",
                 "septic", "latrine", "discharge")
WW_COLLECTED <- c("aerobic_good", "aerobic_poor", "anaerobic_reactor", "lagoon_deep")
MEASURES <- c("organics_diversion", "lfg_energy", "lfg_flaring", "biocover",
              "ww_aerobic_upgrade", "ww_lagoon_cover", "ww_septic_to_sewer")

#' Global warming potential of biogenic methane
#'
#' AR6 value for non-fossil methane, 100-year horizon.
#' @export
GWP_CH4 <- 27

#' Path to a file shipped in inst/extdata
#' @param file file name
#' @export
extdata_path <- function(file) {
  p <- system.file("extdata", file, package = "wastech4")
  if (p == "") p <- file.path("inst", "extdata", file)  # when sourced from the repo
  p
}

#' Read all input tables
#'
#' @param dir folder with the four csv files; defaults to the shipped data
#' @return list with landfill, wastewater, regions and measures data frames
#' @export
load_inputs <- function(dir = NULL) {
  read <- function(f) {
    path <- if (is.null(dir)) extdata_path(f) else file.path(dir, f)
    utils::read.csv(path, stringsAsFactors = FALSE)
  }
  list(
    landfill = read("landfill_parameters.csv"),
    wastewater = read("wastewater_parameters.csv"),
    regions = read("regions.csv"),
    measures = read("measures.csv")
  )
}

#' Check the input tables for gaps and inconsistencies
#'
#' Returns a data frame with one row per problem. An empty result means the
#' tables passed every check.
#' @param inputs list of input tables from [load_inputs()]
#' @export
validate_inputs <- function(inputs) {
  problems <- data.frame(table = character(), row = integer(), issue = character(),
                         stringsAsFactors = FALSE)
  add <- function(tab, row, issue) {
    problems[nrow(problems) + 1, ] <<- list(tab, as.integer(row), issue)
  }

  for (tab in c("landfill", "wastewater")) {
    d <- inputs[[tab]]
    need <- c("parameter", "value", "low", "high", "unit", "source")
    miss <- setdiff(need, names(d))
    if (length(miss)) {
      add(tab, NA, paste("missing columns:", paste(miss, collapse = ", ")))
      next
    }
    for (i in seq_len(nrow(d))) {
      if (any(is.na(d[i, c("value", "low", "high")]))) add(tab, i, "missing value or range")
      else if (d$low[i] > d$value[i] || d$value[i] > d$high[i]) add(tab, i, "value outside [low, high]")
      if (any(d[i, c("value", "low", "high")] < 0, na.rm = TRUE)) add(tab, i, "negative value")
      if (!nzchar(trimws(d$source[i]))) add(tab, i, "no source")
    }
  }

  lf <- inputs$landfill
  for (cl in unique(inputs$regions$climate)) {
    got <- lf$waste_type[lf$parameter == "k" & lf$climate == cl]
    if (!all(WASTE_TYPES %in% got)) add("landfill", NA, paste("decay rates incomplete for climate", cl))
  }

  r <- inputs$regions
  shares <- list(
    composition = c(WASTE_TYPES, "inert"),
    landfill = paste0("lf_", SITE_TYPES),
    wastewater = paste0("ww_", WW_PATHWAYS)
  )
  for (i in seq_len(nrow(r))) {
    s <- sum(unlist(r[i, shares$composition]))
    if (abs(s - 1) > 1e-6) add("regions", i, sprintf("composition sums to %.3f", s))
    s <- sum(unlist(r[i, shares$wastewater]))
    if (abs(s - 1) > 1e-6) add("regions", i, sprintf("wastewater shares sum to %.3f", s))
    s <- sum(unlist(r[i, shares$landfill]))
    if (s > 1 + 1e-6) add("regions", i, sprintf("landfilled share is %.3f (> 1)", s))
    if (any(unlist(r[i, unlist(shares)]) < 0)) add("regions", i, "negative share")
  }

  m <- inputs$measures
  if (!setequal(m$measure, MEASURES)) add("measures", NA, "measure list does not match the model")
  for (i in seq_len(nrow(m))) {
    if (m$eff_low[i] > m$efficiency[i] || m$efficiency[i] > m$eff_high[i]) add("measures", i, "efficiency outside range")
    if (m$cost_low[i] > m$cost[i] || m$cost[i] > m$cost_high[i]) add("measures", i, "cost outside range")
    if (m$max_applicability[i] < 0 || m$max_applicability[i] > 1) add("measures", i, "applicability not in [0, 1]")
  }
  problems
}

#' Central parameter values in the structure the model uses
#' @param inputs list of input tables from [load_inputs()]
#' @export
default_params <- function(inputs) {
  lf <- inputs$landfill
  ww <- inputs$wastewater
  get <- function(par, wt = "all", cl = "all", d = lf) {
    d$value[d$parameter == par & d$waste_type == wt & d$climate == cl]
  }
  climates <- unique(lf$climate[lf$parameter == "k"])
  k <- sapply(WASTE_TYPES, function(w) sapply(climates, function(cl) get("k", w, cl)))
  if (length(climates) == 1) k <- matrix(k, nrow = 1, dimnames = list(climates, WASTE_TYPES))

  ww_mcf <- stats::setNames(ww$value[ww$parameter == "mcf"], ww$pathway[ww$parameter == "mcf"])
  m <- inputs$measures

  list(
    doc = sapply(WASTE_TYPES, function(w) get("doc", w)),
    k = k,
    mcf = sapply(SITE_TYPES, function(s) get("mcf", "all", s)),
    docf = get("docf"),
    f = get("f"),
    ox_managed = get("ox", "all", "managed"),
    ox_unmanaged = get("ox", "all", "unmanaged"),
    bo = ww$value[ww$parameter == "bo"],
    i_collected = ww$value[ww$parameter == "correction_collected"],
    ww_mcf = ww_mcf,
    eff = stats::setNames(m$efficiency, m$measure),
    cost = stats::setNames(m$cost, m$measure),
    msw_scale = 1,
    bod_scale = 1
  )
}
