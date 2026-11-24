#' Population, waste and wastewater activity for one region
#'
#' The 2020 values in regions.csv are projected forward and backcast with
#' constant growth rates. Landfill shares and composition are kept at their
#' 2020 values over the whole period, which is a simplification for the
#' historical deposits that drive current emissions.
#'
#' @param region one row of the regions table
#' @param years years to cover; should start several decades before the
#'   first year of interest so the landfill stock is built up
#' @param params parameter list (uses `msw_scale` and `bod_scale`)
#' @param base_year year of the values in the regions table
#' @export
region_activity <- function(region, years = 1960:2050, params = NULL, base_year = 2020) {
  msw_scale <- if (is.null(params)) 1 else params$msw_scale
  bod_scale <- if (is.null(params)) 1 else params$bod_scale
  dt <- years - base_year
  pop <- region$pop_2020_million * 1e6 * (1 + region$pop_growth)^dt
  msw_cap <- region$msw_kg_cap_day * msw_scale * (1 + region$msw_growth)^dt
  msw <- pop * msw_cap * 365 / 1000                       # t/yr
  comp <- unlist(region[WASTE_TYPES])
  site_share <- as.list(unlist(region[paste0("lf_", SITE_TYPES)]))
  names(site_share) <- SITE_TYPES
  landfilled <- outer(msw, comp)                          # t/yr by waste type, before site split
  colnames(landfilled) <- WASTE_TYPES

  ww_share <- unlist(region[paste0("ww_", WW_PATHWAYS)])
  names(ww_share) <- WW_PATHWAYS
  list(
    region = region$region,
    climate = region$climate,
    years = years,
    population = pop,
    msw = msw,
    landfilled = landfilled,
    site_share = site_share,
    ww_share = ww_share,
    bod_t = pop * region$bod_g_cap_day * bod_scale * 365 / 1e6
  )
}
