inputs <- load_inputs()
p <- default_params(inputs)

test_that("shipped inputs pass validation", {
  expect_equal(nrow(validate_inputs(inputs)), 0)
})

test_that("validation catches broken tables", {
  bad <- inputs
  bad$regions$food[1] <- 0.9
  bad$landfill$value[1] <- 5
  issues <- validate_inputs(bad)$issue
  expect_true(any(grepl("composition", issues)))
  expect_true(any(grepl("outside", issues)))
})

test_that("first order decay conserves carbon", {
  dep <- c(1000, rep(0, 500))
  total <- sum(fod_ch4(dep, doc = 0.4, k = 0.04, mcf = 0.8, docf = 0.5, f = 0.5))
  expect_equal(total, 1000 * 0.4 * 0.5 * 0.8 * 0.5 * 16 / 12, tolerance = 1e-6)
})

test_that("decay starts the year after deposition and follows exp(-k)", {
  gen <- fod_ch4(c(100, 0, 0, 0), doc = 0.2, k = 0.1, mcf = 1)
  expect_equal(gen[1], 0)
  expect_equal(gen[3] / gen[2], exp(-0.1), tolerance = 1e-12)
})

test_that("faster decay releases methane earlier", {
  dep <- c(100, rep(0, 30))
  slow <- cumsum(fod_ch4(dep, 0.15, 0.06, 1))
  fast <- cumsum(fod_ch4(dep, 0.15, 0.40, 1))
  expect_true(all(fast[2:10] > slow[2:10]))
})

test_that("wastewater matches the IPCC worked example", {
  act <- list(years = 2020, climate = "tropical_dry", population = 1e6, bod_t = 21900,
              ww_share = c(aerobic_good = 0, aerobic_poor = 0, anaerobic_reactor = 0,
                           lagoon_deep = 0, septic = 1, latrine = 0, discharge = 0))
  expect_equal(wastewater_emissions(act, p)$emitted, 21900 * 0.6 * 0.5)
})

test_that("measures never increase emissions and costs are zero in the baseline", {
  sc <- scenarios(inputs)
  base <- run_model(inputs, sc$baseline, p, 1960:2050)
  expect_true(all(base$cost_meur == 0))
  for (n in setdiff(names(sc), "baseline")) {
    r <- run_model(inputs, sc[[n]], p, 1960:2050)
    expect_true(all(r$total_ch4 <= base$total_ch4 + 1e-6), info = n)
    expect_true(all(r$total_ch4[r$year < 2025] == base$total_ch4[base$year < 2025]), info = n)
  }
})

test_that("combined measures abate less than the sum of single measures", {
  sc <- scenarios(inputs)
  y <- 2040
  tot <- function(pol) sum(run_model(inputs, pol, p, 1960:y)$total_ch4[run_model(inputs, pol, p, 1960:y)$year == y])
  base <- tot(list())
  singles <- sum(sapply(c("circular", "gas_capture", "biocover"), function(n) base - tot(sc[[n]])))
  both <- base - tot(c(sc$circular, sc$gas_capture, sc$biocover))
  expect_lt(both, singles)
  expect_gt(both, 0)
})

test_that("cost curve is sorted and adds up", {
  m <- mac_curve(inputs, year = 2035)
  expect_false(is.unsorted(m$eur_per_t_co2e))
  expect_equal(tail(m$cum_abatement_kt_co2e, 1), sum(m$abatement_kt_co2e))
  base <- run_model(inputs, list(), p, 1960:2035)
  expect_lt(sum(m$abatement_kt_co2e), sum(base$total_kt_co2e[base$year == 2035]))
})

test_that("monte carlo draws stay inside the input ranges and SRRC finds the driver", {
  d <- sample_params(inputs, n = 40, seed = 3)
  docs <- sapply(d$params, function(x) x$doc[["food"]])
  expect_true(all(docs >= 0.08 & docs <= 0.20))
  set.seed(1)
  X <- data.frame(a = runif(200), b = runif(200), c = runif(200))
  s <- srrc(X, 3 * X$a + 0.2 * X$b + rnorm(200, sd = 0.05))
  expect_equal(s$input[1], "a")
})
