# waste-methane-mitigation

R package and analysis scripts for methane from solid waste disposal and domestic wastewater: how much is emitted, how far mitigation measures can bring it down, at what cost, and how uncertain the numbers are.

The model follows the structure used in integrated assessment work on non-CO2 gases. Activity data (waste, wastewater) are combined with IPCC emission factors, and mitigation measures are described by their removal efficiency, applicability and unit cost. On top of that come scenario projections, a marginal abatement cost curve and a Monte Carlo analysis with global sensitivity.


## What is in the model

**Landfills.** First order decay (FOD) model from the IPCC 2006 Guidelines (Vol. 5, Ch. 3).
- Six degradable waste fractions, each with its own DOC and decay rate.
- Decay rates for four climate zones.
- Four site types with their methane correction factors.
- Waste starts decaying in the year after deposition, which is the IPCC default delay.
- The implementation is checked against a mass balance, the half-life and a hand calculation in `results/validation.md`.

**Domestic wastewater.** IPCC 2006 Vol. 5, Ch. 6, using BOD per person, treatment and discharge pathways with their MCF, and the correction factor for industrial co-discharge in collected systems.

**Mitigation measures** (`inst/extdata/measures.csv`)

| Measure | Acts on |
|---|---|
| Organics diversion (composting, anaerobic digestion) | food and garden waste going to landfill |
| Landfill gas to energy | gas generated at managed sites |
| Landfill gas flaring | remaining gas at managed sites |
| Biocover | surface emissions not captured by gas systems (methane oxidation in the cover) |
| Aerobic plant upgrade | poorly operated aerobic plants |
| Covered lagoons with flaring | deep anaerobic lagoons |
| Septic tanks to sewer | septic tank users |

Measures that work on the same methane interact. If gas collection already captures half of the gas, a biocover only treats what is left. The model applies the measures in that order instead of adding up stand-alone potentials.

**Input database.** All parameters live in four csv tables. Each value comes with a low and high value, a unit and a source. `validate_inputs()` checks ranges, missing sources, incomplete decay rate tables, and shares that do not sum to one. The Monte Carlo analysis draws from the same ranges.

## Data

The IPCC defaults are taken from the 2006 Guidelines. Where the Guidelines give no range, the range is marked as assumed in the source column.

The four regions in `regions.csv` are **illustrative archetypes**, not real countries. They span temperate and tropical climates and high to lower-middle income, with plausible per-capita waste generation, composition, landfill practices and sanitation. The costs in `measures.csv` are also illustrative values chosen to show the method. For a real assessment both tables would be replaced with country data and project cost estimates; the code does not need to change.

## Results

All numbers are for the four example regions together. Measures start in 2025 and reach their full applicability in 2035.

**Scenarios.** In 2040, all measures together cut emissions by 54% against the baseline (100 Mt CO2e).

| Scenario, 2040 | Reduction vs baseline |
|---|---|
| Organics diversion | 19% |
| Landfill gas capture | 19% |
| Biocovers | 16% |
| Wastewater measures | 12% |
| All measures | 54% |

**Timing matters.** Gas capture and biocovers reduce emissions as soon as they are installed. Diverting organic waste only stops new methane-producing waste from being added, while the waste already in place keeps emitting for decades. Diversion therefore achieves only a 4% reduction in 2030 but overtakes gas capture around 2040. Gas capture slowly loses ground because it applies only to managed sites, and the fastest-growing regions in the example rely mostly on unmanaged dumpsites.

**Cost curve, 2040.**

| Measure | Abatement 2040 (Mt CO2e) | EUR per t CO2e |
|---|---|---|
| Landfill gas to energy | 9.9 | -2 |
| Landfill gas flaring | 8.9 | 5 |
| Covered lagoons | 5.9 | 6 |
| Aerobic plant upgrade | 2.8 | 8 |
| Biocover | 11.7 | 16 |
| Organics diversion | 12.9 | 83 |
| Septic to sewer | 3.6 | 202 |

Roughly 40 Mt CO2e, about 70% of the total potential, is available below 20 EUR per tonne. After gas capture, biocovers deliver the largest low-cost potential, because they also work at sites where gas collection is not an option.

Organics diversion looks expensive in a single target year because of the lag described above. Its cost falls as the target year moves later:

| Target year | Diversion cost (EUR per t CO2e) |
|---|---|
| 2030 | 122 |
| 2040 | 83 |
| 2050 | 76 |

Diversion also has benefits outside methane, such as compost, biogas and landfill space, which are not counted here.

**Uncertainty.** 1,000 Monte Carlo draws with triangular distributions over the ranges in the input tables:

| 2040 | 5th percentile | Median | 95th percentile |
|---|---|---|---|
| Baseline emissions (Mt CO2e) | 83 | 101 | 121 |
| Reduction with all measures | 46% | 51% | 56% |

The absolute emission level is uncertain by about ±20%. The relative effect of the measures is much more robust.

Standardised rank regression coefficients (R² above 0.93) show that different inputs drive the two results:
- **Baseline level:** the parameters that set how much carbon becomes methane (F, DOCf, DOC of food waste) and the wastewater Bo.
- **Size of the reduction:** mainly the oxidation efficiency of biocovers, followed by the food waste DOC and the gas collection efficiency.

Better field data on biocover performance would therefore narrow the mitigation estimate more than anything else.

## Running it

```r
# install.packages(c("ggplot2", "testthat"))
# from the repository root:
source("scripts/01_validate.R")      # input checks and model checks
source("scripts/02_scenarios.R")     # scenarios 2000-2050
source("scripts/03_cost_curve.R")    # cost curve and diversion by target year
source("scripts/04_uncertainty.R")   # Monte Carlo and sensitivity (about 1 minute)
```

The scripts use the installed package if available and otherwise source the functions from `R/`. Figures are written to `results/figures`. To install the package:

```r
install.packages(".", repos = NULL, type = "source")
```

Using the functions directly:

```r
library(wastech4)
inputs <- load_inputs()
validate_inputs(inputs)                            # empty data frame = no problems
sc <- scenarios(inputs, start = 2025, full = 2035)
res <- run_model(inputs, sc$combined)              # region x year, t CH4, kt CO2e, MEUR
mac <- mac_curve(inputs, year = 2040)
```

To use your own data, copy the four csv files from `inst/extdata`, edit them and call `load_inputs("path/to/folder")`.

## Structure

```
R/
  inputs.R        input tables, validation, default parameters
  activity.R      population, waste and BOD projections
  landfill.R      first order decay model, gas capture, oxidation and biocovers
  wastewater.R    wastewater pathways and measures
  model.R         scenarios and model runs
  macc.R          marginal abatement cost curve
  uncertainty.R   Monte Carlo sampling and rank regression sensitivity
inst/extdata/     parameter database (csv)
scripts/          analysis scripts that write results/
tests/testthat/   unit tests
```

## Limitations

- Landfill shares and waste composition are held at their 2020 values over the whole period, including the historical deposits that build up the landfill stock.
- Wastewater sludge removal and industrial wastewater are not included.
- Costs are annual costs in the target year without a separate capital and operating split. Revenues from energy are folded into the net unit cost.
- The model counts methane only. CO2 from flaring and energy use, and the avoided emissions from electricity generation, are not included.

## Reference

IPCC (2006). 2006 IPCC Guidelines for National Greenhouse Gas Inventories, Volume 5: Waste. Chapter 3 (Solid Waste Disposal) and Chapter 6 (Wastewater Treatment and Discharge).

## License

MIT
