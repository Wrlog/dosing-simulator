# Dosing Simulator

An R Shiny app for simulating intravenous dosing regimens from a population
pharmacokinetic model. You set the patient population, the regimen and the
drug's PK parameters, and it simulates concentration-time profiles, target
attainment and trough statistics.

It isn't tied to one drug. Typical clearance and volumes, covariate effects and
variability are all inputs, so it works for any compound that two-compartment
linear kinetics describes adequately.

This is for research and teaching only. The defaults are round illustrative
numbers, not a real compound, and nothing here is validated for clinical use or
should be used to guide a patient's treatment. There's no patient data in the
repo: the app runs from the structural model and the parameters you enter, and
doesn't need any individual-level records, NONMEM datasets or output tables.

## Model

Two compartments with first-order elimination from the central compartment,
plus:

- allometric scaling of clearance and volume on body weight (the usual 0.75
  and 1.0 exponents, can be switched off)
- a power function of a renal function marker on clearance. An exponent of 0
  removes the effect and 1 makes clearance proportional to the marker
- log-normal between-subject variability on clearance and central volume
- between-occasion variability on clearance, for two-occasion designs

Parameters go in the way they'd appear in a published report: typical values
at a stated reference weight and renal function, and variability as a
coefficient of variation, which is converted to a log-scale variance.

Units aren't enforced. The app assumes mg, L, L/h and h, which gives
concentrations in mg/L, but any consistent set works.

## Output

Each run simulates a population of virtual subjects, with weight and renal
function drawn across the ranges you set. You get:

- the median concentration-time profile with 50% and 90% prediction
  intervals, the two targets and the assessment window
- target attainment: the proportion of subjects whose trough over the final
  dosing interval stays above each of the two targets
- time above target: the share of the final dosing interval spent above the
  lower target
- trough median, mean, 5th and 95th percentiles and range, plus the median peak

Attainment is assessed over the final complete dosing interval rather than a
fixed window, so it moves with the regimen. Give the simulation enough doses to
get near steady state before reading the numbers.

## Interface

The sidebar has what you'd change during a session: population, regimen and
targets. The Model setup tab has what stays fixed for a compound: disposition
parameters, the covariate model, variability and simulation settings.

| Input | Notes |
|---|---|
| Body weight, renal function | Ranges; subjects are drawn uniformly across them |
| Dose | Per kg or as a flat dose |
| Infusion duration | 0 gives a bolus; must be shorter than the dosing interval |
| Simulation duration | Must cover at least one dosing interval |
| Targets | Two concentration thresholds, in the same units as the model |

## Running it

There's a browser version at <https://wrlog.github.io/dosing-simulator/>. It's
the same app exported to WebAssembly with shinylive, so nothing needs
installing.

To run it locally:

```r
install.packages(c("shiny", "shinydashboard", "DT", "dplyr", "tidyr",
                   "tibble", "ggplot2", "scales"))
shiny::runApp("app.R")
```

The model is linear, so `R/pk_engine.R` solves it in closed form instead of
integrating ODEs. That's what lets it run in the browser, and there's no C++
toolchain to set up. `Rscript tests/test_pk_engine.R` checks the engine against
RK4 integration of the same equations (base R only), and the Pages workflow
runs that check before every deploy.

## Files

- `app.R`: the Shiny app (UI, plotting, summaries)
- `R/pk_engine.R`: the closed-form two-compartment solver and population
  simulation
- `R/theme.R`: styling
- `tests/test_pk_engine.R`: the engine check
- `models/TwoCompartment.cpp`: the original mrgsolve model the engine
  reproduces, kept as a reference

## Using real estimates

Enter the compound's estimates on the Model setup tab: typical CL, V1, Q and
V2, the reference weight and renal function they were normalised to, the renal
exponent and the between-subject variability. The defaults are placeholders,
so replace all of them.

Or from the console, without the app:

```r
source("R/pk_engine.R")

sim <- simulate_population(
  n_subjects = 500, wt_range = c(50, 100), renal_range = c(60, 140),
  dose = 1000, dose_per_kg = FALSE, inf_dur = 0.5,
  interval = 8, n_doses = 6, duration = 48, delta = 0.1,
  tvcl = 8.0, tvv1 = 20.0, tvq = 2.5, tvv2 = 30.0,
  wt_ref = 70, renal_ref = 120, renal_exp_cl = 0.45, allometric = TRUE,
  iiv_cl_var = 0.09, iiv_v1_var = 0.06, iov_cl_var = 0  # log-scale variances
)
```

This returns one row per subject per time point (`ID`, `TIME`, `IPRED` and the
individual parameters).

## License

MIT
