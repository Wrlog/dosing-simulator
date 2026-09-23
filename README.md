# Dosing Simulator

An interactive R Shiny application for simulating intravenous dosing regimens
from a population pharmacokinetic model. Vary the patient population, the
regimen and the drug's PK parameters, and the app returns simulated
concentration-time profiles, target attainment and trough statistics for the
resulting regimen.

The drug is not fixed. Typical clearance and volumes, covariate effects and
variability are all inputs, so any compound adequately described by
two-compartment linear kinetics can be explored by entering its estimates.

> **For research and teaching only.** The default parameters are illustrative
> round numbers and do not describe any real compound. Nothing here is
> validated for clinical use and it must not be used to guide the treatment of
> a patient.

## Model

A generic two-compartment model with first-order elimination from the central
compartment:

- allometric scaling of clearance and volume on body weight, with the
  conventional 0.75 / 1.0 exponents, switchable off
- a power function of a renal function marker on clearance, where an exponent
  of 0 removes the effect and 1 makes clearance proportional to the marker
- log-normal between-subject variability on clearance and central volume
- between-occasion variability on clearance, for two-occasion designs

Parameters are entered as they appear in a published report: typical values at
a stated reference weight and renal function, and variability as a coefficient
of variation, converted internally to the log-scale variance.

Units are not enforced. The convention assumed throughout is amount in mg,
volume in L, clearance in L/h and time in h, giving concentration in mg/L. Any
self-consistent set works.

## What the app reports

Simulation runs a population of virtual subjects whose weight and renal
function are drawn across the requested ranges.

- **Concentration-time profile** — median with 50% and 90% prediction
  intervals, with the two targets and the assessment window marked.
- **Target attainment** — the proportion of subjects whose trough concentration
  over the final dosing interval stays above each of two user-defined targets.
- **Time above target** — the share of the final dosing interval spent above
  the lower target.
- **Trough statistics** — median, mean, 5th and 95th percentiles, range, and
  the median peak.

Attainment is assessed over the **final complete dosing interval** of the
simulation rather than a fixed window, so it follows the regimen instead of
assuming one. Give the simulation enough doses to approach steady state before
reading the numbers off.

## Interface

The sidebar holds what changes during a session: the patient population, the
dosing regimen and the targets. The **Model setup** tab holds what is fixed for
a given compound — disposition parameters, the covariate model, variability and
simulation settings.

| Input | Notes |
|---|---|
| Body weight, renal function | Ranges; subjects are drawn uniformly across them |
| Dose | Per kg or as a flat dose |
| Infusion duration | 0 gives a bolus; must be shorter than the dosing interval |
| Simulation duration | Must cover at least one dosing interval |
| Targets | Two concentration thresholds, in the same units as the model |

## Running it

```r
install.packages(c("shiny", "shinydashboard", "DT", "tidyverse", "mrgsolve"))
shiny::runApp("app.R")
```

`mrgsolve` compiles the model on first run, so a working C++ toolchain is
required (Rtools on Windows, Xcode command line tools on macOS). The model is
compiled once at startup and then updated in place with `param()` and `omat()`,
so changing parameters in the app does not trigger a recompile.

## Contents

- `app.R` — the Shiny application (UI, simulation logic, plotting)
- `models/TwoCompartment.cpp` — the mrgsolve model specification

## A note on data

This repository contains **no patient data**. The application is a pure
simulator: it runs entirely from the structural model and the parameter values
entered in the interface. No individual-level records, NONMEM datasets or
output tables are included, and none are needed to run the app.

## Using it with real parameter estimates

To simulate a specific compound, enter its estimates on the Model setup tab:
typical CL, V1, Q and V2, the reference weight and renal function those
estimates were normalised to, the renal exponent, and the between-subject
variability. The defaults are placeholders and should be replaced wholesale.

The same can be done from the console without the app:

```r
library(mrgsolve)
mod <- mread("TwoCompartment", project = "models")

mod <- param(mod, TVCL = 8.0, TVV1 = 20.0, TVQ = 2.5, TVV2 = 30.0,
             WT_REF = 70, RENAL_REF = 120, RENAL_EXP_CL = 0.45)
mod <- omat(mod, dmat(0.09, 0.06, 0, 0))   # log-scale variances
```

## License

MIT
