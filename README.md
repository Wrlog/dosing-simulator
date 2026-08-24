# Piperacillin Dosing Simulator

An interactive R Shiny application for simulating piperacillin exposure in pediatric
patients under different dosing regimens, built on a published population
pharmacokinetic model.

The model is a two-compartment structure with allometric scaling on weight,
creatinine clearance as a covariate on clearance, and inter-occasion variability
on clearance distinguishing the pre- and post-surgical periods. Users vary weight,
renal function, dose, infusion duration and dosing interval, and the app returns
simulated concentration-time profiles and target attainment for the resulting regimen.

## Reference

Tan WR, Irie K, McIntire C, Torres JL, Jones R, Gibson A, Mizuno T, Tang Girdwood S.
Model-informed dose optimization for prophylactic piperacillin-tazobactam in
perioperative pediatric critically ill patients.
*Antimicrobial Agents and Chemotherapy.* 2025;69(3):e0122724.
doi:[10.1128/aac.01227-24](https://doi.org/10.1128/aac.01227-24)

## Running it

```r
install.packages(c("shiny", "shinydashboard", "DT", "tidyverse", "mrgsolve", "haven", "knitr"))
shiny::runApp("app.R")
```

`mrgsolve` compiles the model on first run, so a working C++ toolchain is required
(Rtools on Windows, Xcode command line tools on macOS).

## Contents

- `app.R` — the Shiny application (UI, simulation logic, plotting)
- `models/Piperacillin.cpp` — the mrgsolve model specification

## A note on data

This repository contains **no patient data**. The application is a pure simulator:
it runs entirely from the structural model and the population parameter estimates
in `models/Piperacillin.cpp`, which are the published estimates from the paper cited
above. No individual-level records, NONMEM datasets, or output tables are included,
and none are needed to run the app.

## License

MIT
