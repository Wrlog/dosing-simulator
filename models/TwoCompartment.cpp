$PROB
// Generic two-compartment population PK model for intravenous dosing.
//
// Nothing here is specific to a particular drug. Every typical value,
// covariate exponent and variance term is a parameter, so a regimen for any
// compound described by two-compartment linear kinetics can be simulated by
// supplying its estimates from the Shiny app (or from `param()` / `omat()`).
//
// Structure:
//   - two compartments, first-order elimination from the central compartment
//   - allometric scaling of all disposition parameters on body weight
//   - a power function of a renal function marker on clearance
//   - log-normal between-subject variability on CL and V1
//   - between-occasion variability on CL, for designs with two occasions
//
// Units are not enforced. The convention assumed throughout is amount in mg,
// volume in L, clearance in L/h and time in h, giving concentration in mg/L.
// Any self-consistent set works.

$PARAM @annotated
  // Typical values, defined at the reference covariate values below.
  TVCL : 5.0  : Typical clearance (L/h)
  TVV1 : 15.0 : Typical central volume of distribution (L)
  TVQ  : 3.0  : Typical inter-compartmental clearance (L/h)
  TVV2 : 25.0 : Typical peripheral volume of distribution (L)

  // Reference covariate values. Typical values above apply to a subject at
  // exactly these; changing them rescales the whole parameterisation, so they
  // should match whatever the source estimates were normalised to.
  WT_REF    : 70  : Reference body weight (kg)
  RENAL_REF : 120 : Reference renal function (mL/min/1.73m2)

  // Allometric exponents. 0.75 for flows and 1.0 for volumes are the
  // conventional fixed values; they are parameters here so they can be
  // estimated values instead, or switched off by setting them to 0.
  WT_EXP_CL : 0.75 : Weight exponent on clearance
  WT_EXP_V1 : 1.0  : Weight exponent on central volume
  WT_EXP_Q  : 0.75 : Weight exponent on inter-compartmental clearance
  WT_EXP_V2 : 1.0  : Weight exponent on peripheral volume

  // Renal function on clearance. 0 removes the effect entirely; 1 makes
  // clearance directly proportional to the marker.
  RENAL_EXP_CL : 0.5 : Renal function exponent on clearance

  // Individual covariates, supplied per subject in the input data set.
  WT    : 70  : Body weight (kg)
  RENAL : 120 : Renal function marker (mL/min/1.73m2)
  OCC   : 1   : Occasion index for between-occasion variability (1 or 2)

$OMEGA @annotated
  // Variances on the log scale. For a coefficient of variation CV expressed
  // as a fraction, the equivalent variance is log(1 + CV^2).
  ECL  : 0.0 : Between-subject variability on clearance
  EV1  : 0.0 : Between-subject variability on central volume
  IOV1 : 0.0 : Between-occasion variability on clearance, occasion 1
  IOV2 : 0.0 : Between-occasion variability on clearance, occasion 2

$SIGMA @annotated
  // Residual error describes the assay, not the exposure, so it is left at
  // zero by default and excluded from IPRED. It is available for anyone
  // simulating observed concentrations rather than true ones.
  PROP_RUV : 0.0 : Proportional residual error variance

$CMT @annotated
  CENT   : Central compartment (mg)
  PERIPH : Peripheral compartment (mg)

$MAIN
  // Between-occasion variability applies to whichever occasion this record
  // belongs to. A record carrying any other OCC value gets no IOV rather
  // than silently falling back to one of the occasions.
  double IOVCL = 0.0;
  if (OCC == 1) IOVCL = ETA(3);
  if (OCC == 2) IOVCL = ETA(4);

  double CL = TVCL * pow(WT / WT_REF, WT_EXP_CL)
                   * pow(RENAL / RENAL_REF, RENAL_EXP_CL)
                   * exp(ETA(1) + IOVCL);
  double V1 = TVV1 * pow(WT / WT_REF, WT_EXP_V1) * exp(ETA(2));
  double Q  = TVQ  * pow(WT / WT_REF, WT_EXP_Q);
  double V2 = TVV2 * pow(WT / WT_REF, WT_EXP_V2);

$ODE
  dxdt_CENT   = -(CL / V1) * CENT - (Q / V1) * CENT + (Q / V2) * PERIPH;
  dxdt_PERIPH =  (Q / V1) * CENT - (Q / V2) * PERIPH;

$TABLE
  // IPRED is the individual's true concentration; DV adds assay error.
  double IPRED = CENT / V1;
  double DV    = IPRED * (1.0 + EPS(1));

$CAPTURE @annotated
  IPRED : Individual predicted concentration (mg/L)
  DV    : Concentration with residual error (mg/L)
  CL    : Individual clearance (L/h)
  V1    : Individual central volume (L)
  Q     : Individual inter-compartmental clearance (L/h)
  V2    : Individual peripheral volume (L)
  WT    : Body weight (kg)
  RENAL : Renal function marker (mL/min/1.73m2)
