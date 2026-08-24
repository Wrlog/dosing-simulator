$PROB
// Based on: run002
// Description: IOV for CL IOV=0 during first op +CrCL on CL
// Author: TANH4M
// Piperacillin Two-compartment model with IOV

$PARAM @annotated
  TVCL : 8.95 : Clearance for a 70kg person with CrCl of 120 mL/min
  TVV1 : 22.4 : Central volume of distribution
  TVQ : 2.51 : Inter-compartmental clearance
  TVV2 : 4.48 : Peripheral volume of distribution
  WT_EXP_CL : 0.75 : Weight exponent for clearance, fixed
  WT_EXP_V1 : 1.0 : Weight exponent for V1, fixed
  WT_EXP_Q : 0.75 : Weight exponent for Q, fixed
  WT_EXP_V2 : 1.0 : Weight exponent for V2, fixed
  CRCL_EXP_CL : 0.457 : Creatinine clearance exponent for CL
  WT : 70 : Patient's weight in kg
  CRCL : 120 : Patient's creatinine clearance in mL/min
  IDUR :0.5 : IDUR 
  OCC : 0 : Occasion variable, should be passed with data

$OMEGA @annotated
  ECL: 0.0733 : IIV for clearance
  EV1: 0.141 : IIV for V1
  IOV_PRE : 0.257 : IOV before surgery
  IOV_POST : 0.197 : IOV after surgery

$SIGMA @annotated
  PROP_RUV : 0 : Proportional residual unexplained variability, fixed

$CMT @annotated
  CENT : Central compartment
  PERIPH : Peripheral compartment

$PLUGIN Rcpp mrgx

$GLOBAL
using namespace Rcpp;
double INFDUR;
#define CP (CENT/V1)
#define CT (PERIPH/V2)


$MAIN
  // IOV effect
  double IOVCL = 0;
  if (OCC == 0) IOVCL = ETA(3);    // Pre-surgery
  if (OCC == 2) IOVCL = ETA(4);    // Post-surgery

  // PK parameters calculation with covariates
  double CL = TVCL * pow(WT/70, WT_EXP_CL) * pow(CRCL/120, CRCL_EXP_CL) * exp(ETA(1) + IOVCL);
  double V1 = TVV1 * pow(WT/70, WT_EXP_V1) * exp(ETA(2));
  double Q = TVQ * pow(WT/70, WT_EXP_Q);
  double V2 = TVV2 * pow(WT/70, WT_EXP_V2);

  // Setting compartment sizes
  // F_CENT = V1;
  // F_PERIPH = V2;

$ODE
  dxdt_CENT = - (Q/V1 + CL/V1) * CENT + Q/V2 * PERIPH;
  dxdt_PERIPH = Q/V1 * CENT - Q/V2 * PERIPH;

$TABLE
  double CONC = CENT / V1 * (1 + PROP_RUV);

$CAPTURE
  CL V1 Q V2 CP EVID WT CRCL
