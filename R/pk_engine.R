# ============================================================================
# Closed-form two-compartment PK engine.
#
# This replaces mrgsolve for the model in models/TwoCompartment.cpp. That
# model is linear and time-invariant, so it has an exact analytical solution
# and never needed a numerical integrator:
#
#   dCENT/dt   = -(CL/V1)*CENT - (Q/V1)*CENT + (Q/V2)*PERIPH  (+ infusion rate)
#   dPERIPH/dt =  (Q/V1)*CENT - (Q/V2)*PERIPH
#
# Why bother: mrgsolve compiles C++ at run time, which cannot happen in a
# browser. With the solver gone the whole app runs under webR, so it can be
# published as a static page instead of needing a server. tests/ checks this
# engine against RK4 integration of the same ODEs.
#
# Units follow the original model: amount mg, volume L, clearance L/h,
# time h, concentration mg/L. Any self-consistent set works.
# ============================================================================

#' Hybrid rate constants for a two-compartment model
#'
#' Returns the macro constants of the unit-bolus central response
#' f(u) = A*exp(-alpha*u) + B*exp(-beta*u), where A + B = 1.
two_cmt_constants <- function(cl, v1, q, v2) {
  k10 <- cl / v1
  k12 <- q / v1
  k21 <- q / v2
  s <- k10 + k12 + k21
  disc <- sqrt(s * s - 4 * k10 * k21)
  alpha <- 0.5 * (s + disc)
  beta <- 0.5 * (s - disc)
  list(
    alpha = alpha,
    beta = beta,
    a_coef = (alpha - k21) / (alpha - beta),
    b_coef = (k21 - beta) / (alpha - beta)
  )
}

#' Concentration-time profile for repeated identical IV doses
#'
#' @param times Numeric vector of observation times.
#' @param amount Amount per dose.
#' @param n_doses Number of doses.
#' @param interval Dosing interval; ignored when n_doses is 1.
#' @param inf_dur Infusion duration; 0 gives a bolus.
#' @return Numeric vector of central-compartment concentrations.
#'
#' Doses superpose, which is exact for a linear system: the response to
#' several doses is the sum of the responses to each one alone.
two_cmt_conc <- function(times, amount, n_doses, interval, inf_dur,
                         cl, v1, q, v2) {
  k <- two_cmt_constants(cl, v1, q, v2)
  alpha <- k$alpha
  beta <- k$beta
  a_coef <- k$a_coef
  b_coef <- k$b_coef

  total <- numeric(length(times))
  for (i in seq_len(n_doses)) {
    t0 <- (i - 1) * interval
    u <- times - t0
    active <- u >= 0
    uu <- ifelse(active, u, 0)

    if (inf_dur > 0) {
      rate <- amount / inf_dur
      # Time spent infusing by uu, then free decay for whatever follows.
      during <- pmin(uu, inf_dur)
      after <- uu - during
      contrib <- (rate / v1) * (
        (a_coef / alpha) * (1 - exp(-alpha * during)) * exp(-alpha * after) +
          (b_coef / beta) * (1 - exp(-beta * during)) * exp(-beta * after)
      )
    } else {
      contrib <- (amount / v1) *
        (a_coef * exp(-alpha * uu) + b_coef * exp(-beta * uu))
    }

    total <- total + ifelse(active, contrib, 0)
  }
  total
}

#' Individual PK parameters from covariates and random effects
#'
#' Mirrors $MAIN in models/TwoCompartment.cpp exactly.
individual_params <- function(wt, renal, tvcl, tvv1, tvq, tvv2,
                              wt_ref, renal_ref,
                              wt_exp_cl, wt_exp_v1, wt_exp_q, wt_exp_v2,
                              renal_exp_cl,
                              eta_cl = 0, eta_v1 = 0, iov_cl = 0) {
  list(
    cl = tvcl * (wt / wt_ref)^wt_exp_cl *
      (renal / renal_ref)^renal_exp_cl * exp(eta_cl + iov_cl),
    v1 = tvv1 * (wt / wt_ref)^wt_exp_v1 * exp(eta_v1),
    q  = tvq  * (wt / wt_ref)^wt_exp_q,
    v2 = tvv2 * (wt / wt_ref)^wt_exp_v2
  )
}

#' Simulate a population
#'
#' Returns a long data frame with one row per subject per output time, with
#' the same columns the mrgsolve version produced downstream: ID, TIME,
#' IPRED, CL, V1, Q, V2, WT, RENAL.
simulate_population <- function(n_subjects, wt_range, renal_range,
                                dose, dose_per_kg, inf_dur,
                                interval, n_doses, duration, delta,
                                tvcl, tvv1, tvq, tvv2,
                                wt_ref, renal_ref, renal_exp_cl,
                                allometric,
                                iiv_cl_var, iiv_v1_var, iov_cl_var,
                                seed = 1234) {
  set.seed(seed)

  exps <- if (isTRUE(allometric)) {
    list(cl = 0.75, v1 = 1.0, q = 0.75, v2 = 1.0)
  } else {
    list(cl = 0, v1 = 0, q = 0, v2 = 0)
  }

  wt <- runif(n_subjects, wt_range[1], wt_range[2])
  renal <- runif(n_subjects, renal_range[1], renal_range[2])

  eta_cl <- if (iiv_cl_var > 0) rnorm(n_subjects, 0, sqrt(iiv_cl_var)) else rep(0, n_subjects)
  eta_v1 <- if (iiv_v1_var > 0) rnorm(n_subjects, 0, sqrt(iiv_v1_var)) else rep(0, n_subjects)
  # Occasion 1 only, matching the app's dosing records.
  iov_cl <- if (iov_cl_var > 0) rnorm(n_subjects, 0, sqrt(iov_cl_var)) else rep(0, n_subjects)

  times <- seq(0, duration, by = delta)
  amount <- if (isTRUE(dose_per_kg)) wt * dose else rep(dose, n_subjects)

  n_t <- length(times)
  out <- data.frame(
    ID = rep(seq_len(n_subjects), each = n_t),
    TIME = rep(times, times = n_subjects),
    IPRED = NA_real_,
    CL = NA_real_, V1 = NA_real_, Q = NA_real_, V2 = NA_real_,
    WT = rep(wt, each = n_t),
    RENAL = rep(renal, each = n_t)
  )

  for (i in seq_len(n_subjects)) {
    p <- individual_params(
      wt[i], renal[i], tvcl, tvv1, tvq, tvv2, wt_ref, renal_ref,
      exps$cl, exps$v1, exps$q, exps$v2, renal_exp_cl,
      eta_cl[i], eta_v1[i], iov_cl[i]
    )
    conc <- two_cmt_conc(times, amount[i], n_doses, interval, inf_dur,
                         p$cl, p$v1, p$q, p$v2)
    rows <- ((i - 1) * n_t + 1):(i * n_t)
    out$IPRED[rows] <- conc
    out$CL[rows] <- p$cl
    out$V1[rows] <- p$v1
    out$Q[rows] <- p$q
    out$V2[rows] <- p$v2
  }

  out
}
