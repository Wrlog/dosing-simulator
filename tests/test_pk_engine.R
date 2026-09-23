# Check the closed-form engine against RK4 integration of the ODEs in
# models/TwoCompartment.cpp. If these agree, the engine computes what the
# mrgsolve model computed, and the solver was never needed.
#
# Run: Rscript tests/test_pk_engine.R    (base R only, no packages)

source(file.path("R", "pk_engine.R"))

# Reference: integrate segment by segment between event times, so infusion
# starts, infusion ends and bolus doses all land exactly on a boundary
# instead of being smeared across a step.
rk4_reference <- function(amount, n_doses, interval, inf_dur,
                          cl, v1, q, v2, t_end, per_segment = 800) {
  k10 <- cl / v1
  k12 <- q / v1
  k21 <- q / v2

  dose_times <- (seq_len(n_doses) - 1) * interval
  events <- sort(unique(c(0, t_end, dose_times,
                          if (inf_dur > 0) dose_times + inf_dur else NULL)))
  events <- events[events <= t_end + 1e-12]

  rate_at <- function(mid) {
    if (inf_dur <= 0) return(0)
    sum(ifelse(dose_times <= mid & mid < dose_times + inf_dur,
               amount / inf_dur, 0))
  }

  y <- c(0, 0)
  ts <- numeric(0)
  cs <- numeric(0)

  for (i in seq_len(length(events) - 1)) {
    a <- events[i]
    b <- events[i + 1]
    if (inf_dur == 0) {
      y[1] <- y[1] + amount * sum(abs(dose_times - a) < 1e-12)
    }
    ts <- c(ts, a)
    cs <- c(cs, y[1] / v1)
    if (b <= a) next

    h <- (b - a) / per_segment
    r <- rate_at((a + b) / 2)
    deriv <- function(yv) {
      c(r - (k10 + k12) * yv[1] + k21 * yv[2],
        k12 * yv[1] - k21 * yv[2])
    }
    for (s in seq_len(per_segment)) {
      k1 <- deriv(y)
      k2 <- deriv(y + h / 2 * k1)
      k3 <- deriv(y + h / 2 * k2)
      k4 <- deriv(y + h * k3)
      y <- y + (h / 6) * (k1 + 2 * k2 + 2 * k3 + k4)
      ts <- c(ts, a + s * h)
      cs <- c(cs, y[1] / v1)
    }
  }
  list(time = ts, conc = cs, dose_times = dose_times)
}

set.seed(42)
worst <- 0
cases <- 0

for (rep_i in 1:12) {
  cl <- runif(1, 1, 12)
  v1 <- runif(1, 5, 40)
  q <- runif(1, 0.5, 8)
  v2 <- runif(1, 5, 60)
  interval <- sample(c(6, 8, 12, 24), 1)
  inf_dur <- sample(c(0, 0.5, 1, 2), 1)
  if (inf_dur >= interval) next
  amount <- runif(1, 100, 1500)
  n_doses <- 3
  t_end <- n_doses * interval

  ref <- rk4_reference(amount, n_doses, interval, inf_dur, cl, v1, q, v2, t_end)

  # Compare on the integrator's own grid so no interpolation enters, and skip
  # points sitting exactly on a dose time: there the reference holds the
  # pre-bolus value while the closed form already counts the dose.
  idx <- seq(1, length(ref$time), by = max(floor(length(ref$time) / 300), 1))
  on_event <- sapply(ref$time[idx], function(tv) any(abs(tv - ref$dose_times) < 1e-9))
  idx <- idx[!on_event]

  tt <- ref$time[idx]
  got <- two_cmt_conc(tt, amount, n_doses, interval, inf_dur, cl, v1, q, v2)
  want <- ref$conc[idx]

  rel <- max(abs(got - want)) / max(max(abs(want)), 1e-12)
  worst <- max(worst, rel)
  cases <- cases + 1
}

cat(sprintf("cases compared: %d\n", cases))
cat(sprintf("worst relative difference vs RK4: %.3e\n", worst))

if (!is.finite(worst) || worst > 1e-8) {
  stop(sprintf("closed-form engine disagrees with numerical integration (%.3e)", worst))
}

# Superposition sanity: at steady state a longer run must not drift.
conc_a <- two_cmt_conc(c(96), 500, 4, 24, 1, 5, 15, 3, 25)
conc_b <- two_cmt_conc(c(96), 500, 8, 24, 1, 5, 15, 3, 25)
if (conc_b <= conc_a) stop("accumulation across doses is not being summed")

# A bolus at t = 0 must equal dose / V1 exactly.
c0 <- two_cmt_conc(0, 1000, 1, 24, 0, 5, 20, 3, 25)
if (abs(c0 - 1000 / 20) > 1e-10) stop("bolus C0 is not dose/V1")

cat("PASS\n")
