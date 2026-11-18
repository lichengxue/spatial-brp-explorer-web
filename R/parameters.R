# Start here when changing the biology or population structure.
# Edit the values below, then run run_example.R again.
# Sourcing this file creates p, the parameter list used by the model functions.
# Source operating_model.R first: the unfished calibration below uses per_recruit().

structure <- "NH"        # "NH": natal homing; "MPO": origin biology; "MPD": region biology
recruitment <- "mean"    # "mean" or "BH" (Beverton-Holt)
life_history <- "short"  # "short" (10 ages) or "long" (30 ages)

# End-of-season movement probabilities, before the natal-homing override.
# Change individual rows to make a custom four-season schedule.
movement <- rbind(
  c(0.30, 0.05),         # season 1: region 1 -> 2, region 2 -> 1
  c(0.30, 0.05),         # season 2: NH replaces this with complete natal return
  c(0.30, 0.05),         # season 3
  c(0.30, 0.05)          # season 4
)

R0 <- c(700, 300)        # recruits/year: fixed mean, or unfished recruitment under BH
BH_slope <- c(12, 12)    # low-density recruitment per unit spawning biomass
steepness <- NULL        # NULL uses BH_slope; use e.g. c(0.75, 0.75) instead

if (life_history == "short") {
  ages <- 1:10
  M <- 0.35
  growth <- 0.27
  selectivity_a50 <- 2.5
  maturity_a50 <- c(1.75, 2.75)
} else {
  ages <- 1:30
  M <- 0.10
  growth <- 0.07
  selectivity_a50 <- 6
  maturity_a50 <- c(7, 9)
}

# Rows are ages; columns are biology types (origin for NH/MPO, region for MPD).
# plogis(x) is R's logistic curve: 1 / (1 + exp(-x)).
# Weight is kg/fish; maturity is the mature fraction at each age.
length_at_age <- cbind(90 * (1 - exp(-growth * ages)),
                       80 * (1 - exp(-growth * ages)))
weight <- 3e-6 * length_at_age^3
maturity <- cbind(plogis(ages - maturity_a50[1]), plogis(ages - maturity_a50[2]))

# Rows are fleets; columns are ages. These can be edited independently.
# Fleet 1 fishes region 1; fleet 2 fishes region 2. Changing fleet selectivity
# does not change survey_selectivity, which is set separately below.
selectivity <- rbind(plogis(ages - selectivity_a50), plogis(ages - selectivity_a50))
survey_selectivity <- plogis(ages - selectivity_a50)

# Put any custom weight, maturity or selectivity vectors above this line.
p <- list(structure = structure, recruitment = recruitment, ages = ages,
  n_ages = length(ages), M = M, R0 = R0, weight = weight, maturity = maturity,
  selectivity = selectivity, survey_selectivity = survey_selectivity, survey_q = 0.1,
  movement = array(0, c(2, 2, 4)))
for (season in 1:4) {
  p$movement[, , season] <- rbind(c(1 - movement[season, 1], movement[season, 1]),
                                 c(movement[season, 2], 1 - movement[season, 2]))
}

# Unfished reference and BH calibration. Recomputed whenever this file is sourced.
# Phi0[origin, region] is lifetime spawning biomass per unfished recruit.
# Choose alpha and beta so R0 = alpha * SSB0 / (1 + beta * SSB0).
# A supplied steepness replaces BH_slope. Neither is used with mean recruitment.
p$Phi0 <- per_recruit(c(0, 0), p)$Phi
p$SSB0 <- as.numeric(R0 %*% p$Phi0)
p$alpha <- BH_slope
p$beta <- rep(NA_real_, 2)
if (recruitment == "BH") {
  if (!is.null(steepness)) {
    p$alpha <- 4 * steepness * R0 / ((1 - steepness) * p$SSB0)
    p$beta <- (5 * steepness - 1) / ((1 - steepness) * p$SSB0)
  } else {
    p$beta <- (p$alpha * p$SSB0 - R0) / (R0 * p$SSB0)
  }
  if (any(p$beta <= 0)) stop("BH inputs must give positive density dependence")
}
