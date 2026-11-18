# Functions for the reference points. These use operating_model.R.
#
# To calculate one type, start with global_brp(), population_brp(), or regional_brp().
# To compare several methods, use spr_reference_points().
# Optional yield calculations are in yield_reference_points.R.
# F1 and F2 are fully selected annual fleet mortalities. An SPR target is a
# fraction (0.40), and is relative to the same model without fishing.


# Global SPR reference point: keep the F1:F2 ratio and find one overall F.
# weighting can be "recruitment", "equal", "survey", or "catch".
# movement = FALSE omits movement when calculating the reference point.
# natal_only = TRUE gives Natal-RecW (with movement and recruitment weights).
# Returns one table row containing F1, F2, SPRs, equilibrium biomass and catch.
global_brp <- function(p, target = 0.40, weighting = "recruitment", movement = TRUE,
    fishing_pattern = c(0.7, 0.3), survey_weights = NULL, catch_weights = NULL,
    natal_only = FALSE) {
  suffix <- switch(weighting, recruitment = "RecW", equal = "EqW",
                   survey = "SurvW", catch = "CatchW", stop("Unknown global weighting"))
  method <- paste0(if (movement) "SpE-" else "SpD-", suffix)
  if (natal_only) {
    if (!movement || weighting != "recruitment")
      stop("Natal-RecW uses movement and recruitment weights")
    method <- "Natal-RecW"
  }
  spr_reference_points(p, survey_weights, catch_weights, target = target,
    fishing_pattern = fishing_pattern, rules = method)
}


# Population SPR reference point: find F1 and F2 so each birth population
# retains the target fraction of its lifetime spawning output.
# No weights between birth populations are needed for this target.
population_brp <- function(p, target = 0.40, movement = TRUE) {
  method <- if (movement) "Pop-Move" else "Pop-NoMove"
  spr_reference_points(p, target = target, rules = method)
}


# Regional reference point: find F1 and F2 so both spawning regions reach target.
# origin_weights = NULL uses biological reference recruitment (the default).
# Examples: c(1, 1) for equal origins, or c(0.25, 0.75) for a custom mixture.
# The same origin weights are used in fished and unfished calculations.
regional_brp <- function(p, target = 0.40, origin_weights = NULL, movement = TRUE) {
  method <- if (movement) "Reg-Move" else "Pop-NoMove"
  spr_reference_points(p, target = target, regional_weights = origin_weights,
    rules = method)
}


# These are the SPR definitions used by all the reference-point functions.
# Phi[s,r] is lifetime spawning biomass in region r from one recruit of origin s.
# rowSums(Phi) follows a birth population across both spawning regions.
# weights %*% Phi combines the origins contributing to each spawning region.
# Each SPR is the fished spawning output divided by its unfished value.
# A regional ratio describes spawning at a place; it is not the SPR of a
# separate birth population. The weights specify which origins contribute.

spr_ratios <- function(F, p, regional_weights = p$R0, global_weights = p$R0) {
  Phi <- per_recruit(F, p)$Phi
  population_fished <- rowSums(Phi)
  population_unfished <- rowSums(p$Phi0)
  regional_fished <- as.numeric(regional_weights %*% Phi)
  regional_unfished <- as.numeric(regional_weights %*% p$Phi0)
  list(
    population = population_fished / population_unfished,
    regional = regional_fished / regional_unfished,
    global = sum(global_weights * population_fished) /
             sum(global_weights * population_unfished),
    natal = sum(global_weights * diag(Phi)) /
            sum(global_weights * diag(p$Phi0))
  )
}


# Global methods have one unknown: the amount of fishing along a fixed pattern.
# uniroot() finds the F multiplier where global SPR - target is zero.
solve_global_spr <- function(p, weights, target, fishing_pattern, natal_only = FALSE) {
  # This normalization follows WHAM: max_age(sum_fleet(F_at_age)) = global F.
  # The multiplier g changes fishing intensity while preserving F1:F2.
  # These are fishing-mortality shares, not shares of the resulting catch.
  coefficient <- fishing_pattern / max(colSums(p$selectivity * fishing_pattern))
  difference <- function(g) {
    F <- g * coefficient
    values <- spr_ratios(F, p, global_weights = weights)
    if (natal_only) values$natal - target else values$global - target
  }
  if (difference(10) > 0) stop("Target not reached within global F <= 10")
  fit <- uniroot(difference, c(0, 10), tol = 1e-11)
  fit$root * coefficient
}


# Population and regional methods have two unknowns, F1 and F2.
# Minimize the two squared SPR errors. Accept a result only when both errors
# are close to zero; a least-squares compromise does not meet both targets.
solve_joint_spr <- function(p, target, type = "population", origin_weights = p$R0) {
  differences <- function(F) {
    values <- spr_ratios(F, p, regional_weights = origin_weights)
    if (type == "regional") values$regional - target else values$population - target
  }
  objective <- function(log_F) sum(differences(exp(log_F))^2)
  # Try several starts because a joint numerical search can fail from one start.
  # Acceptance requires both SPR errors to be small; it does not prove uniqueness.
  starts <- list(c(.2, .2), c(.02, .02), c(.5, .01), c(.01, .5), c(1, 1))
  for (start in starts) {
    fit <- nlminb(log(start), objective,
      lower = rep(log(1e-10), 2), upper = rep(log(10), 2),
      control = list(abs.tol = 1e-28, rel.tol = 1e-15, x.tol = 1e-12,
                     iter.max = 1000, eval.max = 2000))
    F <- exp(fit$par)  # log_F keeps both fishing mortalities positive
    if (max(abs(differences(F))) < 1e-10) return(F)
  }
  stop("No accepted joint solution within 0 < F1, F2 <= 10")
}


# Compare several SPR methods in one table.
# The wrappers above call this same function for a single method.
# p: operating-model parameters created in parameters.R.
# survey_weights / catch_weights: historical regional proportions, when needed.
# target: SPR as a fraction (0.40 means 40%).
# rules: names of the methods to calculate; see the list below or README.md.
# With BH, recruitment-dependent targets update Rref until it matches equilibrium.
# Rref is biological recruitment at the candidate F. regional_weights, when
# supplied, instead defines an index mixture and does not change that biology.
# Survey/catch weights are fixed historical shares, assigned by birth-region label.
spr_reference_points <- function(p, survey_weights = NULL, catch_weights = NULL, target = 0.40,
    fishing_pattern = c(0.7, 0.3), regional_weights = NULL,
    rules = c("SpE-RecW", "SpE-EqW", "SpE-SurvW", "SpE-CatchW", "SpD-RecW",
              "Natal-RecW", "Pop-Move", "Reg-Move", "Pop-NoMove")) {
  # regional_weights = NULL uses biological reference recruitment.
  # Otherwise give two fixed birth-origin weights, e.g. c(0.25, 0.75).
  # SpD-EqW, SpD-SurvW and SpD-CatchW are also available in `rules`.
  allowed <- c("SpE-RecW", "SpE-EqW", "SpE-SurvW", "SpE-CatchW", "SpD-RecW",
    "SpD-EqW", "SpD-SurvW", "SpD-CatchW", "Natal-RecW", "Pop-Move", "Reg-Move", "Pop-NoMove")
  if (any(!rules %in% allowed)) stop("Unknown SPR reference-point name")
  if (target <= 0 || target >= 1) stop("Use a target between 0 and 1")
  if (any(fishing_pattern <= 0)) stop("Both fishing-pattern entries must be positive")
  if (!is.null(regional_weights) && any(regional_weights <= 0))
    stop("Use two positive regional origin weights")

  # Make a second model for calculations that deliberately omit movement.
  # Equilibrium biomass, catch and projections will still use the actual model p.
  closed <- p
  closed$movement <- array(rep(diag(2), 4), c(2, 2, 4))
  closed$Phi0 <- per_recruit(c(0, 0), closed)$Phi
  rows <- vector("list", length(rules))

  for (i in seq_along(rules)) {
    rule <- rules[i]
    message("Calculating ", rule)
    answer <- tryCatch({
      if (endsWith(rule, "SurvW") && is.null(survey_weights)) stop("Supply survey_weights for this method")
      if (endsWith(rule, "CatchW") && is.null(catch_weights)) stop("Supply catch_weights for this method")
      model <- if (startsWith(rule, "SpD-") || rule == "Pop-NoMove") closed else p
      global_rule <- startsWith(rule, "SpE-") || startsWith(rule, "SpD-") || rule == "Natal-RecW"
      Rref <- p$R0
      # Fixed-mean recruitment already gives Rref = R0.
      # With BH, only targets depending on Rref need a recruitment iteration.
      iterate_R <- p$recruitment == "BH" &&
        (rule %in% c("SpE-RecW", "SpD-RecW", "Natal-RecW") ||
         (rule == "Reg-Move" && is.null(regional_weights)))
      recruitment_converged <- !iterate_R

      for (iteration in 1:200) {
        # First choose the origin mixture, then solve the selected SPR equation(s).
        weights <- Rref
        if (endsWith(rule, "EqW")) weights <- c(1, 1)
        if (endsWith(rule, "SurvW")) weights <- survey_weights
        if (endsWith(rule, "CatchW")) weights <- catch_weights
        origin_mix <- Rref
        if (!is.null(regional_weights)) origin_mix <- regional_weights

        # These methods differ in what they count and how many equations they solve.
        if (global_rule) {
          F <- solve_global_spr(model, weights, target, fishing_pattern,
                                natal_only = rule == "Natal-RecW")
        } else if (rule == "Reg-Move") {
          F <- solve_joint_spr(model, target, type = "regional", origin_weights = origin_mix)
        } else {
          F <- solve_joint_spr(model, target, type = "population")
        }
        eq <- equilibrium(F, p)  # biological equilibrium always retains actual movement
        # With BH and recruitment weights, F depends on Rref and Rref depends on F.
        # Repeat until those two parts agree. Fixed-weight targets need one solve.
        if (!iterate_R || max(abs(eq$R - Rref) / pmax(Rref, 1e-12)) < 1e-10) {
          recruitment_converged <- TRUE
          break
        }
        Rref <- (Rref + eq$R) / 2  # take a half-step to keep the update stable
      }
      if (!recruitment_converged) stop("Reference recruitment did not converge")
      Rref <- eq$R
      origin_mix <- if (is.null(regional_weights)) Rref else regional_weights
      values <- spr_ratios(F, p, origin_mix, Rref)
      # Report diagnostics in the actual model. A no-movement calculation can
      # therefore have diagnostic SPRs different from its closed-model target.
      data.frame(rule = rule, F1 = F[1], F2 = F[2],
        global_F = if (global_rule) max(colSums(p$selectivity * F)) else NA_real_,
        population_SPR1 = values$population[1], population_SPR2 = values$population[2],
        regional_SPR1 = values$regional[1], regional_SPR2 = values$regional[2],
        global_SPR_RecW = values$global, Rref1 = Rref[1], Rref2 = Rref[2],
        origin_weight1 = origin_mix[1] / sum(origin_mix),
        origin_weight2 = origin_mix[2] / sum(origin_mix),
        SSB1 = eq$ssb[1], SSB2 = eq$ssb[2],
        catch1 = eq$catch[1], catch2 = eq$catch[2], yield = eq$yield,
        status = "calculated", note = "")
    }, error = function(e) {
      # Keep other reference points when a particular target cannot be solved.
      data.frame(rule = rule, F1 = NA_real_, F2 = NA_real_, global_F = NA_real_,
        population_SPR1 = NA_real_, population_SPR2 = NA_real_,
        regional_SPR1 = NA_real_, regional_SPR2 = NA_real_, global_SPR_RecW = NA_real_,
        Rref1 = NA_real_, Rref2 = NA_real_, origin_weight1 = NA_real_, origin_weight2 = NA_real_,
        SSB1 = NA_real_, SSB2 = NA_real_, catch1 = NA_real_, catch2 = NA_real_, yield = NA_real_,
        status = "unavailable", note = conditionMessage(e))
    })
    rows[[i]] <- answer
  }
  do.call(rbind, rows)
}
