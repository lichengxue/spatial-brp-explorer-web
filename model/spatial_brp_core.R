# Two-region, four-season age-structured operating model and reference points.
# N[s,r,a] tracks numbers by birth origin, occupied region and age.
# Mortality precedes each seasonal transfer; spawning is at 0.625 years.
# NH returns fish home before spawning. MPO retains origin biology, while MPD
# uses occupied-region biology. Recruitment is fixed-mean or Beverton-Holt.
# The final age is a plus group. All calculations use base R.
#
# Reading order: make_parameters() -> unfished() -> burn_in() -> reference_points().
# simulate_year() handles one year's biology; advance_one_year() ages its survivors.
# per_recruit() follows a recruit's lifetime; equilibrium() scales that output by
# long-run recruitment. run_model.R calls these functions for the GUI.
# F and M are annual mortality rates; biomass is kg; movement and maturity are
# fractions from 0 to 1. "sr" means Beverton-Holt recruitment in this GUI model.
# The standalone R/ scripts call that same recruitment option "BH".

N_SEASONS     <- 4      # seasons per year
SEASON_LENGTH <- 0.25   # years
SPAWN_SEASON  <- 3      # spawning happens half-way through season 3 (0.625 yr)

# make_parameters() returns a list with everything that defines one operating model.
#   life_history  "short" (10 ages, M = 0.35) or "long" (30 ages, M = 0.10)
#   movement      "high" (30% of region-1 fish move to region 2 each season, 5% back),
#                 "low" (5% each way) or "none"; m12 and m21 override these rates
#   allocation    relative fully selected regional pattern (default 70:30)
#   F_historical  fishing during the burn-in
#   R0            unfished (and mean) recruitment of stock 1 and stock 2
#   alpha         Beverton-Holt slope at the origin (common to both subpopulations)
#   steepness     optional: calibrate each subpopulation to this steepness instead of a common slope
#   recruitment   "mean" or "sr"
#   survey_time   fraction of the year in [0, 1); default 0.625 (season-3 midpoint)
make_parameters <- function(life_history = c("short", "long"),
                            movement = c("high", "low", "none"),
                            m12 = NULL, m21 = NULL,
                            allocation = c(1.4, 0.6),
                            F_historical = c(0.28, 0.12),
                            R0 = c(700, 300),
                            alpha = 12,
                            region2_Linf = 80,
                            region2_maturity_shift = NULL,
                            recruitment = "mean",
                            survey_time = 0.625,
                            survey_q = 0.1,
                            steepness = NULL) {
  life_history <- match.arg(life_history)
  movement <- match.arg(movement)
  recruitment <- match.arg(recruitment, c("mean", "sr"))

  # ---- Check the inputs ----
  if (length(R0) != 2 || any(!is.finite(R0)) || any(R0 <= 0))
    stop("R0 must contain two finite positive values")
  if (length(allocation) != 2 || any(!is.finite(allocation)) || any(allocation <= 0))
    stop("allocation must contain two finite positive values")
  R0 <- as.numeric(R0)
  allocation <- as.numeric(allocation)
  if (!is.null(steepness)) {
    steepness <- rep_len(steepness, 2)
    if (any(!is.finite(steepness)) || any(steepness <= 0.2 | steepness >= 1))
      stop("steepness must be strictly between 0.2 and 1")
  }
  if (length(survey_q) != 1L || !is.finite(survey_q) || survey_q <= 0)
    stop("survey_q must be one finite positive value")
  if (length(survey_time) != 1L || !is.finite(survey_time) || survey_time < 0 || survey_time >= 1)
    stop("survey_time must be one finite fraction of the year in [0, 1)")
  # ---- Life history ----
  if (life_history == "short") {
    n_ages <- 10              # ages 1 to 10; age 10 is the plus group
    M <- 0.35                 # natural mortality (per year)
    k <- 0.27                 # von Bertalanffy growth rate
    selectivity_a50 <- 2.5    # age at 50% selectivity
    maturity_a50 <- 1.75      # age at 50% maturity (region-1 biology)
    if (is.null(region2_maturity_shift)) region2_maturity_shift <- 1
  } else {
    n_ages <- 30
    M <- 0.10
    k <- 0.07
    selectivity_a50 <- 6
    maturity_a50 <- 7
    if (is.null(region2_maturity_shift)) region2_maturity_shift <- 2
  }
  ages <- 1:n_ages

  # Length, weight and maturity at age for the two biology types.
  # Column 1: region-1 biology. Column 2: region-2 biology (smaller, matures later).
  Linf <- c(90, region2_Linf)
  length_at_age <- cbind(Linf[1] * (1 - exp(-k * ages)),                 # whamMSE Generate_Len
                         Linf[2] * (1 - exp(-k * ages)))
  weight_at_age <- 3e-6 * length_at_age^3                                 # kg
  maturity_at_age <- cbind(1 / (1 + exp(-(ages - maturity_a50))),
                           1 / (1 + exp(-(ages - (maturity_a50 + region2_maturity_shift)))))
  selectivity <- 1 / (1 + exp(-(ages - selectivity_a50)))                # logistic, scale 1

  # One fleet in each region. The GUI can replace either selectivity vector.
  fishing_selectivity <- rbind(selectivity, selectivity)

  # ---- Movement at the end of each season ----
  if (is.null(m12)) m12 <- switch(movement, high = 0.30, low = 0.05, none = 0)   # region 1 -> 2
  if (is.null(m21)) m21 <- switch(movement, high = 0.05, low = 0.05, none = 0)   # region 2 -> 1
  if (any(!is.finite(c(m12, m21))) || any(c(m12, m21) < 0 | c(m12, m21) > 1))
    stop("movement probabilities must lie between zero and one")
  mu <- rbind(c(1 - m12, m12),      # a fish in region 1: stays, or moves to region 2
              c(m21, 1 - m21))      # a fish in region 2: moves to region 1, or stays

  list(life_history = life_history, movement = movement,
       n_ages = n_ages, ages = ages, M = M,
       selectivity = selectivity,                   # survey selectivity
       fishing_selectivity = fishing_selectivity,   # [fleet, age]
       length = length_at_age, weight = weight_at_age, maturity = maturity_at_age,
       spawn_output = weight_at_age * maturity_at_age,   # spawning biomass per fish (kg)
       mu = mu, m12 = m12, m21 = m21,
       allocation = allocation, F_historical = F_historical,
       R0 = R0, alpha = alpha, steepness = steepness,
       recruitment = recruitment,
       survey_time = survey_time, survey_q = survey_q,
       survey_season = floor(survey_time / SEASON_LENGTH) + 1L,
       survey_offset = survey_time %% SEASON_LENGTH)
}


# Normalize the fishing pattern so max_age(F1_at_age + F2_at_age) = global F.
# allocation is proportional to the reference regional fully selected F.
# Its arbitrary common scale cancels when the fleet-age pattern is normalized.
# It is not the WHAM-normalized pi or a reported global F.
wham_fishing_pattern <- function(p, F_reference = p$allocation) {
  if (length(F_reference) != 2L || any(!is.finite(F_reference)) ||
      any(F_reference < 0) || sum(F_reference) <= 0)
    stop("F_reference must contain two nonnegative finite values, at least one positive")
  FAA <- sweep(p$fishing_selectivity, 1L, F_reference, "*")
  F_reference_total <- max(colSums(FAA))
  list(coefficient = F_reference / F_reference_total,
       normalized_FAA = FAA / F_reference_total,
       F_reference_total = F_reference_total,
       age = which.max(colSums(FAA)))
}

# Population structures
# natal_homing      TRUE if every fish returns to its natal region to spawn
# biology           "origin": a spawner keeps the biology of its stock;
#                   "destination": it takes the biology of the region where it spawns
# recruitment_unit  what produces recruits under Beverton-Holt recruitment:
#                   "stock"  (natal homing: each stock's own spawners),
#                   "region" (metapopulation: all spawners in a region)
structure_info <- function(struct) {
  switch(struct,
         NH  = list(natal_homing = TRUE,  biology = "origin",      recruitment_unit = "stock"),
         MPO = list(natal_homing = FALSE, biology = "origin",      recruitment_unit = "region"),
         MPD = list(natal_homing = FALSE, biology = "destination", recruitment_unit = "region"),
         stop("unknown structure: ", struct))
}


# Survival, movement and biology per fish

# Fraction of fish of each age that survive a period of `duration` years in each
# region. Returns a matrix [region, age]. Total mortality Z = M + F * selectivity,
# with the selectivity of the fishing in that region.
survival <- function(F_by_fleet, p, duration) {
  exp(-duration * (p$M + fishing_at_age(F_by_fleet, p)))
}

# Fishing mortality at age in each region: F of the region times its selectivity.
# Returns a matrix [region, age].
fishing_at_age <- function(F_by_fleet, p) {
  F_by_fleet * p$fishing_selectivity      # row r is multiplied by F_by_fleet[r]
}

# The four end-of-season movements for fish of stock s. Each one is a matrix that
# moves numbers between regions:  numbers_after = move %*% numbers_before.
# This needs move[to, from], which is the transpose of mu[from, to].
# Under natal homing the end-of-season-2 movement sends every fish to region s.
movement_matrices <- function(s, p, natal_homing) {
  # Presets reuse p$mu each season; a custom schedule supplies mu_by_season.
  # Natal return replaces season 2's transfer, regardless of the ordinary rates.
  moves <- vector("list", N_SEASONS)
  for (season in 1:N_SEASONS) {
    mu <- if (is.null(p$mu_by_season)) p$mu else p$mu_by_season[, , season]
    moves[[season]] <- t(mu)  # rows: destination; columns: departure region
  }
  if (natal_homing) {
    home <- matrix(0, 2, 2)
    home[s, ] <- 1
    moves[[2]] <- home
  }
  moves
}

# Spawning biomass per fish of stock s that spawns in region 1 or region 2.
# Returns a matrix [age, region where it spawns].
spawning_output_per_fish <- function(s, p, biology) {
  if (biology == "origin") {
    cbind(p$spawn_output[, s], p$spawn_output[, s])   # keeps its own stock's biology
  } else {
    p$spawn_output                                     # takes the biology of the region
  }
}

# Body weight per fish of stock s when it is in region 1 or region 2 (for catch
# and survey biomass). Returns a matrix [age, region].
weight_per_fish <- function(s, p, biology) {
  if (biology == "origin") {
    cbind(p$weight[, s], p$weight[, s])
  } else {
    p$weight
  }
}


# Spawning biomass per recruit (phi)
# Follows one recruit of each stock through its whole life at fishing F_by_fleet
# and records where (and how much) it spawns and how much of it is caught.
#
# Returns a list with
#   Phi[s, r]                  spawning biomass in region r per recruit of stock s
#   YPR[s]                     yield (catch biomass) per recruit of stock s
#   numbers_start[s, r, a]     numbers per recruit at the start of age a, by region
#
# Each year of life is a chain of season steps. Written as matrices (read the
# products from right to left):
#   start of year -> spawning:
#     survive season 1, move, survive season 2, move (or return home), survive half of season 3
#   start of year -> start of next year:
#     ... then survive the rest of season 3, move, survive season 4, move
# The last age is a plus group: it holds the fish of that age and all older ages,
#   n + whole_year %*% n + whole_year^2 %*% n + ... = (I - whole_year)^(-1) %*% n
per_recruit <- function(F_by_fleet, p, struct) {
  info <- structure_info(struct)
  n_ages <- p$n_ages

  surv_season <- survival(F_by_fleet, p, SEASON_LENGTH)       # [region, age]
  surv_half   <- survival(F_by_fleet, p, SEASON_LENGTH / 2)
  # fraction of the fish alive at the start of a season that is caught during it (Baranov)
  F_at_age <- fishing_at_age(F_by_fleet, p)
  Z_at_age <- p$M + F_at_age

  Phi <- matrix(0, 2, 2, dimnames = list(stock = 1:2, region = 1:2))
  YPR <- c(0, 0)
  numbers_start <- array(0, c(2, 2, n_ages))

  for (s in 1:2) {
    move <- movement_matrices(s, p, info$natal_homing)
    spawn_output <- spawning_output_per_fish(s, p, info$biology)   # [age, region]
    weight <- weight_per_fish(s, p, info$biology)                  # [age, region]

    n <- c(0, 0)
    n[s] <- 1                          # one recruit, at age 1, in its natal region

    for (a in 1:n_ages) {
      S_season <- diag(surv_season[, a])     # survival over one season, by region
      S_half   <- diag(surv_half[, a])       # survival over half a season, by region

      to_spawning <- S_half %*% move[[2]] %*% S_season %*% move[[1]] %*% S_season
      whole_year  <- move[[4]] %*% S_season %*% move[[3]] %*% S_half %*% to_spawning

      if (a == n_ages) n <- as.numeric(solve(diag(2) - whole_year, n))   # plus group

      numbers_start[s, , a] <- n

      # Spawning: numbers alive at spawning, by region, times spawning output per fish
      at_spawning <- as.numeric(to_spawning %*% n)
      Phi[s, ] <- Phi[s, ] + at_spawning * spawn_output[a, ]

      # Catch: in every season, the Baranov fraction of the fish alive at its start
      catch_fraction <- (F_at_age[, a] / Z_at_age[, a]) * (1 - surv_season[, a])
      x <- n
      for (season in 1:N_SEASONS) {
        YPR[s] <- YPR[s] + sum(x * catch_fraction * weight[a, ])
        x <- as.numeric(move[[season]] %*% (x * surv_season[, a]))
      }

      n <- as.numeric(whole_year %*% n)      # survivors start the next age
    }
  }
  list(Phi = Phi, YPR = YPR, numbers_start = numbers_start)
}


# SPR statistics

# Lifetime spawning biomass per recruit of each stock, wherever it spawns: phi_s
stock_sbpr <- function(Phi) rowSums(Phi)

# Spawning biomass per recruit of each stock in its natal region only: Phi_{s,s}
# (the quantity WHAM 2.0 uses)
natal_sbpr <- function(Phi) diag(Phi)

# Spawning biomass in each region produced by recruitment R (by stock):
#   SSB_r = R_1 * Phi[1, r] + R_2 * Phi[2, r]
spawning_by_region <- function(Phi, R) as.numeric(R %*% Phi)

# Regional SPR: spawning biomass in each region at F relative to unfished,
# both produced by the reference recruitment Rbar
regional_spr <- function(Phi, Phi0, Rbar) {
  spawning_by_region(Phi, Rbar) / spawning_by_region(Phi0, Rbar)
}

# Global SPR: weighted sum of the stocks' spawning per recruit at F, over
# the same weighted sum unfished. phi_F and phi_0 are per-stock SBPR values.
global_spr <- function(phi_F, phi_0, weights) {
  sum(weights * phi_F) / sum(weights * phi_0)
}

# Unfished state and recruitment
# Returns
#   Phi0     Phi with no fishing
#   S0       unfished spawning biomass of each subpopulation
#   alpha    Beverton-Holt slope of each subpopulation
#   beta     Beverton-Holt density dependence of each subpopulation (NA under mean recruitment)
#   h        steepness of each subpopulation (NA under mean recruitment)
#   pr0      the full per_recruit() result with no fishing
unfished <- function(p, struct) {
  info <- structure_info(struct)
  pr0 <- per_recruit(c(0, 0), p, struct)
  Phi0 <- pr0$Phi

  # Unfished spawning biomass of each subpopulation
  if (info$recruitment_unit == "stock") {
    S0 <- diag(Phi0) * p$R0                   # each stock spawns in its natal region
  } else {
    S0 <- spawning_by_region(Phi0, p$R0)      # spawning biomass in each region
  }
  alpha <- rep_len(p$alpha, 2)

  # Mean recruitment: no stock-recruitment parameters are needed
  if (p$recruitment == "mean") {
    return(list(Phi0 = Phi0, S0 = S0, alpha = alpha, beta = c(NA, NA), h = c(NA, NA), pr0 = pr0))
  }

  # Beverton-Holt with a given steepness h for each subpopulation:
  #   alpha = 4 h R0 / ((1 - h) S0),   beta = (5h - 1) / ((1 - h) S0)
  if (!is.null(p$steepness)) {
    h <- p$steepness
    alpha <- 4 * h * p$R0 / ((1 - h) * S0)
    beta <- (5 * h - 1) / ((1 - h) * S0)
    return(list(Phi0 = Phi0, S0 = S0, alpha = alpha, beta = beta, h = h, pr0 = pr0))
  }

  # Beverton-Holt with a common slope alpha (base design): beta is chosen so that
  # the unfished spawning biomass S0 produces exactly R0,
  #   R0 = alpha S0 / (1 + beta S0)   =>   beta = (alpha S0 - R0) / (R0 S0)
  # and the implied steepness is h = x / (4 + x) with x = alpha S0 / R0
  beta <- (p$alpha * S0 - p$R0) / (p$R0 * S0)
  x <- p$alpha * S0 / p$R0
  if (any(beta <= 0)) stop("unfished equilibrium infeasible for this alpha")
  h <- x / (4 + x)
  list(Phi0 = Phi0, S0 = S0, alpha = alpha, beta = beta, h = h, pr0 = pr0)
}

# Equilibrium at a given fishing level
# Long-run state when the population is fished at F_by_fleet every year.
# Returns
#   R           equilibrium recruitment of each stock
#   ssb_unit    spawning biomass of each subpopulation (stock under NH, region otherwise)
#   ssb_region  spawning biomass in each region
#   depletion   ssb_unit / S0
#   yield       equilibrium catch biomass, sum over stocks of R_s * YPR_s
#   Phi, converged, iterations
equilibrium <- function(F_by_fleet, p, struct, uf = unfished(p, struct), tol = 1e-13, maxit = 1e6) {
  pr <- per_recruit(F_by_fleet, p, struct)
  Phi <- pr$Phi
  info <- structure_info(struct)
  alpha <- uf$alpha
  converged <- TRUE
  iterations <- 0L

  if (p$recruitment == "mean") {
    # Recruitment is fixed at R0
    R <- p$R0
    ssb_region <- spawning_by_region(Phi, R)
    if (info$recruitment_unit == "stock") ssb_unit <- diag(Phi) * R else ssb_unit <- ssb_region

  } else if (info$recruitment_unit == "stock") {
    # Natal homing: each stock is a closed loop, SSB = R * Phi_{s,s}, so the
    # Beverton-Holt equilibrium has a closed form (zero if the stock cannot persist)
    phi <- diag(Phi)
    R <- pmax(0, (alpha * phi - 1) / (uf$beta * phi))
    ssb_unit <- R * phi
    ssb_region <- spawning_by_region(Phi, R)

  } else {
    # Metapopulation: the regions feed each other's recruitment, so iterate
    #   SSB_r = sum_s R_s Phi[s, r],   R_r = alpha_r SSB_r / (1 + beta_r SSB_r)
    # from R0 until recruitment stops changing
    converged <- FALSE
    R <- p$R0
    for (iterations in 1:maxit) {
      S <- spawning_by_region(Phi, R)
      R_new <- alpha * S / (1 + uf$beta * S)
      if (max(abs(R_new - R)) < tol * max(1, max(R))) {
        R <- R_new
        converged <- TRUE
        break
      }
      R <- R_new
    }
    ssb_region <- spawning_by_region(Phi, R)
    ssb_unit <- ssb_region
  }

  if (!converged) stop(sprintf("equilibrium failed to converge after %d iterations (%s)", maxit, struct))
  list(R = R, ssb_unit = ssb_unit, ssb_region = ssb_region, depletion = ssb_unit / uf$S0,
       yield = sum(R * pr$YPR), Phi = Phi, converged = converged, iterations = iterations)
}


# Equation solvers
# Every reference point solves SPR(F) = target: one equation for a global F40%
# (the unknown is the multiplier m of the allocation) or two equations for the
# regional and population-specific F40% (the unknowns are F1 and F2).
# Global targets use uniroot(). Joint targets use nlminb() to minimize
# the two squared residuals over log F. Accept only residuals below SOLVE_TOL.
# Otherwise report the target as unavailable. This does not prove nonexistence
# or establish uniqueness.
NLMINB_CONTROL <- list(abs.tol = 1e-28, rel.tol = 1e-15, x.tol = 1e-12, iter.max = 1000, eval.max = 2000)
SOLVE_TOL <- 1e-10

# residuals: function of log F returning the residual of each equation
# start:     starting value(s) of F (not log F)
# Returns list(f = solution, resid = residuals at the solution, converged = TRUE/FALSE)
solve_equations <- function(residuals, start, lower = log(1e-8), upper = log(10)) {
  fit <- nlminb(log(start), function(log_F) sum(residuals(log_F)^2),
                lower = lower, upper = upper, control = NLMINB_CONTROL)
  res <- residuals(fit$par)
  list(f = exp(fit$par), resid = res, converged = max(abs(res)) < SOLVE_TOL)
}

# One equation in the multiplier m: spr_of_m(m) = target.
# Returns m, or NA if no multiplier up to `upper` reaches the target.
solve_multiplier <- function(spr_of_m, target = 0.4, upper = 10) {
  difference <- function(m) spr_of_m(m) - target
  if (difference(upper) > 0) return(NA_real_)
  uniroot(difference, c(0, upper), tol = 1e-11)$root
}

# Regional F40%: F1 and F2 such that the regional SPR of both regions
# equals the target. movement_off = TRUE solves it with movement set to zero.
solve_regional_F <- function(p, struct, Rbar, target = 0.4, start = c(0.2, 0.2), movement_off = FALSE) {
  p_used <- p
  if (movement_off) {
    p_used$mu <- diag(2)
    p_used$mu_by_season <- NULL
  }
  Phi0 <- per_recruit(c(0, 0), p_used, struct)$Phi
  residuals <- function(log_F) {
    Phi <- per_recruit(exp(log_F), p_used, struct)$Phi
    regional_spr(Phi, Phi0, Rbar) - target
  }
  result <- solve_equations(residuals, start)
  result$Phi0 <- Phi0
  result
}

# Population-specific F40%: F1 and F2 such that the lifetime SPR of both stocks (all
# spawning regions, movement retained) equals the target. Equal to the regional
# F40% under natal homing and without movement; need not exist otherwise.
solve_stock_F <- function(p, struct, target = 0.4, start = c(0.2, 0.2)) {
  phi0 <- stock_sbpr(per_recruit(c(0, 0), p, struct)$Phi)
  residuals <- function(log_F) {
    Phi <- per_recruit(exp(log_F), p, struct)$Phi
    stock_sbpr(Phi) / phi0 - target
  }
  solve_equations(residuals, start)
}

# Reference-point rules
# Reference points for one operating model:
#   SpE-RecW, SpE-EqW, SpE-SurvW, SpE-CatchW
#                 global F40% along the allocation, movement retained, stock SBPR
#                 summed over all spawning regions; recruitment, equal, survey or
#                 catch weights
#   SpD-*        the same four global weightings with movement excluded
#   Natal-RecW    global F40%, natal-region spawning only (WHAM 2.0); metapopulations
#                 only (under natal homing it equals SpE-RecW)
#   Pop-NoMove  separate population F40%: each stock's F40% as a closed population,
#                 applied to the fleet in its natal region (equal to the regional
#                 F40% with movement set to zero)
#   Pop-Move    population-specific F40% with movement retained (omitted where no
#                 solution exists)
#   Reg-Move      regional F40% with movement retained
# Inputs
#   Rbar          reference recruitment by stock
#   survey_share  survey index shares by region (weights for SpE-SurvW)
#   catch_share   catch shares by region (weights for SpE-CatchW; NULL omits it)
#   which         NULL for all rules, or the names of the rules wanted
# Returns one row per rule: F1, F2, multiplier, rule type and biomass target(s):
#   Btgt          global SSB target (global rules)
#   Btgt1, Btgt2  SSB target of each region (regional rules) or stock (stock rule)
reference_points <- function(p, struct, Rbar, survey_share, target = 0.4, uf = unfished(p, struct),
                             which = NULL, catch_share = NULL) {
  info <- structure_info(struct)
  wanted <- function(rule) is.null(which) || rule %in% which

  # Phi at the WHAM global F, with movement (p) or without (p_no_move).
  fishing_pattern <- wham_fishing_pattern(p)
  p_no_move <- p
  # A no-movement BRP changes this calculation copy only. Clear both forms of
  # movement so a custom seasonal schedule cannot remain active by accident.
  p_no_move$mu <- diag(2)
  p_no_move$mu_by_season <- NULL
  Phi_at <- function(m, pars = p) per_recruit(m * fishing_pattern$coefficient, pars, struct)$Phi
  Phi0 <- uf$Phi0
  Phi0_no_move <- per_recruit(c(0, 0), p_no_move, struct)$Phi
  recruitment_weights <- Rbar / sum(Rbar)
  # Weight ratios determine the SPR equation. Rbar in absolute recruits/year
  # separately converts per-recruit spawning into biomass targets for feedback.

  # The two-equation rules try these starting values in turn until one is accepted
  starts <- list(c(0.2, 0.2), c(0.02, 0.02), c(0.5, 0.01), c(0.01, 0.5), c(1, 1))
  solve_from_starts <- function(solver) {
    for (start in starts) {
      result <- solver(start)
      if (result$converged) break
    }
    result
  }

  # One output row; stops if the rule has no accepted solution
  make_row <- function(rule, F_by_fleet, Btgt, type = "global", Btgt_unit = c(NA, NA), converged = TRUE) {
    if (!isTRUE(converged) || any(!is.finite(F_by_fleet)))
      stop(sprintf("reference point %s has no accepted solution within the stated bounds (%s)", rule, struct))
    data.frame(rule = rule, F1 = F_by_fleet[1], F2 = F_by_fleet[2], mult = F_by_fleet[1] / p$allocation[1],
               Btgt = Btgt, Btgt1 = Btgt_unit[1], Btgt2 = Btgt_unit[2], type = type, converged = converged,
               F_global = if (type == "global") max(colSums(fishing_at_age(F_by_fleet, p))) else NA_real_)
  }
  rows <- list()

  # Global methods share one calculation. Only movement and weights differ.
  global_weights <- list(RecW = recruitment_weights, EqW = c(0.5, 0.5),
                         SurvW = survey_share, CatchW = catch_share)
  for (prefix in c("SpE", "SpD")) {
    pars <- if (prefix == "SpE") p else p_no_move
    baseline <- if (prefix == "SpE") Phi0 else Phi0_no_move
    for (weight_name in names(global_weights)) {
      rule <- paste(prefix, weight_name, sep = "-")
      weights <- global_weights[[weight_name]]
      if (!wanted(rule) || is.null(weights)) next
      m <- solve_multiplier(function(m)
        global_spr(stock_sbpr(Phi_at(m, pars)), stock_sbpr(baseline), weights), target)
      rows[[length(rows) + 1]] <- make_row(rule, m * fishing_pattern$coefficient,
        Btgt = sum(Rbar * stock_sbpr(Phi_at(m, pars))))
    }
  }

  # ---- Global F40%, natal-region spawning only (metapopulations) ----
  if (info$recruitment_unit == "region" && wanted("Natal-RecW")) {
    m <- solve_multiplier(function(m) global_spr(natal_sbpr(Phi_at(m)), natal_sbpr(Phi0), recruitment_weights), target)
    rows[[length(rows) + 1]] <- make_row("Natal-RecW", m * fishing_pattern$coefficient,
                                         Btgt = sum(Rbar * natal_sbpr(Phi_at(m))))
  }

  # ---- Separate population F40% (movement excluded) ----
  # Stock s spawns and recruits in region s, so this is the regional F40% with no movement
  if (wanted("Pop-NoMove")) {
    sol <- solve_from_starts(function(start) solve_regional_F(p, struct, Rbar, target, start, movement_off = TRUE))
    Phi_sol <- per_recruit(sol$f, p_no_move, struct)$Phi
    rows[[length(rows) + 1]] <- make_row("Pop-NoMove", sol$f, Btgt = NA, type = "regional",
                                         Btgt_unit = spawning_by_region(Phi_sol, Rbar), converged = sol$converged)
  }

  # ---- Population-specific F40%, movement retained (only where a solution exists) ----
  if (wanted("Pop-Move")) {
    sol <- solve_from_starts(function(start) solve_stock_F(p, struct, target, start))
    if (sol$converged) {
      Phi_sol <- per_recruit(sol$f, p, struct)$Phi
      rows[[length(rows) + 1]] <- make_row("Pop-Move", sol$f, Btgt = NA, type = "stock",
                                           Btgt_unit = Rbar * stock_sbpr(Phi_sol))
    }
  }

  # ---- Regional F40%, movement retained ----
  if (wanted("Reg-Move")) {
    sol <- solve_from_starts(function(start) solve_regional_F(p, struct, Rbar, target, start, movement_off = FALSE))
    Phi_sol <- per_recruit(sol$f, p, struct)$Phi
    rows[[length(rows) + 1]] <- make_row("Reg-Move", sol$f, Btgt = NA, type = "regional",
                                         Btgt_unit = spawning_by_region(Phi_sol, Rbar), converged = sol$converged)
  }

  if (length(rows) == 0) return(NULL)
  do.call(rbind, rows)
}

# Reference points whose reference recruitment matches the operating model.
# For each rule: compute the reference point with reference recruitment Rbar, find
# the equilibrium recruitment the operating model reaches when fished at it, move
# Rbar half-way towards that recruitment, and repeat until the two agree.
# Weights, regional SPR and biomass targets then match the equilibrium the
# operating model reaches at each rule's target. Under mean recruitment the
# answer is Rbar = R0 and the result equals reference_points() at Rbar = R0.
# Adds columns Rref1, Rref2 (final reference recruitment), fp_iter, fp_converged
# and fp_residual.
reference_points_consistent <- function(p, struct, Rbar, survey_share, target = 0.4, uf = unfished(p, struct),
                                        tol = 1e-10, maxit = 200, which = NULL, catch_share = NULL) {
  first_pass <- reference_points(p, struct, Rbar, survey_share, target, uf, which = which, catch_share = catch_share)
  if (is.null(first_pass)) return(NULL)

  rows <- list()
  for (rule in first_pass$rule) {
    R_ref <- Rbar
    converged <- FALSE
    residual <- Inf
    for (iteration in 1:maxit) {
      rp <- reference_points(p, struct, R_ref, survey_share, target, uf, which = rule, catch_share = catch_share)
      if (is.null(rp) || nrow(rp) != 1)
        stop(sprintf("reference point %s disappeared during recruitment iteration", rule))
      R_eq <- equilibrium(c(rp$F1, rp$F2), p, struct, uf)$R
      residual <- max(abs(R_eq - R_ref) / pmax(abs(R_ref), 1e-12))
      if (residual < tol) {
        converged <- TRUE
        break
      }
      R_ref <- 0.5 * R_ref + 0.5 * R_eq       # damped update
    }
    if (!converged)
      stop(sprintf("reference recruitment failed to converge for %s/%s after %d iterations (residual %.3g)",
                   struct, rule, maxit, residual))
    rp$Rref1 <- R_ref[1]
    rp$Rref2 <- R_ref[2]
    rp$fp_iter <- iteration
    rp$fp_converged <- converged
    rp$fp_residual <- residual
    rows[[length(rows) + 1]] <- rp
  }
  do.call(rbind, rows)
}


# Year-by-year simulator
# N[s, r, a]: numbers of stock s in region r at age a at the start of the year.
# Runs the four seasons at fishing F_by_fleet and returns
#   numbers_end[s, r, a]  numbers at the end of the year (before ageing)
#   catch[f]              catch biomass of the fleet in region f
#   survey[r]             survey index biomass in region r (commercial selectivity, q = 0.1)
#   ssb_region[r]         spawning biomass in region r (all stocks)
#   ssb_stock[s]          spawning biomass of stock s (all regions)
#   ssb_natal[s]          spawning biomass of stock s in its natal region (WHAM 2.0)
simulate_year <- function(N, F_by_fleet, p, struct) {
  info <- structure_info(struct)
  n_ages <- p$n_ages

  surv_season <- survival(F_by_fleet, p, SEASON_LENGTH)        # [region, age]
  surv_half   <- survival(F_by_fleet, p, SEASON_LENGTH / 2)
  surv_to_survey <- survival(F_by_fleet, p, p$survey_offset)
  F_at_age <- fishing_at_age(F_by_fleet, p)
  Z_at_age <- p$M + F_at_age
  catch_fraction <- (F_at_age / Z_at_age) * (1 - surv_season)  # Baranov, per season
  survey_selectivity <- matrix(p$selectivity, 2, n_ages, byrow = TRUE)   # commercial selectivity, q = 0.1

  numbers_end <- array(0, c(2, 2, n_ages))
  catch <- c(0, 0)
  survey <- c(0, 0)
  ssb_region <- c(0, 0)
  ssb_stock <- c(0, 0)
  ssb_natal <- c(0, 0)

  for (s in 1:2) {
    move <- movement_matrices(s, p, info$natal_homing)
    spawn_output <- t(spawning_output_per_fish(s, p, info$biology))   # [region, age]
    weight <- t(weight_per_fish(s, p, info$biology))                  # [region, age]
    X <- N[s, , ]                                                     # [region, age]

    for (season in 1:N_SEASONS) {
      # Survey after survival up to its within-season time; movement is later.
      if (season == p$survey_season)
        survey <- survey + p$survey_q * rowSums(X * surv_to_survey * survey_selectivity * weight)
      # catch during the season
      catch <- catch + rowSums(X * catch_fraction * weight)
      # spawning half-way through season 3
      if (season == SPAWN_SEASON) {
        spawning <- rowSums(X * surv_half * spawn_output)              # by region
        ssb_region <- ssb_region + spawning
        ssb_stock[s] <- sum(spawning)
        ssb_natal[s] <- spawning[s]
      }
      # survive the season, then move
      X <- move[[season]] %*% (X * surv_season)
    }
    numbers_end[s, , ] <- X
  }
  list(numbers_end = numbers_end, catch = catch, survey = survey,
       ssb_region = ssb_region, ssb_stock = ssb_stock, ssb_natal = ssb_natal)
}

# Recruitment produced by this year's spawning.
next_recruitment <- function(year, p, struct, uf) {
  info <- structure_info(struct)
  alpha <- uf$alpha

  if (p$recruitment == "mean") {
    return(p$R0)
  }
  if (info$recruitment_unit == "stock") {           # each stock from its own spawners
    S <- year$ssb_stock
    return(alpha * S / (1 + uf$beta * S))
  }
  S <- year$ssb_region                            # each region from the spawners in it
  alpha * S / (1 + uf$beta * S)
}

# Start of next year: every fish is one year older, the plus group keeps its
# survivors, and the recruits of stock s enter age 1 in region s
advance_one_year <- function(numbers_end, R) {
  n_ages <- dim(numbers_end)[3]
  N <- array(0, dim(numbers_end))
  N[, , 2:n_ages] <- numbers_end[, , 1:(n_ages - 1)]
  N[, , n_ages] <- N[, , n_ages] + numbers_end[, , n_ages]
  N[1, 1, 1] <- R[1]
  N[2, 2, 1] <- R[2]
  N
}

# Numbers at age at the start of the year in equilibrium: numbers per recruit times recruitment
equilibrium_numbers <- function(pr, R, n_ages) {
  N <- array(0, c(2, 2, n_ages))
  for (s in 1:2) N[s, , ] <- pr$numbers_start[s, , ] * R[s]
  N
}


# Burn-in (conditioning)
# Starts from a declining age profile in the natal regions.
initial_numbers <- function(p, initial_F = 0.20) {
  # Generic survival profile, independent of the spatial equilibrium solution.
  # All initial fish are in their natal region. The final age includes older fish.
  N <- array(0, c(2, 2, p$n_ages))
  Z <- p$M + initial_F
  for (s in 1:2) {
    n <- p$R0[s] * exp(-Z * (p$ages - 1))
    n[p$n_ages] <- n[p$n_ages] / (1 - exp(-Z))
    N[s, s, ] <- n
  }
  N
}

burn_in <- function(p, struct, uf = unfished(p, struct), F_hist = p$F_historical,
                    n_years = 100) {
  if (length(n_years) != 1 || !is.finite(n_years) || n_years < 1 || n_years %% 1 != 0)
    stop("historical duration must be a positive whole number")
  N <- initial_numbers(p)
  recruitment_history <- matrix(NA, n_years, 2)

  for (t in 1:n_years) {
    N_last <- N
    year <- simulate_year(N, F_hist, p, struct)
    R <- next_recruitment(year, p, struct, uf)
    recruitment_history[t, ] <- R
    N <- advance_one_year(year$numbers_end, R)
  }

  # The GUI reports how close this finite history is to equilibrium.
  eq <- equilibrium(F_hist, p, struct, uf)
  N_eq <- equilibrium_numbers(per_recruit(F_hist, p, struct), eq$R, p$n_ages)
  present <- N_eq > 1e-8
  # N starts the projection; N_last starts the last historical year's surveys.
  # last_year holds that year's observations. Rbar averages recruitment from
  # the last five historical years and seeds the BH reference-recruitment iteration.
  list(N = N, N_last = N_last, last_year = year,
       Rbar = colMeans(tail(recruitment_history, 5)),
       max_rel_diff_analytic = max(abs(N - N_eq)[present] / N_eq[present]),
       equilibrium = eq)
}


# Harvest control rule and projections

# Linear ramp: fishing is scaled down in proportion to SSB / target below the
# target, and is at the target F above it
hcr_multiplier <- function(status) pmin(1, pmax(0, status))
