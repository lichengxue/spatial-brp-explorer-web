# Two birth origins, two regions, one fleet per region, four seasons.
# N[origin, region, age] contains numbers at the start of a year.
# Movement matrices have rows = region of departure, columns = destination.
# p is the parameter list from parameters.R. F = c(F1, F2) is annual fishing
# mortality for the two fleets. All four functions below use the same model.
# F and natural mortality M are rates per year; weight is kg per fish.
# Maturity, selectivity and movement probabilities are fractions from 0 to 1.
# Origins track where fish were recruited. Under a metapopulation, recruitment
# is produced by all spawners in a region, regardless of their birth origin.


# Run one year, without ageing fish or adding next year's recruits.
# Returns survivors, spawning biomass and catch by origin/region, and surveys.
simulate_year <- function(N, F, p) {
  # Convert annual mortality into survival and catch over one quarter-year.
  # The Baranov catch fraction is F_at_age / Z * (1 - exp(-Z * duration)).
  fishing <- matrix(0, 2, p$n_ages)
  for (r in 1:2) fishing[r, ] <- F[r] * p$selectivity[r, ]
  Z <- p$M + fishing
  survival <- exp(-0.25 * Z)
  catch_fraction <- fishing / Z * (1 - survival)
  survey_selectivity <- matrix(p$survey_selectivity, 2, p$n_ages, byrow = TRUE)
  spawning <- catch <- matrix(0, 2, 2)  # rows: origin; columns: region
  surveys <- matrix(0, 4, 2)            # rows: season; columns: region
  survivors <- N * 0

  for (s in 1:2) {
    # NH and MPO retain birth-origin biology; MPD uses occupied-region biology.
    if (p$structure == "MPD") {
      weight <- t(p$weight)
      maturity <- t(p$maturity)
    } else {
      weight <- matrix(p$weight[, s], 2, p$n_ages, byrow = TRUE)
      maturity <- matrix(p$maturity[, s], 2, p$n_ages, byrow = TRUE)
    }
    N_season <- N[s, , ]  # rows are regions; columns are ages

    for (season in 1:4) {
      # Observe fish halfway through the season, before its end-of-season move.
      # Survey times are 0.125, 0.375, 0.625 and 0.875 years in both regions.
      surveys[season, ] <- surveys[season, ] + p$survey_q *
        rowSums(N_season * exp(-0.125 * Z) * survey_selectivity * weight)
      catch[s, ] <- catch[s, ] + rowSums(N_season * catch_fraction * weight)
      if (season == 3) {
        # Spawning is at 0.625 years, after the season-2 natal return under NH.
        spawning[s, ] <- rowSums(N_season * exp(-0.125 * Z) * weight * maturity)
      }
      N_season <- N_season * survival
      if (p$structure == "NH" && season == 2) {
        totals <- colSums(N_season)
        N_season[,] <- 0
        N_season[s, ] <- totals           # complete return before spawning
      } else {
        # Transpose movement to multiply a [region, age] numbers matrix.
        N_season <- t(p$movement[, , season]) %*% N_season
      }
    }
    survivors[s, , ] <- N_season
  }
  list(survivors = survivors, spawning = spawning,
       catch = catch, surveys = surveys)
}


# Follow one recruit from each birth origin through its lifetime.
# Phi gives spawning output by origin and spawning region; YPR gives catch per
# recruit. numbers stores the corresponding numbers at age per annual recruit.
# The final age includes every older fish through the exact plus-group sum.
per_recruit <- function(F, p) {
  N <- array(0, c(2, 2, p$n_ages))
  fishing <- matrix(0, 2, p$n_ages)
  for (r in 1:2) fishing[r, ] <- F[r] * p$selectivity[r, ]
  survival <- exp(-0.25 * (p$M + fishing))
  for (s in 1:2) {
    n <- c(0, 0)
    n[s] <- 1
    for (age in 1:p$n_ages) {
      # transition maps numbers at this age to next year's numbers by region.
      # In each matrix product the rightmost operation happens first.
      transition <- diag(2)
      for (season in 1:4) {
        move <- t(p$movement[, , season])
        if (p$structure == "NH" && season == 2) {
          move[,] <- 0
          move[s, ] <- 1
        }
        transition <- move %*% diag(survival[, age]) %*% transition
      }
      # At the plus group, solve n + T*n + T^2*n + ... = (I - T)^(-1)*n.
      if (age == p$n_ages) n <- as.numeric(solve(diag(2) - transition, n))
      N[s, , age] <- n
      n <- as.numeric(transition %*% n)
    }
  }
  # N holds all ages per annual recruit. Summing one year's spawning/catch
  # across these cohorts gives the lifetime spawning/catch of one recruit.
  year <- simulate_year(N, F, p)
  list(Phi = year$spawning, YPR = rowSums(year$catch),
       catch = year$catch, numbers = N)
}


# Long-run population when the same F is used every year.
# Mean recruitment stays at R0. BH recruitment must agree with spawning biomass.
# Returns recruitment, biomass, catch and numbers at the resulting equilibrium.
equilibrium <- function(F, p) {
  pr <- per_recruit(F, p)
  R <- p$R0
  if (p$recruitment == "BH") {
    no_exchange <- pr$Phi[1, 2] == 0 && pr$Phi[2, 1] == 0
    if (p$structure == "NH" || no_exchange) {
      # In a closed spawning loop, SSB = R * phi. Substitute into BH and solve
      # R = alpha * R * phi / (1 + beta * R * phi); use zero below replacement.
      phi <- diag(pr$Phi)
      R <- pmax(0, (p$alpha * phi - 1) / (p$beta * phi))
    } else {
      # Regional spawners produce recruits entering their spawning region.
      # Iterate recruitment because spawning in either region can include
      # fish born in both regions. Phi stays fixed at the supplied F.
      for (iteration in 1:1000000) {
        SSB <- as.numeric(R %*% pr$Phi)
        next_R <- p$alpha * SSB / (1 + p$beta * SSB)
        if (max(abs(next_R - R)) < 1e-13 * max(1, R)) break
        R <- next_R
      }
      if (iteration == 1000000) stop("BH equilibrium recruitment did not converge")
      R <- next_R
    }
  }
  spawning <- pr$Phi
  catch <- pr$catch
  numbers <- pr$numbers
  for (s in 1:2) {
    # Convert per-recruit outputs into population totals using equilibrium R.
    spawning[s, ] <- R[s] * pr$Phi[s, ]
    catch[s, ] <- R[s] * pr$catch[s, ]
    numbers[s, , ] <- R[s] * pr$numbers[s, , ]
  }
  list(R = R, Phi = pr$Phi, spawning = spawning,
       ssb = colSums(spawning), population_ssb = rowSums(spawning),
       catch = colSums(catch), yield = sum(catch), numbers = numbers)
}


# Simulate a history or projection. N = NULL starts a simple declining age profile.
# Supply an existing N to continue from the end of a historical period.
# A vector F is constant; a years x 2 matrix specifies annual fishing rates.
# sigmaR = 0 gives deterministic recruitment; a positive value adds variability.
# Returns final numbers, annual results, and the last year's survey/catch details.
# The trajectory records recruitment already present at the start of each year;
# recruitment produced by this year's spawning enters the following year.
simulate_population <- function(p, F, years, N = NULL, sigmaR = 0) {
  if (is.null(N)) {
    N <- array(0, c(2, 2, p$n_ages))
    for (s in 1:2) {
      n <- p$R0[s] * exp(-(p$M + 0.20) * (p$ages - 1))
      n[p$n_ages] <- n[p$n_ages] / (1 - exp(-(p$M + 0.20)))
      N[s, s, ] <- n
    }
  }
  if (is.null(dim(F))) F <- matrix(F, years, 2, byrow = TRUE)
  trajectory <- vector("list", years)
  for (year in 1:years) {
    result <- simulate_year(N, F[year, ], p)
    SSB <- colSums(result$spawning)
    R <- if (p$recruitment == "mean") p$R0 else
      p$alpha * SSB / (1 + p$beta * SSB)
    # Subtract sigmaR^2 / 2 on the log scale so the multiplier has mean one.
    R <- R * exp(rnorm(2, -0.5 * sigmaR^2, sigmaR))
    trajectory[[year]] <- data.frame(year = year, F1 = F[year, 1], F2 = F[year, 2],
      ssb1 = SSB[1], ssb2 = SSB[2],
      population_ssb1 = sum(result$spawning[1, ]),
      population_ssb2 = sum(result$spawning[2, ]),
      catch1 = sum(result$catch[, 1]), catch2 = sum(result$catch[, 2]),
      recruitment1 = N[1, 1, 1], recruitment2 = N[2, 2, 1])
    # Age survivors; keep the oldest fish in the final-age plus group.
    next_N <- array(0, dim(N))
    next_N[, , 2:p$n_ages] <- result$survivors[, , 1:(p$n_ages - 1)]
    next_N[, , p$n_ages] <- next_N[, , p$n_ages] + result$survivors[, , p$n_ages]
    next_N[1, 1, 1] <- R[1]
    next_N[2, 2, 1] <- R[2]
    N <- next_N
  }
  list(N = N, trajectory = do.call(rbind, trajectory), last_year = result)
}
