# Optional yield/MSY calculations. Source operating_model.R and reference_points.R first.
# Unlike the SPR solvers, these searches optimize equilibrium catch biomass.
# Yield-SPR requires both regional ratios >= target; they need not both equal it.
# Fmax is a numerical search limit for each fleet, not a biological reference point.
yield_reference_points <- function(p, target = 0.40, origin_weights = p$R0,
    fishing_pattern = c(.7, .3), methods = c("Yield-SPR", "Yield-Max"), Fmax = 10) {
  # Optional. Yield-Max is joint system MSY under BH, or fixed-recruitment yield
  # under mean recruitment. MSY-Pattern keeps the specified F1:F2 ratio.
  # For example: yield_reference_points(p, methods = "Yield-Max").
  # origin_weights stays fixed during the Yield-SPR search. The example script
  # uses Reg-Move's reference recruitment by default, or your chosen origin mix.
  # Start with a small grid, improve its best points, then compare the boundaries.
  if (any(!methods %in% c("Yield-SPR", "Yield-Max", "MSY-Pattern"))) stop("Unknown yield method")
  if ("MSY-Pattern" %in% methods && p$recruitment != "BH") stop("MSY-Pattern requires BH recruitment")
  regional <- function(F) spr_ratios(F, p, origin_weights)$regional
  total_yield <- function(F) equilibrium(F, p)$yield
  coefficient <- fishing_pattern / max(colSums(p$selectivity * fishing_pattern))
  rows <- list()
  for (method in methods) {
    message("Calculating ", method)
    if (method == "MSY-Pattern") {
      upper <- min(Fmax / coefficient)
      grid <- seq(0, upper, length.out = 101)
      values <- numeric(length(grid))
      for (k in seq_along(grid)) values[k] <- total_yield(grid[k] * coefficient)
      j <- which.max(values)
      # The grid locates a promising interval; optimize() refines within it.
      fit <- optimize(function(g) -total_yield(g * coefficient),
        c(grid[max(1, j - 1)], grid[min(length(grid), j + 1)]), tol = 1e-9)
      candidates <- c(grid[j], fit$minimum, 0, upper)
      values <- numeric(length(candidates))
      for (k in seq_along(candidates)) values[k] <- total_yield(candidates[k] * coefficient)
      g <- candidates[which.max(values)]
      F <- g * coefficient
    } else {
      if (method == "Yield-SPR") {
        # cap1 is the largest F1 allowed when F2 = 0.
        # cap2(F1) is the largest F2 allowed at a given F1.
        # z has two fractions from 0 to 1; map(z) converts them to feasible F1,F2.
        cap1 <- if (min(regional(c(Fmax, 0))) >= target) Fmax else
          uniroot(function(x) min(regional(c(x, 0))) - target, c(0, Fmax), tol = 1e-10)$root
        cap2 <- function(x) {
          if (min(regional(c(x, Fmax))) >= target) return(Fmax)
          if (min(regional(c(x, 0))) <= target + 1e-10) return(0)
          uniroot(function(y) min(regional(c(x, y))) - target, c(0, Fmax), tol = 1e-10)$root
        }
        map <- function(z) c(z[1] * cap1, z[2] * cap2(z[1] * cap1))
        upper <- c(1, 1)
        grid <- as.matrix(expand.grid(seq(0, 1, length.out = 25), seq(0, 1, length.out = 13)))
      } else {
        map <- function(z) z
        upper <- rep(Fmax, 2)
        axis <- sort(unique(c(0, .01, .03, .1, .3, 1, 3, Fmax)))
        grid <- as.matrix(expand.grid(axis, axis))
      }
      objective <- function(z) -total_yield(map(z))
      # nlminb() minimizes, so use negative yield. Keep the best grid candidates
      # as well as refined fits; an optimizer flag alone does not select a result.
      values <- numeric(nrow(grid))
      for (k in 1:nrow(grid)) values[k] <- objective(grid[k, ])
      starts <- grid[order(values)[1:5], , drop = FALSE]
      candidates <- starts
      for (k in 1:nrow(starts)) {
        fit <- nlminb(starts[k, ], objective, lower = c(0, 0), upper = upper,
          control = list(iter.max = 300, eval.max = 800, rel.tol = 1e-11, x.tol = 1e-9))
        candidates <- rbind(candidates, fit$par)
      }
      # Include each boundary explicitly, so closing either region is allowed.
      for (j in 1:2) for (fixed in c(0, upper[3 - j])) {
        edge <- function(x) { z <- c(0, 0); z[j] <- x; z[3 - j] <- fixed; z }
        axis <- sort(unique(c(seq(0, upper[j], length.out = 30), starts[, j])))
        edge_values <- numeric(length(axis))
        for (k in seq_along(axis)) edge_values[k] <- objective(edge(axis[k]))
        for (k in seq_along(axis)) {
          if ((k == 1 || edge_values[k] <= edge_values[k - 1]) &&
              (k == length(axis) || edge_values[k] <= edge_values[k + 1])) {
            fit <- optimize(function(x) objective(edge(x)),
              c(axis[max(1, k - 1)], axis[min(length(axis), k + 1)]), tol = 1e-9)
            candidates <- rbind(candidates, edge(axis[k]), edge(fit$minimum))
          }
        }
      }
      values <- numeric(nrow(candidates))
      for (k in 1:nrow(candidates)) values[k] <- objective(candidates[k, ])
      F <- map(candidates[which.min(values), ])
    }
    eq <- equilibrium(F, p)
    values <- spr_ratios(F, p, origin_weights, p$R0)
    rows[[method]] <- data.frame(rule = method, F1 = F[1], F2 = F[2],
      regional_SPR1 = values$regional[1], regional_SPR2 = values$regional[2],
      population_SPR1 = values$population[1], population_SPR2 = values$population[2],
      SSB1 = eq$ssb[1], SSB2 = eq$ssb[2], catch1 = eq$catch[1], catch2 = eq$catch[2],
      yield = eq$yield, status = "calculated", note = "Bounded numerical maximum; uniqueness is not established")
  }
  do.call(rbind, rows)
}
