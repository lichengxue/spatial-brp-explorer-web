# Experimental deterministic Beverton-Holt MSY.
# System MSY maximizes sum_s R_s(F) YPR_s(F), with equilibrium recruitment.
# Component maxima are upper benchmarks, each optimized over the same F bounds.
# NH components are natal populations; metapopulation components are regional catches.
# evaluate(F) returns equilibrium outputs; score() chooses which output to maximize.
# Each search returns its best F pair and value within the stated bounds.

bounded_yield_search <- function(evaluate, score, fmax = 10, seeds = list()) {
  axis <- sort(unique(c(0, exp(seq(log(1e-5), log(fmax), length.out = 14)),
                        seq(0, fmax, length.out = 7), .05, .1, .2, .4)))
  grid <- as.matrix(expand.grid(axis, axis))
  # The grid covers both small fishing rates and the full bounded F range.
  # Refine several promising points, rather than trusting one initial guess.
  values <- apply(grid, 1, function(f) score(evaluate(f)))
  candidates <- lapply(order(values, decreasing = TRUE)[1:6], function(i) grid[i, ])
  starts <- c(candidates, seeds, list(c(.05, .05), c(.2, .2), c(1, 1)))
  objective <- function(f) -score(evaluate(f))
  # nlminb() minimizes: reversing the sign makes it maximize the chosen score.
  fits <- lapply(starts, function(f) nlminb(pmin(fmax, pmax(0, f)), objective,
    lower = c(0, 0), upper = c(fmax, fmax),
    control = list(iter.max = 300, eval.max = 800, rel.tol = 1e-11, x.tol = 1e-9)))
  candidates <- c(candidates, lapply(fits, function(fit) fit$par))
  # Search all four edges, refining every grid-local maximum, including closures.
  for (j in 1:2) for (fixed in c(0, fmax)) {
    edge <- function(x) {
      f <- c(0, 0)
      f[j] <- x
      f[3 - j] <- fixed
      f
    }
    v <- vapply(axis, function(x) score(evaluate(edge(x))), 0.0)
    for (i in seq_along(axis)) {
      if ((i == 1 || v[i] >= v[i-1]) && (i == length(axis) || v[i] >= v[i + 1])) {
        lo <- axis[max(1, i-1)]
        hi <- axis[min(length(axis), i + 1)]
        fit <- optimize(function(x) -score(evaluate(edge(x))), c(lo, hi), tol = 1e-10)
        candidates <- c(candidates, list(edge(axis[i]), edge(fit$minimum)))
      }
    }
  }
  scores <- vapply(candidates, function(f) score(evaluate(f)), 0.0)
  best <- which.max(scores)
  list(F = as.numeric(candidates[[best]]), value = scores[best], grid_best = max(values),
       successful_starts = sum(vapply(fits, function(f) f$convergence == 0, TRUE)))
}

pattern_yield_search <- function(evaluate, coefficient, fmax = 10) {
  # One unknown multiplier keeps F1:F2 fixed. Limit it so neither fleet exceeds fmax.
  upper <- min(fmax / coefficient)
  axis <- sort(unique(c(0, exp(seq(log(1e-6), log(upper), length.out = 100)))))
  objective <- function(g) -evaluate(g * coefficient)$yield
  vals <- vapply(axis, objective, 0.0)
  candidates <- axis
  for (i in seq_along(axis)) if ((i == 1 || vals[i] <= vals[i-1]) &&
      (i == length(axis) || vals[i] <= vals[i + 1])) {
    fit <- optimize(objective, c(axis[max(1, i-1)], axis[min(length(axis), i + 1)]), tol = 1e-10)
    candidates <- c(candidates, fit$minimum)
  }
  g <- candidates[which.min(vapply(candidates, objective, 0.0))]
  list(F = g * coefficient, value = -objective(g), grid_best = -min(vals), successful_starts = NA_integer_)
}

msy_analysis <- function(p, st, uf, which = c("MSY-Pattern", "MSY-System", "MSY-Joint")) {
  if (p$recruitment != "sr") stop("MSY requires Beverton-Holt recruitment; mean-recruitment yield is a different benchmark.")
  evaluate <- function(F) {
    # Let recruitment settle at this F before measuring sustainable annual yield.
    # This is an equilibrium calculation, separate from the finite-history projection.
    eq <- equilibrium(F, p, st, uf)
    pr <- per_recruit(F, p, st)
    yr <- simulate_year(equilibrium_numbers(pr, eq$R, p$n_ages), F, p, st)
    z <- list(yield = eq$yield, component = if (st == "NH") eq$R * pr$YPR else yr$catch,
      regional_catch = yr$catch, R = eq$R, ssb = eq$ssb_region, depletion = eq$depletion,
      pop_spr = stock_sbpr(pr$Phi) / stock_sbpr(uf$Phi0),
      # A fixed R0 reference makes MSY SPR diagnostics independent of SPR target.
      regional_spr = regional_spr(pr$Phi, uf$Phi0, p$R0))
    stopifnot(abs(sum(z$component)-z$yield) < 1e-7 * max(1, z$yield))
    z
  }
  fits <- list()
  diagnostics <- NULL
  component_rows <- NULL
  if ("MSY-Pattern" %in% which)
    fits[["MSY-Pattern"]] <- pattern_yield_search(evaluate, wham_fishing_pattern(p)$coefficient)
  if (any(c("MSY-System", "MSY-Joint") %in% which)) {
    system <- bounded_yield_search(evaluate, function(z) z$yield,
      seeds = lapply(fits, function(x) x$F))
    if ("MSY-System" %in% which) fits[["MSY-System"]] <- system
  }
  if ("MSY-Joint" %in% which) {
    # First maximize each component separately over the same F1,F2 choices.
    # Then maximize the smaller fraction of those two separate maxima. A fraction
    # near one means both maxima were reached together within numerical tolerance.
    individual <- lapply(1:2, function(j) bounded_yield_search(evaluate, function(z) z$component[j],
      seeds = list(system$F)))
    maxima <- vapply(individual, function(f) f$value, 0.0)
    if (any(maxima <= 1e-12)) stop("A component has no positive sustainable yield; simultaneous-MSY ratios are undefined.")
    balanced <- bounded_yield_search(evaluate, function(z) min(z$component / maxima),
      seeds = c(list(system$F), lapply(individual, function(f) f$F)))
    # This is a max-min fraction of separate maxima, not an additional MSY definition.
    fits[["MSY-Balanced"]] <- balanced
    at_balanced <- evaluate(balanced$F)
    diagnostics <- data.frame(component_basis = if (st == "NH") "Natal population catch" else "Regional catch",
      maximum1 = maxima[1], maximum2 = maxima[2], sum_separate_maxima = sum(maxima),
      system_MSY = system$value, system_F1 = system$F[1], system_F2 = system$F[2],
      balanced_F1 = balanced$F[1], balanced_F2 = balanced$F[2],
      fraction1 = at_balanced$component[1] / maxima[1], fraction2 = at_balanced$component[2] / maxima[2],
      joint_fraction = balanced$value, tolerance = 1e-4,
      simultaneous_found = balanced$value >= 1-1e-4,
      interpretation = if (balanced$value >= 1-1e-4) "Simultaneous component maxima found within numerical tolerance" else
        "Simultaneous component maxima not found; balanced point is a compromise, not a proof of nonexistence",
      F_upper = 10, reference_basis = "Unfished R0 composition")
    component_rows <- do.call(rbind, lapply(1:2, function(j) {
      f <- individual[[j]]$F
      z <- evaluate(f)
      data.frame(component = j, F1 = f[1], F2 = f[2], maximum_yield = maxima[j],
        component1_yield = z$component[1], component2_yield = z$component[2], total_yield = z$yield,
        grid_best = individual[[j]]$grid_best, successful_starts = individual[[j]]$successful_starts)
    }))
  }
  rows <- do.call(rbind, lapply(names(fits), function(name) {
    fit <- fits[[name]]
    f <- fit$F
    z <- evaluate(f)
    eq <- equilibrium(f, p, st, uf)
    pr <- per_recruit(f, p, st)
    N <- equilibrium_numbers(pr, eq$R, p$n_ages)
    yr <- simulate_year(N, f, p, st)
    residual <- max(abs(advance_one_year(yr$numbers_end, next_recruitment(yr, p, st, uf))-N)) / max(1, max(N))
    # One annual update should leave an equilibrium state unchanged. Report the
    # remaining numerical difference alongside the fitted reference point.
    boundary <- any(f > 9.99)
    # A found boundary optimum is checked with an expanded F <= 20 box.
    sensitive <- NA
    if (boundary) {
      score <- if (name == "MSY-Balanced") function(z) min(z$component / maxima) else function(z) z$yield
      extended <- if (name == "MSY-Pattern") pattern_yield_search(evaluate, wham_fishing_pattern(p)$coefficient, 20) else
        bounded_yield_search(evaluate, score, 20, seeds = list(f))
      sensitive <- extended$value > fit$value + 1e-5 * max(1, abs(fit$value))
    }
    data.frame(rule = name, F1 = f[1], F2 = f[2], yield = z$yield,
      component1_yield = z$component[1], component2_yield = z$component[2],
      eq_catch1 = z$regional_catch[1], eq_catch2 = z$regional_catch[2],
      eq_ssb1 = z$ssb[1], eq_ssb2 = z$ssb[2], R1 = z$R[1], R2 = z$R[2],
      pop_spr1 = z$pop_spr[1], pop_spr2 = z$pop_spr[2],
      regional_spr1 = z$regional_spr[1], regional_spr2 = z$regional_spr[2],
      depletion1 = z$depletion[1], depletion2 = z$depletion[2],
      equilibrium_residual = residual, upper_boundary = boundary, upper_sensitive = sensitive,
      grid_best = fit$grid_best, successful_starts = fit$successful_starts, F_upper = 10)
  }))
  list(summary = rows, joint = diagnostics, components = component_rows)
}
