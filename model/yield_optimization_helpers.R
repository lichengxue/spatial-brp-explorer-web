# Optional equilibrium-yield benchmarks, using the actual movement and biology.
# p: model parameters; st: NH/MPO/MPD; uf: unfished calibration.
# reg is the accepted Reg-Move row, or an empty table if it was unavailable.
# Rref sets the biological reference composition; Wref sets the constraint index.
# Return one summary row containing whichever yield benchmarks were requested.
yield_optimum <- function(p, st, uf, reg, target = 0.4,
    which = c("Yield-SPR", "Yield-Max"), origin_weights = NULL,
    weight_label = "Reference recruitment") {
  has_reg <- nrow(reg) == 1L && all(is.finite(c(reg$F1, reg$F2, reg$Rref1, reg$Rref2)))
  Rref <- if (has_reg) c(reg$Rref1, reg$Rref2) else p$R0
  Wref <- if (is.null(origin_weights)) Rref else origin_weights

  # Hold this origin mixture fixed. Feasibility needs only spawning per recruit.
  constraint_spr <- function(F) {
    Phi <- per_recruit(F, p, st)$Phi
    if (st == "NH") stock_sbpr(Phi) / stock_sbpr(uf$Phi0) else
      regional_spr(Phi, uf$Phi0, Wref)
  }
  evaluate <- function(F) {
    eq <- equilibrium(F, p, st, uf)
    spr <- if (st == "NH") stock_sbpr(eq$Phi) / stock_sbpr(uf$Phi0) else
      regional_spr(eq$Phi, uf$Phi0, Wref)
    list(yield = eq$yield, spr = spr, dep = eq$depletion)
  }
  fmax <- 10
  Freg <- if (has_reg) c(reg$F1, reg$F2) else c(.2, .2)
  reg_result <- if (has_reg) evaluate(Freg) else list(yield = NA_real_, spr = c(NA, NA))
  Fopt <- Fy <- c(NA_real_, NA_real_)
  opt <- free <- list(yield = NA_real_, spr = c(NA, NA), dep = c(NA, NA))
  grid_values <- NA_real_
  upper_sensitive <- NA

  if ("Yield-SPR" %in% which) {
    # cap1: largest feasible F1 with F2 = 0.
    # cap2(F1): largest feasible F2 at a given F1.
    # Fractions u,v in [0,1] describe the entire feasible F region.
    cap1 <- if (min(constraint_spr(c(fmax, 0))) >= target) fmax else
      uniroot(function(x) min(constraint_spr(c(x, 0))) - target,
              c(0, fmax), tol = 1e-10)$root
    cap2 <- function(F1) {
      if (min(constraint_spr(c(F1, fmax))) >= target) return(fmax)
      if (min(constraint_spr(c(F1, 0))) <= target + 1e-10) return(0)
      uniroot(function(F2) min(constraint_spr(c(F1, F2))) - target,
              c(0, fmax), tol = 1e-10)$root
    }
    feasible_F <- function(z) c(z[1] * cap1, z[2] * cap2(z[1] * cap1))
    # Every trial remains inside the regional SPR constraints. Maximize yield
    # there by minimizing its negative; constraints are inequalities, not equalities.
    objective <- function(z) -evaluate(feasible_F(z))$yield
    grid <- as.matrix(expand.grid(seq(0, 1, length.out = 25), seq(0, 1, length.out = 13)))
    grid_values <- numeric(nrow(grid))
    for (i in 1:nrow(grid)) grid_values[i] <- objective(grid[i, ])
    starts <- grid[order(grid_values)[1:4], , drop = FALSE]
    if (has_reg) {
      reg_start <- c(Freg[1] / cap1, Freg[2] / max(1e-12, cap2(min(Freg[1], cap1))))
      starts <- rbind(starts, pmin(1, pmax(0, reg_start)))
    }
    starts <- rbind(starts, c(.5, .5), c(.2, 1), c(.8, 1))
    fits <- vector("list", nrow(starts))
    for (i in 1:nrow(starts)) {
      fits[[i]] <- nlminb(starts[i, ], objective, lower = c(0, 0), upper = c(1, 1),
        control = list(iter.max = 300, eval.max = 800, rel.tol = 1e-11, x.tol = 1e-9))
    }
    best <- which.min(vapply(fits, function(fit) fit$objective, numeric(1)))
    Fopt <- feasible_F(fits[[best]]$par)
    opt <- evaluate(Fopt)
    # Accept only a feasible result that is no worse than the grid and any
    # feasible regional BRP used as a comparison. This is not a uniqueness test.
    stopifnot(min(opt$spr) >= target - 1e-7,
      !has_reg || min(reg_result$spr) < target - 1e-7 ||
        opt$yield >= reg_result$yield * (1 - 1e-7),
      opt$yield >= -min(grid_values) * (1 - 1e-7))
  }

  if ("Yield-Max" %in% which) {
    # Fixed starting points keep this independent of SPR targets and checkboxes.
    starts <- rbind(c(.2, .2), c(.05, .05), c(.01, .01), c(.1, .1),
                    c(.4, .4), c(.03, .6), c(.6, .03), c(2, 2))
    candidates <- matrix(0, nrow(starts), 2)
    for (i in 1:nrow(starts)) {
      fit <- nlminb(log(starts[i, ]), function(z) -evaluate(exp(z))$yield,
        lower = rep(log(1e-8), 2), upper = rep(log(fmax), 2),
        control = list(iter.max = 300, eval.max = 800, rel.tol = 1e-11, x.tol = 1e-9))
      candidates[i, ] <- exp(fit$par)
    }
    # Also allow either fleet to close completely.
    for (region in 1:2) {
      objective <- function(x) {
        F <- c(0, 0)
        F[region] <- x
        -evaluate(F)$yield
      }
      grid <- c(0, exp(seq(log(1e-5), log(fmax), length.out = 60)))
      values <- vapply(grid, objective, numeric(1))
      j <- which.min(values)
      fit <- optimize(objective, c(grid[max(1, j - 1)], grid[min(length(grid), j + 1)]), tol = 1e-10)
      F <- c(0, 0)
      F[region] <- fit$minimum
      candidates <- rbind(candidates, F)
    }
    yields <- apply(candidates, 1, function(F) evaluate(F)$yield)
    Fy <- candidates[which.max(yields), ]
    free <- evaluate(Fy)
    upper_sensitive <- FALSE
    if (any(Fy > 9.99)) {
      fit <- nlminb(log(pmax(Fy, 1e-8)), function(z) -evaluate(exp(z))$yield,
        lower = rep(log(1e-8), 2), upper = rep(log(20), 2))
      upper_sensitive <- -fit$objective - free$yield > 1e-5 * max(1, free$yield)
    }
    if (is.finite(opt$yield)) stopifnot(free$yield >= opt$yield * (1 - 1e-6))
  }

  basis <- if (!is.null(origin_weights)) paste0(weight_label, "; fixed origin mix") else
    if (has_reg) "Reg-Move equilibrium composition" else "Unfished R0 composition"
  # Missing fields correspond to an unselected search. The GUI uses the same
  # summary layout whether one or both benchmarks were selected.
  data.frame(rec = p$recruitment, lh = p$life_history, move = p$movement, struct = st,
    Rref1 = Rref[1], Rref2 = Rref[2],
    Reg_F1 = if (has_reg) Freg[1] else NA_real_, Reg_F2 = if (has_reg) Freg[2] else NA_real_,
    Reg_yield = reg_result$yield, Opt_F1 = Fopt[1], Opt_F2 = Fopt[2], Opt_yield = opt$yield,
    Opt_SPR1 = opt$spr[1], Opt_SPR2 = opt$spr[2], Opt_dep1 = opt$dep[1], Opt_dep2 = opt$dep[2],
    Reg_over_opt = reg_result$yield / opt$yield, binding_constraints = sum(abs(opt$spr - target) < 1e-5),
    optimum_differs = if (has_reg) max(abs(Fopt - Freg)) > 1e-4 else NA,
    YieldMax_F1 = Fy[1], YieldMax_F2 = Fy[2], YieldMax_yield = free$yield,
    YieldMax_SPR1 = free$spr[1], YieldMax_SPR2 = free$spr[2],
    YieldMax_dep1 = free$dep[1], YieldMax_dep2 = free$dep[2],
    YieldMax_upper_boundary = any(Fy > 9.99), YieldMax_upper_sensitive = upper_sensitive,
    coarse_grid_yield = -min(grid_values), constrained_search_domain = fmax,
    reference_basis = basis, regional_weight1 = Wref[1] / sum(Wref), regional_weight2 = Wref[2] / sum(Wref))
}
