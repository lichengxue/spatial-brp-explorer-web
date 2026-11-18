# Read GUI settings, run the R model, and write the result tables.
# This file handles the workflow; biological equations are in spatial_brp_core.R.
# The Python server supplies a key/value CSV. Lists inside it use semicolons,
# which lets the R side read GUI settings without an additional package.
# Steps below: inputs -> history and weights -> SPR/projections -> optional yield
# and MSY comparisons -> CSV output for plotting and download.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) stop("Expected configuration CSV and output directory")
script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
model_dir <- dirname(normalizePath(sub("^--file=", "", script_arg[1]), winslash = "/"))
source(file.path(model_dir, "spatial_brp_core.R"))
source(file.path(model_dir, "seasonal_movement.R"))
source(file.path(model_dir, "regional_weighting.R"))
cfg <- read.csv(args[1], stringsAsFactors = FALSE)
cfg <- setNames(as.list(cfg$value), cfg$key)
out <- args[2]
dir.create(out, recursive = TRUE, showWarnings = FALSE)
num <- function(k) as.numeric(cfg[[k]])
progress <- function(message) writeLines(message, file.path(out, "progress.txt"))
share <- num("fishing_share1") / 100
# GUI inputs use percentages; the model uses fractions and annual mortality rates.
allocation <- 2 * c(share, 1 - share)
p <- make_parameters(life_history = cfg$life_history, movement = if (cfg$movement == "custom") "high" else cfg$movement,
  recruitment = cfg$recruitment, allocation = allocation,
  F_historical = num("historical_F") * c(share, 1 - share),
  R0 = c(num("R01"), num("R02")),
  alpha = num("alpha"),
  steepness = if (cfg$bh_calibration == "steepness") num("steepness") else NULL)
if (identical(cfg$custom_biology, "true")) {
  at_age <- function(field) as.numeric(strsplit(cfg[[paste0("bio_", field)]], ";", fixed = TRUE)[[1]])
  p$fishing_selectivity <- rbind(at_age("selectivity1"), at_age("selectivity2"))
  p$weight <- cbind(at_age("weight1"), at_age("weight2"))
  p$maturity <- cbind(at_age("maturity1"), at_age("maturity2"))
  p$spawn_output <- p$weight * p$maturity
}
st <- cfg$structure
if (cfg$movement == "custom") {
  movement_values <- function(direction) as.numeric(strsplit(cfg[[paste0("seasonal_", direction)]], ";", fixed = TRUE)[[1]]) / 100
  p <- configure_seasonal_movement(p, cbind(movement_values("m12"), movement_values("m21")))
}
write.csv(movement_schedule(p, st), file.path(out, "movement.csv"), row.names = FALSE, na = "")
target <- num("target") / 100
split_selection <- function(value) {
  if (is.null(value) || is.na(value) || !nzchar(value)) return(character())
  strsplit(value, ";", fixed = TRUE)[[1]]
}
rules <- split_selection(cfg$rules)
benchmark_names <- split_selection(cfg$benchmarks)
msy_names <- split_selection(cfg$msy)
if (identical(benchmark_names, "true")) benchmark_names <- c("Yield-SPR", "Yield-Max")
if (identical(benchmark_names, "false")) benchmark_names <- character()
historical_years <- as.integer(num("historical_years"))
progress(sprintf("Conditioning the population over %d historical years…", historical_years))
uf <- unfished(p, st)
bi <- burn_in(p, st, uf, n_years = historical_years)
# Compare the chosen finite history with the analytical historical equilibrium.
# This is a reported diagnostic; the projection still starts from the finite history.
ssb_gap <- abs(bi$last_year$ssb_region - bi$equilibrium$ssb_region) / pmax(abs(bi$equilibrium$ssb_region), 1e-12)
max_numbers_gap <- bi$max_rel_diff_analytic
stable <- all(is.finite(c(ssb_gap, max_numbers_gap))) && max(c(ssb_gap, max_numbers_gap)) <= 0.01
write.csv(data.frame(years = historical_years, max_ssb_gap = max(ssb_gap),
  max_numbers_gap = max_numbers_gap, tolerance = 0.01, stable = stable),
  file.path(out, "history_diagnostics.csv"), row.names = FALSE, na = "")
progress("Sampling eight historical surveys at the four seasonal midpoints…")
N_last <- bi$N_last
# Sample the final historical year from its starting numbers. Each survey sees
# mortality up to its midpoint and all movement that occurred before that time.
selected_surveys <- strsplit(cfg$surveys, ";", fixed = TRUE)[[1]]
survey_rows <- list()
for (season in 1:4) {
  pp <- p
  pp$survey_time <- (season - 0.5) * SEASON_LENGTH
  pp$survey_season <- season
  pp$survey_offset <- SEASON_LENGTH / 2
  indices <- simulate_year(N_last, p$F_historical, pp, st)$survey
  for (r in 1:2) survey_rows[[length(survey_rows) + 1L]] <- data.frame(
    region = r, season = season, time = pp$survey_time,
    index = indices[r], selected = paste(r, season, sep = "-") %in% selected_surveys)
}
survey_data <- do.call(rbind, survey_rows)
region_index <- vapply(1:2, function(r) {
  values <- subset(survey_data, region == r & selected)$index
  if (!length(values)) stop("Select at least one survey per region")
  if (cfg$survey_ensemble == "true") mean(values) else sum(values)
}, numeric(1))
survey_share <- region_index / sum(region_index)
# Hold the selected historical survey/catch shares fixed across BRP comparisons.
write.csv(survey_data, file.path(out, "surveys.csv"), row.names = FALSE)
catch_share <- bi$last_year$catch / sum(bi$last_year$catch)
if (any(!is.finite(catch_share))) stop("Historical catches cannot define catch weights")
regional_mode <- if (is.null(cfg$regional_weighting)) "recruitment" else cfg$regional_weighting
regional_custom_share <- if (is.null(cfg$regional_share1)) .5 else num("regional_share1") / 100
regional_weights <- function(Rref) regional_origin_weights(regional_mode, Rref, survey_share, catch_share, regional_custom_share)
calculate_regional <- function() regional_reference_point(p, st, bi$Rbar, survey_share, target, uf,
  catch_share, regional_mode, regional_custom_share)
years <- as.integer(num("years"))
sigma <- if (cfg$stochastic == "true") num("sigmaR") else 0
set.seed(as.integer(num("seed")))
# Paired recruitment draws: selecting/deselecting a rule never changes its draws.
devs <- matrix(rnorm(years * 2, sd = sigma), nrow = years)
rows <- list()
trajectories <- list()
calculated_rules <- list()
# One projection routine for SPR, yield and MSY results.
# Supply rp for an SPR rule with optional biomass feedback. Without rp, F is fixed.
project_trajectory <- function(F_rule, rule_name, rp = NULL) {
  N <- bi$N
  previous <- bi$last_year
  tr <- vector("list", years)
  for (t in seq_len(years)) {
    F_now <- F_rule
    if (cfg$projection == "feedback" && !is.null(rp)) {
      # Reduce F below the rule's biomass target, updated annually.
      status <- if (rp$type == "regional") previous$ssb_region / c(rp$Btgt1, rp$Btgt2) else
        if (rp$type == "stock") previous$ssb_stock / c(rp$Btgt1, rp$Btgt2) else
        if (rule_name == "Natal-RecW") sum(previous$ssb_natal) / rp$Btgt else
        sum(previous$ssb_region) / rp$Btgt
      F_now <- F_rule * hcr_multiplier(status)
    }
    # Spawning produces next year's recruits. Values written below describe the
    # current year; advance_one_year() then ages survivors and adds those recruits.
    yr <- simulate_year(N, F_now, p, st)
    R <- next_recruitment(yr, p, st, uf) * exp(devs[t, ] - 0.5 * sigma^2)
    tr[[t]] <- data.frame(rule = rule_name, year = t, F1 = F_now[1], F2 = F_now[2],
      ssb1 = yr$ssb_region[1], ssb2 = yr$ssb_region[2],
      stock_ssb1 = yr$ssb_stock[1], stock_ssb2 = yr$ssb_stock[2],
      depletion1 = yr$ssb_region[1] / spawning_by_region(uf$Phi0, p$R0)[1],
      depletion2 = yr$ssb_region[2] / spawning_by_region(uf$Phi0, p$R0)[2],
      abundance1 = sum(N[, 1, ]), abundance2 = sum(N[, 2, ]),
      recruitment1 = N[1, 1, 1], recruitment2 = N[2, 2, 1],
      catch1 = yr$catch[1], catch2 = yr$catch[2])
    N <- advance_one_year(yr$numbers_end, R)
    previous <- yr
  }
  do.call(rbind, tr)
}

for (i in seq_along(rules)) {
  rule_name <- rules[i]
  progress(sprintf("Calculating %s (%d of %d)…", rule_name, i, length(rules)))
  result <- tryCatch({
    # NH spawns only at home, so Natal-RecW uses the same equation as SpE-RecW.
    calc_name <- if (st == "NH" && rule_name == "Natal-RecW") "SpE-RecW" else rule_name
    if (rule_name == "Reg-Move") {
      rp <- calculate_regional()
    } else if (p$recruitment == "mean") {
      rp <- reference_points(p, st, p$R0, survey_share, target, uf,
        which = calc_name, catch_share = catch_share)
      if (!is.null(rp)) {
        rp$Rref1 <- p$R0[1]
        rp$Rref2 <- p$R0[2]
      }
    } else {
      rp <- reference_points_consistent(p, st, bi$Rbar, survey_share, target, uf,
        which = calc_name, catch_share = catch_share)
    }
    if (is.null(rp) || nrow(rp) != 1L)
      stop("No accepted joint population solution within the model bounds (F > 0, F <= 10).")
    rp$rule <- rule_name
    F_rule <- c(rp$F1, rp$F2)
    pr <- per_recruit(F_rule, p, st)
    eq <- equilibrium(F_rule, p, st, uf)
    eq_N <- equilibrium_numbers(pr, eq$R, p$n_ages)
    eq_year <- simulate_year(eq_N, F_rule, p, st)
    pop_spr <- stock_sbpr(pr$Phi) / stock_sbpr(uf$Phi0)
    Rref <- c(rp$Rref1, rp$Rref2)
    reg_weights <- regional_weights(Rref)
    # Keep selected index weights separate from biological reference recruitment.
    # Export both regional ratios so readers can distinguish the two definitions.
    reg_spr <- regional_spr(pr$Phi, uf$Phi0, reg_weights)
    reg_spr_rec <- regional_spr(pr$Phi, uf$Phi0, Rref)
    global_rec_spr <- global_spr(stock_sbpr(pr$Phi), stock_sbpr(uf$Phi0), c(rp$Rref1, rp$Rref2))
    list(rp = rp, pop_spr = pop_spr, reg_spr = reg_spr, reg_spr_rec = reg_spr_rec,
      reg_weights = reg_weights, global_rec_spr = global_rec_spr, eq = eq,
      eq_year = eq_year, trajectory = project_trajectory(F_rule, rule_name, rp))
  }, error = function(e) e)
  if (inherits(result, "error")) {
    rows[[i]] <- data.frame(rule = rule_name, status = "unavailable", note = conditionMessage(result),
      F1 = NA, F2 = NA, F_global = NA, global_spr_rec = NA, pop_spr1 = NA, pop_spr2 = NA,
      regional_spr1 = NA, regional_spr2 = NA, regional_spr_rec1 = NA, regional_spr_rec2 = NA,
      regional_weight1 = NA, regional_weight2 = NA, eq_ssb1 = NA, eq_ssb2 = NA,
      eq_catch1 = NA, eq_catch2 = NA, Rref1 = NA, Rref2 = NA)
  } else {
    calculated_rules[[rule_name]] <- result$rp
    alias <- st == "NH" && rule_name == "Natal-RecW"
    rows[[i]] <- data.frame(rule = rule_name, status = if (alias) "equivalent" else "calculated",
      note = if (alias) "Equals SpE-RecW under natal homing: all spawning occurs at home." else
        if (rule_name == "Reg-Move" && regional_mode != "recruitment") paste("Regional spawning-output target uses", regional_weight_label(regional_mode), "assigned by birth region; actual reference recruitment remains separate.") else
        if (rule_name %in% c("SpD-EqW", "SpD-SurvW", "SpD-CatchW")) "Additional global variant: BRP calculation omits movement; projections retain the actual movement scenario." else "",
      F1 = result$rp$F1, F2 = result$rp$F2, F_global = result$rp$F_global,
      global_spr_rec = result$global_rec_spr,
      pop_spr1 = result$pop_spr[1], pop_spr2 = result$pop_spr[2],
      regional_spr1 = result$reg_spr[1], regional_spr2 = result$reg_spr[2],
      regional_spr_rec1 = result$reg_spr_rec[1], regional_spr_rec2 = result$reg_spr_rec[2],
      regional_weight1 = result$reg_weights[1], regional_weight2 = result$reg_weights[2],
      eq_ssb1 = result$eq$ssb_region[1], eq_ssb2 = result$eq$ssb_region[2],
      eq_catch1 = result$eq_year$catch[1], eq_catch2 = result$eq_year$catch[2],
      Rref1 = result$rp$Rref1, Rref2 = result$rp$Rref2)
    trajectories[[rule_name]] <- result$trajectory
  }
}
if (length(benchmark_names)) {
  # Use a single origin mixture during each constrained-yield search. Reuse the
  # regional SPR result when available, or calculate it to set that mixture.
  progress(paste("Searching selected equilibrium-yield benchmarks:", paste(benchmark_names, collapse = ", ")))
  benchmark_result <- tryCatch({
    reg <- calculated_rules[["Reg-Move"]]
    if (is.null(reg)) {
      reg <- tryCatch(calculate_regional(), error = function(e) NULL)
    }
    if (is.null(reg)) reg <- data.frame(rule = character())
    source(file.path(model_dir, "yield_optimization_helpers.R"))
    opt <- yield_optimum(p, st, uf, reg, target, which = benchmark_names,
      origin_weights = if (regional_mode == "recruitment") NULL else regional_weights(p$R0),
      weight_label = regional_weight_label(regional_mode))
    write.csv(opt, file.path(out, "benchmark_summary.csv"), row.names = FALSE, na = "")
    bench_rows <- list()
    Rref <- c(opt$Rref1, opt$Rref2)
    for (kind in benchmark_names) {
      constrained <- kind == "Yield-SPR"
      f <- if (constrained) c(opt$Opt_F1, opt$Opt_F2) else
        c(opt$YieldMax_F1, opt$YieldMax_F2)
      eq <- equilibrium(f, p, st, uf)
      pr <- per_recruit(f, p, st)
      eq_yr <- simulate_year(equilibrium_numbers(pr, eq$R, p$n_ages), f, p, st)
      pop_spr <- stock_sbpr(pr$Phi) / stock_sbpr(uf$Phi0)
      reg_weights <- regional_weights(Rref)
      reg_spr <- regional_spr(pr$Phi, uf$Phi0, reg_weights)
      reg_spr_rec <- regional_spr(pr$Phi, uf$Phi0, Rref)
      bench_rows[[kind]] <- data.frame(rule = kind, status = "benchmark",
        note = paste(if (constrained) "Highest equilibrium yield found with both regional SPRs at least the target." else
          "Highest equilibrium yield found without SPR constraints; numerical bounded search, not a proven global optimum.",
          "SPR diagnostic reference:", opt$reference_basis),
        F1 = f[1], F2 = f[2], F_global = NA,
        global_spr_rec = global_spr(stock_sbpr(pr$Phi), stock_sbpr(uf$Phi0), Rref),
        pop_spr1 = pop_spr[1], pop_spr2 = pop_spr[2],
        regional_spr1 = reg_spr[1], regional_spr2 = reg_spr[2],
        regional_spr_rec1 = reg_spr_rec[1], regional_spr_rec2 = reg_spr_rec[2],
        regional_weight1 = reg_weights[1], regional_weight2 = reg_weights[2],
        eq_ssb1 = eq$ssb_region[1], eq_ssb2 = eq$ssb_region[2],
        eq_catch1 = eq_yr$catch[1], eq_catch2 = eq_yr$catch[2], Rref1 = Rref[1], Rref2 = Rref[2])
      trajectories[[kind]] <- project_trajectory(f, kind)
    }
    do.call(rbind, bench_rows)
  }, error = function(e) data.frame(rule = "Yield benchmarks", status = "unavailable", note = conditionMessage(e)))
  write.csv(benchmark_result, file.path(out, "benchmarks.csv"), row.names = FALSE, na = "")
}
if (length(msy_names)) {
  # MSY searches use deterministic BH equilibrium, even if projected recruitment
  # is stochastic. Projection noise is applied later to the fitted fishing rates.
  progress("Searching experimental Beverton-Holt MSY and joint component comparisons…")
  msy_rows <- tryCatch({
    source(file.path(model_dir, "msy_helpers.R"))
    msy_result <- msy_analysis(p, st, uf, msy_names)
    # Keep summary exports consistent with the comparison-table diagnostics.
    msy_result$summary$regional_spr_rec1 <- msy_result$summary$regional_spr1
    msy_result$summary$regional_spr_rec2 <- msy_result$summary$regional_spr2
    for (i in seq_len(nrow(msy_result$summary))) {
      f <- as.numeric(msy_result$summary[i, c("F1", "F2")])
      w <- regional_weights(p$R0)
      ratios <- regional_spr(per_recruit(f, p, st)$Phi, uf$Phi0, w)
      msy_result$summary$regional_spr1[i] <- ratios[1]
      msy_result$summary$regional_spr2[i] <- ratios[2]
      msy_result$summary$regional_weight1[i] <- w[1]
      msy_result$summary$regional_weight2[i] <- w[2]
    }
    write.csv(msy_result$summary, file.path(out, "msy_summary.csv"), row.names = FALSE, na = "")
    if (!is.null(msy_result$joint)) write.csv(msy_result$joint, file.path(out, "msy_joint.csv"), row.names = FALSE, na = "")
    if (!is.null(msy_result$components)) write.csv(msy_result$components, file.path(out, "msy_components.csv"), row.names = FALSE, na = "")
    comparison_rows <- vector("list", nrow(msy_result$summary))
    for (i in seq_len(nrow(msy_result$summary))) {
      z <- msy_result$summary[i, ]
      f <- c(z$F1, z$F2)
      kind <- z$rule
      note <- switch(kind, "MSY-Pattern" = "Experimental system MSY along the selected fully selected F ratio.",
        "MSY-System" = "Experimental system MSY: joint optimization of total yield over F1 and F2; does not maximize every component separately.",
        "MSY-Balanced" = "Experimental max-min fraction of separate component maxima; a compromise, not system MSY.")
      note <- paste(note, "Deterministic Beverton-Holt equilibrium; no SPR or component-persistence constraints. Recruitment-weighted diagnostics use fixed R0. Bounded numerical search (F <= 10), not a certified global optimum.",
        if (isTRUE(z$upper_sensitive)) "Warning: yield improves when the F bound is expanded to 20." else "",
        "Displayed regional diagnostics use", regional_weight_label(regional_mode), "with fixed R0 as the recruitment reference.")
      ms_phi <- per_recruit(f, p, st)$Phi
      ms_w <- regional_weights(p$R0)
      ms_reg <- regional_spr(ms_phi, uf$Phi0, ms_w)
      ms_rec <- regional_spr(ms_phi, uf$Phi0, p$R0)
      trajectories[[kind]] <- project_trajectory(f, kind)
      comparison_rows[[i]] <- data.frame(rule = kind, status = "benchmark", note = note, F1 = f[1], F2 = f[2],
        F_global = if (kind == "MSY-Pattern") max(colSums(fishing_at_age(f, p))) else NA_real_,
        global_spr_rec = global_spr(stock_sbpr(per_recruit(f, p, st)$Phi), stock_sbpr(uf$Phi0), p$R0),
        pop_spr1 = z$pop_spr1, pop_spr2 = z$pop_spr2, regional_spr1 = ms_reg[1], regional_spr2 = ms_reg[2],
        regional_spr_rec1 = ms_rec[1], regional_spr_rec2 = ms_rec[2], regional_weight1 = ms_w[1], regional_weight2 = ms_w[2],
        eq_ssb1 = z$eq_ssb1, eq_ssb2 = z$eq_ssb2, eq_catch1 = z$eq_catch1, eq_catch2 = z$eq_catch2,
        Rref1 = p$R0[1], Rref2 = p$R0[2])
    }
    do.call(rbind, comparison_rows)
  }, error = function(e) data.frame(rule = "Experimental MSY", status = "unavailable", note = conditionMessage(e)))
  write.csv(msy_rows, file.path(out, "msy.csv"), row.names = FALSE, na = "")
}
brp_table <- if (length(rows)) do.call(rbind, rows) else data.frame(rule = character(), status = character(), note = character())
# Keep equilibrium reference points and annual trajectories in separate tables.
# Empty/unavailable results can be displayed without discarding successful rules.
write.csv(brp_table, file.path(out, "brps.csv"), row.names = FALSE, na = "")
tr <- if (length(trajectories)) do.call(rbind, trajectories) else data.frame(rule = character(), year = numeric())
write.csv(tr, file.path(out, "trajectories.csv"), row.names = FALSE, na = "")
write.csv(data.frame(region = 1:2, R0 = p$R0, historical_F = p$F_historical,
  reference_recruitment = bi$Rbar, survey_weight = survey_share, catch_weight = catch_share,
  unfished_ssb = spawning_by_region(uf$Phi0, p$R0),
  historical_years = historical_years, historical_equilibrium_ssb = bi$equilibrium$ssb_region,
  historical_ssb_gap = ssb_gap,
  initial_ssb = bi$last_year$ssb_region, BH_alpha = uf$alpha,
  BH_beta = uf$beta, BH_steepness = uf$h), file.path(out, "conditioning.csv"), row.names = FALSE, na = "")
write.csv(data.frame(age = p$ages, weight1 = p$weight[, 1], weight2 = p$weight[, 2],
  maturity1 = p$maturity[, 1], maturity2 = p$maturity[, 2],
  selectivity1 = p$fishing_selectivity[1, ], selectivity2 = p$fishing_selectivity[2, ],
  survey_selectivity = p$selectivity),
  file.path(out, "biology.csv"), row.names = FALSE)
progress("Complete")
