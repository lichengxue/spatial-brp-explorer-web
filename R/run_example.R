# Start here. Set this folder as your working directory, then run:
# source("run_example.R")
# Change biology in parameters.R and calculation settings below.
# Workflow: set inputs -> run history -> derive weights -> solve BRPs -> project.
# All methods start their projections from the same end-of-history numbers.
source("operating_model.R")
source("parameters.R")
source("reference_points.R")

# 1. Choose what to calculate
target <- 0.40
fishing_pattern <- c(0.70, 0.30)  # fully selected F ratio, not catch shares
historical_F <- c(0.28, 0.12)
historical_years <- 100
# A finite history lets the age structure settle; it is not an exact equilibrium.
projection_years <- 60
sigmaR <- 0                      # 0 = deterministic; e.g. 0.4 adds lognormal noise
seed <- 2026

rules <- c("SpE-RecW", "SpE-EqW", "SpE-SurvW", "SpE-CatchW", "SpD-RecW",
           "Natal-RecW", "Pop-Move", "Reg-Move", "Pop-NoMove")
# Additional choices: "SpD-EqW", "SpD-SurvW", "SpD-CatchW".

regional_weighting <- "recruitment"  # "recruitment", "equal", "survey", "catch", "custom"
custom_origin_weights <- c(0.25, 0.75)
survey_use <- matrix(TRUE, 4, 2)     # season rows, region columns; eight surveys
survey_ensemble <- TRUE             # TRUE: average selected indices; FALSE: sum
# Surveys are always calculated at all eight midseason region/season combinations.
# survey_use chooses which indices contribute to the weighting calculation.

yield_methods <- NULL               # NULL means SPR only (the default)
# Optional: c("Yield-SPR", "Yield-Max"). With BH, Yield-Max is JOINT SYSTEM MSY.
# Add "MSY-Pattern" under BH for MSY along the selected fishing ratio.

# 2. Run the history and calculate survey/catch weights
# Use the final historical year's observations, then hold these weights fixed.
# Averaging and summing give the same shares when both regions select the same
# number of surveys. They differ when the numbers of selected surveys differ.
history <- simulate_population(p, historical_F, historical_years)
survey_index <- numeric(2)
for (r in 1:2) {
  x <- history$last_year$surveys[survey_use[, r], r]
  if (!length(x)) stop("Select at least one survey in each region")
  survey_index[r] <- if (survey_ensemble) mean(x) else sum(x)
}
survey_weights <- survey_index / sum(survey_index)
catch_weights <- colSums(history$last_year$catch) / sum(history$last_year$catch)
regional_weights <- switch(regional_weighting,
  recruitment = NULL, equal = c(.5, .5), survey = survey_weights,
  catch = catch_weights, custom = custom_origin_weights,
  stop("Unknown regional weighting"))
# For regional targets, survey/catch shares are proxies for origin contributions,
# assigned by birth-region label. They do not change actual recruitment.

# 3. Calculate the selected reference points in one table
brps <- spr_reference_points(p,
  survey_weights = survey_weights, catch_weights = catch_weights,
  target = target, fishing_pattern = fishing_pattern,
  regional_weights = regional_weights, rules = rules)

# For a single type, you can also run these lines on their own after setup:
# global_brp(p)                                      # global, recruitment weights
# global_brp(p, weighting = "equal")                  # global, equal weights
# global_brp(p, movement = FALSE)                    # global, no movement
# population_brp(p)                                 # population-specific, with movement
# regional_brp(p)                                   # regional, with movement
# regional_brp(p, origin_weights = c(0.25, 0.75))     # regional, custom origin mix
# regional_brp(p, movement = FALSE)                  # closed-region calculation

if (length(yield_methods)) {
  source("yield_reference_points.R")
  # Use one fixed origin mixture throughout the constrained-yield search.
  reg <- brps[brps$rule == "Reg-Move", ]
  if (!nrow(reg)) reg <- regional_brp(p, target = target, origin_weights = regional_weights)
  yield_mix <- if (!is.null(regional_weights)) regional_weights else
    if (reg$status == "calculated") c(reg$Rref1, reg$Rref2) else p$R0
  yields <- yield_reference_points(p, target, yield_mix, fishing_pattern, yield_methods)
  # Yield and SPR results have a few different columns. Add missing columns
  # before combining them, so the saved table includes every selected method.
  columns <- union(names(brps), names(yields))
  for (name in setdiff(columns, names(brps))) brps[[name]] <- NA
  for (name in setdiff(columns, names(yields))) yields[[name]] <- NA
  brps <- rbind(brps[, columns], yields[, columns])
}

# 4. Project each accepted F from the same historical population
# This standalone example holds each BRP's F constant through the projection.
# The GUI additionally offers an annual biomass-feedback fishing rule.
trajectories <- list()
for (i in which(brps$status == "calculated")) {
  set.seed(seed)                     # paired recruitment variation across BRPs
  projection <- simulate_population(p, c(brps$F1[i], brps$F2[i]), projection_years,
    N = history$N, sigmaR = sigmaR)
  trajectories[[brps$rule[i]]] <- data.frame(rule = brps$rule[i], projection$trajectory)
}
projections <- do.call(rbind, trajectories)

dir.create("results", showWarnings = FALSE)
# reference_points.csv contains solved F and equilibrium diagnostics.
# projections.csv contains the annual path from history toward that equilibrium.
write.csv(brps, "results/reference_points.csv", row.names = FALSE, na = "")
write.csv(projections, "results/projections.csv", row.names = FALSE, na = "")
print(brps[, c("rule", "F1", "F2", "population_SPR1", "population_SPR2",
                "regional_SPR1", "regional_SPR2", "yield", "status")], row.names = FALSE)

# 5. Plot biomass, catch and fishing mortality
if (length(trajectories)) {
  colors <- rep(c("#24435C", "#A9502F", "#487A72", "#8A6F3D", "#65718B",
                  "#815973", "#6B773F", "#202D3A", "#A2816B"), length.out = length(trajectories))
  line_types <- rep(c(1, 1, 1, 1, 2, 3, 2, 4, 5), length.out = length(trajectories))
  png("results/projections.png", width = 1800, height = 1500, res = 150)
  layout(matrix(c(1, 2, 3, 4, 5, 6, 7, 7), 4, 2, byrow = TRUE), heights = c(1, 1, 1, .35))
  par(mar = c(4.5, 4.5, 2, 1), mgp = c(2.7, .8, 0), cex = .9)
  fields <- c("ssb1", "ssb2", "catch1", "catch2", "F1", "F2")
  labels <- c("Region 1 spawning biomass", "Region 2 spawning biomass",
              "Region 1 catch", "Region 2 catch", "Region 1 fishing mortality", "Region 2 fishing mortality")
  for (j in seq_along(fields)) {
    values <- matrix(0, projection_years, length(trajectories))
    for (k in seq_along(trajectories)) values[, k] <- trajectories[[k]][[fields[j]]]
    matplot(1:projection_years, values, type = "l", lty = line_types, col = colors, lwd = 1.8,
      xlab = "Projection year", ylab = if (j <= 2) "kg" else if (j <= 4) "kg / year" else "F / year",
      main = labels[j])
  }
  par(mar = rep(0, 4)); plot.new()
  legend("center", names(trajectories), col = colors, lty = line_types, lwd = 2,
         ncol = 3, bty = "n", cex = .9)
  dev.off()
}
message("Finished. Reference points, projections and a plot are in results/.")
