# Spatial reference points in R

A standalone, base-R version of the two-region model. Start with `run_example.R`.
No external packages or files from the GUI are required.

## Files

- `parameters.R`: edit population structure, recruitment, biology and movement.
- `operating_model.R`: one-year dynamics, lifetime per-recruit calculations,
  equilibrium recruitment, and annual population simulation.
- `reference_points.R`: SPR definitions and reference points. Start with the
  three short functions at the top.
- `yield_reference_points.R`: optional yield/MSY optimization, loaded only when selected.
- `run_example.R`: choose methods and weights, run a history, calculate BRPs,
  project the population, and save two CSV files and one plot in `results/`.

## Run

Open this folder in RStudio and set it as the working directory. Then:

```r
source("run_example.R")
```

Or run `Rscript run_example.R` from this folder. Edit the scripts and rerun.
The default is natal homing, fixed-mean recruitment, 100 historical years,
SPR 40%, and the nine model reference-point formulations.

## Follow the calculation

Read the numbered steps in `run_example.R`: choose settings, run the history,
derive fixed survey/catch weights, solve the selected BRPs, then project each F.
In `operating_model.R`, start with `simulate_year()`, then `per_recruit()` and
`equilibrium()`. `reference_points.R` shows how each SPR definition determines F.

The main names are `p` (parameters), `N[origin, region, age]` (fish numbers),
`Phi[origin, spawning region]` (lifetime spawning biomass per recruit), and
`Rref` (biological reference recruitment). Fishing mortality is per year, biomass
is kg, and targets/probabilities are fractions. Reference-point biomass/catch
describe equilibrium; projection rows describe individual years on the way there.

## Calculate one type at a time

To load just the model and functions:

```r
source("operating_model.R")
source("parameters.R")
source("reference_points.R")

global_result <- global_brp(p)          # global 40%, with recruitment weights
population_result <- population_brp(p) # each birth population at 40%
regional_result <- regional_brp(p)     # each spawning region at 40%
```

Each call returns a one-row data frame. The main columns are `F1`, `F2`, the
population and regional SPRs, equilibrium biomass/catch, and solution status.
`target` is a fraction: use `0.40`, not `40`.

Some other choices:

```r
global_brp(p, target = 0.30)
global_brp(p, weighting = "equal")
global_brp(p, movement = FALSE)
global_brp(p, natal_only = TRUE)        # Natal-RecW

regional_brp(p, origin_weights = c(1, 1))       # equal origin contributions
regional_brp(p, origin_weights = c(.25, .75))  # a custom origin mixture
regional_brp(p, movement = FALSE)             # closed-region reference point
```

`movement = FALSE` omits movement in the reference-point calculation. It keeps
the actual operating model for the reported biomass, catch and SPR diagnostics.
This is why those diagnostic SPRs can differ from the closed-model target.

For survey or catch weights, first run a history:

```r
history <- simulate_population(p, F = c(.28, .12), years = 100)
survey_weights <- colMeans(history$last_year$surveys)
catch_weights <- colSums(history$last_year$catch)

global_brp(p, weighting = "survey", survey_weights = survey_weights)
global_brp(p, weighting = "catch", catch_weights = catch_weights)
regional_brp(p, origin_weights = survey_weights)
regional_brp(p, origin_weights = catch_weights)
```

The functions use weight ratios, so these vectors do not need to sum to one.
`run_example.R` also lets you select individual surveys and average or sum them.

## Compare several methods

Use the same underlying calculation to make one combined table:

```r
comparison <- spr_reference_points(p,
  survey_weights = survey_weights,
  catch_weights = catch_weights,
  rules = c("SpE-RecW", "SpD-RecW", "Pop-Move", "Reg-Move", "Pop-NoMove"))
```

The three entry functions choose a method for you. They share the same numerical
code as this combined calculation. In `reference_points.R`, `solve_global_spr()`
shows the one-equation calculation and `solve_joint_spr()` shows the two-equation
calculation. Short `#` comments explain inputs, outputs and the main steps.

## Operating model

There are two birth origins, two regions, one fleet per region, and four equal
seasons. Numbers are stored as `N[origin, region, age]`. Mortality occurs before
each end-of-season transfer. Spawning is at year fraction 0.625; surveys are at
the midpoint of every season in both regions. The last age is a plus group.

`NH` returns every fish home before spawning. `MPO` allows spawning elsewhere
with origin biology. `MPD` uses occupied-region biology. With metapopulation
recruitment, regional spawners produce recruits entering that region.
Movement, fleet selectivity, weight and maturity can be edited directly.
The history and projections use the same `simulate_population()` function.

## Reference points

- `SpE-*`: global SPR with movement; `SpD-*`: global SPR without movement.
  Suffixes `RecW`, `EqW`, `SurvW`, `CatchW` specify recruitment, equal,
  historical survey or historical catch weights.
- `Natal-RecW`: recruitment-weighted global SPR counting natal-region spawning.
- `Pop-Move`: both birth-population SPRs equal the target, retaining movement.
- `Reg-Move`: both regional spawning-output ratios equal the target.
- `Pop-NoMove`: independent closed-region targets, applied to the actual model.

`Phi[s,r]` is lifetime spawning biomass in region r per recruit of origin s.
Population SPR uses row sums of Phi. Regional ratios combine its columns using
the selected birth-origin weights, identical in fished and unfished calculations.
Reference recruitment is the default; equal, survey, catch and custom mixtures
are alternatives. Survey/catch shares are proxies assigned by birth-region label.
Positive weights cancel under complete natal homing or no movement.

Global methods solve one overall fishing multiplier at a fixed F1:F2 ratio.
Population/regional methods solve F1 and F2 jointly. With BH, recruitment-dependent
targets iterate reference recruitment to match the biological equilibrium.
Unsolved SPR methods return an unavailable row; this does not prove nonexistence
or establish uniqueness. A few numerical safeguards remain to prevent invalid
answers; the folder contains no validation or sensitivity-analysis workflow.

## Optional yield comparisons

Set `yield_methods` in `run_example.R`:

- `Yield-SPR`: maximize total equilibrium yield with both selected regional
  ratios at least the target, using one fixed reference mixture.
- `Yield-Max`: maximize total equilibrium yield with F1 and F2 free. Under BH
  this is joint system MSY; under mean recruitment it is fixed-recruitment yield.
- `MSY-Pattern`: under BH, maximize system yield along your fishing ratio.

You can also call it directly, for example:

```r
# Set recruitment = "BH" in parameters.R and source that file again first.
source("yield_reference_points.R")
msy <- yield_reference_points(p, methods = "Yield-Max")
```

These are bounded numerical maxima over regional F in [0,10], not certificates
of a unique global maximum. Biomass depletion and SPR are different under BH.
