# Spatial BRP explorer

A two-region, four-season fish population model for comparing spawning-potential
reference points, population dynamics and catch.

## Try it in your browser

**[Open the Spatial BRP explorer](https://lichengxue.github.io/spatial-brp-explorer-web/)**

[Quick start and model guide](https://lichengxue.github.io/spatial-brp-explorer-web/guide.html)

No installation or sign-in is needed. The first visit loads R into your browser;
allow about a minute on a slower connection. Calculations run on your device.

1. Start with the opening natal-homing example, or choose another story.
2. Select BRPs on the left. Change movement, biology, recruitment, surveys or
   fishing patterns in **Model settings**.
3. Click **Calculate & project**, then inspect **Dynamics**, **BRP comparison**
   and **Equations**. Optional yield/MSY searches take longer than SPR.
4. Use **Export results** to save settings and result tables. Use **Copy scenario
   link** to share a preset with others.

The web version executes the same R model as the local GUI using WebR.
Reloading clears the session, so export results you want to keep.
The public website source is in
[spatial-brp-explorer-web](https://github.com/lichengxue/spatial-brp-explorer-web).

## Use the R scripts

Set `R/` as your working directory and run:

```r
source("run_example.R")
```

Edit `R/parameters.R` for biology, movement and recruitment, and
`R/run_example.R` for the fishing pattern, SPR target and methods to compare.
The functions `global_brp()`, `population_brp()` and `regional_brp()` calculate
one type at a time. `spr_reference_points()` makes a combined table.
See [R/README.md](R/README.md) for examples. The R scripts need no extra packages.

## Open the interface

Install Python 3.10 or later and base R, then run:

```sh
python app.py
```

On Windows, you can double-click `Launch_GUI.bat`. The app opens at
http://127.0.0.1:8765. Set `RSCRIPT` if Rscript is not on your PATH.
The included Codespaces setup forwards the same port.
If Python is installed elsewhere on Windows, put its full executable path in
`python-path.txt` or set `BRP_GUI_PYTHON`. The path file is ignored by Git.

## Model and reference points

The model supports natal homing and metapopulations with origin- or
destination-based biology. You can change seasonal movement, fleet selectivity,
weight and maturity at age, recruitment, and the fishing pattern.

- Global SPR methods keep a fixed F1:F2 pattern and use recruitment, equal,
  survey or catch weights, with or without movement in the calculation.
- Population and regional methods solve F1 and F2 jointly. Regional origin
  weights can use reference recruitment or an alternative fixed mixture.
- Each region has four surveys, one at each season midpoint. Selected indices
  are averaged or summed within each region to form survey weights.
- Recruitment can be fixed at its mean or follow Beverton-Holt. SPR is the
  default; constrained yield and system MSY comparisons are optional.

The interface shows population, catch and fishing-mortality trajectories,
reference-point tables and equations. Results can be
exported as CSV and JSON. Share the public app link for direct use. A localhost
link works only on the computer running the local server.

## Code

- `R/`: self-contained R calculations and an example.
- `model/`: the operating model and the R code used by the interface.
- `app.py`: the local Python server, using only the standard library.
- `static/`: HTML, CSS and JavaScript.
- `web/`: browser R connection, quick-start guide and static-site build script.
- `tests/test_model.py`: a small regression check.

The GUI calculations are in `model/spatial_brp_core.R`. The self-contained
R scripts provide a shorter implementation of the same equations.
To follow the GUI model, read `simulate_year()` (one year), `per_recruit()`
(one recruit's lifetime), `equilibrium()`, then `reference_points()`.
`model/run_model.R` connects these functions to GUI settings and CSV output.
Optional yield and MSY searches are kept in separate files.

Run the regression check with `python -m unittest discover -s tests`.
Unsolved targets are reported as unavailable. Yield/MSY searches are bounded
numerical optima; an accepted result does not establish uniqueness.

## Build the public app

Run `python web/build_site.py`. This creates `dist/`, including the interface,
R model, guide and downloadable R scripts. To preview it, run
`python -m http.server 8767 --directory dist` and open http://localhost:8767.
The build changes only command-line paths in the GUI runner; biological and BRP
calculations stay in the original R files.

GitHub Pages uses the included **Publish web app** workflow. Choose **GitHub
Actions** under **Settings → Pages → Source**. Pushing to `main` then rebuilds
and publishes the app. GitHub Free requires a public repository for Pages.
