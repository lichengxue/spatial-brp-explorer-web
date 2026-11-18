# GUI extensions: origin mixtures for regional spawning-output ratios.
# Keep these diagnostic/target weights separate from biological recruitment.
# A survey/catch share is a regional observation used as an origin-mixture proxy,
# assigned by birth-region label; it is not an estimate of observed fish origins.
regional_weight_label <- function(mode) switch(mode,
  recruitment = "Reference recruitment", equal = "Equal origin contributions",
  survey = "Historical survey-share proxy", catch = "Historical catch-share proxy",
  custom = "Custom birth-origin mix", stop("Unknown regional weighting"))

regional_origin_weights <- function(mode, Rref, survey_share, catch_share, custom_share = .5) {
  w <- switch(mode, recruitment = Rref, equal = c(.5, .5), survey = survey_share,
    catch = catch_share, custom = c(custom_share, 1-custom_share),
    stop("Unknown regional weighting"))
  # Extinct reference recruitment cannot define a mixture; preserve missing diagnostics.
  if (length(w) != 2L || any(!is.finite(w)) || any(w < 0) || sum(w) <= 0)
    return(c(NA_real_, NA_real_))
  w / sum(w)
}

regional_reference_point <- function(p, struct, Rbar, survey_share, target, uf,
                                     catch_share, mode = "recruitment", custom_share = .5) {
  if (mode == "recruitment") {
    if (p$recruitment == "mean") {
      rp <- reference_points(p, struct, p$R0, survey_share, target, uf,
        which = "Reg-Move", catch_share = catch_share)
      rp$Rref1 <- p$R0[1]
      rp$Rref2 <- p$R0[2]
      return(rp)
    }
    return(reference_points_consistent(p, struct, Rbar, survey_share, target, uf,
      which = "Reg-Move", catch_share = catch_share))
  }
  w <- regional_origin_weights(mode, p$R0, survey_share, catch_share, custom_share)
  if (any(!is.finite(w))) stop("The selected regional origin mix is undefined")
  rp <- reference_points(p, struct, w, survey_share, target, uf,
    which = "Reg-Move", catch_share = catch_share)
  f <- c(rp$F1, rp$F2)
  Rref <- if (p$recruitment == "mean") p$R0 else equilibrium(f, p, struct, uf)$R
  # The index determines F; actual biological recruitment determines biomass targets.
  # This matters when the GUI uses these targets in its biomass-feedback projection.
  biomass <- spawning_by_region(per_recruit(f, p, struct)$Phi, Rref)
  rp$Btgt1 <- biomass[1]
  rp$Btgt2 <- biomass[2]
  rp$Rref1 <- Rref[1]
  rp$Rref2 <- Rref[2]
  rp
}
