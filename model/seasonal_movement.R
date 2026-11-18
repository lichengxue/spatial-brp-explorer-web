# Convert the four editable pairs of movement probabilities into matrices.
# Rows are departure regions; columns are destinations.
configure_seasonal_movement <- function(p, probabilities) {
  if (!is.matrix(probabilities) || !identical(dim(probabilities), c(4L, 2L)) ||
      any(!is.finite(probabilities)) || any(probabilities < 0 | probabilities > 1))
    stop("Seasonal movement needs four rows of m12 and m21 probabilities in [0,1].")
  p$mu_by_season <- array(0, c(2, 2, 4))
  for (season in 1:4) {
    m12 <- probabilities[season, 1]
    m21 <- probabilities[season, 2]
    p$mu_by_season[, , season] <- rbind(c(1 - m12, m12), c(m21, 1 - m21))
  }
  p$movement <- "custom"
  p
}

# Table used for the movement display and CSV export.
# The actual transfers, including natal return, are in movement_matrices().
movement_schedule <- function(p, st) {
  mu <- if (is.null(p$mu_by_season)) array(rep(p$mu, 4), c(2, 2, 4)) else p$mu_by_season
  home <- st == "NH" & 1:4 == 2
  data.frame(season = 1:4, transfer_time = (1:4) / 4, natal_return = home,
    ordinary_m12 = mu[1, 2, ], ordinary_m21 = mu[2, 1, ],
    applied_m12 = ifelse(home, NA_real_, mu[1, 2, ]),
    applied_m21 = ifelse(home, NA_real_, mu[2, 1, ]))
}
