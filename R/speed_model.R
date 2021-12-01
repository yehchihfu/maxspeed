#' Fit time-dependent maximum speed model
#'
#' @param dat `[data.frame]` Subset of the maximum speed database. Requires columns `max_spd_kmh` and `mass_kg`.
#'
#' @return `[nls]` A fitted non-linear regression model.
#' @export
fit_maxspeed <- function(dat) {
  # Numerical methods sometimes fail to fit the model, so just keep trying until
  # it works
  tryCatch({
    stats::nls(
      formula = max_spd_kmh ~ a * mass_kg^b * (1 - exp(-h * mass_kg^i)),
      data = dat,
      start = list(a = runif(1, 11.2 - 0.91, 142.8 + 16.7),
                   b = runif(1, 0.24 - 0.01, 0.36 + 0.02),
                   h = runif(1, 2.4 - 1.4, 19.5 + 13.6),
                   i = runif(1, -0.72 - 0.26, -0.56 + 0.07))
    )},
    error = function(e) fit_maxspeed(dat)
  )
}

#' Maximum speed predictions
#'
#' @param mass_kg `[numeric]` Mass of animals for model prediction (in kg)
#' @param mod `[nls]` A fitted time-dependent maximum speed model (see `fit_maxspeed()`)
#'
#' @return `[numeric]` Predicted maximum speeds for animals of size `mass_kg`
#' @export
predict_maxspeed <- function(mass_kg, mod) {
  stats::predict(mod, newdata = data.frame(mass_kg = mass_kg))
}




