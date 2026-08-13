univar_theory_elbo_trace <- function(fit_result) {
  tr <- fit_result$elbo
  if (is.null(tr)) return(numeric(0))
  as.numeric(tr)
}
