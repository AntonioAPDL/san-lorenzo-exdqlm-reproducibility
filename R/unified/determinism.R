# unified/determinism.R

unified_default_threads <- function(mode = c("strict", "fast")) {
  mode <- match.arg(mode)
  if (identical(mode, "strict")) {
    return(list(omp = 1L, openblas = 1L, mkl = 1L, veclib = 1L, numexpr = 1L, mc_cores = 1L))
  }
  list(omp = as.integer(Sys.getenv("OMP_NUM_THREADS", "1")),
       openblas = as.integer(Sys.getenv("OPENBLAS_NUM_THREADS", "1")),
       mkl = as.integer(Sys.getenv("MKL_NUM_THREADS", "1")),
       veclib = as.integer(Sys.getenv("VECLIB_MAXIMUM_THREADS", "1")),
       numexpr = as.integer(Sys.getenv("NUMEXPR_NUM_THREADS", "1")),
       mc_cores = as.integer(Sys.getenv("MC_CORES", "1")))
}

unified_apply_thread_env <- function(threads) {
  Sys.setenv(
    OMP_NUM_THREADS = as.character(threads$omp),
    OPENBLAS_NUM_THREADS = as.character(threads$openblas),
    MKL_NUM_THREADS = as.character(threads$mkl),
    VECLIB_MAXIMUM_THREADS = as.character(threads$veclib),
    NUMEXPR_NUM_THREADS = as.character(threads$numexpr)
  )
  options(mc.cores = as.integer(threads$mc_cores))
  invisible(NULL)
}

unified_rng_policy <- function(mode = c("strict", "fast")) {
  mode <- match.arg(mode)
  if (identical(mode, "strict")) {
    list(fit = c("Mersenne-Twister", "Inversion", "Rejection"),
         post = c("Mersenne-Twister", "Inversion", "Rejection"))
  } else {
    list(fit = c("Mersenne-Twister", "Inversion", "Rejection"),
         post = c("Mersenne-Twister", "Inversion", "Rounding"))
  }
}

unified_apply_seed <- function(seed, mode = c("strict", "fast")) {
  mode <- match.arg(mode)
  threads <- unified_default_threads(mode)
  unified_apply_thread_env(threads)

  set.seed(as.integer(seed))
  fit_rng <- unified_rng_policy(mode)$fit
  do.call(RNGkind, as.list(fit_rng))

  Sys.setenv(DISC_BASE_SEED = as.character(as.integer(seed)))
  if (exists("set_sampling_exal_seed", mode = "function")) {
    set_sampling_exal_seed(as.numeric(seed))
  }
  if (exists("set_sampling_truncnorm_seed", mode = "function")) {
    set_sampling_truncnorm_seed(as.numeric(seed))
  }

  invisible(list(seed = as.integer(seed), mode = mode, threads = threads, fit_rng = fit_rng, post_rng = unified_rng_policy(mode)$post))
}
