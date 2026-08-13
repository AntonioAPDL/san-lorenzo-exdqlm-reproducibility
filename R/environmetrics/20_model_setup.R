###############################################################################
# Model setup and core matrices
# Inputs:
#   - X, X_f, Y, timestamps, model hyperparameters
# Outputs:
#   - Model matrices, priors, and forecast structures
# Dependencies:
#   - 00_constants.R, 02_helpers_core.R
###############################################################################

structure_helper_path <- file.path("R", "unified", "families", "exdqlm_multivar_structure.R")
if (!file.exists(structure_helper_path)) {
  stop(sprintf("Missing exdqlm multivar structure helper: %s", structure_helper_path), call. = FALSE)
}
source(structure_helper_path)

if(use_covariates){
  ending <- "_exAL_synth_DISC"
}else{
  ending <- "_exAL_synth_simp"
}
#
# Model setup without covariates
s_yy <- sd(Y, na.rm = TRUE)
m_yy <- mean(Y, na.rm = TRUE) + s_yy * qnorm(p0)
kk <- 0.1 * s_yy
structure_spec <- exdqlm_multivar_read_structure_spec_from_env(
  include_trend_keys = c("UNIFIED_EXDQLM_MULTIVAR_INCLUDE_TREND", "DISC_W_INCLUDE_TREND"),
  enabled_harmonic_keys = c("UNIFIED_EXDQLM_MULTIVAR_ENABLED_HARMONIC_INDICES", "DISC_W_ENABLED_HARMONIC_INDICES"),
  default_harmonics = harmonics
)
structure_model <- exdqlm_multivar_build_structure(
  m_yy = m_yy,
  kk = kk,
  df_t = df_t,
  df_s1 = df_s1,
  df_s2 = df_s2,
  df_s67 = df_s67,
  lam1 = lam1,
  lam2 = lam2,
  include_trend = structure_spec$include_trend,
  enabled_harmonic_indices = structure_spec$enabled_harmonic_indices,
  default_harmonics = harmonics,
  season_period = 363.5854,
  trend_c0_scale = 1.0,
  season_c0_scale = 0.08
)
harm <- structure_model$enabled_harmonics
model <- structure_model$model
p <- structure_model$p
#
idx <- 1:TT
y <- Y[,idx]
TT_sub <- length(idx)
#
if (is.null(nrow(y))) {
  JJJ <- 1
  y <- array(y, c(JJJ, length(y)))
} else {
  JJJ <- nrow(Y)
  y <- array(y, c(JJJ, ncol(y)))
}
#
gam.init <- array(rep(0, JJJ), c(JJJ, 1))
sig.init <- array(rep(1, JJJ), c(JJJ, 1))
PriorSigma <- array(NA_real_, c(JJJ, 2))
PriorGamma <- array(NA_real_, c(JJJ, 3))
verbose <- TRUE

###########################################################################################
###########################################################################################
###########################################################################################
m0 <- c(model$m0, rep(0, p*J))
C0 <- bdiag(model$C0, 0.1 * kk * diag(p*J))
##########################################  
##########################################
df <- structure_model$df
df.discrep <- df.discrep*rep(df,J)
dim.df <- structure_model$dim.df
k <- 10
##########################################2
##########################################
model_simp <- model
df_simp <- df
dim.df_simp <- dim.df
model_simp$GG <- array(model_simp$GG, c(p, p, TT))
model_simp$FF <- array(model_simp$FF, c(p, 1, TT))
##########################################2
##########################################
df.mat <- make_df_mat(df, dim.df, p)
df.mat.k <- make_df_mat_k(df, dim.df, p, k)

df1 <- structure_model$df1
df.mat_f1 <- make_df_mat(df1, dim.df, p)
df.mat.k_f1 <- make_df_mat_k(df1, dim.df, p, k)
df2 <- structure_model$df2
df.mat_f2 <- make_df_mat(df2, dim.df, p)
df.mat.k_f2 <- make_df_mat_k(df2, dim.df, p, k)

# df.mat_f2 <- make_df_mat(df*lam1, dim.df, p)
# df.mat.k_f2 <- make_df_mat_k(df*lam1, dim.df, p, k)
# df.mat_f2 <- make_df_mat(df*lam2, dim.df, p)
# df.mat.k_f2 <- make_df_mat_k(df*lam2, dim.df, p, k)

if (J <= 0) {
  ex.df.mat <- df.mat
  ex.df.mat.k <- df.mat.k
} else {
  extra_df.mat <- make_df_mat(df.discrep, c(rep(dim.df,J)), p*J)
  extra_df.mat.k<- make_df_mat_k(df.discrep, c(rep(dim.df,J)), p*J, k)
  
  ex.df.mat <- bdiag(df.mat, extra_df.mat)
  ex.df.mat.k <- bdiag(df.mat.k, extra_df.mat.k)

  ex.df.mat_f_T <- bdiag(df.mat_f1, extra_df.mat)
  ex.df.mat_f_T <- as.matrix(ex.df.mat_f_T)

  ex.df.mat.k_f_T <- bdiag(df.mat.k_f1, extra_df.mat.k)
  ex.df.mat.k_f_T <- as.matrix(ex.df.mat.k_f_T)

  ex.df.mat_f <- bdiag(df.mat_f2, extra_df.mat)
  ex.df.mat_f <- as.matrix(ex.df.mat_f)

  ex.df.mat.k_f <- bdiag(df.mat.k_f2, extra_df.mat.k)
  ex.df.mat.k_f <- as.matrix(ex.df.mat.k_f)

  # Get the dimensions of the input matrices
  n <- nrow(ex.df.mat_f)
  m <- ncol(ex.df.mat_f)
  DF.MAT <- array(0, dim = c(n, m, 2))
  DF.MAT[,,1] <- ex.df.mat_f_T
  DF.MAT[,,2] <- ex.df.mat_f

  DF.MAT_k <- array(0, dim = c(n, m, 2))
  DF.MAT_k[,,1] <- ex.df.mat.k_f_T
  DF.MAT_k[,,2] <- ex.df.mat.k_f

}

create_block_diag <- exdqlm_multivar_create_block_diag


# Discrepancies
A <- model$GG; n <- J+1;
result_GG <- create_block_diag(A, n);
GG <- array(result_GG, dim = c(dim(result_GG)[1], dim(result_GG)[1], TT))
model$GG <- GG

A <- model$FF; n <- J+1;
result_FF <- create_block_diag(A, n);
result_FF[1:p,] <- matrix(model$FF, p, J + 1)
FF <- array(result_FF, c(p*(1 + J), 1 + J, TT))
model$FF <- FF

FF <- model$FF
GG <- model$GG
model$m0 <- m0 
model$C0 <- C0 
ppx <- 0




if (use_covariates) {
  px <- dim(X)[2]
  ppx <- px + 1

  FFx <- array(0, c(dim(FF)[1] + ppx, dim(FF)[2], TT))
  FFx[1:dim(FF)[1],1:dim(FF)[2],] <- FF
  GGx <- array(0, c(dim(GG)[1] + ppx, dim(GG)[2]+ ppx, TT))
  GGx[1:dim(GG)[1],1:dim(GG)[2],] <- GG

  Fx <- rbind(rep(1, J + 1), matrix(0, nrow = px, ncol = J + 1))
  FFx[(dim(FF)[1]+1):dim(FFx)[1],,] <- Fx 

  Gx <- as.matrix(bdiag(lambda, diag(px)))
  Gx <- array(rep(Gx, TT), dim = c(ppx, ppx, TT))
  if (ppx > 1L) {
    Gx[1, 2:ppx, ] <- as.matrix(t(X))
  }
  GGx[(dim(GG)[1]+1):dim(GGx)[1],(dim(GG)[2]+1):dim(GGx)[1],] <- Gx

  model$FF <- FFx
  model$GG <- GGx

  extra_df.mat <- make_df_mat(c(df_trans,df_covs), c(1,px), ppx)
  extra_df.mat.k <- make_df_mat_k(c(df_trans,df_covs), c(1,px), ppx, k)

  ex.df.mat <- bdiag(ex.df.mat, extra_df.mat)
  ex.df.mat.k <- bdiag(ex.df.mat.k, extra_df.mat.k)

  model$m0 <- c(model$m0, rep(0, ppx))
  model$C0 <- bdiag(model$C0, 0.01 * kk * diag(ppx))
  
  FF <- model$FF
  GG <- model$GG

}




L = L_fn(p0)
U = U_fn(p0)

FF_list <- vector("list", J)
GG_list <- vector("list", J)

######################
# Forecast transfer mode in post/legacy synthesis:
# `drop` keeps legacy behavior; `keep` retains transfer coordinates in forecast FF/GG lists.
forecast_transfer_mode <- tolower(trimws(Sys.getenv("UNIFIED_MULTIVAR_FORECAST_TRANSFER_MODE", "drop")))
if (!forecast_transfer_mode %in% c("drop", "keep")) {
  forecast_transfer_mode <- "drop"
}
keep_transfer_forecast <- isTRUE(use_covariates) && ppx > 0L && identical(forecast_transfer_mode, "keep")

ranges_per_local <- if (J > 1) ranges - c(ranges[2:J], 0) else ranges
r_vec_local <- rev(ranges_per_local)
seg_start_local <- cumsum(c(1, head(r_vec_local, -1)))

for (j in 1:J) {
  jj <- J-j+1
  core_dim <- p * (jj + 1L)
  GG_tsc <- result_GG[1:core_dim, 1:core_dim, drop = FALSE]
  FF_tsc <- result_FF[1:core_dim, 2:(jj+1), drop = FALSE]

  if (keep_transfer_forecast) {
    seg_len <- as.integer(r_vec_local[j])
    state_dim <- core_dim + ppx
    G_transfer <- as.matrix(bdiag(lambda, diag(px)))
    GG_base <- matrix(0, nrow = state_dim, ncol = state_dim)
    GG_base[1:core_dim, 1:core_dim] <- GG_tsc
    GG_base[(core_dim + 1L):state_dim, (core_dim + 1L):state_dim] <- G_transfer

    has_time_varying_future_covs <-
      ppx > 1L &&
      exists("X_f", inherits = TRUE) &&
      is.numeric(X_f) &&
      nrow(X_f) > 0L &&
      is.finite(seg_len) &&
      seg_len > 0L

    if (has_time_varying_future_covs) {
      seg_from <- seg_start_local[j]
      seg_to <- seg_from + seg_len - 1L
      seg_to <- min(seg_to, nrow(X_f))
      seg_from <- max(1L, min(seg_from, seg_to))
      X_seg <- as.matrix(X_f[seg_from:seg_to, seq_len(ppx - 1L), drop = FALSE])
      if (nrow(X_seg) < seg_len) {
        pad_n <- seg_len - nrow(X_seg)
        X_seg <- rbind(
          X_seg,
          matrix(rep(X_seg[nrow(X_seg), ], each = pad_n), nrow = pad_n, byrow = TRUE)
        )
      }
      if (nrow(X_seg) > seg_len) {
        X_seg <- X_seg[seq_len(seg_len), , drop = FALSE]
      }

      GG_seg <- array(0, dim = c(state_dim, state_dim, seg_len))
      for (tt in seq_len(seg_len)) {
        GG_tt <- GG_base
        GG_tt[core_dim + 1L, (core_dim + 2L):state_dim] <- as.numeric(X_seg[tt, , drop = TRUE])
        GG_seg[, , tt] <- GG_tt
      }
      GG_list[[j]] <- GG_seg
    } else {
      GG_list[[j]] <- GG_base
    }
    transfer_load <- rbind(rep(1, jj), matrix(0, nrow = px, ncol = jj))
    FF_list[[j]] <- rbind(FF_tsc, transfer_load)
  } else {
    GG_list[[j]] <- matrix(GG_tsc, nrow = core_dim, ncol = core_dim)
    FF_list[[j]] <- matrix(FF_tsc, nrow = core_dim, ncol = jj)
  }
}

########### For every j
bad_gam <- !is.na(gam.init[, 1]) & (gam.init[, 1] < L | gam.init[, 1] > U)
if (any(bad_gam)) {
  stop(sprintf(
    "gam.init must be between %s and %s for %s quantile",
    round(L, 3), round(U, 3), p0
  ))
}
###########################################################################################
########### For every j
m_sigma <- 1
v_sigma <- 1e+10
idx_sigma <- is.na(PriorSigma[, 1]) | is.na(PriorSigma[, 2])
if (any(idx_sigma)) {
  PriorSigma[idx_sigma, 1] <- (m_sigma^2) / v_sigma + 2
  PriorSigma[idx_sigma, 2] <- (m_sigma^3) / v_sigma + m_sigma
}
###########################################################################################
########### For every j
idx_gamma <- is.na(PriorGamma[, 1]) | is.na(PriorGamma[, 2]) | is.na(PriorGamma[, 3])
if (any(idx_gamma)) {
  PriorGamma[idx_gamma, 1] <- 0
  PriorGamma[idx_gamma, 2] <- 1e+10
  PriorGamma[idx_gamma, 3] <- 1
}
###########################################################################################
########### For every j
gam0 = gam.init 
sig0 = sig.init 
A0 <- A_fn(p0, gam0)
B0 <- B_fn(p0, gam0)
C0 <- C_fn(p0, gam0)
abs_gam0 <- abs(gam0)

preallocate_matrix_list <- function(column_counts, num_rows) {
  n_list <- length(column_counts)
  if (length(num_rows) != n_list) {
    stop(sprintf(
      "preallocate_matrix_list: num_rows length (%d) must match column_counts length (%d)",
      as.integer(length(num_rows)),
      as.integer(n_list)
    ), call. = FALSE)
  }
  matrix_list <- vector("list", n_list)
  for (i in seq_along(column_counts)) {
    num_cols <- suppressWarnings(as.integer(column_counts[i]))
    num_rows_i <- suppressWarnings(as.integer(num_rows[i]))
    if (!is.finite(num_cols) || num_cols <= 0L) {
      stop(sprintf("preallocate_matrix_list: invalid num_cols at i=%d (%s)", as.integer(i), as.character(column_counts[i])), call. = FALSE)
    }
    if (!is.finite(num_rows_i) || num_rows_i <= 0L) {
      stop(sprintf("preallocate_matrix_list: invalid num_rows at i=%d (%s)", as.integer(i), as.character(num_rows[i])), call. = FALSE)
    }
    matrix_list[[i]] <- matrix(NA_real_, nrow = num_rows_i, ncol = num_cols)
  }
  matrix_list
}
fill_with_scalar <- function(matrix_list, scalar, label) {
  val <- suppressWarnings(as.numeric(scalar))
  if (length(val) != 1L || !is.finite(val)) {
    stop(sprintf("%s must be a finite scalar; got length=%d value=%s", label, as.integer(length(val)), as.character(scalar)), call. = FALSE)
  }
  for (i in seq_along(matrix_list)) {
    matrix_list[[i]][] <- val
  }
  matrix_list
}

###########################################################################################
########### For every j 

# Gamma, Sigma
E1 <- array(NA_real_, c(J+1,1))
E1[,] <- 1
E2 <- array(NA_real_, c(J+1,1))
E2[,] <- 1
new.gamsig.out = list(E.gam = gam0,
                      V.gam = E1, 
                      E.sigma = sig0, 
                      V.sig = E2,
                      E.inv.sigma = 1/sig0, 
                      E.c2.invb.absgam2.sigma = sig0 * (C0^2) * (abs_gam0^2) / B0, 
                      E.c.invb.absgam = C0 * abs_gam0 / B0,  
                      E.c.a.invb.absgam = C0 * A0 * abs_gam0 / B0, 
                      E.a2.invb.inv.sigma = (A0^2) / (B0 * sig0), 
                      E.invb.inv.sigma = 1 / (sig0 * B0), 
                      E.a.invb.inv.sigma = A0 / (B0 * sig0),
                      E.log.sig.b = log(sig0 * B0),
                      E.log.sig = log(sig0),
                      E.prior.sig.gam = array(0, c(J+1,1)),
                      entrop = array(0, c(J+1,1))  )
###########################################################################################
########### For every j

# S_t (Before Forecast)
E1 <- array(NA_real_, c(J+1,TT_sub))
E1[,] <- truncnorm::etruncnorm(a = 0, b = Inf,  mean = 1, sd = 0.1)
E2 <- array(NA_real_, c(J+1,TT_sub))
E2[,] <- E1[,]^2 
new.sts.out = list(E.sts = E1, 
                    E.sts2 = E2,
                    tot.entrop = array(0, c(J+1,1)) )
# S_t (After Forecast)
E1 <- preallocate_matrix_list(num_mem, ranges)
E2 <- preallocate_matrix_list(num_mem, ranges)
E1 <- fill_with_scalar(E1, 1, "new.sts.out_f E.sts init")
E2 <- fill_with_scalar(E2, 1, "new.sts.out_f E.sts2 init")

entrop_s <- preallocate_matrix_list(num_mem, rep(1,J) )
entrop_s <- fill_with_scalar(entrop_s, 0, "new.sts.out_f entrop init")

new.sts.out_f = list(E.sts = E1, 
                    E.sts2 = E2,
                    tot.entrop = entrop_s )

###########################################################################################
########### For every j

# U_t (Before Forecast)
E1 <- array(NA_real_, c(J+1,TT_sub))
E1[,] <- 1/sig0
E2 <- array(NA_real_, c(J+1,TT_sub))
E2[,] <- sig0
new.uts.out = list(E.uts = E1, 
                    E.inv.uts = E2,
                    E.log.uts = array(0, c(J+1,1)),
                    tot.entrop = array(0, c(J+1,1)) )

# U_t (After Forecast)
E1 <- preallocate_matrix_list(num_mem, ranges)
E2 <- preallocate_matrix_list(num_mem, ranges)
for (jj in seq_len(J)) {
  sigma_j <- suppressWarnings(as.numeric(sig0[jj + 1, 1]))
  if (!is.finite(sigma_j) || sigma_j <= 0) {
    stop(sprintf("Invalid sigma seed for forecast ensemble j=%d: %s", as.integer(jj), as.character(sigma_j)), call. = FALSE)
  }
  E1[[jj]][] <- 1 / sigma_j
  E2[[jj]][] <- sigma_j
}

entrop_u <- preallocate_matrix_list(num_mem, rep(1,J))
entrop_u <- fill_with_scalar(entrop_u, 0, "new.uts.out_f entrop init")

new.uts.out_f = list(E.uts = E1, 
                    E.inv.uts = E2,
                    E.log.uts = entrop_u,
                    tot.entrop = entrop_u )

###########################################################################################
# Exps
init.dlm = dlm_df(colMeans(y), model_simp, df_simp, dim.df_simp, 
                  s.priors = list(l0 = 1, S0 = mean(sig0)), 
                  just.lik = FALSE)
FF_t <- aperm(model_simp$FF, c(2, 1, 3))
multiply_matrices <- function(slice_index) {
  t(FF_t[1,,slice_index]) %*% init.dlm$m[slice_index,]
}
result_list <- lapply(1:TT_sub, multiply_matrices)
result_array <- array(unlist(result_list), dim = c(TT_sub,1))
exps0 = c(result_array) + stats::qnorm(p0, 0, sqrt(init.dlm$s[TT_sub]))
exps0 = t(replicate(J+1, exps0))

exps0 <- cbind(exps0,mean_forecast)
exps2 <- exps0^2

new.theta.out = list(exps = exps0, 
                      exps2 = exps2)
###########################################################################################
iter = 0
conv.count = 0
new.max = Inf
###########################################################################################
########### For every j
seq.gamma = new.gamsig.out$E.gam
seq.sigma = new.gamsig.out$E.sigma
###########################################################################################
update_sts<-function(y, exps,inv.uts,c2.invb.absgam2.sigma,c.invb.absgam,c.a.invb.absgam, TTT){
  s.sig2<-1/(1+c2.invb.absgam2.sigma*inv.uts); s.sig = sqrt(s.sig2)
  s.mu<-s.sig2*(c.invb.absgam*(y-exps)*inv.uts-c.a.invb.absgam)
  #
  E.sts = truncnorm::etruncnorm(a=rep(0,TTT),b=rep(Inf,TTT),mean=s.mu,sd=s.sig)
  V.sts = truncnorm::vtruncnorm(a=rep(0,TTT),b=rep(Inf,TTT),mean=s.mu,sd=s.sig)
  E.sts2 = s.mu^2 + s.sig2 + s.mu*s.sig*exp(stats::dnorm(-s.mu/s.sig,log = TRUE)-stats::pnorm(s.mu/s.sig,log.p = TRUE))
  return(list(sts.sig2=s.sig2,sts.mu=s.mu,
              E.sts=E.sts,E.sts2=E.sts2,
              tot.entrop = sum(0.5*log2(2*pi*exp(1)*s.sig2) - 1 )))
}

Kprime <- function(x){
sqrt(pi/2/x) * expint_E1(2*x) * exp(x)
}

gig_entrop <- function(a,b){
nu <- 0.5
s.ab <- sqrt(a*b)
K1 <- besselK(s.ab, nu)
K2 <- besselK(s.ab, nu+1)
K3 <- besselK(s.ab, nu-1)
y <- 0.5*log(b/a) + log(2*K1) - (nu-1)*Kprime(s.ab)/K1 + s.ab/2/K1*(K2 + K3)
return(y)
}

###########################################################################################

update_uts<-function(y, exps,exps2,sts,sts2,inv.sigma,a2.invb.inv.sigma,invb.inv.sigma,c.invb.absgam,c2.invb.absgam2.sigma){
  u.lambda = 0.5
  u.psi = (a2.invb.inv.sigma + 2*inv.sigma)
  u.chi = invb.inv.sigma*(y^2-2*y*exps+exps2) - 2*c.invb.absgam*sts*(y-exps) + c2.invb.absgam2.sigma*sts2
  u.chi[u.chi<=0] = 1e-6
  #
  E.uts = sapply(u.chi,function(x){sqrt(x/u.psi)*HyperbolicDist::besselRatio(sqrt(x*u.psi),u.lambda,1,Inf)})
  E.inv.uts = sapply(u.chi,function(x){sqrt(u.psi/x)*HyperbolicDist::besselRatio(sqrt(x*u.psi),u.lambda,1,Inf)-2*u.lambda/x})

  nu <- 0.5
  s.ab <- sqrt(u.psi*u.chi)
  K1 <- besselK(s.ab, nu)

  return(list(uts.lambda=u.lambda,
              uts.psi=u.psi,uts.chi=u.chi,
              E.uts=E.uts,E.inv.uts=E.inv.uts,
              E.log.uts=sum(Kprime(s.ab)/K1-0.5*log(u.psi/u.chi)),
              tot.entrop=sum(gig_entrop(u.psi,u.chi))))
}

###########################################################################################
########################
PriorGammaDens <- function(gamma, prior) {
  crch::dtt(gamma, 
            location = prior[1], 
            scale = prior[2],   
            df = prior[3], 
            left = L, right = U, 
            log = FALSE)
}

LL <- L+0.001
UU <- U-0.001

# -----------------------------------------------------------------------------
# STALE DUPLICATE LAPALCE-DELTA PATH
#
# This block is retained as historical reference material for older workflows.
# It is not the authoritative sigma/gamma implementation for current unified
# launches. The active path lives in:
#   DISC_Optimal_Synth_Ranges_W_transfer_forecast.r
#
# Important differences:
# - this duplicate path still uses the historical interior gamma map
#   gamma = LL + (UU - LL) * exp(-exp(theta_g))
# - current production launches use the logistic transform audited in the
#   exdqlm theory docs and implemented in the active script above
#
# Keep this block semantically readable, but do not treat it as current theory
# or current production behavior.
# -----------------------------------------------------------------------------
update_gamma_sigma<-function( y, nn, prior_g, prior_s, 
                              gamma,var.gam,sigma,var.sig,
                              exps,exps2,
                              sts,sts2,
                              uts,inv.uts, 
                              s_init, g_init,
                              Climate_Center,
                              ensembles_j = NULL, num_mem_j = NULL, k_forecast = NULL,
                              sts_f = NULL,sts2_f = NULL,
                              uts_f= NULL,inv.uts_f= NULL){

if(!Climate_Center){
  dq_transf <- function(theta_s,theta_g){
      sig <- exp(theta_s)
      gam <- LL+(-LL+UU)*exp(-exp(theta_g))
          a = A_fn(p0,gam); b = B_fn(p0,gam); c = C_fn(p0,gam); p_fn(p0,gam)

      # Prior
      yy <- log(PriorGammaDens(gam, prior_g)) - (prior_s[1] + 1) * log(sig) - prior_s[2]/sig

      # Likelihood
      yy <- yy - (1.5*nn)*log(sig) - (0.5*nn)*log(b)-sum(uts)/sig 
      yy <- yy - 0.5*sum( inv.uts*(y^2-2*y*exps+exps2)/sig
                      - (y-exps)*2*(inv.uts*c*abs(gam)*sts + a/sig)
                      + sig*inv.uts*(c^2)*(abs(gam)^2)*sts2
                      + 2*c*abs(gam)*sts*a
                      + (uts*a^2)/sig )/b
      
      # Jacobian
      yy <- yy + theta_s + theta_g - exp(theta_g)                   
      return(yy)
  }
}else{

  ensembles_j <- matrix(c(as.matrix(ensembles_j)),ncol = 1)
  sts_f <-  matrix(c(as.matrix(sts_f)),ncol = 1)
  sts2_f <-  matrix(c(as.matrix(sts2_f)),ncol = 1)
  uts_f <-  matrix(c(as.matrix(uts_f)),ncol = 1)
  inv.uts_f <-  matrix(c(as.matrix(inv.uts_f)),ncol = 1)

  dq_transf <- function(theta_s,theta_g){
      sig <- exp(theta_s)
      gam <- LL+(-LL+UU)*exp(-exp(theta_g))
          a = A_fn(p0,gam); b = B_fn(p0,gam); c = C_fn(p0,gam);

      # Prior
      yy <- log(PriorGammaDens(gam, prior_g)) - (prior_s[1] + 1) * log(sig) - prior_s[2]/sig

      # Likelihood
      yy <- yy - 1.5*(nn+k_forecast*num_mem_j)*log(sig) - (0.5*(nn+k_forecast*num_mem_j))*log(b)-(sum(uts)+sum(uts_f))/sig 
      # Before Forecast
      yy <- yy - 0.5*sum( inv.uts*(y^2-2*y*exps[1:nn]+exps2[1:nn])/sig
                      - (y-exps[1:nn])*2*(inv.uts*c*abs(gam)*sts + a/sig)
                      + sig*inv.uts*(c^2)*(abs(gam)^2)*sts2
                      + 2*c*abs(gam)*sts*a
                      + (uts*a^2)/sig )/b
      # After Forecast
      yy <- yy - 0.5*sum( inv.uts_f*(ensembles_j^2-2*ensembles_j*exps[(nn+1):(nn+k_forecast)]+exps2[(nn+1):(nn+k_forecast)])/sig
                      - (ensembles_j-exps[(nn+1):(nn+k_forecast)])*2*(inv.uts_f*c*abs(gam)*sts_f + a/sig)
                      + sig*inv.uts_f*(c^2)*(abs(gam)^2)*sts2_f
                      + 2*c*abs(gam)*sts_f*a
                      + (uts_f*a^2)/sig )/b
      # Jacobian
      yy <- yy + theta_s + theta_g - exp(theta_g)                   
      return(yy)
  }
}

  theta_s_init <- log(s_init)
  theta_g_init <- log(log((-L+U)/(-L+g_init)))
  initial_values <- c(theta_s_init, theta_g_init)

  # Optimization step
  optim_results <- optim(par = initial_values, 
                      fn = function(x) -dq_transf(x[1], x[2]), # Maximizing by minimizing the negative
                      method = "L-BFGS-B", # This method allows box constraints
                      lower = c(-Inf, -Inf), # Transform bounds for gam to theta_g space if needed
                      upper = c(Inf, Inf),
                      hessian = TRUE)
  # Evaluate the Hessian at the optimal value
  hessian_at_optimal <- -optim_results$hessian # SINCE WE MIN -f, not MAX f
  # Take the inverse of the Hessian
  inverse_hessian <- solve(hessian_at_optimal)

  LD_mu <- optim_results$par
  LD_S <- -inverse_hessian 

  Expected_f <- function(f, theta_s, theta_g){
      x <- numDeriv::hessian(func = f, x = LD_mu)%*%LD_S
      e <- f(LD_mu) + 0.5*sum(diag(x))
    return(e)
  }

  f.exp.theta_g <- function(theta){
    sig = exp(theta[1]); gam = LL+(-LL+UU)*exp(-exp(theta[2]));
    a = A_fn(p0,gam); b = B_fn(p0,gam); c = C_fn(p0,gam);
    yy <- exp(theta[2])
    return(yy)
  }

  f.log.sig.b <- function(theta){
    sig = exp(theta[1]); gam = LL+(-LL+UU)*exp(-exp(theta[2]));
    a = A_fn(p0,gam); b = B_fn(p0,gam); c = C_fn(p0,gam);
    yy <- log(sig*b)
    return(yy)
  }

  f.log.sig <- function(theta){
    sig = exp(theta[1]); gam = LL+(-LL+UU)*exp(-exp(theta[2]));
    a = A_fn(p0,gam); b = B_fn(p0,gam); c = C_fn(p0,gam);
    yy <- log(sig)
    return(yy)
  }

  f.prior.sig.gam <- function(theta){
    sig = exp(theta[1]); gam = LL+(-LL+UU)*exp(-exp(theta[2]));
    a = A_fn(p0,gam); b = B_fn(p0,gam); c = C_fn(p0,gam);
    yy <- crch::dtt(gam, location = prior_g[1], scale = prior_g[2], df = prior_g[3], left = L, right = U, log = TRUE)
    yy <- yy + nimble::dinvgamma(sig, shape = prior_s[1], scale =  prior_s[2], log = TRUE)
    return(yy)
  }


  f.c2.s.abs.g2.inv.b <- function(theta){
    sig = exp(theta[1]); gam = LL+(-LL+UU)*exp(-exp(theta[2]));
    a = A_fn(p0,gam); b = B_fn(p0,gam); c = C_fn(p0,gam);
    yy <- c^2*sig*abs(gam)^2/b
    return(yy)
  }

  f.inv.sig <- function(theta){
    sig = exp(theta[1])
    yy <- 1/sig
    return(yy)
  }

  f.c.abs.g.inv.b <- function(theta){
    gam = LL+(-LL+UU)*exp(-exp(theta[2]))
    b = B_fn(p0,gam); c = C_fn(p0,gam);
    yy <- c*abs(gam)/b
    return(yy)
  }

  f.c.abs.g.a.inv.b <- function(theta){
    sig = exp(theta[1]); gam = LL+(-LL+UU)*exp(-exp(theta[2]));
    a = A_fn(p0,gam); b = B_fn(p0,gam); c = C_fn(p0,gam);
    yy <- c*abs(gam)*a/b
    return(yy)
  }

  f.inv.s.inv.b <- function(theta){
    sig = exp(theta[1]); gam = LL+(-LL+UU)*exp(-exp(theta[2]));
    a = A_fn(p0,gam); b = B_fn(p0,gam); c = C_fn(p0,gam);
    yy <- 1/sig/b
    return(yy)
  }

  f.a.inv.s.inv.b <- function(theta){
    sig = exp(theta[1]); gam = LL+(-LL+UU)*exp(-exp(theta[2]));
    a = A_fn(p0,gam); b = B_fn(p0,gam); c = C_fn(p0,gam);
    yy <- a/sig/b
    return(yy)
  }

  f.a2.inv.s.inv.b <- function(theta){
    sig = exp(theta[1]); gam = LL+(-LL+UU)*exp(-exp(theta[2]));
    a = A_fn(p0,gam); b = B_fn(p0,gam); c = C_fn(p0,gam);
    yy <- a^2/sig/b
    return(yy)
  }

  f.sig <- function(theta){
    sig = exp(theta[1]); 
    yy <- sig
    return(yy)
  }

  f.gam <- function(theta){
    gam = LL+(-LL+UU)*exp(-exp(theta[2]));
    yy <- gam
    return(yy)
  }

  #############################################################################################################################################
  #############################################################################################################################################

  E.sig = Expected_f(f.sig, LD_mu[1], LD_mu[2]);
  E.gam = Expected_f(f.gam, LD_mu[1], LD_mu[2]);


  E.inv.sigma = Expected_f(f.inv.sig, LD_mu[1], LD_mu[2])
  E.c2.invb.absgam2.sigma = Expected_f(f.c2.s.abs.g2.inv.b, LD_mu[1], LD_mu[2])
  E.c.invb.absgam = Expected_f(f.c.abs.g.inv.b, LD_mu[1], LD_mu[2])
  E.c.a.invb.absgam = Expected_f(f.c.abs.g.a.inv.b, LD_mu[1], LD_mu[2])
  E.a2.invb.inv.sigma = Expected_f(f.a2.inv.s.inv.b, LD_mu[1], LD_mu[2])
  E.invb.inv.sigma = Expected_f(f.inv.s.inv.b, LD_mu[1], LD_mu[2])
  E.a.invb.inv.sigma = Expected_f(f.a.inv.s.inv.b, LD_mu[1], LD_mu[2])
  E.log.sig.b = Expected_f(f.log.sig.b, LD_mu[1], LD_mu[2])
  E.log.sig = Expected_f(f.log.sig, LD_mu[1], LD_mu[2])
  E.prior.sig.gam = Expected_f(f.prior.sig.gam, LD_mu[1], LD_mu[2])
  E.exp.theta_g =  Expected_f(f.exp.theta_g, LD_mu[1], LD_mu[2])

  entrop <- log(2*pi*exp(1)) + 0.5*determinant(as.matrix(LD_S), logarithm = TRUE)$modulus[1]-(log(-LL+UU)+sum(LD_mu)-E.exp.theta_g)

  return(list(E.sigma=E.sig,E.inv.sigma=E.inv.sigma,E.gam=E.gam,
              E.c2.invb.absgam2.sigma = E.c2.invb.absgam2.sigma, E.c.invb.absgam = E.c.invb.absgam,
              E.c.a.invb.absgam = E.c.a.invb.absgam, E.a2.invb.inv.sigma = E.a2.invb.inv.sigma,
              E.invb.inv.sigma = E.invb.inv.sigma, E.a.invb.inv.sigma = E.a.invb.inv.sigma,
              Sigma.LD = LD_S,
              Hess.LD = LD_S,
              E.log.sig.b=E.log.sig.b, 
              E.log.sig = E.log.sig, 
              E.prior.sig.gam= E.prior.sig.gam,
              E.theta = LD_mu,
              entrop = entrop))
}

########################
T_size <- c(TT, (TT+ranges))
