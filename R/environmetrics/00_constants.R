###############################################################################
# Constants and global configuration
# Inputs:
#   - None (constants only)
# Outputs:
#   - Global constants used by model/plots
# Dependencies:
#   - None
###############################################################################

n.samp <- 2000
cut <- 1
m <- 2
USE_PREV <- TRUE

p0 <- 0.5
harmonics = c(1, 2, 1/6.8068493)
# harmonics = c(363.5854/90, 363.5854/180, 1/6.8068493)

# Optional compile flags (kept for reference only)
# Sys.setenv("PKG_CXXFLAGS"="-ILOCAL_RCPP_LIB_ROOT/eigen -ILOCAL_RCPP_LIB_ROOT/boost/include -DEIGEN_DONT_VECTORIZE")
# Sys.setenv("PKG_LIBS"="-LLOCAL_RCPP_LIB_ROOT/lib64 -LLOCAL_RCPP_LIB_ROOT/boost/lib -llapack -lblas -lboost_random -lboost_system -fopenmp")
# Sys.setenv(LD_LIBRARY_PATH="LOCAL_RCPP_LIB_ROOT/lib64:LOCAL_RCPP_LIB_ROOT/boost/lib:/lib64")

# Rcpp::sourceCpp("SOURCE_WORKFLOW_ROOT/sampling_exal.cpp")
# Rcpp::sourceCpp("SOURCE_WORKFLOW_ROOT/sampling_truncnorm.cpp")
# Rcpp::sourceCpp("SOURCE_WORKFLOW_ROOT/DISC_kalman_synth.cpp")
# Rcpp::sourceCpp("SOURCE_WORKFLOW_ROOT/task_medium/src/exAL.cpp")

initial_delta   <- c(0.9999995, 0.9997, 0.9997, 0.9997, 0.999, 0.8995)
# initial_delta <- c(df_t  , df_s1 , df_s2 , df_s67, df_discrep, lambda)

delta <- initial_delta

SIMS <- TRUE
use_covariates <- TRUE

lam1 <- 1-1e-6 # Sudden correction at start of forecast period
lam2 <- 1-1e-6 # Correction during forecast period from historical period

df_t        <- delta[1]
df_s1       <- delta[2]
df_s2       <- delta[3]
df_s67      <- delta[4]
df.discrep  <- delta[5]
df_trans      <- 0.99999999
df_covs       <- 0.99999
lambda      <- delta[6]
