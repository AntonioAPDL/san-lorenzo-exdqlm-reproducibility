// [[Rcpp::plugins(cpp14)]]
// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>

using namespace Rcpp;
using namespace arma;

namespace {

struct StabilizationStats {
  int calls = 0;
  int cov_projected = 0;
  int cov_floor_clipped = 0;
  int cov_cap_clipped = 0;
  int cov_nonfinite_inputs = 0;
};

inline arma::mat symmetrize(const arma::mat& x) {
  return 0.5 * (x + x.t());
}

inline arma::mat regularize(const arma::mat& x, double eps = 1e-10) {
  return x + eps * arma::eye<arma::mat>(x.n_rows, x.n_cols);
}

inline arma::mat robust_svd_inv(const arma::mat& x, double tolerance = 1e-12) {
  arma::mat U, V;
  arma::vec s;
  if (!arma::svd(U, s, V, x)) {
    Rcpp::stop("ndlm_univar backend: SVD failed during robust inverse");
  }
  arma::vec s_inv = s;
  for (arma::uword i = 0; i < s.n_elem; ++i) {
    double si = std::abs(s[i]);
    if (!std::isfinite(si) || si < tolerance) si = tolerance;
    s_inv[i] = 1.0 / si;
  }
  return V * arma::diagmat(s_inv) * U.t();
}

inline arma::mat safe_inv_with_svd(const arma::mat& x, double diag_jitter = 1e-10) {
  arma::mat sx = symmetrize(x);
  arma::mat inv_try;
  bool ok = arma::inv_sympd(inv_try, sx);
  if (ok && inv_try.is_finite()) return inv_try;
  arma::mat reg = regularize(sx, std::max(1e-8, diag_jitter));
  ok = arma::inv_sympd(inv_try, reg);
  if (ok && inv_try.is_finite()) return inv_try;
  arma::mat inv_svd = robust_svd_inv(reg);
  if (!inv_svd.is_finite()) {
    Rcpp::stop("ndlm_univar backend: robust SVD inverse produced non-finite result");
  }
  return inv_svd;
}

inline arma::mat stabilize_covariance(
    const arma::mat& x,
    const double eig_floor,
    const double eig_cap,
    const double diag_jitter,
    StabilizationStats* stats) {
  if (stats != nullptr) {
    stats->calls += 1;
  }

  arma::mat sx = symmetrize(x);
  if (!sx.is_finite()) {
    sx.transform([](double v) { return std::isfinite(v) ? v : 0.0; });
    if (stats != nullptr) {
      stats->cov_nonfinite_inputs += 1;
    }
  }

  if (sx.n_rows != sx.n_cols || sx.n_rows == 0) {
    return arma::eye<arma::mat>(sx.n_rows, sx.n_cols) * eig_floor;
  }

  arma::vec eig_vals;
  bool eig_ok = arma::eig_sym(eig_vals, sx);
  bool needs_projection = !eig_ok || !eig_vals.is_finite();
  bool floor_hit = needs_projection;
  bool cap_hit = needs_projection;

  if (eig_ok && eig_vals.is_finite()) {
    double min_eval = eig_vals.min();
    double max_eval = eig_vals.max();
    floor_hit = (!std::isfinite(min_eval) || min_eval < eig_floor);
    cap_hit = (!std::isfinite(max_eval) || max_eval > eig_cap);
    needs_projection = floor_hit || cap_hit;
  }

  arma::mat out = sx;
  if (needs_projection) {
    if (stats != nullptr) {
      stats->cov_projected += 1;
      if (floor_hit) stats->cov_floor_clipped += 1;
      if (cap_hit) stats->cov_cap_clipped += 1;
    }

    arma::vec vals;
    arma::mat vecs;
    if (arma::eig_sym(vals, vecs, sx) && vals.is_finite() && vecs.is_finite()) {
      vals.transform([eig_floor, eig_cap](double v) {
        if (!std::isfinite(v)) return eig_floor;
        if (v < eig_floor) return eig_floor;
        if (v > eig_cap) return eig_cap;
        return v;
      });
      out = vecs * arma::diagmat(vals) * vecs.t();
    } else {
      out = arma::eye<arma::mat>(sx.n_rows, sx.n_cols) * eig_floor;
    }
  }

  arma::mat stabilized = regularize(symmetrize(out), diag_jitter);
  for (int iter = 0; iter < 3; ++iter) {
    arma::vec post_vals;
    bool post_ok = arma::eig_sym(post_vals, stabilized);
    double post_min = (post_ok && post_vals.is_finite()) ? post_vals.min() : arma::datum::nan;
    if (std::isfinite(post_min) && post_min >= eig_floor) {
      break;
    }
    double shift = eig_floor;
    if (std::isfinite(post_min)) {
      shift = eig_floor - post_min;
    }
    if (!std::isfinite(shift) || shift < 0.0) {
      shift = eig_floor;
    }
    stabilized += (shift + std::max(diag_jitter, 0.0)) * arma::eye<arma::mat>(stabilized.n_rows, stabilized.n_cols);
    if (stats != nullptr) {
      if (stats->cov_projected == 0) stats->cov_projected += 1;
      if (stats->cov_floor_clipped == 0) stats->cov_floor_clipped += 1;
    }
  }
  return stabilized;
}

inline arma::cube cube_from_nullable(
    const Rcpp::Nullable<Rcpp::NumericVector>& arr_in,
    const int p,
    const int Tn,
    const char* name) {
  if (arr_in.isNull()) {
    return arma::cube();
  }
  Rcpp::NumericVector arr(arr_in);
  Rcpp::IntegerVector dims = arr.attr("dim");
  if (dims.size() != 3) {
    Rcpp::stop("%s must be a 3D array", name);
  }
  if (dims[0] != p || dims[1] != p || dims[2] != Tn) {
    Rcpp::stop("%s must have shape p x p x T", name);
  }
  return Rcpp::as<arma::cube>(arr);
}

inline arma::mat matrix_from_nullable(
    const Rcpp::Nullable<Rcpp::NumericMatrix>& mat_in,
    const int p,
    const char* name) {
  if (mat_in.isNull()) {
    return arma::mat();
  }
  arma::mat M = Rcpp::as<arma::mat>(mat_in);
  if (M.n_rows != static_cast<arma::uword>(p) || M.n_cols != static_cast<arma::uword>(p)) {
    Rcpp::stop("%s must have shape p x p", name);
  }
  return M;
}

inline double finite_or(const double x, const double fallback) {
  if (std::isfinite(x)) return x;
  return fallback;
}

}  // namespace

// [[Rcpp::export]]
Rcpp::List ndlm_univar_filter_step_cpp(
    const arma::vec& F_t,
    const arma::mat& G_t,
    const arma::mat& W_star_t_in,
    const double y_t,
    const arma::vec& m_prev,
    const arma::mat& C_prev_star_in,
    const double n_prev,
    const double S_prev,
    const double cov_eig_floor = 1e-8,
    const double cov_eig_cap = 1e8,
    const double cov_diag_jitter = 1e-10) {
  const int p = static_cast<int>(m_prev.n_elem);
  if (p <= 0) {
    Rcpp::stop("ndlm_univar_filter_step_cpp: state dimension p must be positive");
  }
  if (F_t.n_elem != static_cast<arma::uword>(p)) {
    Rcpp::stop("ndlm_univar_filter_step_cpp: F_t length must equal p");
  }
  if (G_t.n_rows != static_cast<arma::uword>(p) || G_t.n_cols != static_cast<arma::uword>(p)) {
    Rcpp::stop("ndlm_univar_filter_step_cpp: G_t must be p x p");
  }
  if (W_star_t_in.n_rows != static_cast<arma::uword>(p) || W_star_t_in.n_cols != static_cast<arma::uword>(p)) {
    Rcpp::stop("ndlm_univar_filter_step_cpp: W_star_t must be p x p");
  }
  if (C_prev_star_in.n_rows != static_cast<arma::uword>(p) || C_prev_star_in.n_cols != static_cast<arma::uword>(p)) {
    Rcpp::stop("ndlm_univar_filter_step_cpp: C_prev_star must be p x p");
  }

  if (!std::isfinite(n_prev) || n_prev <= 0) {
    Rcpp::stop("ndlm_univar_filter_step_cpp: n_prev must be finite and > 0");
  }
  if (!std::isfinite(S_prev) || S_prev <= 0) {
    Rcpp::stop("ndlm_univar_filter_step_cpp: S_prev must be finite and > 0");
  }

  StabilizationStats stats;
  arma::mat C_prev_star = stabilize_covariance(
    C_prev_star_in,
    cov_eig_floor,
    cov_eig_cap,
    cov_diag_jitter,
    &stats
  );
  arma::mat W_star_t = stabilize_covariance(
    W_star_t_in,
    cov_eig_floor,
    cov_eig_cap,
    cov_diag_jitter,
    &stats
  );

  arma::vec a_t = G_t * m_prev;
  arma::mat P_t_star = symmetrize(G_t * C_prev_star * G_t.t());
  arma::mat R_t_star = stabilize_covariance(
    P_t_star + W_star_t,
    cov_eig_floor,
    cov_eig_cap,
    cov_diag_jitter,
    &stats
  );

  const double f_t = arma::dot(F_t, a_t);
  double Q_t_star = 1.0 + arma::as_scalar(F_t.t() * R_t_star * F_t);
  if (!std::isfinite(Q_t_star) || Q_t_star < 1e-10) Q_t_star = 1e-10;

  const double e_t = y_t - f_t;
  arma::vec A_t = (R_t_star * F_t) / Q_t_star;
  arma::vec m_t = a_t + A_t * e_t;
  arma::mat C_t_star = stabilize_covariance(
    R_t_star - (A_t * A_t.t()) * Q_t_star,
    cov_eig_floor,
    cov_eig_cap,
    cov_diag_jitter,
    &stats
  );

  const double n_t = n_prev + 1.0;
  double S_t = (n_prev * S_prev + (e_t * e_t) / Q_t_star) / n_t;
  if (!std::isfinite(S_t) || S_t <= 0) S_t = S_prev;

  arma::mat R_t_scale = S_prev * R_t_star;
  arma::mat C_t_scale = S_t * C_t_star;
  const double Q_t_scale = S_prev * Q_t_star;
  const double pred_var_actual = (n_prev > 2.0) ? (n_prev / (n_prev - 2.0)) * Q_t_scale : arma::datum::nan;
  const double post_var_actual = (n_t > 2.0)
    ? (n_t / (n_t - 2.0)) * arma::as_scalar(F_t.t() * C_t_scale * F_t)
    : arma::datum::nan;

  return Rcpp::List::create(
    Rcpp::Named("a") = a_t,
    Rcpp::Named("P_star") = P_t_star,
    Rcpp::Named("W_star") = W_star_t,
    Rcpp::Named("R_star") = R_t_star,
    Rcpp::Named("f") = f_t,
    Rcpp::Named("Q_star") = Q_t_star,
    Rcpp::Named("e") = e_t,
    Rcpp::Named("A") = A_t,
    Rcpp::Named("m") = m_t,
    Rcpp::Named("C_star") = C_t_star,
    Rcpp::Named("n") = n_t,
    Rcpp::Named("S") = S_t,
    Rcpp::Named("R_scale") = R_t_scale,
    Rcpp::Named("Q_scale") = Q_t_scale,
    Rcpp::Named("C_scale") = C_t_scale,
    Rcpp::Named("pred_var_actual") = pred_var_actual,
    Rcpp::Named("post_var_actual") = post_var_actual,
    Rcpp::Named("stabilization") = Rcpp::List::create(
      Rcpp::Named("calls") = stats.calls,
      Rcpp::Named("cov_projected") = stats.cov_projected,
      Rcpp::Named("cov_floor_clipped") = stats.cov_floor_clipped,
      Rcpp::Named("cov_cap_clipped") = stats.cov_cap_clipped,
      Rcpp::Named("cov_nonfinite_inputs") = stats.cov_nonfinite_inputs
    )
  );
}

// [[Rcpp::export]]
Rcpp::List ndlm_univar_filter_forward_cpp(
    const arma::vec& y,
    const arma::mat& F_mat,
    const arma::cube& G_array,
    const Rcpp::Nullable<Rcpp::NumericVector>& W_star_array_in,
    const Rcpp::Nullable<Rcpp::NumericMatrix>& discount_mat_in,
    const arma::vec& m0,
    const arma::mat& C0_star,
    const double n0,
    const double S0,
    const double cov_eig_floor = 1e-8,
    const double cov_eig_cap = 1e8,
    const double cov_diag_jitter = 1e-10) {
  const int Tn = static_cast<int>(y.n_elem);
  if (Tn <= 0) {
    Rcpp::stop("ndlm_univar_filter_forward_cpp: y must be non-empty");
  }
  if (F_mat.n_rows != static_cast<arma::uword>(Tn)) {
    Rcpp::stop("ndlm_univar_filter_forward_cpp: F_mat must have T rows");
  }

  const int p = static_cast<int>(F_mat.n_cols);
  if (p <= 0) {
    Rcpp::stop("ndlm_univar_filter_forward_cpp: p must be positive");
  }
  if (G_array.n_rows != static_cast<arma::uword>(p) ||
      G_array.n_cols != static_cast<arma::uword>(p) ||
      G_array.n_slices != static_cast<arma::uword>(Tn)) {
    Rcpp::stop("ndlm_univar_filter_forward_cpp: G_array must have shape p x p x T");
  }
  if (m0.n_elem != static_cast<arma::uword>(p)) {
    Rcpp::stop("ndlm_univar_filter_forward_cpp: m0 length must equal p");
  }
  if (C0_star.n_rows != static_cast<arma::uword>(p) || C0_star.n_cols != static_cast<arma::uword>(p)) {
    Rcpp::stop("ndlm_univar_filter_forward_cpp: C0_star must be p x p");
  }
  if (!std::isfinite(n0) || n0 <= 0) {
    Rcpp::stop("ndlm_univar_filter_forward_cpp: n0 must be finite and > 0");
  }
  if (!std::isfinite(S0) || S0 <= 0) {
    Rcpp::stop("ndlm_univar_filter_forward_cpp: S0 must be finite and > 0");
  }

  arma::cube W_star_array = cube_from_nullable(W_star_array_in, p, Tn, "W_star_array");
  bool use_w_star_array = (W_star_array.n_slices == static_cast<arma::uword>(Tn));

  arma::mat discount_mat = matrix_from_nullable(discount_mat_in, p, "discount_mat");
  bool use_discount = (discount_mat.n_rows == static_cast<arma::uword>(p));
  if (use_discount && !discount_mat.is_finite()) {
    Rcpp::stop("ndlm_univar_filter_forward_cpp: discount_mat must be finite");
  }
  if (use_discount) {
    discount_mat = symmetrize(discount_mat);
    discount_mat.transform([](double v) { return (v < 0.0) ? 0.0 : v; });
  }

  arma::mat a_mat(p, Tn, arma::fill::zeros);
  arma::mat m_mat(p, Tn, arma::fill::zeros);
  arma::mat A_mat(p, Tn, arma::fill::zeros);
  arma::cube P_star_cube(p, p, Tn, arma::fill::zeros);
  arma::cube W_star_cube(p, p, Tn, arma::fill::zeros);
  arma::cube R_star_cube(p, p, Tn, arma::fill::zeros);
  arma::cube C_star_cube(p, p, Tn, arma::fill::zeros);

  arma::vec f_vec(Tn, arma::fill::zeros);
  arma::vec Q_star_vec(Tn, arma::fill::zeros);
  arma::vec e_vec(Tn, arma::fill::zeros);
  arma::vec n_prev_vec(Tn, arma::fill::zeros);
  arma::vec S_prev_vec(Tn, arma::fill::zeros);
  arma::vec n_vec(Tn, arma::fill::zeros);
  arma::vec S_vec(Tn, arma::fill::zeros);
  arma::vec Q_scale_vec(Tn, arma::fill::zeros);
  arma::vec pred_var_actual_vec(Tn, arma::fill::zeros);
  arma::vec fitted_mean_vec(Tn, arma::fill::zeros);
  arma::vec fitted_scale_vec(Tn, arma::fill::zeros);
  arma::vec fitted_var_actual_vec(Tn, arma::fill::zeros);

  StabilizationStats stats;
  arma::vec m_prev = m0;
  arma::mat C_prev_star = stabilize_covariance(
    C0_star,
    cov_eig_floor,
    cov_eig_cap,
    cov_diag_jitter,
    &stats
  );
  double n_prev = n0;
  double S_prev = S0;

  for (int t = 0; t < Tn; ++t) {
    arma::vec F_t = F_mat.row(static_cast<arma::uword>(t)).t();
    arma::mat G_t = G_array.slice(static_cast<arma::uword>(t));

    arma::vec a_t = G_t * m_prev;
    arma::mat P_t_star = symmetrize(G_t * C_prev_star * G_t.t());

    arma::mat W_t_star;
    if (use_w_star_array) {
      W_t_star = W_star_array.slice(static_cast<arma::uword>(t));
    } else if (use_discount) {
      W_t_star = discount_mat % P_t_star;
    } else {
      W_t_star = arma::zeros<arma::mat>(p, p);
    }

    W_t_star = stabilize_covariance(
      W_t_star,
      cov_eig_floor,
      cov_eig_cap,
      cov_diag_jitter,
      &stats
    );

    arma::mat R_t_star = stabilize_covariance(
      P_t_star + W_t_star,
      cov_eig_floor,
      cov_eig_cap,
      cov_diag_jitter,
      &stats
    );

    const double f_t = arma::dot(F_t, a_t);
    double Q_t_star = 1.0 + arma::as_scalar(F_t.t() * R_t_star * F_t);
    if (!std::isfinite(Q_t_star) || Q_t_star < 1e-10) Q_t_star = 1e-10;

    const double e_t = y[static_cast<arma::uword>(t)] - f_t;
    arma::vec A_t = (R_t_star * F_t) / Q_t_star;
    arma::vec m_t = a_t + A_t * e_t;

    arma::mat C_t_star = stabilize_covariance(
      R_t_star - (A_t * A_t.t()) * Q_t_star,
      cov_eig_floor,
      cov_eig_cap,
      cov_diag_jitter,
      &stats
    );

    double n_t = n_prev + 1.0;
    double S_t = (n_prev * S_prev + (e_t * e_t) / Q_t_star) / n_t;
    if (!std::isfinite(S_t) || S_t <= 0) S_t = S_prev;

    const double Q_scale = S_prev * Q_t_star;
    const double pred_var_actual = (n_prev > 2.0) ? (n_prev / (n_prev - 2.0)) * Q_scale : arma::datum::nan;
    const double fitted_mean = arma::dot(F_t, m_t);
    const double fitted_scale = S_t * arma::as_scalar(F_t.t() * C_t_star * F_t);
    const double fitted_var_actual = (n_t > 2.0) ? (n_t / (n_t - 2.0)) * fitted_scale : arma::datum::nan;

    a_mat.col(static_cast<arma::uword>(t)) = a_t;
    m_mat.col(static_cast<arma::uword>(t)) = m_t;
    A_mat.col(static_cast<arma::uword>(t)) = A_t;
    P_star_cube.slice(static_cast<arma::uword>(t)) = P_t_star;
    W_star_cube.slice(static_cast<arma::uword>(t)) = W_t_star;
    R_star_cube.slice(static_cast<arma::uword>(t)) = R_t_star;
    C_star_cube.slice(static_cast<arma::uword>(t)) = C_t_star;
    f_vec[static_cast<arma::uword>(t)] = f_t;
    Q_star_vec[static_cast<arma::uword>(t)] = Q_t_star;
    e_vec[static_cast<arma::uword>(t)] = e_t;
    n_prev_vec[static_cast<arma::uword>(t)] = n_prev;
    S_prev_vec[static_cast<arma::uword>(t)] = S_prev;
    n_vec[static_cast<arma::uword>(t)] = n_t;
    S_vec[static_cast<arma::uword>(t)] = S_t;
    Q_scale_vec[static_cast<arma::uword>(t)] = Q_scale;
    pred_var_actual_vec[static_cast<arma::uword>(t)] = pred_var_actual;
    fitted_mean_vec[static_cast<arma::uword>(t)] = fitted_mean;
    fitted_scale_vec[static_cast<arma::uword>(t)] = fitted_scale;
    fitted_var_actual_vec[static_cast<arma::uword>(t)] = fitted_var_actual;

    m_prev = m_t;
    C_prev_star = C_t_star;
    n_prev = n_t;
    S_prev = S_t;
  }

  return Rcpp::List::create(
    Rcpp::Named("a") = a_mat,
    Rcpp::Named("m") = m_mat,
    Rcpp::Named("A") = A_mat,
    Rcpp::Named("P_star") = P_star_cube,
    Rcpp::Named("W_star") = W_star_cube,
    Rcpp::Named("R_star") = R_star_cube,
    Rcpp::Named("C_star") = C_star_cube,
    Rcpp::Named("f") = f_vec,
    Rcpp::Named("Q_star") = Q_star_vec,
    Rcpp::Named("e") = e_vec,
    Rcpp::Named("n_prev") = n_prev_vec,
    Rcpp::Named("S_prev") = S_prev_vec,
    Rcpp::Named("n") = n_vec,
    Rcpp::Named("S") = S_vec,
    Rcpp::Named("Q_scale") = Q_scale_vec,
    Rcpp::Named("pred_var_actual") = pred_var_actual_vec,
    Rcpp::Named("fitted_mean") = fitted_mean_vec,
    Rcpp::Named("fitted_scale") = fitted_scale_vec,
    Rcpp::Named("fitted_var_actual") = fitted_var_actual_vec,
    Rcpp::Named("stabilization") = Rcpp::List::create(
      Rcpp::Named("calls") = stats.calls,
      Rcpp::Named("cov_projected") = stats.cov_projected,
      Rcpp::Named("cov_floor_clipped") = stats.cov_floor_clipped,
      Rcpp::Named("cov_cap_clipped") = stats.cov_cap_clipped,
      Rcpp::Named("cov_nonfinite_inputs") = stats.cov_nonfinite_inputs
    )
  );
}

// [[Rcpp::export]]
Rcpp::List ndlm_univar_forecast_h_cpp(
    const arma::mat& F_future,
    const arma::cube& G_future,
    const Rcpp::Nullable<Rcpp::NumericVector>& W_star_future_in,
    const Rcpp::Nullable<Rcpp::NumericMatrix>& discount_mat_in,
    const arma::vec& m_t,
    const arma::mat& C_t_star,
    const double n_t,
    const double S_t,
    const double cov_eig_floor = 1e-8,
    const double cov_eig_cap = 1e8,
    const double cov_diag_jitter = 1e-10) {
  const int H = static_cast<int>(F_future.n_rows);
  if (H <= 0) {
    Rcpp::stop("ndlm_univar_forecast_h_cpp: F_future must have at least one row");
  }

  const int p = static_cast<int>(F_future.n_cols);
  if (p <= 0) {
    Rcpp::stop("ndlm_univar_forecast_h_cpp: p must be positive");
  }
  if (m_t.n_elem != static_cast<arma::uword>(p)) {
    Rcpp::stop("ndlm_univar_forecast_h_cpp: m_t length must equal p");
  }
  if (C_t_star.n_rows != static_cast<arma::uword>(p) || C_t_star.n_cols != static_cast<arma::uword>(p)) {
    Rcpp::stop("ndlm_univar_forecast_h_cpp: C_t_star must be p x p");
  }
  if (G_future.n_rows != static_cast<arma::uword>(p) ||
      G_future.n_cols != static_cast<arma::uword>(p) ||
      G_future.n_slices != static_cast<arma::uword>(H)) {
    Rcpp::stop("ndlm_univar_forecast_h_cpp: G_future must have shape p x p x H");
  }
  if (!std::isfinite(n_t) || n_t <= 0) {
    Rcpp::stop("ndlm_univar_forecast_h_cpp: n_t must be finite and > 0");
  }
  if (!std::isfinite(S_t) || S_t <= 0) {
    Rcpp::stop("ndlm_univar_forecast_h_cpp: S_t must be finite and > 0");
  }

  arma::cube W_star_future = cube_from_nullable(W_star_future_in, p, H, "W_star_future");
  bool use_w_star_future = (W_star_future.n_slices == static_cast<arma::uword>(H));

  arma::mat discount_mat = matrix_from_nullable(discount_mat_in, p, "discount_mat");
  bool use_discount = (discount_mat.n_rows == static_cast<arma::uword>(p));
  if (use_discount && !discount_mat.is_finite()) {
    Rcpp::stop("ndlm_univar_forecast_h_cpp: discount_mat must be finite");
  }
  if (use_discount) {
    discount_mat = symmetrize(discount_mat);
    discount_mat.transform([](double v) { return (v < 0.0) ? 0.0 : v; });
  }

  StabilizationStats stats;
  arma::mat C0_star = stabilize_covariance(
    C_t_star,
    cov_eig_floor,
    cov_eig_cap,
    cov_diag_jitter,
    &stats
  );

  arma::mat a_all(p, H + 1, arma::fill::zeros);
  arma::cube R_star_all(p, p, H + 1, arma::fill::zeros);
  a_all.col(0) = m_t;
  R_star_all.slice(0) = C0_star;

  arma::vec f_vec(H, arma::fill::zeros);
  arma::vec Q_star_vec(H, arma::fill::zeros);
  arma::vec Q_scale_vec(H, arma::fill::zeros);
  arma::vec Q_var_actual_vec(H, arma::fill::zeros);

  arma::vec a_prev = m_t;
  arma::mat R_prev_star = C0_star;

  for (int h = 0; h < H; ++h) {
    arma::vec F_h = F_future.row(static_cast<arma::uword>(h)).t();
    arma::mat G_h = G_future.slice(static_cast<arma::uword>(h));

    arma::vec a_h = G_h * a_prev;
    arma::mat P_h_star = symmetrize(G_h * R_prev_star * G_h.t());

    arma::mat W_h_star;
    if (use_w_star_future) {
      W_h_star = W_star_future.slice(static_cast<arma::uword>(h));
    } else if (use_discount) {
      W_h_star = discount_mat % P_h_star;
    } else {
      W_h_star = arma::zeros<arma::mat>(p, p);
    }

    W_h_star = stabilize_covariance(
      W_h_star,
      cov_eig_floor,
      cov_eig_cap,
      cov_diag_jitter,
      &stats
    );

    arma::mat R_h_star = stabilize_covariance(
      P_h_star + W_h_star,
      cov_eig_floor,
      cov_eig_cap,
      cov_diag_jitter,
      &stats
    );

    double Q_h_star = 1.0 + arma::as_scalar(F_h.t() * R_h_star * F_h);
    if (!std::isfinite(Q_h_star) || Q_h_star < 1e-10) Q_h_star = 1e-10;

    double f_h = arma::dot(F_h, a_h);
    double Q_h_scale = S_t * Q_h_star;
    double Q_h_var_actual = (n_t > 2.0) ? (n_t / (n_t - 2.0)) * Q_h_scale : arma::datum::nan;

    a_all.col(static_cast<arma::uword>(h + 1)) = a_h;
    R_star_all.slice(static_cast<arma::uword>(h + 1)) = R_h_star;
    f_vec[static_cast<arma::uword>(h)] = f_h;
    Q_star_vec[static_cast<arma::uword>(h)] = Q_h_star;
    Q_scale_vec[static_cast<arma::uword>(h)] = Q_h_scale;
    Q_var_actual_vec[static_cast<arma::uword>(h)] = Q_h_var_actual;

    a_prev = a_h;
    R_prev_star = R_h_star;
  }

  return Rcpp::List::create(
    Rcpp::Named("a") = a_all,
    Rcpp::Named("R_star") = R_star_all,
    Rcpp::Named("f") = f_vec,
    Rcpp::Named("Q_star") = Q_star_vec,
    Rcpp::Named("Q_scale") = Q_scale_vec,
    Rcpp::Named("Q_var_actual") = Q_var_actual_vec,
    Rcpp::Named("stabilization") = Rcpp::List::create(
      Rcpp::Named("calls") = stats.calls,
      Rcpp::Named("cov_projected") = stats.cov_projected,
      Rcpp::Named("cov_floor_clipped") = stats.cov_floor_clipped,
      Rcpp::Named("cov_cap_clipped") = stats.cov_cap_clipped,
      Rcpp::Named("cov_nonfinite_inputs") = stats.cov_nonfinite_inputs
    )
  );
}

// [[Rcpp::export]]
Rcpp::List ndlm_univar_backward_smoother_cpp(
    const arma::mat& m_mat,
    const arma::cube& C_star_cube,
    const arma::mat& a_mat,
    const arma::cube& R_star_cube,
    const arma::cube& G_array,
    const double n_T,
    const double S_T,
    const double cov_eig_floor = 1e-8,
    const double cov_eig_cap = 1e8,
    const double cov_diag_jitter = 1e-10) {
  const int p = static_cast<int>(m_mat.n_rows);
  const int Tn = static_cast<int>(m_mat.n_cols);
  if (p <= 0 || Tn <= 0) {
    Rcpp::stop("ndlm_univar_backward_smoother_cpp: m_mat must have shape p x T with p,T >= 1");
  }
  if (a_mat.n_rows != static_cast<arma::uword>(p) || a_mat.n_cols != static_cast<arma::uword>(Tn)) {
    Rcpp::stop("ndlm_univar_backward_smoother_cpp: a_mat must match m_mat shape p x T");
  }
  if (C_star_cube.n_rows != static_cast<arma::uword>(p) ||
      C_star_cube.n_cols != static_cast<arma::uword>(p) ||
      C_star_cube.n_slices != static_cast<arma::uword>(Tn)) {
    Rcpp::stop("ndlm_univar_backward_smoother_cpp: C_star_cube must have shape p x p x T");
  }
  if (R_star_cube.n_rows != static_cast<arma::uword>(p) ||
      R_star_cube.n_cols != static_cast<arma::uword>(p) ||
      R_star_cube.n_slices != static_cast<arma::uword>(Tn)) {
    Rcpp::stop("ndlm_univar_backward_smoother_cpp: R_star_cube must have shape p x p x T");
  }
  if (G_array.n_rows != static_cast<arma::uword>(p) ||
      G_array.n_cols != static_cast<arma::uword>(p) ||
      G_array.n_slices != static_cast<arma::uword>(Tn)) {
    Rcpp::stop("ndlm_univar_backward_smoother_cpp: G_array must have shape p x p x T");
  }
  if (!std::isfinite(S_T) || S_T <= 0) {
    Rcpp::stop("ndlm_univar_backward_smoother_cpp: S_T must be finite and > 0");
  }

  StabilizationStats stats;
  arma::mat a_smooth = m_mat;
  arma::cube R_smooth_star = C_star_cube;

  for (int t = Tn - 2; t >= 0; --t) {
    arma::mat C_t_star = stabilize_covariance(
      C_star_cube.slice(static_cast<arma::uword>(t)),
      cov_eig_floor,
      cov_eig_cap,
      cov_diag_jitter,
      &stats
    );
    arma::mat G_next = G_array.slice(static_cast<arma::uword>(t + 1));
    arma::mat R_next_star = stabilize_covariance(
      R_star_cube.slice(static_cast<arma::uword>(t + 1)),
      cov_eig_floor,
      cov_eig_cap,
      cov_diag_jitter,
      &stats
    );

    arma::mat R_next_inv = safe_inv_with_svd(R_next_star, cov_diag_jitter);
    arma::mat B_t = C_t_star * G_next.t() * R_next_inv;

    arma::vec mean_delta = a_smooth.col(static_cast<arma::uword>(t + 1)) - a_mat.col(static_cast<arma::uword>(t + 1));
    a_smooth.col(static_cast<arma::uword>(t)) = m_mat.col(static_cast<arma::uword>(t)) + B_t * mean_delta;

    arma::mat smooth_delta = R_smooth_star.slice(static_cast<arma::uword>(t + 1)) - R_next_star;
    arma::mat R_t_star = C_t_star + B_t * smooth_delta * B_t.t();
    R_smooth_star.slice(static_cast<arma::uword>(t)) = stabilize_covariance(
      R_t_star,
      cov_eig_floor,
      cov_eig_cap,
      cov_diag_jitter,
      &stats
    );
  }

  arma::cube R_smooth_scale = R_smooth_star;
  for (int t = 0; t < Tn; ++t) {
    R_smooth_scale.slice(static_cast<arma::uword>(t)) *= S_T;
  }

  double var_factor = arma::datum::nan;
  if (std::isfinite(n_T) && n_T > 2.0) {
    var_factor = n_T / (n_T - 2.0);
  }

  return Rcpp::List::create(
    Rcpp::Named("a_smooth") = a_smooth,
    Rcpp::Named("R_smooth_star") = R_smooth_star,
    Rcpp::Named("R_smooth_scale") = R_smooth_scale,
    Rcpp::Named("var_factor") = var_factor,
    Rcpp::Named("stabilization") = Rcpp::List::create(
      Rcpp::Named("calls") = stats.calls,
      Rcpp::Named("cov_projected") = stats.cov_projected,
      Rcpp::Named("cov_floor_clipped") = stats.cov_floor_clipped,
      Rcpp::Named("cov_cap_clipped") = stats.cov_cap_clipped,
      Rcpp::Named("cov_nonfinite_inputs") = stats.cov_nonfinite_inputs
    )
  );
}
