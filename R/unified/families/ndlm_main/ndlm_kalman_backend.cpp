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

inline arma::mat robust_svd_inv(const arma::mat& x, double tolerance = 1e-12) {
  arma::mat U, V;
  arma::vec s;
  if (!arma::svd(U, s, V, x)) {
    Rcpp::stop("ndlm_kalman_smoother_cpp: SVD failed during robust inverse");
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
  // Keep fast SPD path first, but always degrade to robust SVD inversion.
  bool ok = arma::inv_sympd(inv_try, sx);
  if (ok && inv_try.is_finite()) return inv_try;
  arma::mat reg = regularize(sx, std::max(1e-8, diag_jitter));
  ok = arma::inv_sympd(inv_try, reg);
  if (ok && inv_try.is_finite()) return inv_try;
  arma::mat inv_svd = robust_svd_inv(reg);
  if (!inv_svd.is_finite()) {
    Rcpp::stop("ndlm_kalman_smoother_cpp: robust SVD inverse produced non-finite result");
  }
  return inv_svd;
}

} // namespace

// [[Rcpp::export]]
Rcpp::List ndlm_kalman_smoother_cpp(
    const arma::vec& y,
    const arma::mat& H_mat,
    const arma::vec& R_vec_in,
    const arma::vec& q_diag_in,
    const Rcpp::Nullable<Rcpp::NumericMatrix>& df_mat_in,
    const arma::vec& m0,
    const arma::mat& C0,
    const double cov_eig_floor = 1e-8,
    const double cov_eig_cap = 1e8,
    const double cov_diag_jitter = 1e-10) {
  const int Tn = static_cast<int>(y.n_elem);
  if (Tn <= 0) {
    Rcpp::stop("ndlm_kalman_smoother_cpp requires non-empty y");
  }
  if (H_mat.n_rows != static_cast<arma::uword>(Tn)) {
    Rcpp::stop("ndlm_kalman_smoother_cpp: H_mat row count must match y length");
  }
  const int d = static_cast<int>(H_mat.n_cols);
  if (d <= 0) {
    Rcpp::stop("ndlm_kalman_smoother_cpp requires H_mat with at least one column");
  }
  if (m0.n_elem != static_cast<arma::uword>(d)) {
    Rcpp::stop("ndlm_kalman_smoother_cpp: m0 length must equal ncol(H_mat)");
  }
  if (C0.n_rows != static_cast<arma::uword>(d) || C0.n_cols != static_cast<arma::uword>(d)) {
    Rcpp::stop("ndlm_kalman_smoother_cpp: C0 shape must be d x d");
  }
  if (q_diag_in.n_elem != static_cast<arma::uword>(d)) {
    Rcpp::stop("ndlm_kalman_smoother_cpp: q_diag length must equal ncol(H_mat)");
  }
  if (R_vec_in.n_elem != static_cast<arma::uword>(Tn)) {
    Rcpp::stop("ndlm_kalman_smoother_cpp: R_vec length must equal y length");
  }

  arma::vec R_vec = R_vec_in;
  for (int i = 0; i < Tn; ++i) {
    if (!std::isfinite(R_vec[i]) || R_vec[i] < 1e-10) R_vec[i] = 1e-10;
  }

  arma::vec q_diag = q_diag_in;
  for (int i = 0; i < d; ++i) {
    if (!std::isfinite(q_diag[i]) || q_diag[i] < 1e-10) q_diag[i] = 1e-10;
  }
  arma::mat Q = arma::diagmat(q_diag);
  bool use_discount = false;
  arma::mat DF;
  if (df_mat_in.isNotNull()) {
    DF = Rcpp::as<arma::mat>(df_mat_in);
    if (DF.n_rows != static_cast<arma::uword>(d) || DF.n_cols != static_cast<arma::uword>(d)) {
      Rcpp::stop("ndlm_kalman_smoother_cpp: df_mat must be d x d when provided");
    }
    if (!DF.is_finite()) {
      Rcpp::stop("ndlm_kalman_smoother_cpp: df_mat must be finite");
    }
    DF = symmetrize(DF);
    DF.transform([](double v) { return (v < 0.0) ? 0.0 : v; });
    use_discount = true;
  }

  arma::mat a(d, Tn, arma::fill::zeros);
  arma::mat m(d, Tn, arma::fill::zeros);
  arma::cube Rpred(d, d, Tn, arma::fill::zeros);
  arma::cube C(d, d, Tn, arma::fill::zeros);
  arma::vec pred_mean(Tn, arma::fill::zeros);
  arma::vec pred_var(Tn, arma::fill::zeros);
  arma::vec filt_mean(Tn, arma::fill::zeros);
  arma::vec filt_var(Tn, arma::fill::zeros);

  arma::vec m_prev = m0;
  StabilizationStats stab_stats;
  arma::mat C_prev = stabilize_covariance(
    C0,
    cov_eig_floor,
    cov_eig_cap,
    cov_diag_jitter,
    &stab_stats
  );

  for (int t = 0; t < Tn; ++t) {
    arma::vec H_t = H_mat.row(static_cast<arma::uword>(t)).t();
    arma::vec a_t = m_prev;
    arma::mat P_t = stabilize_covariance(
      C_prev,
      cov_eig_floor,
      cov_eig_cap,
      cov_diag_jitter,
      &stab_stats
    );
    arma::mat R_t;
    if (use_discount) {
      arma::mat W_t = DF % P_t;
      R_t = stabilize_covariance(
        P_t + W_t + Q,
        cov_eig_floor,
        cov_eig_cap,
        cov_diag_jitter,
        &stab_stats
      );
    } else {
      R_t = stabilize_covariance(
        C_prev + Q,
        cov_eig_floor,
        cov_eig_cap,
        cov_diag_jitter,
        &stab_stats
      );
    }

    double Qy = arma::as_scalar(H_t.t() * R_t * H_t) + R_vec[static_cast<arma::uword>(t)];
    if (!std::isfinite(Qy) || Qy < 1e-10) Qy = 1e-10;

    arma::vec K = (R_t * H_t) / Qy;
    double innov = y[static_cast<arma::uword>(t)] - arma::as_scalar(H_t.t() * a_t);
    arma::vec m_t = a_t + K * innov;
    arma::mat C_t = stabilize_covariance(
      R_t - (R_t * (H_t * H_t.t()) * R_t) / Qy,
      cov_eig_floor,
      cov_eig_cap,
      cov_diag_jitter,
      &stab_stats
    );
    pred_mean[static_cast<arma::uword>(t)] = arma::dot(H_t, a_t);
    double pv = arma::as_scalar(H_t.t() * R_t * H_t) + R_vec[static_cast<arma::uword>(t)];
    if (!std::isfinite(pv) || pv < 1e-10) pv = 1e-10;
    pred_var[static_cast<arma::uword>(t)] = pv;
    filt_mean[static_cast<arma::uword>(t)] = arma::dot(H_t, m_t);
    double fv_f = arma::as_scalar(H_t.t() * C_t * H_t) + R_vec[static_cast<arma::uword>(t)];
    if (!std::isfinite(fv_f) || fv_f < 1e-10) fv_f = 1e-10;
    filt_var[static_cast<arma::uword>(t)] = fv_f;

    a.col(static_cast<arma::uword>(t)) = a_t;
    m.col(static_cast<arma::uword>(t)) = m_t;
    Rpred.slice(static_cast<arma::uword>(t)) = R_t;
    C.slice(static_cast<arma::uword>(t)) = C_t;

    m_prev = m_t;
    C_prev = C_t;
  }

  arma::mat ms = m;
  arma::cube Cs = C;
  if (Tn >= 2) {
    for (int t = Tn - 2; t >= 0; --t) {
      arma::mat R_next = stabilize_covariance(
        Rpred.slice(static_cast<arma::uword>(t + 1)),
        cov_eig_floor,
        cov_eig_cap,
        cov_diag_jitter,
        &stab_stats
      );
      arma::mat R_next_inv = safe_inv_with_svd(R_next, cov_diag_jitter);
      arma::mat J_t = C.slice(static_cast<arma::uword>(t)) * R_next_inv;
      ms.col(static_cast<arma::uword>(t)) =
        m.col(static_cast<arma::uword>(t)) +
        J_t * (ms.col(static_cast<arma::uword>(t + 1)) - a.col(static_cast<arma::uword>(t + 1)));
      arma::mat Cs_t = C.slice(static_cast<arma::uword>(t)) +
        J_t * (Cs.slice(static_cast<arma::uword>(t + 1)) - R_next) * J_t.t();
      Cs.slice(static_cast<arma::uword>(t)) = stabilize_covariance(
        Cs_t,
        cov_eig_floor,
        cov_eig_cap,
        cov_diag_jitter,
        &stab_stats
      );
    }
  }

  arma::vec smooth_mean(Tn, arma::fill::zeros);
  arma::vec smooth_var(Tn, arma::fill::zeros);
  for (int t = 0; t < Tn; ++t) {
    arma::vec H_t = H_mat.row(static_cast<arma::uword>(t)).t();
    smooth_mean[static_cast<arma::uword>(t)] = arma::dot(H_t, ms.col(static_cast<arma::uword>(t)));
    double fv = arma::as_scalar(H_t.t() * Cs.slice(static_cast<arma::uword>(t)) * H_t) + R_vec[static_cast<arma::uword>(t)];
    if (!std::isfinite(fv) || fv < 1e-10) fv = 1e-10;
    smooth_var[static_cast<arma::uword>(t)] = fv;
  }

  return Rcpp::List::create(
    Rcpp::Named("smooth_mean") = ms,
    Rcpp::Named("smooth_cov") = Cs,
    Rcpp::Named("predicted_mean") = pred_mean,
    Rcpp::Named("predicted_var") = pred_var,
    Rcpp::Named("filtered_mean") = filt_mean,
    Rcpp::Named("filtered_var") = filt_var,
    Rcpp::Named("smoothed_mean") = smooth_mean,
    Rcpp::Named("smoothed_var") = smooth_var,
    Rcpp::Named("fitted_mean") = smooth_mean,
    Rcpp::Named("fitted_var") = smooth_var,
    Rcpp::Named("stabilization") = Rcpp::List::create(
      Rcpp::Named("calls") = stab_stats.calls,
      Rcpp::Named("cov_projected") = stab_stats.cov_projected,
      Rcpp::Named("cov_floor_clipped") = stab_stats.cov_floor_clipped,
      Rcpp::Named("cov_cap_clipped") = stab_stats.cov_cap_clipped,
      Rcpp::Named("cov_nonfinite_inputs") = stab_stats.cov_nonfinite_inputs
    )
  );
}

// [[Rcpp::export]]
Rcpp::List ndlm_kalman_smoother_tv_cpp(
    const Rcpp::List& y_list,
    const Rcpp::List& H_list,
    const Rcpp::List& R_list,
    const Rcpp::List& G_list,
    const Rcpp::List& Q_list,
    const arma::vec& m0,
    const arma::mat& C0,
    const double cov_eig_floor = 1e-8,
    const double cov_eig_cap = 1e8,
    const double cov_diag_jitter = 1e-10) {
  const int Tn = y_list.size();
  if (Tn <= 0) {
    Rcpp::stop("ndlm_kalman_smoother_tv_cpp requires non-empty sequence inputs");
  }
  if (H_list.size() != Tn || R_list.size() != Tn || G_list.size() != Tn || Q_list.size() != Tn) {
    Rcpp::stop("ndlm_kalman_smoother_tv_cpp requires equal-length sequence lists");
  }
  if (C0.n_rows != m0.n_elem || C0.n_cols != m0.n_elem) {
    Rcpp::stop("ndlm_kalman_smoother_tv_cpp: C0 shape must match length(m0)");
  }

  auto as_numeric_vector = [](SEXP x) -> arma::vec {
    if (Rf_isNull(x)) return arma::vec();
    arma::vec out = Rcpp::as<arma::vec>(x);
    if (!out.is_finite()) {
      for (arma::uword i = 0; i < out.n_elem; ++i) {
        if (!std::isfinite(out[i])) out[i] = 0.0;
      }
    }
    return out;
  };

  auto as_numeric_matrix = [](SEXP x) -> arma::mat {
    if (Rf_isNull(x)) return arma::mat();
    arma::mat out = Rcpp::as<arma::mat>(x);
    if (!out.is_finite()) {
      out.transform([](double v) { return std::isfinite(v) ? v : 0.0; });
    }
    return out;
  };

  std::vector<int> dims(Tn, 0);
  dims[0] = static_cast<int>(m0.n_elem);
  StabilizationStats stab_stats;

  std::vector<arma::vec> a(Tn);
  std::vector<arma::vec> m(Tn);
  std::vector<arma::mat> Rpred(Tn);
  std::vector<arma::mat> C(Tn);
  std::vector<arma::vec> ms(Tn);
  std::vector<arma::mat> Cs(Tn);
  std::vector<arma::mat> lag_next(Tn);

  arma::vec m_prev = m0;
  arma::mat C_prev = stabilize_covariance(C0, cov_eig_floor, cov_eig_cap, cov_diag_jitter, &stab_stats);

  for (int t = 0; t < Tn; ++t) {
    arma::vec y_t = as_numeric_vector(y_list[t]);
    arma::mat H_t = as_numeric_matrix(H_list[t]);
    arma::vec R_obs = as_numeric_vector(R_list[t]);
    if (H_t.n_rows != y_t.n_elem || R_obs.n_elem != y_t.n_elem) {
      Rcpp::stop("ndlm_kalman_smoother_tv_cpp: observation dimensions do not match");
    }

    arma::vec a_t;
    arma::mat R_t;
    if (t == 0) {
      if (H_t.n_cols != m0.n_elem) {
        Rcpp::stop("ndlm_kalman_smoother_tv_cpp: first H_list state dimension must match length(m0)");
      }
      dims[t] = static_cast<int>(H_t.n_cols);
      a_t = m_prev;
      R_t = C_prev;
    } else {
      arma::mat G_t = as_numeric_matrix(G_list[t]);
      arma::mat Q_t = as_numeric_matrix(Q_list[t]);
      if (G_t.n_cols != static_cast<arma::uword>(dims[t - 1])) {
        Rcpp::stop("ndlm_kalman_smoother_tv_cpp: G_list state transition has wrong previous-state dimension");
      }
      if (Q_t.n_rows != G_t.n_rows || Q_t.n_cols != G_t.n_rows) {
        Rcpp::stop("ndlm_kalman_smoother_tv_cpp: Q_list covariance shape must match current state dimension");
      }
      if (H_t.n_cols != G_t.n_rows) {
        Rcpp::stop("ndlm_kalman_smoother_tv_cpp: H_list state dimension must match transition row count");
      }
      dims[t] = static_cast<int>(G_t.n_rows);
      a_t = G_t * m_prev;
      arma::mat P_t = G_t * C_prev * G_t.t();
      R_t = stabilize_covariance(P_t + Q_t, cov_eig_floor, cov_eig_cap, cov_diag_jitter, &stab_stats);
    }

    arma::vec m_t = a_t;
    arma::mat C_t = stabilize_covariance(R_t, cov_eig_floor, cov_eig_cap, cov_diag_jitter, &stab_stats);
    for (arma::uword i = 0; i < y_t.n_elem; ++i) {
      arma::vec h = H_t.row(i).t();
      double r = (i < R_obs.n_elem && std::isfinite(R_obs[i]) && R_obs[i] > 1e-10) ? R_obs[i] : 1e-10;
      double qy = arma::as_scalar(h.t() * C_t * h) + r;
      if (!std::isfinite(qy) || qy < 1e-10) qy = 1e-10;
      arma::vec K = (C_t * h) / qy;
      double innov = y_t[i] - arma::as_scalar(h.t() * m_t);
      m_t = m_t + K * innov;
      C_t = stabilize_covariance(C_t - (C_t * (h * h.t()) * C_t) / qy, cov_eig_floor, cov_eig_cap, cov_diag_jitter, &stab_stats);
    }

    a[t] = a_t;
    Rpred[t] = R_t;
    m[t] = m_t;
    C[t] = C_t;
    ms[t] = m_t;
    Cs[t] = C_t;

    m_prev = m_t;
    C_prev = C_t;
  }

  lag_next[Tn - 1] = arma::mat();
  if (Tn >= 2) {
    for (int t = Tn - 2; t >= 0; --t) {
      arma::mat G_next = as_numeric_matrix(G_list[t + 1]);
      arma::mat R_next = stabilize_covariance(Rpred[t + 1], cov_eig_floor, cov_eig_cap, cov_diag_jitter, &stab_stats);
      arma::mat R_next_inv = safe_inv_with_svd(R_next, cov_diag_jitter);
      arma::mat J_t = C[t] * G_next.t() * R_next_inv;
      ms[t] = m[t] + J_t * (ms[t + 1] - a[t + 1]);
      arma::mat Cs_t = C[t] + J_t * (Cs[t + 1] - R_next) * J_t.t();
      Cs[t] = stabilize_covariance(Cs_t, cov_eig_floor, cov_eig_cap, cov_diag_jitter, &stab_stats);
      lag_next[t] = J_t * Cs[t + 1];
    }
  }

  Rcpp::List pred_mean_out(Tn), pred_cov_out(Tn), filt_mean_out(Tn), filt_cov_out(Tn), smooth_mean_out(Tn), smooth_cov_out(Tn), lag_cov_out(Tn);
  for (int t = 0; t < Tn; ++t) {
    pred_mean_out[t] = a[t];
    pred_cov_out[t] = Rpred[t];
    filt_mean_out[t] = m[t];
    filt_cov_out[t] = C[t];
    smooth_mean_out[t] = ms[t];
    smooth_cov_out[t] = Cs[t];
    lag_cov_out[t] = lag_next[t];
  }

  return Rcpp::List::create(
    Rcpp::Named("pred_mean") = pred_mean_out,
    Rcpp::Named("pred_cov") = pred_cov_out,
    Rcpp::Named("filter_mean") = filt_mean_out,
    Rcpp::Named("filter_cov") = filt_cov_out,
    Rcpp::Named("smooth_mean") = smooth_mean_out,
    Rcpp::Named("smooth_cov") = smooth_cov_out,
    Rcpp::Named("lag_cov_next") = lag_cov_out,
    Rcpp::Named("state_dim") = Rcpp::wrap(dims),
    Rcpp::Named("stabilization") = Rcpp::List::create(
      Rcpp::Named("calls") = stab_stats.calls,
      Rcpp::Named("cov_projected") = stab_stats.cov_projected,
      Rcpp::Named("cov_floor_clipped") = stab_stats.cov_floor_clipped,
      Rcpp::Named("cov_cap_clipped") = stab_stats.cov_cap_clipped,
      Rcpp::Named("cov_nonfinite_inputs") = stab_stats.cov_nonfinite_inputs
    )
  );
}
