// SparCC implementation in RcppArmadillo

#include <RcppArmadillo.h>
#include <vector>
#ifdef _OPENMP
#include <omp.h>
#endif

using namespace Rcpp;
using namespace arma;

static const double TOL = 1e-15;

// CLR for a single vector (by columns for data: taxa x samples)
arma::vec clr_vec(const arma::vec& x) {
  int n = x.n_elem;
  arma::vec out(n);
  double sum_log = 0.0;
  int nz = 0;
  for (int i = 0; i < n; i++) {
    if (x(i) >= TOL) {
      sum_log += std::log(x(i));
      nz++;
    }
  }
  double mean_log = (nz > 0) ? sum_log / nz : 0.0;
  for (int i = 0; i < n; i++) {
    if (x(i) >= TOL) {
      out(i) = std::log(x(i)) - mean_log;
    } else {
      out(i) = 0.0;
    }
  }
  return out;
}

// CLR by columns: data is (taxa x samples), apply to each column
arma::mat clr_matrix(const arma::mat& data) {
  int p = data.n_cols;
  arma::mat out(data.n_rows, p);
  for (int j = 0; j < p; j++) {
    out.col(j) = clr_vec(data.col(j));
  }
  return out;
}

// Dirichlet: draw from Gamma(alpha_i, 1) and normalize
arma::vec rdiric_one(const arma::vec& alpha) {
  int n = alpha.n_elem;
  arma::vec x(n);
  double s = 0.0;
  for (int i = 0; i < n; i++) {
    x(i) = R::rgamma(alpha(i) + 1.0, 1.0);
    s += x(i);
  }
  return x / s;
}

// Normalize Dirichlet for each row: data is samples x taxa
arma::mat norm_diric_row(const arma::mat& data, int rep) {
  int n = data.n_rows;
  int p = data.n_cols;
  arma::mat out(n, p, fill::zeros);
  for (int i = 0; i < n; i++) {
    arma::vec row = data.row(i).t();
    arma::vec alpha = row + 1.0;
    for (int r = 0; r < rep; r++) {
      out.row(i) += rdiric_one(alpha).t();
    }
  }
  return out / (double)rep;
}

// Aitchison variation matrix; data is (taxa x samples)
arma::mat av(const arma::mat& data) {
  arma::mat clr = clr_matrix(data);      // CLR each column (sample)
  arma::mat cov_clr = cov(clr.t());      // cov of rows -> (taxa x taxa)
  int p = cov_clr.n_rows;
  arma::mat j(p, p, fill::ones);
  arma::vec d = diagvec(cov_clr);
  arma::mat T_mat = j * diagmat(d) + diagmat(d) * j - 2 * cov_clr;
  return T_mat;
}

// Basis variance
void basis_var(const arma::mat& T_mat, arma::mat& M, const arma::uvec* excluded,
               arma::vec& Vbase, double Vmin = 1e-4) {
  int p = T_mat.n_rows;
  arma::mat Twork = T_mat;
  if (excluded != NULL) {
    for (arma::uword k = 0; k < excluded->n_elem; k++) {
      arma::uword idx = (*excluded)(k);
      Twork(idx) = 0;
    }
  }
  arma::vec Ti = sum(Twork, 1);
  arma::vec CovVec = arma::zeros<arma::vec>(p);  // R uses CovMat=0, so CovVec=0
  arma::mat M_inv;
  if (!arma::inv(M_inv, M)) {
    M_inv = arma::pinv(M);
  }
  Vbase = M_inv * (Ti + 2 * CovVec);
  for (arma::uword i = 0; i < Vbase.n_elem; i++) {
    if (Vbase(i) < Vmin) Vbase(i) = Vmin;
  }
}

// C from V
void C_from_V(const arma::mat& T_mat, const arma::vec& Vbase,
              arma::mat& Cov, arma::mat& Cor) {
  int p = T_mat.n_rows;
  arma::mat j(p, p, fill::ones);
  arma::mat Vdiag = diagmat(Vbase);
  Cov = 0.5 * (j * Vdiag + Vdiag * j - T_mat);
  Cov = (Cov + Cov.t()) / 2;
  arma::vec sd = sqrt(diagvec(Cov));
  sd.elem(arma::find(sd < 1e-10)).fill(1e-10);  // avoid division by zero
  Cor = Cov / (sd * sd.t());
  for (arma::uword i = 0; i < Cor.n_elem; i++) {
    if (std::abs(Cor(i)) > 1.0) Cor(i) = (Cor(i) > 0) ? 1.0 : -1.0;
  }
}

// Exclude pairs: find max off-diagonal (i,j), subtract 1 from M[[i,j],[i,j]]
bool exclude_pairs(arma::mat& Cor, arma::mat& M, double th,
                  arma::uvec& excluded) {
  int p = Cor.n_rows;
  arma::mat C_temp = abs(Cor - diagmat(diagvec(Cor)));
  if (excluded.n_elem > 0) {
    for (arma::uword k = 0; k < excluded.n_elem; k++) {
      C_temp(excluded(k)) = 0;
    }
  }
  double cmax = C_temp.max();
  if (cmax <= th) return true;
  arma::uvec idx = find(abs(C_temp - cmax) < 1e-10);
  if (idx.n_elem < 1) return true;
  int i = idx(0) % p;
  int j = idx(0) / p;
  M(i, i) -= 1;
  M(i, j) -= 1;
  M(j, i) -= 1;
  M(j, j) -= 1;
  excluded = join_cols(excluded, idx.subvec(0, 0));
  if (idx.n_elem > 1 && idx(1) != idx(0)) {
    excluded = join_cols(excluded, idx.subvec(1, 1));
  }
  return false;
}

// Single SparCC inner iteration
arma::mat sparccinner_rcpp(const arma::mat& data, int inner_iter, double th) {
  arma::mat T_mat = av(data);
  int p = T_mat.n_rows;
  arma::mat M = arma::mat(p, p, fill::ones) + (p - 2) * arma::eye(p, p);
  arma::uvec excluded;
  arma::vec Vbase;
  arma::mat Cov, Cor;

  basis_var(T_mat, M, NULL, Vbase);
  C_from_V(T_mat, Vbase, Cov, Cor);

  for (int i = 0; i < inner_iter; i++) {
    bool done = exclude_pairs(Cor, M, th, excluded);
    if (done) break;
    basis_var(T_mat, M, &excluded, Vbase);
    C_from_V(T_mat, Vbase, Cov, Cor);
  }
  return Cor;
}

// [[Rcpp::export]]
arma::mat sparcc_matrix_cpp(const arma::mat& data, int iter, int inner_iter, double th, int nthreads = 0) {
  int p = data.n_cols;
  arma::cube cors(p, p, iter);

  // Phase 1: Pre-generate all Dirichlet samples (sequential, uses R RNG)
  std::vector<arma::mat> data_norm_list(iter);
  for (int i = 0; i < iter; i++) {
    arma::mat data_norm = norm_diric_row(data, 1);
    data_norm_list[i] = data_norm.t();  // taxa x samples for av
  }

  // Phase 2: Run sparccinner (parallel when OpenMP available; no RNG used)
#ifdef _OPENMP
  if (nthreads > 0) omp_set_num_threads(nthreads);
#pragma omp parallel for schedule(dynamic)
#endif
  for (int i = 0; i < iter; i++) {
    cors.slice(i) = sparccinner_rcpp(data_norm_list[i], inner_iter, th);
  }

  // Phase 3: Optimized median (reuse vector to avoid per-cell allocation)
  arma::mat corMed(p, p);
  arma::vec tube_vals(iter);
  for (int i = 0; i < p; i++) {
    for (int j = 0; j < p; j++) {
      for (int k = 0; k < iter; k++) tube_vals(k) = cors(i, j, k);
      corMed(i, j) = arma::median(tube_vals);
    }
  }
  return corMed;
}
