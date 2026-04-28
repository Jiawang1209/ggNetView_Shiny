// SPIEC-EASI CLR normalization in RcppArmadillo
// spieceasi_norm: t(clr_matrix(data+1, 1)) -> taxa x samples

#include <RcppArmadillo.h>

using namespace Rcpp;
using namespace arma;

// CLR applied to each row of (data+1), then transpose
// Input: (n_samples x n_taxa), output: (n_taxa x n_samples)
// [[Rcpp::export]]
arma::mat spieceasi_norm_cpp(const arma::mat& data, double tol = 1e-15) {
  arma::mat x = data + 1.0;
  int n = x.n_rows;
  int p = x.n_cols;
  arma::mat out(p, n);

  for (int i = 0; i < n; i++) {
    arma::rowvec row = x.row(i);
    double sum_log = 0.0;
    int nz = 0;
    for (int j = 0; j < p; j++) {
      if (row(j) >= tol) {
        sum_log += std::log(row(j));
        nz++;
      }
    }
    double mean_log = (nz > 0) ? sum_log / nz : 0.0;
    for (int j = 0; j < p; j++) {
      if (row(j) >= tol) {
        out(j, i) = std::log(row(j)) - mean_log;
      } else {
        out(j, i) = 0.0;
      }
    }
  }
  return out;
}
