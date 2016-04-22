#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector measureColonies( NumericMatrix img, NumericVector l, NumericVector r, NumericVector t, NumericVector b, NumericVector bg ) {
  int n = l.size();
  NumericVector out(n);

  for (int i = 0; i < n; ++i) {
    NumericMatrix::Sub sub = img( Range(l[i] - 1, r[i] - 1), Range(t[i] - 1, b[i] - 1) );
    NumericMatrix colony(sub);
    out[i] = Rcpp::sum(colony) - (bg[i] * colony.rows() * colony.cols());
  }

  return out;
}
