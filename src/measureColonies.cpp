#include <Rcpp.h>
#include <cmath>

using namespace Rcpp;

// [[Rcpp::export]]
List measureColonies( NumericMatrix img, NumericVector l, NumericVector r, NumericVector t, NumericVector b, NumericVector bg ) {
  int n = l.size();
  NumericVector measurements(n);
  List colonies(n);

  for (int i = 0; i < n; ++i) {
    NumericMatrix::Sub sub = img( Range(l[i] - 1, r[i] - 1), Range(t[i] - 1, b[i] - 1) );
    NumericMatrix colony(sub);
    NumericMatrix colonybg(colony - bg[i]); // subtract background
    colonies[i] = colonybg;
    measurements[i] = Rcpp::sum(colonybg);
    if (measurements[i] < 0) measurements[i] = 0; // set 0 as minimum measurement
  }

  return List::create(_["measurements"] = measurements, _["colonies"] = colonies);
}
