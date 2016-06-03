#include <Rcpp.h>
#include <cmath>

using namespace Rcpp;

// [[Rcpp::export]]
List measureColonies( NumericMatrix img, NumericVector l, NumericVector r, NumericVector t, NumericVector b, Function background, NumericVector thresh ) {
  int n = l.size();
  NumericVector measurements(n);
  List colonies(n);

  for (int i = 0; i < n; ++i) {
    NumericMatrix::Sub sub = img( Range(l[i] - 1, r[i] - 1), Range(t[i] - 1, b[i] - 1) );
    NumericMatrix colony(sub);
    NumericVector bg = background(colony, thresh[0]);
    NumericMatrix colonybg(colony - bg[0]); // subtract background

    // Replace values less than 0.1 with 0
    int xsize = colonybg.nrow() * colonybg.ncol();
    for (int i = 0; i < xsize; i++) {
      if (colonybg[i] < 0.03) {
        colonybg[i] = 0;
      }
    }

    // Save this colony matrix and its sum
    colonies[i] = colonybg;
    measurements[i] = Rcpp::sum(colonybg);
  }

  return List::create(_["measurements"] = measurements, _["colonies"] = colonies);
}
