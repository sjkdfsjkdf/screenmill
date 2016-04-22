/** Identify nearest neighbors
 *
 *  Identify each nearest neighbor of a set of points in 2D space.
 *
 *  @param x a numeric vector.
 *  @param y a numeric vector.
 *
 *  @details
 *    Motivation: the R package spatstat contains fast versions of
 *    nndist and nnwhich written in C. However, this package is large with
 *    numerous dependencies, so here nearestNeighbor has been written to avoid
 *    dependency bloat. The implementation could use improvement.
 *
 *  @note
 *    This function is roughly 4x slower than spatstat, but 15x faster than
 *    the fastest R version I could come up with.
 *
 *    Distances are only measured to float precision (i.e. not double)
 *
 *  @return
 *    A dataframe with columns 'nndist' and 'nnwhich'
 */

#include <Rcpp.h>
#include <limits>
#include <cmath>

using namespace Rcpp;

// [[Rcpp::export]]
DataFrame nearestNeighbor( NumericVector x, NumericVector y) {
  int n = x.size();
  NumericVector out_nnd(n);
  IntegerVector out_nnj(n);

  for (int i = 0; i < n; ++i) {
    float nnd = std::numeric_limits<float>::infinity(); // Note loss of precision
    int nnj = 0;

    for (int j = 0; j < n; ++j) {
      // skip if self
      if (i == j) continue;
      // Compute dx and skip if bigger than nnd
      float dx = std::abs(x[j] - x[i]);
      if (dx > nnd) continue;
      // Compute dy and skip if bigger than nnd
      float dy = std::abs(y[j] - y[i]);
      if (dy > nnd) continue;
      // Compute distance and update if closer than previous nearest neighbor
      float distance = std::sqrt(float(std::pow(dx, 2) + std::pow(dy, 2)));
      if (distance < nnd) {
        nnd = distance;
        nnj = j + 1;
      }
    }
    // Store nearest neighbor for this point
    out_nnd[i] = nnd;
    out_nnj[i] = nnj;
  }

  return DataFrame::create(_["dist"] = out_nnd, _["which"] = out_nnj);
}


/*** R
# For comparison to spatstat
library(spatstat)
library(microbenchmark)
library(dplyr)

x <- sample(1500)
y <- sample(1500)

# Note that for spatstat functions "k" is generalized to accept a vector (awesome!)
spat_nn <- data_frame(dist = nndist(x, y, k = 1), which = nnwhich(x, y, k = 1))
my_nn   <- nearestNeighbor(x, y)

microbenchmark(
  spat_nn = data_frame(dist = nndist(x, y, k = 1), which = nnwhich(x, y, k = 1)),
  my_nn   = nearestNeighbor(x, y),  # ~6.5 ms vs. ~1.5 ms
  times = 10L,
  unit = 'ms'
)
*/
