#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector cone_radius_cpp(NumericVector x) {
  int n = x.size();
  NumericVector result(n);

  for (int i = 0; i < n; ++i) {
    if (x[i] < 0) {
      result[i] = 0;
    } else if (x[i] < 8) {
      result[i] = x[i] / 8;
    } else if (x[i] < 8 + M_PI / 2) {
      result[i] = 1 + 1.5 * sin(x[i] - 8);
    } else if (x[i] < 10) {
      result[i] = 2.5 - 2 * cos(x[i] - 8);
    } else {
      result[i] = 0;
    }
  }

  return result;
}
