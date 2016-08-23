#include <Rcpp.h>
using namespace Rcpp;

//' (function to calculate distance matrix of integers)
//' takes vector of size n and vector of size m and gives back matrix of n rows and m columns
//' @param x a vector of type numeric
//' @param y a vector of type numeric
//' @keywords internal
// [[Rcpp::export]]
IntegerMatrix dist_mat_absolute(IntegerVector x, IntegerVector y) {
  int n = x.size();
  int m = y.size();
  IntegerMatrix out(n, m);

  for(int i = 0; i < n; ++i) {
    for(int j = 0; j < m; ++j) {
      out(i,j) = abs(x[i]-y[j]);
    }
  }

  return out;
}



