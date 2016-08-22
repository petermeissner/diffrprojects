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


//' (function to calculate minimum and position of minimum)
//' takes vector of size n and vector of size m and gives back list with
//' vectors of size n (minimum distance and location of minimum in y)
//' @param x a vector of type integer
//' @param y a vector of type integer
//' @keywords internal
// [[Rcpp::export]]
List which_dist_min_absolute(IntegerVector x, IntegerVector y) {
  int n = x.size();
  int m = y.size();

  NumericVector minimum(n);
  NumericVector location(n);

  int mind;

  for ( int i = 0 ; i < n ; i++ )
  {
    minimum[i] = abs(x[i]-y[0]);
    location[i] = 1;
    if( minimum[i] == 0 )
    {
      continue;
    }
    for ( int c = 0 ; c < m ; c++ )
    {
      mind = abs(x[i]-y[c]) ;
      if ( mind  < minimum[i] )
      {
        minimum[i]  = mind ;
        location[i] = c+1;
        if( mind == 0 ){
          break;
        }
      }
    } ;
  }

  return List::create(
    Named("minimum", minimum),
    Named("location", location)
  ) ;
}
