// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// dist_mat_absolute
IntegerMatrix dist_mat_absolute(IntegerVector x, IntegerVector y);
RcppExport SEXP diffrprojects_dist_mat_absolute(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< IntegerVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type y(ySEXP);
    __result = Rcpp::wrap(dist_mat_absolute(x, y));
    return __result;
END_RCPP
}
// which_dist_min_absolute
List which_dist_min_absolute(IntegerVector x, IntegerVector y);
RcppExport SEXP diffrprojects_which_dist_min_absolute(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< IntegerVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type y(ySEXP);
    __result = Rcpp::wrap(which_dist_min_absolute(x, y));
    return __result;
END_RCPP
}
