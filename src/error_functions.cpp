#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

// [[Rcpp::export]]
arma::vec squared_error(const arma::vec & actual, const arma::vec & predicted) {
    return pow(actual - predicted,2);
}

// [[Rcpp::export]]
double mean_sq_error(const arma::vec & actual, const arma::vec & predicted) {
  return arma::mean(squared_error(actual, predicted));
}

// [[Rcpp::export]]
double rmse(const arma::vec & actual, const arma::vec & predicted) {
  return pow(arma::mean(squared_error(actual, predicted))),0.5;
}

// [[Rcpp::export]]
arma::vec absolute_error(const arma::vec & actual, const arma::vec & predicted) {
  return arma::abs(actual - predicted);
}

// [[Rcpp::export]]
double mean_abs_error(const arma::vec & actual, const arma::vec & predicted) {
  return arma::mean(absolute_error(actual, predicted));
}



// CharacterVector x = CharacterVector::create("foo", "bar");
// NumericVector y   = NumericVector::create(0.0, 2.0);
// List z            = List::create(x, y);
