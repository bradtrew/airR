#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

// [[Rcpp::export]]
arma::vec squared_error(const arma::vec & actual, const arma::vec & predicted) {
  if(actual.n_elem != predicted.n_elem) {
    stop("actual and predicted have a different number of elements.");
  }
  return pow(actual - predicted,2);
}

// [[Rcpp::export]]
double mean_sq_error(const arma::vec & actual, const arma::vec & predicted) {
  if(actual.n_elem != predicted.n_elem) {
    stop("actual and predicted have a different number of elements.");
  }
  return arma::mean(squared_error(actual, predicted));
}

// [[Rcpp::export]]
double rmse(const arma::vec & actual, const arma::vec & predicted) {
  if(actual.n_elem != predicted.n_elem) {
    stop("actual and predicted have a different number of elements.");
  }
  return pow(arma::mean(squared_error(actual, predicted)),0.5);
}

// [[Rcpp::export]]
arma::vec absolute_error(const arma::vec & actual, const arma::vec & predicted) {
  if(actual.n_elem != predicted.n_elem) {
    stop("actual and predicted have a different number of elements.");
  }
  return arma::abs(actual - predicted);
}

// [[Rcpp::export]]
double mean_abs_error(const arma::vec & actual, const arma::vec & predicted) {
  if(actual.n_elem != predicted.n_elem) {
    stop("actual and predicted have a different number of elements.");
  }
  return arma::mean(absolute_error(actual, predicted));
}

// [[Rcpp::export]]
double median_abs_error(const arma::vec & actual, const arma::vec & predicted) {
  if(actual.n_elem != predicted.n_elem) {
    stop("actual and predicted have a different number of elements.");
  }
  return arma::median(absolute_error(actual, predicted));
}


// [[Rcpp::export]]
double r2_score(const arma::vec & actual, const arma::vec & predicted) {
  if(actual.n_elem != predicted.n_elem) {
    stop("actual and predicted have a different number of elements.");
  }
  double const_model = arma::sum(pow(actual - arma::mean(actual),2));
  if (const_model ==0) {
    warning("Your actual predictions are constant, r2_score defined as -Inf");
    }
  return 1 - arma::sum(squared_error(actual,predicted)) / const_model;
}

// CharacterVector x = CharacterVector::create("foo", "bar");
// NumericVector y   = NumericVector::create(0.0, 2.0);
// List z            = List::create(x, y);
