#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

//' Accuracy
//'
//' This function computes the accuracy of predictions (%correct).
//'
//' @param actual A vector containing the observed class of a point
//' @param predicted A vector containing the predicted class or probability
//' of class membership at a point
//' @param threshold Optional parameter which is the probability threshold
//' at which a point is classified as a member of class 1.
// [[Rcpp::export]]
double accuracy(const arma::vec & actual, const arma::vec & predicted, const double threshold = 0.5) {
  if(actual.n_elem != predicted.n_elem) {
    stop("actual and predicted have a different number of elements.");
  }
  arma::vec results = arma::conv_to<arma::vec>::from(actual == (predicted > threshold));
  return arma::mean(results);
}



// CharacterVector x = CharacterVector::create("foo", "bar");
// NumericVector y   = NumericVector::create(0.0, 2.0);
// List z            = List::create(x, y);
