#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

//' Squared error
//'
//' This function will calculate the element-wise squared error between two vectors
//'
//' @param actual A vector containing the observed value of a point
//' @param predicted A vector containing the predicted value at a point
//'
// [[Rcpp::export]]
arma::vec squared_error(const arma::vec & actual, const arma::vec & predicted) {
  if(actual.n_elem != predicted.n_elem) {
    stop("actual and predicted have a different number of elements.");
  }
  return pow(actual - predicted,2);
}


//' Mean squared error
//'
//' This function will calculate the mean squared error between two vectors.
//'
//' @param actual A vector containing the observed value of a point
//' @param predicted A vector containing the predicted value at a point

// [[Rcpp::export]]
double mean_sq_error(const arma::vec & actual, const arma::vec & predicted) {
  if(actual.n_elem != predicted.n_elem) {
    stop("actual and predicted have a different number of elements.");
  }
  return arma::mean(squared_error(actual, predicted));
}

//' Root mean squared error
//'
//' This function will calculate the root mean squared error between two vectors.
//'
//' @param actual A vector containing the observed value of a point
//' @param predicted A vector containing the predicted value at a point
// [[Rcpp::export]]
double rmse(const arma::vec & actual, const arma::vec & predicted) {
  if(actual.n_elem != predicted.n_elem) {
    stop("actual and predicted have a different number of elements.");
  }
  return pow(arma::mean(squared_error(actual, predicted)),0.5);
}


//' Absolute error
//'
//' This function will calculate the element-wise absolute error between two vectors.
//'
//' @param actual A vector containing the observed value of a point
//' @param predicted A vector containing the predicted value at a point
// [[Rcpp::export]]
arma::vec absolute_error(const arma::vec & actual, const arma::vec & predicted) {
  if(actual.n_elem != predicted.n_elem) {
    stop("actual and predicted have a different number of elements.");
  }
  return arma::abs(actual - predicted);
}

//' Mean absolute error
//'
//' This function will calculate the mean absolute error between two vectors.
//'
//' @param actual A vector containing the observed value of a point
//' @param predicted A vector containing the predicted value at a point
// [[Rcpp::export]]
double mean_abs_error(const arma::vec & actual, const arma::vec & predicted) {
  if(actual.n_elem != predicted.n_elem) {
    stop("actual and predicted have a different number of elements.");
  }
  return arma::mean(absolute_error(actual, predicted));
}

//' Median absolute error
//'
//' This function will calculate the median absolute error between two vectors.
//'
//' @param actual A vector containing the observed value of a point
//' @param predicted A vector containing the predicted value at a point
// [[Rcpp::export]]
double median_abs_error(const arma::vec & actual, const arma::vec & predicted) {
  if(actual.n_elem != predicted.n_elem) {
    stop("actual and predicted have a different number of elements.");
  }
  return arma::median(absolute_error(actual, predicted));
}

//' R2 score
//'
//' This function will calculate the r2 score between two vectors.
//'
//' @param actual A vector containing the observed value of a point
//' @param predicted A vector containing the predicted value at a point
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
