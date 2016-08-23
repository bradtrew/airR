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

double fbeta_score(const arma::vec & actual, const arma::vec & predicted, const double beta = 1) {
  if(actual.n_elem != predicted.n_elem) {
    stop("actual and predicted have a different number of elements.");
  }
  double true_pos, true_neg, false_pos, false_neg;
  for (int i =0; i< actual.n_elem;i++) {
    if (actual(i)==predicted(i)) {
      if(actual(i)==1) {
        true_pos +=1;
      } else {
        true_neg +=1;
      }
    } else {
      if(actual(i)==1) {
        false_neg +=1;
      } else {
        false_pos +=1;
      }
    }
  }
  return ((1 + pow(beta,2)) * true_pos) /
  ((1 + pow(beta,2))* true_pos + pow(beta,2)*false_neg + false_pos);
}

double f1_score(const arma::vec & actual, const arma::vec & predicted) {
  return fbeta_score(actual,predicted,1);
}



// CharacterVector x = CharacterVector::create("foo", "bar");
// NumericVector y   = NumericVector::create(0.0, 2.0);
// List z            = List::create(x, y);
