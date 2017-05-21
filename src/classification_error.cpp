#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

//using namespace Rcpp;

//' Accuracy
//'
//' This function computes the accuracy of predictions (proportion of correct predictions).
//'
//' @param actual A vector containing the observed class of an observation
//' @param predicted A vector containing the predicted class or probability
//' of class membership of an observation
//' @param threshold Optional parameter which is the probability threshold
//' at which an observation is classified as a member of class 1.
//' @export
// [[Rcpp::export]]
double accuracy(const arma::vec & actual, const arma::vec & predicted, const double threshold = 0.5) {
  if(actual.n_elem < 1) {
    Rcpp::stop("actual is of length 0.");
  }
  if(actual.n_elem != predicted.n_elem) {
    Rcpp::stop("actual and predicted have a different number of elements.");
  }
  arma::vec results = arma::conv_to<arma::vec>::from(actual == (predicted > threshold));
  return arma::mean(results);
}
//' F beta score
//'
//' This function computes the F beta score for predictions.
//'
//' @param actual A vector containing the observed class of an observation
//' @param predicted A vector containing the predicted class or probability
//' of class membership at an observation
//' @param beta Specifies which F score you want to calculate (defaults to 1)
//' @export
// [[Rcpp::export]]
double fbeta_score(const arma::vec & actual, const arma::vec & predicted, const double beta = 1) {
  if(actual.n_elem < 1) {
    Rcpp::stop("actual is of length 0.");
  }
  if(actual.n_elem != predicted.n_elem) {
    Rcpp::stop("actual and predicted have a different number of elements.");
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

//' F 1 score
//'
//' This function computes the F 1 score for predictions.
//'
//' @param actual A vector containing the observed class of an observation
//' @param predicted A vector containing the predicted class or probability
//' of class membership at an observation
//' @export
// [[Rcpp::export]]
double f1_score(const arma::vec & actual, const arma::vec & predicted) {
  return fbeta_score(actual,predicted,1);
}

//' Precision
//'
//' This function computes the precision of predictions.
//'
//' @param actual A vector containing the observed class of an observation
//' @param predicted A vector containing the predicted class
//' @export
// [[Rcpp::export]]
double precision(const arma::vec & actual, const arma::vec & predicted) {
  if(actual.n_elem < 1) {
    Rcpp::stop("actual is of length 0.");
  }
  if(actual.n_elem != predicted.n_elem) {
    Rcpp::stop("actual and predicted have a different number of elements.");
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
  return true_pos/(true_pos + false_pos);
}

//' Recall
//'
//' This function computes the recall of predictions.
//'
//' @param actual A vector containing the observed class of an observation
//' @param predicted A vector containing the predicted class
//' @export
// [[Rcpp::export]]
double recall(const arma::vec & actual, const arma::vec & predicted) {
  if(actual.n_elem < 1) {
    Rcpp::stop("actual is of length 0.");
  }
  if(actual.n_elem != predicted.n_elem) {
    Rcpp::stop("actual and predicted have a different number of elements.");
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
  return true_pos/(true_pos + false_neg);
}


//' True Negative Rate
//'
//' This function computes the true negative rate of predictions.
//'
//' @param actual A vector containing the observed class of an observation
//' @param predicted A vector containing the predicted class
//' @export
// [[Rcpp::export]]
double tru_neg_rate(const arma::vec & actual, const arma::vec & predicted) {
  if(actual.n_elem < 1) {
    Rcpp::stop("actual is of length 0.");
  }
  if(actual.n_elem != predicted.n_elem) {
    Rcpp::stop("actual and predicted have a different number of elements.");
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
  return true_neg/(true_neg + false_pos);
}




//' Brier Score
//'
//' This function computes the brier score of predictions.
//'
//' @param actual A vector containing the observed class of an observation
//' @param predicted A vector containing the predicted probabilities
//' @export
// [[Rcpp::export]]
double brier_score(const arma::vec & actual, const arma::vec & predicted) {
  if(actual.n_elem < 1) {
    Rcpp::stop("actual is of length 0.");
  }
  if(actual.n_elem != predicted.n_elem) {
    Rcpp::stop("actual and predicted have a different number of elements.");
  }
  return arma::mean(pow(actual - predicted,2));
}



//' Confusion Matrix
//'
//' This function displays a confusion matrix of predictions.
//'
//' @param actual A vector containing the observed class of an observation
//' @param predicted A vector containing the predicted probabilities
//' @export
// [[Rcpp::export]]
Rcpp::NumericMatrix conf_mat(const arma::vec & actual, const arma::vec & predicted) {
  if(actual.n_elem < 1) {
    Rcpp::stop("actual is of length 0.");
  }
  if(actual.n_elem != predicted.n_elem) {
    Rcpp::stop("actual and predicted have a different number of elements.");
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
  Rcpp::NumericMatrix temp(2);
  temp(0,0) = true_neg;
  temp(0,1) = false_pos;
  temp(1,0) = false_neg;
  temp(1,1) = true_pos;
  arma::mat armaM(temp.begin(), temp.nrow(), temp.ncol(), true, true);
  Rcpp::NumericMatrix output =  Rcpp::wrap(arma::floor(armaM));
  colnames(output) = Rcpp::CharacterVector::create("Predicted False","Predicted True");
  rownames(output) = Rcpp::CharacterVector::create("Actual False","Actual True");
  return output;
}




//' Hamming Distance
//'
//' This function computes the hamming distance of predictions.
//'
//' @param actual A vector containing the observed class of an observation
//' @param predicted A vector containing the predicted class.
//' @export
// [[Rcpp::export]]
double hamming_dist(const arma::vec & actual, const arma::vec & predicted) {
  if(actual.n_elem < 1) {
    Rcpp::stop("actual is of length 0.");
  }
  if(actual.n_elem != predicted.n_elem) {
    Rcpp::stop("actual and predicted have a different number of elements.");
  }
  double correct, incorrect;
  for (int i =0; i< actual.n_elem;i++) {
    if (actual(i)==predicted(i)) {
      correct +=1;
    } else {
      incorrect +=1;
    }
  }
  return incorrect;
}

//' Threshhold function
//'
//' This function will convert predicted probabilites to class labels (binary only).
//'
//' @param predicted A vector containing the predicted class.
//' @param threshhold The threshhold to classify as a 1 or 0 (defaults to 0.5).
//' @export
// [[Rcpp::export]]
arma::vec thresh(const arma::vec & predicted, const double threshhold = 0.5) {
  if(predicted.n_elem < 1) {
    Rcpp::stop("predicted is of length 0.");
  }
  int n = predicted.n_elem;
  arma::vec output(n);
  for(int i = 0; i < n; i++) {
    if (predicted(i) >= threshhold){
      output(i) = 1;
    } else {
      output(i) =0;
    }
  }
  return output;
}


