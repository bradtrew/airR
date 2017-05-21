#' Xgboost Error
#'
#' A wrapper function to allow your error metrics to be used as evaluation metrics
#' for xgboost. Note that this does not mean it is the objective function,
#' it is only used as an evaluation (checks the error as the algorithm progresses)
#'
#' @param err_func Either a function a character vector of length one containinng the name
#' of the function. The error function must return a single value.
#' @return A function which can be used within xgboost.
#'
#' @importFrom xgboost getinfo
#' @export
xgb_error <- function(err_func) {
  UseMethod("xgb_error")
}

#' @rdname xgb_error
#' @export
xgb_error.default <- function(err_func) {
  stop("xgb_error only accepts character vectors or functions")
}

#' @rdname xgb_error
#' @export
xgb_error.function <- function(err_func) {
  return(function(preds, dtrain) {
    labels <- xgboost::getinfo(dtrain, "label")
    if (length(unique(labels))<3) {
      preds <- thresh(preds)
    }
    err <- err_func(labels, preds)
    return(list(metric = "error", value = err))
  })
}

#' @rdname xgb_error
#' @export
xgb_error.character <- function(err_func) {
  if (length(err_func) != 1) {
    warning("err_func contains more than one element. Only the first will be used.")
  }
  err_func <- eval(parse(text = err_func[1]))
  return(function(preds, dtrain) {
    labels <- xgboost::getinfo(dtrain, "label")
    if(length(unique(labels))<3) {
      preds <- thresh(preds)
    }
    err <- err_func(labels, preds)
    return(list(metric = "error", value = err))
  })
}



#
#
# library(xgboost)
# # creating some data
# df <- data.frame(x1=rnorm(1000),
#                  x2 = runif(1000),
#                  x3= rnorm(1000,5,10))
# # looks confusing, just creating y as a function of the x's
# df$y <- ifelse(1/(1+exp(-(10*df$x1 + 2*df$x2 + 3*df$x3 + rnorm(1000,3,10))))<0.3,ifelse(runif(1)<0.3,1,0),ifelse(runif(1)<0.3,0,1))
# dtrain <- xgboost::xgb.DMatrix(as.matrix(df[1:700,-4]),label = df$y[1:700])
# dval <- xgboost::xgb.DMatrix(as.matrix(df[701:1000,-4]),label = df$y[701:1000])
#
# param <-  list(
#   eta = 0.1,
#   max_depth = 5,
#   subsample = 0.7,
#   colsample_bytree=0.7,
#   gamma = 0.1,
#   min_child_weight = 3,
#   eval_metric = xgb_error(f1_score)
# )
#
# bst <- xgboost::xgb.train(dtrain, params = param, nrounds = 10, watchlist = list(val = dval, train = dtrain))
#
