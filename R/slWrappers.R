#' SL.svmLinear.caretMod
#' Uses SL.caretMod to train a linear svm with 15 choices of 
#' tuning parameter
#' 
#' @param trControl Passed to \code{caret::train}
#' @param tuneLength The number of tuning parameter combinations
#' @param method Character describing what algorithm to train
#' @param ... Other arguments passed to \code{SL.caretMod}
#' 
#' @export

SL.svmLinear.caretMod <- function(...,method="svmLinear",tuneLength = 15, 
                                  trControl = caret::trainControl(method = "cv", number = 2)){
  SL.caretMod(...,method=method,tuneLength=tuneLength)
}

#' SL.rpart.caretMod 
#' 
#' Uses SL.caretMod with five-fold CV to train regression tree 
#' with ten choices of tuning parameter.
#' 
#' @param method Character describing what algorithm to train
#' @param tuneLength The number of tuning parameter combinations
#' @param trControl Passed to \code{caret::train}
#' @param ... Other arguments passed to SL.caretMod
#' 
#' @export

SL.rpart.caretMod <- function(...,method="rpart",tuneLength = 10, trControl = caret::trainControl(method = "cv", number = 2)){
  SL.caretMod(...,method=method,tuneLength=tuneLength, trControl = trControl)
}

#' SL.rf.caretMod
#' 
#' Uses SL.caretMod with five-fold CV to train random forest
#' with ten choices of tuning parameter.
#' @param trControl Passed to \code{caret::train}
#' @param tuneLength The number of tuning parameter combinations
#' @param method Character describing what algorithm to train
#' @param ... Other arguments passed to \code{SL.caretMod}
#' 
#' @export

SL.rf.caretMod <- function (..., method = "rf", tuneLength = 10, 
                            trControl = caret::trainControl(method = "cv", number = 5)) 
{
    SL.caretMod(..., method = method, tuneLength = tuneLength, trControl = trControl)
}

#' SL.randomGLM.caretMod
#' Uses SL.caretMod with five-fold CV to train a random GLM
#' with ten choices of tuning parameter.
#' 
#' @param trControl Passed to \code{caret::train}
#' @param tuneLength The number of tuning parameter combinations
#' @param method Character describing what algorithm to train
#' @param ... Other arguments passed to \code{SL.caretMod}
#' 
#' @export 
SL.randomGLM.caretMod <- function(...,method="randomGLM",tuneLength=10, 
                                  trControl = caret::trainControl(method = "cv", number = 2)){
  SL.caretMod(...,method=method,tuneLength=tuneLength)
}


#' SL.nnet.caretMod
#' 
#' Uses SL.caretMod to train with five-fold CV to train a neural network
#' with twenty choices of tuning parameter.
#' 
#' @param trControl Passed to \code{caret::train}
#' @param tuneLength The number of tuning parameter combinations
#' @param method Character describing what algorithm to train
#' @param ... Other arguments passed to \code{SL.caretMod}
#' 
#' @export 
SL.nnet.caretMod <- function(...,method="nnet", tuneLength=20, trControl = caret::trainControl(method = "cv", number = 2)){
  SL.caretMod(...,method=method,tuneLength=tuneLength)
}

#' SL.glmnet.caretMod
#' 
#' Uses SL.caretMod with five-fold CV to train a glmnet regression
#' with ten choices of tuning parameter.
#' 
#' @param trControl Passed to \code{caret::train}
#' @param tuneLength The number of tuning parameter combinations
#' @param method Character describing what algorithm to train
#' @param ... Other arguments passed to \code{SL.caretMod}
#' 
#' @export 
SL.glmnet.caretMod <- function(...,method="glmnet", tuneLength=10, trControl = caret::trainControl(method = "cv", number = 2)){
  SL.caretMod(...,method=method,tuneLength=tuneLength)
}


#' SL.gbm.caretMod
#' 
#' Uses SL.caretMod to train a gbm with 10 choices of tuning parameters and 5-fold CV
#' 
#' @param trControl Passed to \code{caret::train}
#' @param tuneLength The number of tuning parameter combinations
#' @param method Character describing what algorithm to train
#' @param ... Other arguments passed to \code{SL.caretMod}
#' 
#' @export

SL.gbm.caretMod <- function (..., method = "gbm", tuneLength = 5, 
                           trControl = caret::trainControl(method = "cv", number = 5)) 
{
    SL.caretMod(..., method = method, tuneLength = tuneLength, trControl = trControl)
}

#' SL.gamSpline.caretMod
#' 
#' Uses SL.caretMod with 10 choices of tuning parameters and 5-fold CV
#' to train a generalized additive model.
#' 
#' 
#' @param trControl Passed to \code{caret::train}
#' @param tuneLength The number of tuning parameter combinations
#' @param method Character describing what algorithm to train
#' @param ... Other arguments passed to \code{SL.caretMod}
#'  
#' @export 
SL.gamSpline.caretMod <- function(...,method="gamSpline",tuneLength=10, 
                                  trControl = caret::trainControl(method = "cv", number = 2)){
  SL.caretMod(...,method=method,tuneLength=tuneLength)
}

#' SL.caretMod 
#' 
#' A modification of the SL.caret function from the SuperLearner package that
#' suppresses some output when method = "gbm" or "nnet". See \code{?SL.caret}
#' for more information.
#' 
#' @param Y Training outcomes
#' @param X Training predictors 
#' @param newX Test set predictors 
#' @param obsWeights Weights for the observations
#' @param method Character describing what algorithm to train
#' @param tuneLength The number of tuning parameter combinations
#' @param trControl Passed to \code{caret::train}
#' @param family Character indicating family argument (ignored)
#' @param ... Other arguments (not currently used)
#' 
#' @importFrom caret train
#' @importFrom stats predict
#' 
#' @export

SL.caretMod <- function (Y, X, newX, family, obsWeights, method, tuneLength = 10, 
                       trControl = caret::trainControl(method = "cv", number = 5, verboseIter = FALSE), 
                       ...) 
{
  if (length(unique(Y))>2){
    if(is.matrix(Y)) Y <- as.numeric(Y)
    metric <- "RMSE"
    if(method=="gbm"){
      suppressWarnings(
        # pass verbose==FALSE directly to train (verboseIter doesn't 
        # suppress output)
        fit.train <- caret::train(x = X, y = Y, weights = obsWeights, 
                                  metric = metric, method = method, 
                                  tuneLength = tuneLength, 
                                  trControl = trControl,verbose=FALSE)
      )
    }else if(method=="nnet"){
      suppressWarnings(
        fit.train <- caret::train(x = X, y = Y, weights = obsWeights, 
                                  metric = metric, method = method, 
                                  tuneLength = tuneLength, 
                                  trControl = trControl,trace=FALSE)
      )
    }else{
      suppressWarnings(
        fit.train <- caret::train(x = X, y = Y, weights = obsWeights, 
                                  metric = metric, method = method, 
                                  tuneLength = tuneLength, 
                                  trControl = trControl)
      )
    }
    pred <- stats::predict(fit.train, newdata = newX, type = "raw")
  }
  if (length(unique(Y))<=2) {
    metric <- "Accuracy"
    Y.f <- as.factor(Y)
    levels(Y.f) <- c("A0", "A1")
    if(method=="gbm"){
      suppressWarnings(
        # pass verbose==FALSE directly to train (verboseIter doesn't 
        # suppress output)
        fit.train <- caret::train(x = X, y = Y.f, weights = obsWeights,
                                  metric = metric, method = method, 
                                  tuneLength = tuneLength, 
                                  trControl = trControl, verbose = FALSE)
      )
    }else if(method=="nnet"){
      suppressWarnings(
        # pass trace==FALSE directly to train (verboseIter doesn't 
        # suppress output)
        fit.train <- caret::train(x = X, y = Y.f, weights = obsWeights,
                                  metric = metric, method = method, 
                                  tuneLength = tuneLength, 
                                  trControl = trControl,trace=FALSE)
      )
    }else{
      suppressWarnings(
        fit.train <- caret::train(x = X, y = Y.f, weights = obsWeights, 
                                  metric = metric, method = method, 
                                  tuneLength = tuneLength, 
                                  trControl = trControl)
      )
    }
    pred <- stats::predict(fit.train, newdata = newX, type = "prob")[,2]
  }
  fit <- list(object = fit.train)
  out <- list(pred = pred, fit = fit)
  class(out$fit) <- c("SL.caret")
  return(out)
}



