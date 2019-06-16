library(tidyverse)
library(DescTools)
library(ranger)
library(caret)
library(doParallel)
library(tictoc)
library(MLmetrics)

set.seed(1909)

# parallelization
cl <- makePSOCKcluster(6) # run with 6 threads
registerDoParallel(cl)


preProcess2 <- function(df_train, method = c("center", "scale", "nzv"), 
                        df_validation = NULL, df_test = NULL,
                        verbose = TRUE) {

  # preprocess training set
  # exclude target variable from dataset
  X_train <- df_train %>% select(-shot_made_flag)
  y_train <- df_train %>% select(shot_made_flag)
  
  dummy_object <- dummyVars(~ ., data = X_train, drop2nd = TRUE)
  df_train_dummies <- predict(dummy_object, newdata = X_train)
  
  pp_object <- preProcess(df_train_dummies, method = method, verbose = verbose)
  df_train_pp <- predict(pp_object, newdata = df_train_dummies) %>%
    as_tibble() %>%
    bind_cols(y_train = y_train)
  result <- list(
    "train" = df_train_pp,
    "validation" = NULL,
    "test" = NULL
    )
  
  # use predict to preprocess validation and test set on the same model
  reproduce <- function(df, dummy_object, pp_object) {
    X <- df %>% select(-shot_made_flag)
    y <- df %>% select(shot_made_flag)
    df_dummies <- predict(dummy_object, newdata = X)
    df_pp <- predict(pp_object, newdata = df_dummies) %>%
      as_tibble() %>%
      bind_cols(y = y)
    df_pp
  }
  
  # preprocess validation set
  if (!is.null(df_validation)) {
    result[["validation"]] <- reproduce(df_validation, dummy_object, pp_object)
  }
  # preprocess test set
  if (!is.null(df_test)) {
    result[["test"]] <- reproduce(df_test, dummy_object, pp_object)
  }
  result
}



pp <- preProcess2(df_train = shots_train, df_validation = shots_validation, df_test = shots_test)
shots_train_pp <- pp[["train"]]
shots_validation_pp <- pp[["validation"]]
shots_test_pp <- pp[["test"]]

# SVM

control <- trainControl(method = "repeatedcv", number = 5, repeats = 3, 
                        search = "random", classProbs = TRUE,
                        summaryFunction = mnLogLoss)
tic()
svm1 <- train(y ~ ., data = train_pp, method = "svmLinear", 
              metric = "logLoss", tuneLength = 2, trControl = control)
toc()

# xGBoost
tic()
set.seed(4711)
control <- trainControl(method = "repeatedcv", number = 5, repeats = 3, 
                        search = "random", classProbs = TRUE,
                        summaryFunction = mnLogLoss)

xgb1 <- train(shot_made_flag ~ ., data = shots_train_pp, method = "xgbTree", 
              metric = "logLoss", tuneLength = 15, trControl = control)
toc()
xgb1


vi_xgb1 <- varImp(xgb1)
vi_xgb1

pred_xgb1 <- predict(xgb1, newdata = shots_validation_pp, type = "prob")
summary(pred_xgb1)

y_pred <- pred_xgb1[, 2]
y_validation <- ifelse(shots_validation$shot_made_flag == "made", 1, 0)
LogLoss(y_pred = y_pred, y_true = y_validation)



stopCluster(cl)
