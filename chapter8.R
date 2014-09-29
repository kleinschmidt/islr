library(tree)
library(randomForest)
library(ISLR)
library(MASS)
library(dplyr)
library(ggplot2)
library(knitr)
library(gbm)
library(tidyr)

knitr::chunk_opts$set(cache=TRUE, autodep=TRUE)

#' # Chapter 8: Trees
#'
#' # 7. Boston
#'
#' Problem is to fit the random forests with a more comprehensive set of `mtry` (number of variables to allow) and `ntree` (number of trees in the forest) parameters

set.seed(1)

train <- sample(nrow(Boston), size=floor(nrow(Boston)/2))
test <- (1:nrow(Boston))[-train]

# compute MSE for random forest on Boston data, passing any parameters
bos.rand.forest <- function(...) {
  # construct random forest, passing all arguments to randomForest
  rf <- randomForest(medv ~ ., data=Boston, subset=train, ...)
  test.pred <- predict(rf, newdata=Boston[test, ])
  mse <- mean((test.pred - Boston[test, 'medv'])^2)
}

mtry.max <- ncol(Boston) - 1
mtry.range <- seq(1, mtry.max, 2)

ntree.max <- 500
ntree.range <- round(seq(1, ntree.max, length.out=50))

params <- expand.grid(mtry=mtry.range, ntree=ntree.range)

simulations <- params %>%
  group_by(mtry, ntree) %>%
  summarise(test.mse=bos.rand.forest(mtry=mtry, ntree=ntree))

#+ boston_rand_forest_test_mse, fig.width=7.5, fig.height=4.25
ggplot(simulations, aes(x=ntree, y=test.mse, color=as.factor(mtry), group=mtry)) +
  geom_line()

#' Things seem to level out pretty well by about 400 trees (or maybe even more like 300).  Once you get to 5 trees, test accuracy doesn't really get much better, and even gets a little _worse_ by you reach the max again at 13.

#' # 10. Hitters salary
#'
#' ## a) Data cleaning
#'
#' Remove rows with missing salary info and log-transform Salary

hitters <- Hitters %>%
  filter(! is.na(Salary)) %>%
    transform(Salary = log(Salary))

#' ## b) Split into training and test sets
train <- 1:200
test <- (1:nrow(hitters))[-train]

#' ## c) Vary shrinkage, look at training error
#'
#' Using 1000 trees and $\lambda$ from 0.0001 to 0.1 in log space
params <- data.frame(lambda=exp(seq(log(0.00001), log(0.5), length.out=30))) %>%
  group_by(lambda)

simulations <- params %>%
  do(fit = gbm(Salary ~ ., data=hitters[train, ], distribution="gaussian",
               n.trees=1000, shrinkage=.$lambda))

gbm.hitters.train.mse <- function(fit) {
  train.pred <- predict(fit,n.trees=1000)
  mse <- mean((train.pred - hitters[train, 'Salary'])^2)
}

gbm.hitters.test.mse <- function(fit) {
  test.pred <- predict(fit,n.trees=1000, newdata=hitters[test, ])
  mse <- mean((test.pred - hitters[test, 'Salary'])^2)
}

hitters.mses <- simulations %>%
  do(data.frame(lambda=.$lambda, 
                train.mse=gbm.hitters.train.mse(.$fit),
                test.mse=gbm.hitters.test.mse(.$fit)))

gbm.best <- which.min(hitters.mses$test.mse)
(gbm.best.test.mse <- hitters.mses$test.mse[gbm.best])

ggplot(hitters.mses %>% gather(subset, mse, -lambda),
       aes(x=lambda, y=mse, color=subset)) +
         geom_line()

#' ## d) Look at test error
#'
#' (see above).
#'
#' ## e) Other approaches
#'
#' Regression! Lasso!

lm.fit <- lm(Salary ~ ., data=hitters, subset=train)
summary(lm.fit)
lm.mse <- mean((predict(lm.fit, hitters[test, ]) - hitters[test, 'Salary'])^2)

library(glmnet)

x <- model.matrix(Salary ~ ., data=hitters)
y <- hitters$Salary

ridge_fit <- cv.glmnet(x[train, ], y[train], alpha=1, standardize=TRUE,
                       lambda=exp(seq(-10, 20, length=100)))
                       #lambda=exp(seq(-10, 20, length=100)))
(ridge_best_lambda <- ridge_fit$lambda.min)

ridge_pred <- predict(ridge_fit, newx=x[test, ], s=ridge_best_lambda)
(ridge_mse <- mean((y[test] - ridge_pred)^2))
