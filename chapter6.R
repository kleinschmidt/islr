#' # 9. Number of applications
#'
#' ## a) Split into training and test
library(ISLR)
set.seed(1)
 
# randomly shuffle row indices of Auto
inds <- sample(nrow(College))
# split these randomly shuffled indices 
split_point <- floor(nrow(College)/2)
train <- inds[1:split_point]
test  <- inds[(split_point+1):nrow(College)]

#' ## b) Least-squares regression

lm_fit <- lm(Apps ~ ., College, subset=train)
lm_test <- predict(lm_fit, College[test, ])
(lm_mse <- mean((College[test, "Apps"] - lm_test)^2))

#' ## c) Ridge regression
library(glmnet)

x <- model.matrix(Apps ~ ., College)[, -1]
y <- College$Apps

ridge_fit <- cv.glmnet(x[train, ], y[train], alpha=0, standardize=FALSE,
                       lambda=exp(seq(1, 15, length=100)))
(ridge_best_lambda <- ridge_fit$lambda.min)

ridge_pred <- predict(ridge_fit, newx=x[test, ], s=ridge_best_lambda)
(ridge_mse <- mean((y[test] - ridge_pred)^2))

#' ## d) Lasso
#'
lasso_fit <- cv.glmnet(x[train, ], y[train], alpha=1, standardize=FALSE,
                       lambda=exp(seq(1, 15, length=100)))
(lasso_best_lambda <- lasso_fit$lambda.min)

lasso_pred <- predict(lasso_fit, newx=x[test, ], s=lasso_best_lambda)
(lasso_mse <- mean((y[test] - lasso_pred)^2))

#' ## e) PCR

library(pls)

pcr_fit <- pcr(Apps ~ ., data=College, subset=train, scale=TRUE, validation="CV")
(best_m <- which.min(pcr_fit$validation$PRESS))

pcr_pred <- predict(pcr_fit, x[test, ], ncomp=best_m)
(pcr_mse <- mean((pcr_pred - y[test])^2))

#' ## f) PLS

pls_fit <- plsr(Apps ~ ., data=College, subset=train, scale=TRUE, validation="CV")
(best_m <- which.min(pls_fit$validation$PRESS))

pls_pred <- predict(pls_fit, x[test, ], ncomp=best_m)
(pls_mse <- mean((pls_pred - y[test])^2))

