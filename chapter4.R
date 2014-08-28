library(ggplot2)
library(dplyr)
library(tidyr)
library(MASS)
library(ISLR)

#' # 11.
#'
#' ## a) mpg01
Auto <- transform(Auto,
                  mpg01 = (mpg > median(mpg)) + 0)

#' ## b) Graphically explore
#+ , fig.width=3.2, fig.height=6.7
ggplot(gather(Auto, variable, value, cylinders:origin),
       aes(y=value, x=mpg01)) +
         geom_point(position="jitter")+
           facet_grid(variable ~ ., scales="free")

#' Cylinders, displacement, horsepower, weight look particularly useful.
#' Possibly year and origin, but there's a lot of overlap there.
#'


#' 
#' ## c) Train/test set
set.seed(1)

# randomly shuffle row indices of Auto
inds <- sample(nrow(Auto))
# split these randomly shuffled indices 
split_point <- floor(nrow(Auto)/2)
train <- inds[1:split_point]
test  <- inds[(split_point+1):nrow(Auto)]

#' ## d) LDA
#'
mpg_lda <- lda(mpg01 ~ cylinders + displacement + horsepower + weight + year,
               data=Auto, subset=train)

mean(predict(mpg_lda)$class == Auto[train, "mpg01"])
mpg_lda_class <- predict(mpg_lda, Auto[test, ])$class
mean(mpg_lda_class == Auto[test, "mpg01"])

#' ## e) QDA
#'
mpg_qda <- qda(mpg01 ~ cylinders + displacement + horsepower + weight + year,
               data=Auto, subset=train)

mean(predict(mpg_qda)$class == Auto[train, "mpg01"])
mpg_qda_class <- predict(mpg_qda, Auto[test, ])$class
mean(mpg_qda_class == Auto[test, "mpg01"])


#' ## f) Logistic regression
#'
mpg_glm <- glm(mpg01 ~ cylinders + displacement + horsepower + weight + year,
               data=Auto, subset=train, family="binomial")

mean((predict(mpg_glm)>0)+0 == Auto[train, "mpg01"])
mpg_glm_class <- (predict(mpg_glm, Auto[test, ]) > 0) + 0
mean(mpg_glm_class == Auto[test, "mpg01"])

#' ## g) KNN
cols <- c("cylinders", "displacement", "horsepower", "weight", "year")
train.x <- Auto[train, cols]
train.mpg01 <- Auto[train, "mpg01"]
test.x <- Auto[test, cols]
test.mpg01 <- Auto[test, "mpg01"]

mpg_knn <- knn(train.x, test.x, train.mpg01, k=10)
mean(mpg_knn == test.mpg01)

ks <- 1:196
test.accs <- sapply(ks,
                    function(kk)
                      mean(knn(train.x, test.x, train.mpg01, k=kk) == test.mpg01))
plot(ks, test.accs, type='p')
ks[which.max(test.accs)]
