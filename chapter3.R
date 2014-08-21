library(ggplot2)
#' # 14.
#'
#' ## a) Generate data

set.seed(1)
x1 <- runif(100)
x2 <- 0.5 * x1 + rnorm(100)/10
y <- 2 + 2*x1 + 0.3*x2 + rnorm(100)

data <- data.frame(x1, x2, y)

#' ## b) Correlation and scatterplot

cor(x1, x2)
ggplot(data, aes(x=x1, y=x2)) + geom_point()

#' ## c) Linear model

fit <- lm(y ~ x1 + x2, data)
summary(fit)

#' We can reject the null hypothesis that $\beta_1 = 0$, but not $\beta_2 = 0$.
#' The coefficients estimates underestimate $\beta_1$ (estimated at `r coef(fit)[2]`, actual value is 2) and overestimate $\beta_2$ (estimated at `r coef(fit)[3]`, actually 0.3).  The intercept is pretty close.
#'
#' ## d) Just `x1`
#'

fit1 <- lm(y ~ x1, data)
summary(fit1)

#' Here $\beta_1$ is highly significant, and $\hat \beta_1 =$ `r coef(fit1)[1]`, which is pretty close to the true value.
#' 
#' ## e) Just `x2`

fit2 <- lm(y ~ x2, data)
summary(fit2)

#' Here, too, $\beta_1$ (now the slope for `x2`) is highly significant, but it
#' (as before) overestimates the true value ($\hat \beta_1 =$ `r coef(fit2)[2]`)
#'
#' ## f)
#'
#' No, there's no contradiction.  Both `x1` and `x2` predict `y` well, but they
#' are highly correlated, so when they are fit together what they each
#' individually contributed to the variance is ambiguous.
#'
#' ## g)

data2 <- rbind(data, data.frame(x1=0.1, x2=0.8, y=6))
summary(fit.2 <- lm(y ~ x1 + x2, data2))
summary(fit1.2 <- lm(y ~ x1, data2))
summary(fit2.2 <- lm(y ~ x2, data2))

ggplot(data.frame(data2, new=c(rep("No", 100), "Yes")),
       aes(x=x1, y=x2, color=new)) + geom_point()

par(mfrow=c(2,2))

plot(fit.2)
plot(fit1.2)
plot(fit2.2)

#' Yeah that's pretty fricked up.  In the full model it's very high leverage
#' _and_ has a relatively high value, and in the `x2` model it has moderately
#' high leverage (you can see this on the scatter plot of where this new point
#' is in `x1`-`x2` space). It also completely switches whether `x1` or `x2` is
#' significant in the full model.

library(stargazer)
stargazer(fit, fit.2, type='text')

#' We can see _why_ by looking at the x1-x2 scatter plot with color showing the
#' y values: the new point has a high y value, which is more consistent with its x2
#' coordiante (high) than its x1 coordinate (relatively low):

ggplot(data.frame(data2, new=c(rep("No", 100), "Yes")),
       aes(x=x1, y=x2, color=y)) + geom_point()
