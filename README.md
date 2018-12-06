[![Build Status](https://travis-ci.org/AprilYuge/bis557.svg?branch=master)](https://travis-ci.org/AprilYuge/bis557)

BIS557
===

This is a repository for storing all code, documentation, and digital 
artifacts for BIS557.

So far we've created and documented some functions including

(1) linear_model, which onstructs linear regression model.

```{R}
library(bis557)
fit <- linear_model(Sepal.Length ~., iris)
summary(fit)
```
(2) ridge_reg, which constructs ridge regression model.

```{R}
data("ridge_train")
train_scale <- as.data.frame(scale(ridge_train))
fit_ridge <- ridge_reg(y~. -1, train_scale)
fit_ridge$coef
```
(3) sparse.matrix, return an object with class "sparse.matrix". We can also 
implement normalmatrix operator '+', '%*%' and 't()' on the object.

```{R}
sm <- sparse.matrix(i = c(1, 2), j = c(1, 1), x = c(3, 1), dims = c(3, 2))
sm
sm + sm
t(sm) %*% sm
```
