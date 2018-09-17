[![Build Status](https://travis-ci.org/AprilYuge/bis557.svg?branch=master)](https://travis-ci.org/AprilYuge/bis557)

BIS557
===

This is a repository for storing all code, documentation, and digital 
artifacts for BIS557.

So far we've created and documented a function that constructs linear 
model, and gives the coefficiens and residuals. You can use it like this:

```{R}
library(bis557)
fit <- linear_model(Sepal.Length ~., iris)
fit
```
