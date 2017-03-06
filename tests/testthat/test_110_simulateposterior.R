# Copyright 2016 Google Inc. All Rights Reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

context("SimulatePosterior")

dftbr1 <- data.frame(as.Date("2016-05-01") + 0:20,
                     c(-1, -1, rep(0, 8), NA, rep(1, 8), 2, 3),
                     0,
                     rnorm(21, mean=100 + 10 * rep(c(-1, 0, 1), 7), sd=1))
names(dftbr1) <- c(kDate, kPeriod, kY, kX)
obj.tbr <- TBRAnalysisData(dftbr1)
obj.tbr[[kY]] <- (5.0 + obj.tbr[[kX]] * 3.0 +
                  rep(5 * c(1, 0, -1), length.out=21))
in.prediction <- IsInPeriod(obj.tbr, periods="prediction")
kIncrement <- 20.0
obj.tbr[[kY]][in.prediction] <- (obj.tbr[[kY]][in.prediction] + kIncrement)
obj.fit <- DoTBRAnalysisTbr1(obj.tbr)
dftest <- obj.fit[in.prediction, , drop=FALSE]
x.test <- dftest[[kX]]
y.test <- dftest[[kY]]
lmfit <- GetInfo(obj.fit, "fit")
sigma <- summary(lmfit)$sigma
dof <- summary(lmfit)$df[2]
n.x <- nrow(dftest)

test_that("n.sims must be a positive integer >= 2", {
  expect_error(SimulatePosterior(obj.fit, n.sims=0))
  expect_error(SimulatePosterior(obj.fit, n.sims=1))
  expect_error(SimulatePosterior(obj.fit, n.sims=-1))
  expect_error(SimulatePosterior(obj.fit, n.sims=1000.1))
  expect_error(SimulatePosterior(obj.fit, n.sims="default"))
})


context("SimulatePosterior.TBRAnalysisFitTbr1")

test_that("result is a n * n.sims matrix of class 'PosteriorSimulations'", {
  n.sims <- 100
  expect_is(obj.sim <- SimulatePosterior(obj.fit, n.sims=n.sims),
            "PosteriorSimulations")
  expect_is(obj.sim, "matrix")
  expect_equal(dim(obj.sim), c(n.x, n.sims))
})

test_that("simulated quantiles match with those predicted", {
  set.seed(1)
  obj.sim <- SimulatePosterior(obj.fit, n.sims=1e5)
  probs <- c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95)
  for (i in seq(from=1, to=n.x)) {
    y.dif <- dftest[i, kYpredDif]  # Location of the t distribution.
    y.sd <- dftest[i, kYpredSd]  # Scale of the t distribution.
    y.dif.post <- quantile(obj.sim[i, ], probs=probs)
    names(y.dif.post) <- NULL
    y.dif.pred <- (y.dif + y.sd * qt(probs, df=dof))
    for (i in seq_along(probs)) {
      expect_equal(1, y.dif.pred[i] / y.dif.post[i], tol=0.001)
    }
  }
})

test_that("simulations are drawn when model has only 1 parameter", {
  # Fix covariate to a constant => one-parameter model chosen.
  obj.tbr[[kX]] <- 0
  obj.fit <- DoTBRAnalysisTbr1(obj.tbr)
  expect_equal(1, GetInfo(obj.fit)[["fit"]][["rank"]])
  expect_error(obj.sim <- SimulatePosterior(obj.fit, n.sims=2),
               regexp=NA)
})


context("quantile.PosteriorSimulations")

obj.sim <- SimulatePosterior(obj.fit, n.sims=10)

test_that("error occurs if 'probs' is empty", {
  expect_error(quantile(obj.sim, probs=NULL))
  expect_error(quantile(obj.sim, probs=numeric(0)))
})

test_that("error occurs if 'probs' is not numeric", {
  expect_error(quantile(obj.sim, probs="10%"))
  expect_error(quantile(obj.sim, probs=list(0.1)))
})

test_that("single 'probs' returns a matrix nevertheless", {
  expect_equal(dim(quantile(obj.sim, probs=0.5)), c(n.x, 1))
})

test_that(">= 2 'probs' returns a matrix", {
  expect_equal(dim(quantile(obj.sim, probs=c(0.5, 0.9))), c(n.x, 2))
})

test_that("quantiles match", {
  probs <- c(0.5, 0.95, 0.9, 0.1, 0.05)
  x <- t(apply(obj.sim, MARGIN=1, quantile, probs=probs, names=TRUE))
  expect_equal(x, quantile(obj.sim, probs=probs))
})
