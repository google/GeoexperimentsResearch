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

context("DoTBRAnalysisTbr1.TBRAnalysisData")

# Create a data set with almost no noise.
set.seed(1)
dftbr1 <- data.frame(as.Date("2016-05-01") + 0:20,
                     as.integer(c(-1, -1, rep(0, 8), NA, rep(1, 8), 2, 3)),
                     NA_real_,
                     abs(rnorm(21)))

names(dftbr1) <- c(kDate, kPeriod, kY, kX)
tolerance <- 0.001
dftbr1[[kY]] <- (5.0 + dftbr1[[kX]] * 3.0 + rnorm(nrow(dftbr1), sd=0.0001))
obj.tbr <- TBRAnalysisData(dftbr1)
in.analysis <- IsInPeriod(obj.tbr, periods="analysis")
in.prediction <- IsInPeriod(obj.tbr, period="prediction")
kIncrement <- 20.0
obj.tbr[[kY]][in.prediction] <- (obj.tbr[[kY]][in.prediction] + kIncrement)

test_that("structure is as expected", {
  expect_is(x <- DoTBRAnalysisTbr1(obj.tbr), "TBRAnalysisFitTbr1")
  cols <- c(kDate, kPeriod, kY, kX, kYpredMean, kYpredSd, kYpredDif,
            kYpredCumdif, kYpredCumdifSd, kYestSd)
  expect_is(x, "data.frame")
  expect_identical(names(x), cols)
  expect_equal(as.list(x[c(kDate, kY, kX)]),
               as.list(obj.tbr[c(kDate, kY, kX)]))
})

x <- DoTBRAnalysisTbr1(obj.tbr)

test_that("differences are correctly computed", {
  # Pointwise differences.
  ypredmean <- x[[kYpredMean]][in.prediction]
  y <- x[[kY]][in.prediction]
  preddif <- x[[kYpredDif]][in.prediction]
  expect_equal(y - ypredmean, preddif, tol=tolerance)
  # Cumulative differences.
  predcumdif <- x[[kYpredCumdif]][in.prediction]
  expect_equal(cumsum(y - ypredmean), predcumdif, tol=tolerance)
})

test_that("estimates are as expected", {
  # The increment is correctly estimated.
  preddif <- x[[kYpredDif]][in.prediction]
  increments <- rep(kIncrement, length(preddif))
  expect_equal(preddif, increments, tol=tolerance)
  # Essentially zero s.d.
  predsd <- x[[kYpredSd]][in.prediction]
  expect_equal(predsd, rep(0, length(predsd)), tol=tolerance)
  # Cumulative s.d. also essentially zero.
  predcumdifsd <- x[[kYpredCumdifSd]][in.prediction]
  expect_equal(predcumdifsd, rep(0, length(predsd)), tol=tolerance)
})


test_that("cumulative differences in the pretest period are all NA", {
  # Cumulative differences in the pretest period would be the cumulative
  # residuals, which does not make sense as a visual diagnostic. We replace
  # them currently with NAs.
  in.pretest <- IsInPeriod(x, periods="pretest")
  preddif <- x[[kYpredCumdif]][in.pretest]
  expect_true(all(is.na(preddif)))
})

test_that("estimation s.d. equals the pointwise s.d. of the residuals", {
  in.pretest <- IsInPeriod(x, periods="pretest")
  control <- x[[kX]][in.pretest]
  se <- x[[kYestSd]][in.pretest]
  v <- vcov(GetInfo(x, "fit"))
  s.d. <- sqrt(v[1, 1] + 2 * control * v[1, 2] + control^2 * v[2, 2])
  expect_equal(se, s.d.)
  # Outside pretest, all should be NA.
  expect_true(all(is.na(x[[kYestSd]][!in.pretest])))
})


context("summary.TBRAnalysisFitTbr1")

test_that("one-sided 90% interval is the default", {
  s0 <- summary(x)
  s1 <- summary(x, level=0.90, interval.type="one-sided")
  expect_identical(s0, s1)
})

test_that("allow levels 0.5 or even lower", {
  expect_error(summary(x, level=0.5),
               regexp=NA)
  expect_error(summary(x, level=0.1),
               regexp=NA)
})

test_that("disallow levels 0 or 1", {
  expect_error(summary(x, level=0))
  expect_error(summary(x, level=1))
})

test_that("disallow levels that don't make sense", {
  expect_error(summary(x, level=-0.1))
  expect_error(summary(x, level=1.1))
})

test_that("one-sided and two-sided intervals are correctly calculated", {
  s1 <- summary(x, level=0.90, interval.type="one-sided")
  s2 <- summary(x, level=0.80, interval.type="two-sided")
  expect_identical(s1[["lower"]], s2[["lower"]])
  s3 <- summary(x, level=0.975, interval.type="one-sided")
  s4 <- summary(x, level=0.95, interval.type="two-sided")
  expect_identical(s3[["lower"]], s4[["lower"]])
})

s0 <- summary(x, level=0.90, interval.type="one-sided")

test_that("the model is equivalent to kTBRModel1", {
  expect_identical(s0[["model"]], kTBRModel1)
})

test_that("the estimate matches cumulative diff. at end of analysis", {
  pred.cum.dif.at.end <- tail(x[[kYpredCumdif]][in.analysis], 1)
  expect_equal(s0[["estimate"]], pred.cum.dif.at.end)
})

test_that("the standard error in the summary matches predictive s.d.", {
  pred.cum.dif.sd.at.end <- tail(x[[kYpredCumdifSd]][in.analysis], 1)
  expect_equal(s0[["se"]], pred.cum.dif.sd.at.end)
})

test_that("precision matches the margin of error", {
  level <- 0.90
  s1 <- summary(x, level=level, interval.type="one-sided")
  s.e. <- tail(x[[kYpredCumdifSd]][in.analysis], 1)
  dof <- GetInfo(x, field="fit")[["df.residual"]]
  margin.of.error <- s.e. * qt(level, df=dof)
  expect_equal(s1[["precision"]], margin.of.error)
})

test_that("two-sided lower and upper bounds match the actual ones", {
  level <- 0.90
  s1 <- summary(x, level=level, interval.type="two-sided")
  estimate <- s1[["estimate"]]
  precision <- s1[["precision"]]
  expect_equal(estimate - precision, s1[["lower"]])
  expect_equal(estimate + precision, s1[["upper"]])
})

test_that("one-sided lower and upper bounds match the actual ones", {
  level <- 0.90
  s1 <- summary(x, level=level, interval.type="one-sided")
  estimate <- s1[["estimate"]]
  precision <- s1[["precision"]]
  expect_equal(estimate - precision, s1[["lower"]])
  expect_equal(Inf, s1[["upper"]])
})

test_that("if x==0 and y==0, predictions are 0 with no uncertainty", {
  tbr <- obj.tbr
  in.pretest <- IsInPeriod(x, periods="pretest")
  tbr[[kY]][in.pretest] <- 0
  tbr[[kX]] <- 0
  x <- DoTBRAnalysisTbr1(tbr)
  expect_equal(x[[kYpredMean]][in.prediction], rep(0, sum(in.prediction)))
  expect_equal(x[[kYpredSd]][in.prediction], rep(0, sum(in.prediction)))
})

test_that("if x==constant and y==0, predictions==0", {
  tbr <- obj.tbr
  in.pretest <- (tbr[[kPeriod]] %in% kStandardPeriods[["pretest"]])
  tbr[[kY]][in.pretest] <- 0
  tbr[[kX]] <- 1234
  x <- DoTBRAnalysisTbr1(tbr)
  expect_equal(x[[kYpredMean]][in.prediction], rep(0, sum(in.prediction)))
  expect_equal(x[[kYpredSd]][in.prediction], rep(0, sum(in.prediction)))
})

test_that("if x==constant and y==y0 (const), predictions==y0", {
  tbr <- obj.tbr
  in.pretest <- (tbr[[kPeriod]] %in% kStandardPeriods[["pretest"]])
  tbr[[kY]][in.pretest] <- 1
  tbr[[kX]] <- 1234
  x <- DoTBRAnalysisTbr1(tbr)
  in.prediction <- (x[[kPeriod]] %in% kStandardPeriods[["prediction"]])
  expect_equal(x[[kYpredMean]][in.prediction], rep(1, sum(in.prediction)))
  expect_equal(x[[kYpredSd]][in.prediction], rep(0, sum(in.prediction)))
})

test_that("if x==0 and y==y0 + noise, predictions==y0", {
  tbr <- obj.tbr
  in.pretest <- (tbr[[kPeriod]] %in% kStandardPeriods[["pretest"]])
  y <- rnorm(sum(in.pretest))
  tbr[[kY]][in.pretest] <- y
  tbr[[kX]] <- 1234
  x <- DoTBRAnalysisTbr1(tbr)
  fit <- GetInfo(x)[["fit"]]
  sigma <- summary(fit)[["sigma"]]
  beta0.var <- vcov(fit)[1, 1]
  s.d. <- sqrt(beta0.var + sigma^2)
  expect_equal(x[[kYpredMean]][in.prediction], rep(mean(y), sum(in.prediction)))
  expect_equal(x[[kYpredSd]][in.prediction], rep(s.d., sum(in.prediction)))
})

test_that("predictive s.d. in pretest variance is zero", {
  x <- DoTBRAnalysisTbr1(obj.tbr)
  in.pretest <- IsInPeriod(x, periods="pretest")
  expect_equal(x[[kYpredSd]][in.pretest], rep(0, sum(in.pretest)))
})

test_that("posterior tail probability is 0.5 at the estimate", {
  x <- DoTBRAnalysisTbr1(obj.tbr)
  estimate <- summary(x)[["estimate"]]
  expect_equal(0.5, summary(x, threshold=estimate)[["prob"]])
})

test_that("Pr(effect > lower bound of one-sided x% CI) = x.", {
  x <- DoTBRAnalysisTbr1(obj.tbr)
  # Pr(beta2 > lower bound of one-sided x% CI) = x.
  # Pr(beta2 > upper bound of one-sided x% CI) = 1 - x.
  browser()
  for (one.sided.level in c(0.975, 0.95, 0.9, 0.8)) {
    two.sided.level <- (1 - 2 * (1 - one.sided.level))
    summ <- summary(x, level=two.sided.level, interval.type="two-sided")
    lower.bound <- summ[["lower"]]
    upper.bound <- summ[["upper"]]
    expect_equal(one.sided.level,
                 summary(x, threshold=lower.bound)[["prob"]])
    expect_equal(1 - one.sided.level,
                 summary(x, threshold=upper.bound)[["prob"]])
  }
})
