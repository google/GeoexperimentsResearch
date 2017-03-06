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

context("EstimateIncremental.GBRROASAnalysisData")

iROAS <- 4.0
ad.spend.diff.daily <- 100
total.ad.spend.diff <- (n.test.days * ad.spend.diff.daily)

obj.gbr <- .MakeGBRDataObject(dfged3, iroas=iROAS,
                              ad.spend.diff.daily=ad.spend.diff.daily,
                              ad.spend.daily=0)

test_that("spend is correctly calculated if pre-period is all zeros", {
  expect_is(incr.spend <- EstimateIncremental(obj.gbr, variable="cost"),
            "numeric")
  # Incremental spend in Control should be zero.
  ctrl <- obj.gbr[["control"]]
  tst <- (!ctrl)
  expect_equal(sum(incr.spend[ctrl]), 0)
  # Total incremental spend in Test should be equal to the total.
  sum.cost.clicks <- sum(with(obj.gbr, cost.test[tst] - cost.pre[tst]))
  expect_equal(sum.cost.clicks, sum(incr.spend))
  expect_equal(sum(incr.spend), total.ad.spend.diff)
})

test_that("spend is correctly calculated if pre-period is nonzero", {
  obj.gbr <- .MakeGBRDataObject(dfged3, iroas=iROAS,
                                ad.spend.diff.daily=ad.spend.diff.daily,
                                ad.spend.daily=1000)
  expect_is(incr.spend <- EstimateIncremental(obj.gbr, variable="cost"),
            "numeric")
  # Incremental spend in Control should be zero.
  ctrl <- obj.gbr[["control"]]
  expect_equal(sum(incr.spend[ctrl]), 0)
  # Sum of point estimates.
  sum.cost.clicks <- sum(incr.spend)
  # Should be within a reasonably close range to the actual ad spend difference.
  expect_true(abs(1 - sum.cost.clicks / total.ad.spend.diff) < 0.05)
})


context("summary.GBRROASAnalysisFitGbr1")

obj <- DoGBRROASAnalysis(obj.gbr)

test_that("an object of proper class is returned", {
  expect_is(summary(obj), "ROASAnalysisResults")
})

test_that("results match for default one-sided intervals", {
  r <- summary(obj)
  kDefaultOneSidedLevel <- 0.9
  expect_identical(r$upper, Inf)  # One-sided.
  expect_equal(r$level, kDefaultOneSidedLevel)  # Default.
  expect_identical(r$estimate, coef(obj)["incr.cost", "Estimate"])
  # This conversion keeps the lower bounds the same.
  two.sided.level <- (2 * kDefaultOneSidedLevel - 1)
  ci <- confint(obj, level=two.sided.level)["incr.cost", ]
  expect_identical(r$lower, as.numeric(ci[1L]))
  expect_equal(r$precision, (r$estimate - r$lower))
  expect_identical(r$model, kGBRModel1)
})

test_that("results match for 2-sided intervals", {
  kLevel <- 0.9
  r <- summary(obj, level=kLevel, interval.type="two-sided")
  expect_identical(r$estimate, coef(obj)["incr.cost", "Estimate"])
  ci <- confint(obj, level=kLevel)["incr.cost", ]
  expect_identical(r$lower, as.numeric(ci[1L]))
  expect_identical(r$upper, as.numeric(ci[2L]))
  expect_equal(r$precision, 0.5 * (r$upper - r$lower))
  expect_equal(r$level, kLevel)
  expect_identical(r$model, kGBRModel1)
})

test_that("posterior tail probability is 0.5 at the estimate", {
  estimate <- summary(obj)[["estimate"]]
  expect_equal(0.5, summary(obj, threshold=estimate)$prob)
})

test_that("Pr(beta2 > lower bound of one-sided x% CI) = x.", {
  # Pr(beta2 > lower bound of one-sided x% CI) = x.
  # Pr(beta2 > upper bound of one-sided x% CI) = 1 - x.
  for (one.sided.level in c(0.975, 0.95, 0.9, 0.8)) {
    two.sided.level <- (1 - 2 * (1 - one.sided.level))
    ci <- confint(obj, level=two.sided.level)
    lower.bound <- ci[1]
    expect_equal(one.sided.level, summary(obj, threshold=lower.bound)$prob)
  }
})

test_that("Pr(beta2 > upper bound of one-sided x% CI) = 1 - x.", {
  for (one.sided.level in c(0.975, 0.95, 0.9, 0.8)) {
    two.sided.level <- (1 - 2 * (1 - one.sided.level))
    ci <- confint(obj, level=two.sided.level)
    upper.bound <- ci[2]
    expect_equal(1 - one.sided.level, summary(obj, threshold=upper.bound)$prob)
  }
})

test_that("allow levels 0.5 or even lower", {
  expect_error(summary(obj, level=0.5),
               regexp=NA)
  expect_error(summary(obj, level=0.1),
               regexp=NA)
})

test_that("disallow levels 0 or 1", {
  expect_error(summary(obj, level=0))
  expect_error(summary(obj, level=1))
})

test_that("disallow levels that don't make sense", {
  expect_error(summary(x, level=-0.1))
  expect_error(summary(x, level=1.1))
})

context("DoGBRROASAnalysis.GBRROASAnalysisData")

test_that("Model fit is reasonably close to the truth", {
  expect_is(obj <- DoGBRROASAnalysis(obj.gbr), "GBRROASAnalysisFitGbr1")
  r <- summary(obj)
  expect_equal(round(r$estimate[1], digits=1), iROAS)
  expect_equal(round(r$precision[1], digits=2), 0.63)
})

test_that("Doubling the spend should halve the CI", {
  obj.gbr2 <- .MakeGBRDataObject(dfged3, iroas=iROAS,
                                 ad.spend.diff.daily=2 * ad.spend.diff.daily,
                                 ad.spend.daily=0)
  expect_is(obj2 <- DoGBRROASAnalysis(obj.gbr2), "GBRROASAnalysisFitGbr1")
  r1 <- summary(obj)
  r2 <- summary(obj2)
  expect_equal(round(r2$precision[1] / r1$precision[1], digits=2), 0.50)
})


context("DoGBRROASAnalysis.GeoExperimentData")

test_that("GBRROASAnalysis of GED == GBRROASAnalysis of GBRROASAnalysisData", {
  dfged0 <- .AddSpendAndROAS(dfged3, iroas=iROAS,
                             ad.spend.diff.daily=ad.spend.diff.daily,
                             ad.spend.daily=0)
  obj.gts1 <- GeoTimeseries(dfged0, metrics=c("sales", "cost.clicks"))
  obj.ged1 <- GeoExperimentData(obj.gts1)
  obj.gbr1 <- as.GBRROASAnalysisData(obj.ged1, response="sales",
                            cost="cost.clicks", pre.period=0L,
                            intervention.period=1L, cooldown.period=2L)
  # A GBR analysis using the GBRROASAnalysisData object.
  obj.fit1 <- DoGBRROASAnalysis(obj.gbr1)
  # Now perform GBR analysis directly using the GeoExperimentData object.
  obj.fit2 <- DoGBRROASAnalysis(obj.ged1, response="sales",
                            cost="cost.clicks", pre.period=0L,
                            intervention.period=1L, cooldown.period=2L)
  expect_identical(summary(obj.fit1), summary(obj.fit2))
})


test_that("group numbers are passed over to GBRROASAnalysisData", {
  dfged0 <- .AddSpendAndROAS(dfged3, iroas=4,
                             ad.spend.diff.daily=ad.spend.diff.daily,
                             ad.spend.daily=0)
  dfged0[["cost.clicks"]] <- abs(rnorm(nrow(dfged0),
                                       mean=dfged0[["cost.clicks"]]))
  obj.gts1 <- GeoTimeseries(dfged0, metrics=c("sales", "cost.clicks"))
  obj.ged1 <- GeoExperimentData(obj.gts1)
  obj.gbr1 <- as.GBRROASAnalysisData(obj.ged1,
                                     response="sales", cost="cost.clicks",
                                     pre.period=0L, intervention.period=1L,
                                     cooldown.period=2L, control.group=1,
                                     treatment.group=2)
  # Groups switched.
  obj.gbr2 <- as.GBRROASAnalysisData(obj.ged1,
                                     response="sales", cost="cost.clicks",
                                     pre.period=0L, intervention.period=1L,
                                     cooldown.period=2L, control.group=2,
                                     treatment.group=1)
  # A GBR analysis using the GBRROASAnalysisData object.
  obj.fit.12a <- DoGBRROASAnalysis(obj.gbr1)
  obj.fit.21a <- DoGBRROASAnalysis(obj.gbr2)
  # Now switch groups.
  obj.fit.12b <- DoGBRROASAnalysis(obj.ged1,
                                   response="sales", cost="cost.clicks",
                                   pre.period=0L, intervention.period=1L,
                                   cooldown.period=2L, control.group=1,
                                   treatment.group=2)
  obj.fit.21b <- DoGBRROASAnalysis(obj.ged1,
                                   response="sales", cost="cost.clicks",
                                   pre.period=0L, intervention.period=1L,
                                   cooldown.period=2L, control.group=2,
                                   treatment.group=1)
  expect_equal(obj.fit.12a, obj.fit.12b)
  expect_equal(obj.fit.21a, obj.fit.21b)
})
