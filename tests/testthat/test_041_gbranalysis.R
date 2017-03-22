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

context("ComputeLinearModelWeights")

options("geoexperiments.gbr1.weight.power"=NULL)

test_that("zero values map to NAs", {
  for (x in list(0, c(0, 0, 0, 0, 0), c(1, 2, 3, 4, 0))) {
    expect_error(w <- ComputeLinearModelWeights(x),
                 regexp=NA)
    expect_identical(is.na(w), x == 0)
  }
})

test_that("default weights are equal to 1/x^2", {
  for (x in list(c(1, 2, 3, 4, 5), c(1, 2, 3, 4, 0))) {
    expect_error(w <- ComputeLinearModelWeights(x),
                 regexp=NA)
    x[x == 0] <- NA_real_
    expect_equivalent(w, 1 / x^2)
  }
})

test_that("the 'power' argument produces weights equal to 1/x^power", {
  x <- c(1, 2, 3, 4, 5)
  expect_error(w <- ComputeLinearModelWeights(x, power=1.0),
               regexp=NA)
  expect_equivalent(w, 1 / x)
  expect_equal(attr(w, "power"), 1.0)
  x <- c(1, 2, 3, 4, 0)
  expect_error(w <- ComputeLinearModelWeights(x, power=1.5),
               regexp=NA)
  x[5] <- NA_real_
  expect_equivalent(w, 1 / x^1.5)
  expect_equal(attr(w, "power"), 1.5)
})

test_that("option geoexperiments.gbr1.weight.power overrides 'power'", {
  on.exit(options("geoexperiments.gbr1.weight.power"=NULL))
  x <- c(1, 2, 3, 4, 0)
  expect_error(w.default <- ComputeLinearModelWeights(x),
               regexp=NA)
  expect_error(w.1.5 <- ComputeLinearModelWeights(x, power=1.5),
               regexp=NA)
  # Set the option. Overrides.
  options("geoexperiments.gbr1.weight.power"=1.5)
  expect_error(w <- ComputeLinearModelWeights(x),
               regexp=NA)
  expect_equal(attr(w, "power"), 1.5)
  expect_identical(w, w.1.5)
  # The option overrides the argument value.
  expect_identical(ComputeLinearModelWeights(x),
                   ComputeLinearModelWeights(x, power=1.75))
})

context("EstimateIncremental.GBRROASAnalysisData")

iROAS <- 4.0
ad.spend.diff.daily <- 1000
total.ad.spend.diff <- (n.test.days * ad.spend.diff.daily)

obj.gbr <- .MakeGBRDataObject(dfged3, iroas=iROAS,
                              ad.spend.diff.daily=ad.spend.diff.daily,
                              ad.spend.daily=0)

test_that("spend is correctly calculated if pretest values are all zeros", {
  expect_is(incr.spend <- EstimateIncremental(obj.gbr, variable="cost"),
            "numeric")
  # Incremental spend in Control should be zero.
  ctrl <- obj.gbr[[kControl]]
  tst <- (!ctrl)
  expect_equal(sum(incr.spend[ctrl]), 0)
  # Total incremental spend in Test should be equal to the total.
  sum.cost.clicks <- sum(with(obj.gbr, cost.test[tst] - cost.pre[tst]))
  expect_equal(sum.cost.clicks, sum(incr.spend))
  expect_equal(sum(incr.spend), total.ad.spend.diff)
})

test_that("spend is correctly calculated if most pretest values are zeros", {
  ctrl <- obj.gbr[[kControl]]
  test <- !ctrl
  obj.gbr[[kCostPre]] <- 0
  obj.gbr[[kCostPre]][which(ctrl)[1:3]] <- 1.0
  expect_is(incr.spend <- EstimateIncremental(obj.gbr, variable="cost"),
            "numeric")
  # Incremental spend in Control should be zero.
  expect_equal(sum(incr.spend[ctrl]), 0)
  # Total incremental spend in Test should be equal to the total.
  sum.cost.clicks <- sum(with(obj.gbr, cost.test[test] - cost.pre[test]))
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
  ctrl <- obj.gbr[[kControl]]
  expect_equal(sum(incr.spend[ctrl]), 0)
  # Sum of point estimates.
  sum.cost.clicks <- sum(incr.spend)
  # Should be within a reasonably close range to the actual ad spend difference.
  expect_true(abs(1 - sum.cost.clicks / total.ad.spend.diff) < 0.05)
})

context("DoGBRROASAnalysis.GBRROASAnalysisData")

test_that("fit fails if there are no geos available with weights", {
  # Eliminate all geos.
  obj.gbr[[kRespPre]] <- 0
  # Also a warning is given.
  expect_warning(expect_error(DoGBRROASAnalysis(obj.gbr),
                              regexp=paste0("Cannot fit GBR model:",
                                  " no data points available")),
                 regexp="^[0-9]+ geos .* have zero pretest response")

})

test_that("fit fails if there are fewer than 4 geos available with weights", {
  # Enable only minimum required minus 1 geos.
  obj.gbr[[kRespPre]] <- 0
  n <- kGbr1MinObs - 1L
  obj.gbr[[kRespPre]][seq(n)] <- seq(n)
  # Also a warning is given.
  expect_warning(expect_error(DoGBRROASAnalysis(obj.gbr),
                              regexp=sprintf(paste0("Cannot fit GBR model:",
                                  " only %d data points available"), n)),
                 regexp="^[0-9]+ geos .* have zero pretest response")
})

test_that("fit succeeds if at least 4 data points and neither group empty", {
  # All geos with resp.pre == 0 are omitted from the analysis.
  resp.pre <- obj.gbr[[kRespPre]]
  control <- obj.gbr[[kControl]]
  test <- !control
  n.control <- sum(control)
  n.test <- sum(test)
  obj.gbr[[kRespPre]][control] <- 0
  obj.gbr[[kRespPre]][test] <- 0
  obj.gbr[[kRespPre]][which(control)[1:2]] <- c(2.4, 1.2)
  n <- kGbr1MinObs - 2L
  obj.gbr[[kRespPre]][which(test)[seq(n)]] <- seq(n)
  # Also a warning is given.
  expect_error(expect_warning(DoGBRROASAnalysis(obj.gbr),
                              regexp=paste0("^[0-9]+ geos .* have ",
                                  "zero pretest response")),
               regexp=NA)
})

test_that("fit fails if no control or test geos available with weights", {
  # All geos with resp.pre == 0 are omitted from the analysis.
  resp.pre <- obj.gbr[[kRespPre]]
  control <- obj.gbr[[kControl]]
  # Eliminate all control geos.
  obj.gbr[[kRespPre]][control] <- 0
  # Also a warning is given.
  expect_warning(expect_error(DoGBRROASAnalysis(obj.gbr),
                              regexp=paste0("Cannot fit GBR model:",
                                  " no control geos")),
                 regexp="^[0-9]+ geos .* have zero pretest response")
  # Restore control geos, eliminate all treatment geos.
  obj.gbr[[kRespPre]] <- resp.pre
  obj.gbr[[kRespPre]][!control] <- 0
  # Also a warning is given.
  expect_warning(expect_error(DoGBRROASAnalysis(obj.gbr),
                              regexp=paste0("Cannot fit GBR model:",
                                  " no treatment geos")),
                 regexp="^[0-9]+ geos .* have zero pretest response")
})

test_that("fit fails if resp.pre is constant", {
  obj.gbr[[kRespPre]] <- 1.0
  expect_error(obj <- DoGBRROASAnalysis(obj.gbr),
               regexp="GBR model fit failed")
})

test_that("fit fails if cost.pre and cost.test are constant", {
  obj.gbr[[kCostPre]] <- 1.0
  obj.gbr[[kCostTest]] <- 1.0
  expect_error(obj <- DoGBRROASAnalysis(obj.gbr),
               regexp="GBR model fit failed")
})

test_that("a warning is given if some geos have pretest response zero", {
  obj.gbr[[kRespPre]][1:2] <- 0
  geos.txt <- paste0(obj.gbr[[kGeo]][1:2], collapse=", ")
  expect_warning(obj <- DoGBRROASAnalysis(obj.gbr),
                 regexp=sprintf("2 geos \\(%s\\) have zero pretest response",
                     geos.txt))
})

test_that("model fit is reasonably close to the truth", {
  expect_is(obj <- DoGBRROASAnalysis(obj.gbr), "GBRROASAnalysisFitGbr1")
  r <- summary(obj)
  expect_equal(round(r$estimate[1], digits=1), iROAS)
  expect_equal(round(r$precision[1], digits=2), 0.06)
})

test_that("Doubling the spend should halve the CI", {
  expect_is(obj <- DoGBRROASAnalysis(obj.gbr), "GBRROASAnalysisFitGbr1")
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
  expect_error(summary(obj, level=-0.1))
  expect_error(summary(obj, level=1.1))
})
