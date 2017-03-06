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

# Methods:
# - quantile.TBRAnalysisFitTbr
# - quantile.TBRROASAnalysiFit
# - as.matrix.TBRQuantiles

first.date <- as.Date("2016-09-26")
geo.names <- c("01", "02", "03", "04")
df.gts <- expand.grid(date=first.date + seq(from=0, to=7 * 7 - 1),
                      geo=geo.names)
df.gts[["sales"]] <- runif(nrow(df.gts), min=0,
                           max=100 * as.integer(df.gts[[kGeo]]))
obj.gts <- GeoTimeseries(df.gts, metrics="sales")
# There is 1 week posttest period after Cooldown.
obj.per <- ExperimentPeriods(first.date + c(0L, 3 * 7, 5 * 7, 6 * 7 - 1),
                             period.names=c("Pretest", "Test", "Cooldown"))
obj.ga <- GeoAssignment(data.frame(geo=geo.names, geo.group=c(1, 2, 1, 2)))
obj.ged <- GeoExperimentData(obj.gts, period=obj.per, geo.assignment=obj.ga,
                             treat.assignment=DefaultTreatmentAssignment())
spend.change <- 2000
SetSpendChange(obj.ged, prop.to="sales") <- spend.change
obj.tbr <- as.TBRAnalysisData(obj.ged, response="sales")
tbr1.fit <- DoTBRAnalysisTbr1(obj.tbr)
n.sims <- 17

context("quantile.TBRAnalysisFitTbr1")

test_that("resulting object is a TBRQuantiles object", {
  for (dist in c("pointwise", "cumulative", "counterfactual")) {
    expect_error(x <- quantile(tbr1.fit, probs=c(0.9, 0.1, 0.5),
                               distribution=dist),
                 regexp=NA)
    expect_is(x, "TBRQuantiles")
    expect_is(x[[kDate]], "Date")
    expect_is(x[[kPeriod]], "integer")
    # Quantile columns must be in the specified order.
    expect_equal(names(x), c(kDate, kPeriod, "90%", "10%", "50%"))
  }
})

test_that("it is possible to specify just one 'probs'", {
  for (dist in c("pointwise", "cumulative", "counterfactual")) {
    expect_error(x <- quantile(tbr1.fit, probs=0.5,
                               distribution=dist),
                 regexp=NA)
    expect_equal(names(x), c(kDate, kPeriod, "50%"))
  }
})

test_that("the default quantiles are 0.1, 0.5, and 0.9", {
  for (dist in c("pointwise", "cumulative")) {
    expect_error(x <- quantile(tbr1.fit, distribution=dist),
                 regexp=NA)
    expect_equal(names(x), c(kDate, kPeriod, "10%", "50%", "90%"))
  }
})

test_that("quantiles are those of the t-distribution", {
  probs <- c(0.9, 0.5)
  tbr <- tbr1.fit
  tbr[[".temp"]] <- tbr[[kYpredSd]]
  in.pretest <- IsInPeriod(tbr, periods="pretest")
  tbr[[".temp"]][in.pretest] <- tbr[[kYestSd]][in.pretest]
  for (dist in c("pointwise", "cumulative", "counterfactual")) {
    location <- switch(dist, pointwise=kYpredDif, cumulative=kYpredCumdif,
                       counterfactual=kYpredMean)
    scale <- switch(dist, pointwise=kYpredSd, cumulative=kYpredCumdifSd,
                    counterfactual=".temp")
    df.residual <- GetInfo(tbr1.fit, "fit")[["df.residual"]]
    #pred <- tbr1.fit[IsInPeriod(tbr1.fit, periods="prediction"), ]
    q <- qt(probs, df=df.residual)
    expect_error(x <- quantile(tbr, probs=probs, distribution=dist),
                 regexp=NA)
    expect_equivalent(x[["50%"]], tbr[[location]])
    expect_equivalent(x[["90%"]], tbr[[location]] + tbr[[scale]] * q[[1]])
  }
})


context("quantile.TBRROASAnalysisFit")

roas.fit.fixed.cost <- DoTBRROASAnalysis(obj.ged,
                                         response="sales", cost=spend.change,
                                         model=kTBRModel1, n.sims=17L)
roas.fit.const.cost <- DoTBRROASAnalysis(obj.ged,
                                         response="sales", cost=kSpendChange,
                                         model=kTBRModel1, n.sims=17L)
incr.spend <- obj.ged[[kSpendChange]]
obj.ged[[kSpendChange]] <- abs(rnorm(nrow(obj.ged), mean=incr.spend,
                                     sd=1))
roas.fit.nonconst.cost <- DoTBRROASAnalysis(obj.ged,
                                            response="sales",
                                            cost=kSpendChange,
                                            model=kTBRModel1, n.sims=17)

test_that("resulting object is a TBRQuantiles object", {
  for (obj in list(roas.fit.fixed.cost, roas.fit.const.cost,
                   roas.fit.nonconst.cost)) {
    for (dist in c("pointwise", "cumulative")) {
      expect_error(x <- quantile(tbr1.fit, probs=c(0.9, 0.1, 0.5),
                                 distribution=dist),
                   regexp=NA)
      expect_is(x, "TBRQuantiles")
      expect_is(x[[kDate]], "Date")
      expect_is(x[[kPeriod]], "integer")
      # Quantile columns must be in the specified order.
      expect_equal(names(x), c(kDate, kPeriod, "90%", "10%", "50%"))
    }
  }
})

test_that("it is possible to specify just one 'probs'", {
  for (dist in c("pointwise", "cumulative")) {
    expect_error(x <- quantile(roas.fit.nonconst.cost, probs=0.5,
                               distribution=dist),
                 regexp=NA)
    expect_equal(names(x), c(kDate, kPeriod, "50%"))
  }
})

test_that("the default quantiles are 0.1, 0.5, and 0.9", {
  for (dist in c("pointwise", "cumulative")) {
    expect_error(x <- quantile(roas.fit.nonconst.cost, distribution=dist),
                 regexp=NA)
    expect_equal(names(x), c(kDate, kPeriod, "10%", "50%", "90%"))
  }
})

test_that("(const. cost) quantiles are those of the t-distribution", {
  # The quantiles are exact t-distribution quantiles derived from the scaled
  # quantiles of the posterior of the response variable.
  probs <- c(0.9, 0.5)
  for (obj in list(roas.fit.fixed.cost, roas.fit.const.cost)) {
    for (dist in c("pointwise", "cumulative")) {
      tbr.resp.fit <- GetInfo(obj, "tbr.resp")
      in.prediction <- IsInPeriod(tbr.resp.fit, periods="prediction")
      cost.vector <- switch(dist, pointwise=obj[["cost"]],
                            cumulative=cumsum(obj[["cost"]]))
      expect_error(m <- quantile(tbr.resp.fit, probs=probs, distribution=dist),
                   regexp=NA)
      expect_error(x <- quantile(obj, probs=probs, distribution=dist),
                   regexp=NA)
      # Division by zero should result in an NA.
      cost.vector[cost.vector == 0] <- NA_real_
      stopifnot(length(cost.vector) == sum(in.prediction))
      # Outside the prediction period, must have NA as iROAS is not defined
      # there.
      m[!in.prediction, 3:4] <- NA_real_
      m[in.prediction, 3:4] <- m[in.prediction, 3:4] / cost.vector
      expect_equivalent(x, m)
    }
  }
})

test_that("(nonconst. cost) quantiles are simulated", {
  probs <- c(0.9, 0.5)
  obj <- roas.fit.nonconst.cost
  tbr.resp <- GetInfo(obj, "tbr.resp")
  for (dist in c("pointwise", "cumulative")) {
    resp <- switch(dist, pointwise=obj[["response"]],
                   cumulative=cumsum(obj[["response"]]))
    cost <- switch(dist, pointwise=obj[["cost"]],
                   cumulative=cumsum(obj[["cost"]]))
    expect_error(x <- quantile(obj, probs=probs, distribution=dist),
                 regexp=NA)
    iroas <- resp / cost
    expect_error(q2 <- quantile(iroas, probs=probs),
                 regexp=NA)
    in.prediction <- IsInPeriod(tbr.resp, periods="prediction")
    m <- tbr.resp[, c(kDate, kPeriod)]
    m[, colnames(q2)] <- NA_real_
    m[in.prediction, colnames(q2)] <- q2
    expect_equivalent(x, m)
  }
})

context("as.matrix.TBRQuantiles")

test_that("resulting object is a matrix", {
  for (dist in c("pointwise", "cumulative")) {
    expect_error(x <- quantile(tbr1.fit, probs=c(0.1, 0.5, 0.9),
                               distribution=dist),
                 regexp=NA)
    expect_equivalent(as.matrix(x), as.matrix(as.data.frame(x[-(1:2)])))
  }
})
