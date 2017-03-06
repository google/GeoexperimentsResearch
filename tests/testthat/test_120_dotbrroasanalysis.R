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

context("DoTBRROASAnalysis")

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
spend.change <- 1000
SetSpendChange(obj.ged, prop.to="sales") <- spend.change
obj.tbr <- as.TBRAnalysisData(obj.ged, response="sales")
tbr1.fit <- DoTBRAnalysisTbr1(obj.tbr)
in.intervention <- IsInPeriod(tbr1.fit, periods="intervention")
n.intervention <- sum(in.intervention)
in.prediction <- IsInPeriod(tbr1.fit, periods="prediction")
n.prediction <- sum(in.prediction)
n.sims <- 17
kLevel <- 0.9
first.posttest.date <- (obj.per[3, "End"] + 1L)

context("DoTBRROASAnalysis.GeoExperimentData")

test_that("[tbr1] ROAS is computed if cost specified as a real number", {
  cost <- 2
  expect_is(x <- DoTBRROASAnalysis(obj.ged, response="sales", cost=cost,
                                   model=kTBRModel1, n.sims=n.sims),
            "TBRROASAnalysisFit")
  info <- GetInfo(x)
  expect_identical(info[["response"]], "sales")
  expect_identical(info[["cost"]], cost)
  expect_identical(info[["model"]], kTBRModel1)
  expect_equal(info[["tbr.resp"]], tbr1.fit)
  expect_equal(dim(x[["response"]]), c(n.prediction, n.sims))
  cost.pred <- c(rep(cost / n.intervention, n.intervention),
                 rep(0, n.prediction - n.intervention))
  expect_equal(x[["cost"]], cost.pred)
})

test_that("[tbr1] ROAS is computed if cost is estimated to be constant", {
  cost <- 4.0
  SetSpendChange(obj.ged, prop.to="sales") <- cost
  expect_is(x <- DoTBRROASAnalysis(obj.ged, response="sales",
                                   cost=kSpendChange,
                                   model=kTBRModel1, n.sims=n.sims),
            "TBRROASAnalysisFit")
  info <- GetInfo(x)
  expect_identical(info[["cost"]], kSpendChange)
  cost.pred <- c(rep(cost / n.intervention, n.intervention),
                 rep(0, n.prediction - n.intervention))
  expect_equal(x[["cost"]], cost.pred)
})

test_that("zero cost is not allowed", {
  expect_error(x <- DoTBRROASAnalysis(obj.ged, response="sales", cost=0,
                                      model=kTBRModel1, n.sims=n.sims),
               regexp="'cost' must not be zero")
})

test_that("[tbr1] ROAS is computed if cost is not a constant", {
  SetSpendChange(obj.ged, prop.to="sales") <- 100
  incr.spend <- obj.ged[[kSpendChange]]
  obj.ged[[kSpendChange]] <- abs(rnorm(nrow(obj.ged), mean=incr.spend,
                                       sd=1))
  expect_error(x <- DoTBRROASAnalysis(obj.ged, response="sales",
                                      cost=kSpendChange,
                                      model=kTBRModel1, n.sims=n.sims),
               regexp=NA)
  info <- GetInfo(x)
  expect_identical(info[["cost"]], kSpendChange)
  expect_is(x[["cost"]], "PosteriorSimulations")
})


test_that("[tbr1] arguments are passed on to DoTBRAnalysis()", {
  tbr1 <- DoTBRAnalysis(obj.ged, response="sales",
                        control.group.id=2,
                        treatment.group.id=1,
                        pretest.period=0,
                        intervention.period=1,
                        cooldown.period=2,
                        model=kTBRModel1)
  expect_is(x <- DoTBRROASAnalysis(obj.ged, response="sales", cost=1,
                                   control.group.id=2,
                                   treatment.group.id=1,
                                   pretest.period=0,
                                   intervention.period=1,
                                   cooldown.period=2,
                                   model=kTBRModel1, n.sims=n.sims),
            "TBRROASAnalysisFit")
  info <- GetInfo(x)
  expect_equal(info[["tbr.resp"]], tbr1)
})

test_that("[tbr1] a warning is given if cost is not strictly >0 XOR <0", {
  set.seed(1)
  obj.ged[[kSpendChange]] <- rnorm(nrow(obj.ged))
  expect_warning(x <- DoTBRROASAnalysis(obj.ged, response="sales",
                                        cost=kSpendChange, model=kTBRModel1,
                                        n.sims=n.sims),
                 regexp="Unstable result: cost may be zero")
})

test_that("[tbr1] n.sims==1 and cost==const.: point estimates given", {
  expect_is(x <- DoTBRROASAnalysis(obj.ged, response="sales",
                                   cost=2, model=kTBRModel1, n.sims=1),
            "TBRROASAnalysisFit")
  info <- GetInfo(x)
  expect_equal(dim(x[["response"]]), c(n.prediction, 1))
  expect_equal(x[["response"]][, ],
               tbr1.fit[in.prediction, kYpredDif, drop=TRUE])
})

test_that("[tbr1] n.sims==1 & cost est. to be const.: point estimates given", {
  cost <- 4.0
  SetSpendChange(obj.ged, prop.to="sales") <- cost
  expect_is(x <- DoTBRROASAnalysis(obj.ged, response="sales",
                                   cost=kSpendChange,
                                   model=kTBRModel1, n.sims=1),
            "TBRROASAnalysisFit")
  info <- GetInfo(x)
  expect_equal(dim(x[["response"]]), c(n.prediction, 1))
  expect_equal(x[["response"]][, ],
               tbr1.fit[in.prediction, kYpredDif, drop=TRUE])
})

test_that("[tbr1] n.sims==1 & non-const cost: error is thrown", {
  SetSpendChange(obj.ged, prop.to="sales") <- 100
  incr.spend <- obj.ged[[kSpendChange]]
  obj.ged[[kSpendChange]] <- abs(rnorm(nrow(obj.ged), mean=incr.spend,
                                       sd=1))
  expect_error(DoTBRROASAnalysis(obj.ged, response="sales", cost=kSpendChange,
                                 model=kTBRModel1, n.sims=1),
               regexp="Cannot estimate iROAS when n.sims == 1")
})


test_that("[tbr1] no error if cost==constant & test==intervention only", {
  # Exclude cooldown and posttest periods.
  obj.ged2 <- obj.ged[obj.ged[[kPeriod]] %in% 0:1, ]
  expect_error(x <- DoTBRROASAnalysis(obj.ged2, response="sales", cost=1,
                                      control.group.id=2,
                                      treatment.group.id=1,
                                      pretest.period=0,
                                      intervention.period=1,
                                      cooldown.period=NULL,
                                      model=kTBRModel1, n.sims=n.sims),
               regexp=NA)
})


test_that("[tbr1] (cost=const) NA in posttest is carried over", {
  first.posttest.day <- which(obj.ged[[kDate]] == first.posttest.date)
  obj.ged[[kSpendChange]][first.posttest.day] <- NA_real_
  expect_error(x <- DoTBRROASAnalysis(obj.ged, response="sales",
                                      cost=kSpendChange,
                                      control.group.id=2,
                                      treatment.group.id=1,
                                      pretest.period=0,
                                      intervention.period=1,
                                      cooldown.period=2,
                                      model=kTBRModel1, n.sims=n.sims),
               regexp=NA)
  in.posttest <- IsInPeriod(x, periods="posttest")
  first.posttest.index <- which(in.posttest)[1]
  has.na.index <- which(is.na(x[["cost"]]))
  expect_equivalent(first.posttest.index, has.na.index)
})

test_that("[tbr1] (cost=const) NA in posttest response is carried over", {
  first.posttest.day <- which(obj.ged[[kDate]] == first.posttest.date)
  obj.ged[["sales"]][first.posttest.day] <- NA_real_
  expect_error(x <- DoTBRROASAnalysis(obj.ged, response="sales",
                                      cost=kSpendChange,
                                      control.group.id=2,
                                      treatment.group.id=1,
                                      pretest.period=0,
                                      intervention.period=1,
                                      cooldown.period=2,
                                      model=kTBRModel1, n.sims=n.sims),
               regexp=NA)
  in.posttest <- IsInPeriod(x, periods="posttest")
  first.posttest.index <- which(in.posttest)[1]
  has.na.index <- which(is.na(x[["response"]][, 1]))
  expect_equivalent(first.posttest.index, has.na.index)
})

test_that("[tbr1] (cost!=const) NA in posttest cost is carried over", {
  SetSpendChange(obj.ged, prop.to="sales") <- 100
  incr.spend <- obj.ged[[kSpendChange]]
  obj.ged[[kSpendChange]] <- abs(rnorm(nrow(obj.ged), mean=incr.spend,
                                       sd=1))
  first.posttest.day <- which(obj.ged[[kDate]] == first.posttest.date)
  obj.ged[[kSpendChange]][first.posttest.day] <- NA_real_
  expect_error(x <- DoTBRROASAnalysis(obj.ged, response="sales",
                                      cost=kSpendChange,
                                      control.group.id=2,
                                      treatment.group.id=1,
                                      pretest.period=0,
                                      intervention.period=1,
                                      cooldown.period=2,
                                      model=kTBRModel1, n.sims=n.sims),
               regexp=NA)
  in.posttest <- IsInPeriod(x, periods="posttest")
  first.posttest.index <- which(in.posttest)[1]
  has.na.index <- which(is.na(x[["cost"]][, 1]))
  expect_equivalent(first.posttest.index, has.na.index)
})


context("[tbr1] summary.TBRROASAnalysisFit: constant cost")

spend.change <- 2

obj <- DoTBRROASAnalysis(obj.ged, response="sales",
                         cost=spend.change,
                         model=kTBRModel1, n.sims=1)

test_that("an object of proper class is returned", {
  expect_is(summary(obj), "ROASAnalysisResults")
})

test_that("the default interval is 0.9 one-sided", {
  s1 <- summary(obj)
  s2 <- summary(obj, level=0.9, interval.type="one-sided")
  expect_identical(s1, s2)
  expect_identical(s1[["upper"]], Inf)
  expect_equal(s1[["level"]], 0.9)
})

test_that("point estimates are computed from the response TBR analysis", {
  sroas <- summary(obj, level=kLevel, interval.type="one-sided")
  sresp <- summary(GetInfo(obj, "tbr.resp"), level=kLevel,
                   interval.type="one-sided")
  expect_identical(sroas[["estimate"]],
                   sresp[["estimate"]] / spend.change)
  expect_identical(sroas[["lower"]],
                   sresp[["lower"]] / spend.change)
  expect_equal(sroas[["precision"]],
                   (sresp[["estimate"]] - sresp[["lower"]]) / spend.change)
  expect_identical(sroas[["upper"]], Inf)

})

test_that("incremental response matches", {
  sroas <- summary(obj)
  sresp <- summary(GetInfo(obj, "tbr.resp"), level=kLevel,
                   interval.type="one-sided")
  expect_equivalent(sroas[["incr.resp"]], sresp[["estimate"]])
})

test_that("level, model match", {
  sroas <- summary(obj)
  expect_identical(sroas[["level"]], kLevel)
  expect_identical(sroas[["model"]], kTBRModel1)
})

test_that("posterior tail probability is 0.5 at the estimate", {
  estimate <- summary(obj)[["estimate"]]
  expect_equal(0.5, summary(obj, threshold=estimate)[["prob"]])
})

test_that("[tbr1] Pr(iROAS > lower bound of one-sided x% CI) = x.", {
  for (one.sided.level in c(0.975, 0.95, 0.9, 0.8)) {
    lower.bound <- summary(obj, level=one.sided.level,
                           interval.type="one-sided")[["lower"]]
    expect_equal(one.sided.level,
                 summary(obj, threshold=lower.bound)[["prob"]])
  }
})

test_that("[tbr1] Pr(iROAS > upper bound of two-sided x% CI) = (1-x)/2.", {
  for (two.sided.level in c(0.95, 0.9, 0.85, 0.8)) {
    upper.bound <- summary(obj, level=two.sided.level,
                           interval.type="two-sided")[["upper"]]
    expect_equal(0.5 * (1 - two.sided.level),
                 summary(obj, threshold=upper.bound)[["prob"]])
  }
})


context("summary.TBRROASAnalysisFit: non-constant cost")

incr.spend <- obj.ged[[kSpendChange]]
incr.spend[which(is.na(incr.spend))] <- 0
obj.ged[[kSpendChange]] <- abs(rnorm(nrow(obj.ged), mean=incr.spend,
                                     sd=1))
obj <- DoTBRROASAnalysis(obj.ged, response="sales", cost=kSpendChange,
                         model=kTBRModel1, n.sims=100)

test_that("an object of proper class is returned", {
  expect_is(summary(obj), "ROASAnalysisResults")
})

test_that("point estimates are computed from quantiles of the simulations", {
  # One-sided interval.
  iroas <- (cumsum(obj[["response"]]) / cumsum(obj[["cost"]]))
  sroas <- summary(obj, level=0.9, interval.type="one-sided")
  qobj <- quantile(iroas, c(0.5, 0.1))
  in.analysis <- IsInPeriod(obj, periods="analysis")
  end.of.analysis <- tail(which(in.analysis), 1)
  total <- qobj[end.of.analysis, , drop=TRUE]
  expect_equivalent(sroas[["estimate"]], total["50%"])
  expect_equivalent(sroas[["lower"]], total["10%"])
  expect_equal(sroas[["upper"]], Inf)
  # Two-sided interval.
  sroas <- summary(obj, level=0.9, interval.type="two-sided")
  qobj <- quantile(iroas, c(0.5, 0.05, 0.95))
  total <- qobj[end.of.analysis, , drop=TRUE]
  expect_equivalent(sroas[["estimate"]], total["50%"])
  expect_equivalent(sroas[["lower"]], total["5%"])
  expect_equivalent(sroas[["upper"]], total["95%"])
})

test_that("incremental response matches", {
  sroas <- summary(obj)
  sresp <- summary(GetInfo(obj, "tbr.resp"), level=kLevel,
                   interval.type="one-sided")
  expect_equivalent(sroas[["incr.resp"]], sresp[["estimate"]])
})

test_that("level, model match", {
  sroas <- summary(obj)
  expect_identical(sroas[["level"]], kLevel)
  expect_identical(sroas[["model"]], kTBRModel1)
})

test_that("posterior tail probability is 0.5 at the estimate", {
  estimate <- summary(obj)[["estimate"]]
  expect_equal(0.5, summary(obj, threshold=estimate)[["prob"]])
})

test_that("[tbr1] tail probabilities are computed from the simulations", {
  in.analysis <- IsInPeriod(obj, periods="analysis")
  end.of.analysis <- tail(which(in.analysis), 1)
  iroas <- (cumsum(obj[["response"]]) / cumsum(obj[["cost"]]))
  m <- iroas[end.of.analysis, , drop=TRUE]
  for (threshold in c(0, 0.25, 0.5, 1)) {
    prob <- summary(obj, threshold=threshold)[["prob"]]
    expect_equivalent(prob, mean(m > threshold))
  }
})
