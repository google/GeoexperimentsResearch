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

context("as.TBRPlotData")

data(salesandcost)
data(geoassignment)
obj.gts <- GeoTimeseries(salesandcost, metrics=c("sales", "cost"))
# Data starts from 2016-01-05: define one week of pre-experiment period.
obj.per <- ExperimentPeriods(c("2015-01-12", "2015-02-16", "2015-03-15"))
obj.ga <- GeoAssignment(geoassignment)
obj.ged <- GeoExperimentData(obj.gts, periods=obj.per, geo.assignment=obj.ga,
                             treat.assignment=DefaultTreatmentAssignment())
tbr <- DoTBRAnalysis(obj.ged, model=kTBRModel1, response="sales",
                     pretest.period=0, intervention.period=1,
                     cooldown.period=NULL, control.group=1, treatment.group=2)
q.default <- c("10%"=0.1, "90%"=0.9)
in.pretest <- IsInPeriod(tbr, periods="pretest")


context("as.TBRPlotData.TBRAnalysisFitTbr1")

test_that("object inherits from TBRPlotData", {
  expect_is(obj <- as.TBRPlotData(tbr),
            "TBRPlotData")
})

test_that("periods are correctly extracted for each panel", {
  for (panel in names(kTBRPlotPanels)) {
    for (period in names(kStandardPeriods)) {
      in.this.period <- IsInPeriod(tbr, periods=period)
      expect_error(obj <- as.TBRPlotData(tbr, panels=panel, periods=period),
                   regexp=NA)
      expect_equal(obj[[kDate]], tbr[[kDate]][in.this.period])
    }
  }
})

test_that("in preexperiment period all predictions are NA", {
  expect_error(obj <- as.TBRPlotData(tbr, periods="preexperiment"),
               regexp=NA)
  df.plot <- obj[c("lower", "predicted", "upper")]
  expect_true(all(is.na(df.plot)))
})

test.quantiles <- list(q.default,
                       c("2.5%"=0.025, "97.5%"=0.975),
                       c("10%"=0.1, "50%"=0.5))
# Test here also that the upper quantile can be 50%, same as the 'predicted'
# point estimate.
for (quantiles in test.quantiles) {
  for (period in names(kStandardPeriods)) {
    for (panel in names(kTBRPlotPanels)) {
      test_that(sprintf("[%s:%s] estimate and bounds match the quantiles",
                        panel, period),
                {
                  obj <- as.TBRPlotData(tbr,
                                        panels=panel,
                                        periods=period,
                                        quantiles=quantiles)
                  probs <- c(quantiles[1], "50%"=0.5, quantiles[2])
                  obj.q <- quantile(tbr, probs=probs,
                                    distribution=panel)
                  in.this.period <- IsInPeriod(obj.q, periods=period)
                  object <- obj[c("lower", "predicted", "upper")]
                  compare <- obj.q[in.this.period, names(probs)]
                  expect_equivalent(object, compare)
                })
    }
  }
}

context("as.TBRPlotData.TBRROASAnalysisFit")

tbr.roas <- DoTBRROASAnalysis(obj.ged, model=kTBRModel1, response="sales",
                              cost="cost",
                              pretest.period=0, intervention.period=1,
                              cooldown.period=NULL,
                              control.group=1, treatment.group=2)

test_that("object inherits from TBRPlotData", {
  expect_is(obj <- as.TBRPlotData(tbr.roas),
            "TBRPlotData")
})

test_that("periods are correctly extracted for each panel", {
  for (panel in names(kTBRROASPlotPanels)) {
    for (period in names(kStandardPeriods)) {
      expect_error(obj <- as.TBRPlotData(tbr.roas,
                                         panels=panel, periods=period),
                   regexp=NA)
      tbr.resp <- GetInfo(tbr.roas, "tbr.resp")
      in.this.period <- IsInPeriod(tbr.resp, periods=period)
      expect_equal(obj[[kDate]], tbr.resp[[kDate]][in.this.period])
    }
    expect_true(all(obj[[kPanelLabel]] %in%
                    kTBRROASPlotPanels[[panel]][[kPanelLabel]]))
  }
})

test_that("in preexperiment + pretest period all predictions are NA", {
  expect_error(obj <- as.TBRPlotData(tbr.roas,
                                     periods=c("preexperiment", "pretest")),
                                     regexp=NA)
  df.plot <- obj[c("lower", "predicted", "upper")]
  expect_true(all(is.na(df.plot)))
})

# Test here also that the upper quantile can be 50%, same as the 'predicted'
# point estimate.
for (quantiles in test.quantiles) {
  for (period in names(kStandardPeriods)) {
    for (panel in names(kTBRROASPlotPanels)) {
      test_that(sprintf("[%s:%s] estimate and bounds match the quantiles",
                        panel, period),
                {
                  obj <- as.TBRPlotData(tbr.roas,
                                        panels=panel,
                                        periods=period,
                                        quantiles=quantiles)
                  probs <- c(quantiles[1], "50%"=0.5, quantiles[2])
                  obj.q <- quantile(tbr.roas, probs=probs,
                                    distribution=panel)
                  in.this.period <- IsInPeriod(obj.q, periods=period)
                  object <- obj[c("lower", "predicted", "upper")]
                  compare <- obj.q[in.this.period, names(probs)]
                  expect_equivalent(object, compare)
                })
    }
  }
}
