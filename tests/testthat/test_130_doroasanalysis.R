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

context("DoROASAnalysis")

first.date <- as.Date("2016-09-26")
geo.names <- c("01", "02", "03", "04")
df.gts <- expand.grid(date=first.date + seq(from=0, to=7 * 7 - 1),
                      geo=geo.names)
df.gts[["sales"]] <- runif(nrow(df.gts), min=0,
                           max=100 * as.integer(df.gts[[kGeo]]))
obj.gts <- GeoTimeseries(df.gts, metrics="sales")
obj.per <- ExperimentPeriods(first.date + c(0L, 3 * 7, 5 * 7, 6 * 7 - 1),
                             period.names=c("Pretest", "Test", "Cooldown"))
obj.ga <- GeoAssignment(data.frame(geo=geo.names, geo.group=c(1, 2, 1, 2)))
obj.ged <- GeoExperimentData(obj.gts, period=obj.per, geo.assignment=obj.ga,
                             treat.assignment=DefaultTreatmentAssignment())
SetSpendChange(obj.ged, prop.to="sales") <- 1

test_that("GBR analysis is dispatched correctly", {
  expect_is(x <- DoROASAnalysis(obj.ged, model=kGBRModel1,
                                response="sales", cost=kSpendChange),
            "GBRROASAnalysisFitGbr1")
  gbr.fit <- DoGBRROASAnalysis(obj.ged, response="sales", cost=kSpendChange)
  expect_equivalent(x, gbr.fit)
})

test_that("TBR analysis is dispatched correctly", {
  set.seed(1)
  expect_is(x <- DoROASAnalysis(obj.ged, model=kTBRModel1,
                                response="sales", cost=kSpendChange, n.sims=12),
            "TBRROASAnalysisFit")
  set.seed(1)
  tbr.fit <- DoTBRROASAnalysis(obj.ged, model=kTBRModel1, response="sales",
                               cost=kSpendChange, n.sims=12)
  expect_equivalent(x, tbr.fit)
})

test_that("unknown model throws an error", {
  expect_error(DoROASAnalysis(obj.ged, model="f00"),
               regexp="Unknown model: 'f00'")
})
