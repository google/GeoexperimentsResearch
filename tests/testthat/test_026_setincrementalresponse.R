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

context("SetIncrementalResponse<-")

first.date <- as.Date("2016-09-26")
geo.names <- c("01", "02", "03", "04")
df.gts <- expand.grid(date=first.date + seq(from=0, to=7 * 7 - 1),
                      geo=geo.names)
df.gts[["sales"]] <- as.integer(df.gts[[kGeo]])
df.gts[["other"]] <- as.integer(df.gts[[kGeo]])^2
obj.gts <- GeoTimeseries(df.gts, metrics=c("sales", "other"))
periods <- ExperimentPeriods(first.date + c(0L, 3 * 7, 5 * 7, 6 * 7 - 1),
                             period.names=c("Pretest", "Test", "Cooldown"))
obj.ga <- GeoAssignment(data.frame(geo=geo.names, geo.group=c(1, 2, 1, 2)))
obj.ged <- GeoExperimentData(obj.gts, periods=periods, geo.assignment=obj.ga,
                             treat.assignment=DefaultTreatmentAssignment())
obj.ged.original <- obj.ged
SetSpendChange(obj.ged, prop.to="sales") <- 10


context("SetIncrementalResponse<-.GeoExperimentData")

test_that("NA resets the column", {
  SetIncrementalResponse(obj.ged) <- NA
  metrics <- GetInfo(obj.ged, "metrics")
  expect_true(kResponse %in% metrics)
  expect_true(all(is.na(obj.ged[[kResponse]])))
})

test_that("error occurs if kSpendChange is not a metric", {
  expect_error(SetIncrementalResponse(obj.ged.original, response="sales") <- 1,
               regexp=paste0("No metric '", kSpendChange, "' available"))
})

test_that("error occurs if the specified column is missing", {
  expect_error(SetIncrementalResponse(obj.ged, response="bad") <- 1,
               regexp="Specified response 'bad' is not a column")
})

test_that("error occurs if the specified column is not a metric", {
  obj.ged[["foo"]] <- NA
  expect_error(SetIncrementalResponse(obj.ged, response="foo") <- 1,
               regexp=paste0("Specified response 'foo' is not a metric"))
})

test_that("error occurs if the period is not available", {
  expect_error(SetIncrementalResponse(obj.ged,
                                      response="sales", periods=3:4) <- 1,
               regexp="Unknown periods: 3, 4")
})

test_that("ROAS == 0 implies kResponse == response if .spend is not NA", {
  expect_error(SetIncrementalResponse(obj.ged, response="sales") <- 0,
               regexp=NA)
  response <- obj.ged[[kResponse]]
  response[is.na(obj.ged[[kSpendChange]])] <- NA_real_
  expect_equal(obj.ged[[kResponse]], response)
})

test_that("ROAS != 0 implies kResponse = response + ROAS * spend", {
  expect_error(SetIncrementalResponse(obj.ged, response="sales") <- 2.0,
               regexp=NA)
  response <- (obj.ged[["sales"]] + 2.0 * obj.ged[[kSpendChange]])
  expect_equal(obj.ged[[kResponse]], response)
  # Negative ROAS is also allowed.
  expect_error(SetIncrementalResponse(obj.ged, response="sales") <- (-2.0),
               regexp=NA)
  response <- (obj.ged[["sales"]] - 2.0 * obj.ged[[kSpendChange]])
  expect_equal(obj.ged[[kResponse]], response)
})

test_that(".response can be chosen as the response", {
  a <- 2.0
  b <- 3.0
  response <- (obj.ged[["sales"]] + (a + b) * obj.ged[[kSpendChange]])
  expect_error(SetIncrementalResponse(obj.ged, response="sales") <- a,
               regexp=NA)
  expect_error(SetIncrementalResponse(obj.ged, response=kResponse) <- b,
               regexp=NA)
  expect_equal(obj.ged[[kResponse]], response)
})

new.trt.assignment <- TreatmentAssignment(data.frame(geo.group=c(2, 2),
                                                     period=c(1, 2),
                                                     assignment=c(1, 1)))
obj.ged <- GeoExperimentData(obj.gts, periods=periods, geo.assignment=obj.ga,
                             treat.assignment=new.trt.assignment)
SetSpendChange(obj.ged, prop.to="sales", periods=1) <- 1.0
SetSpendChange(obj.ged, prop.to="sales", periods=2) <- 2.0


test_that("chosen period is taken into account", {
  # Incremental change outside the period is zero.
  expect_error(SetIncrementalResponse(obj.ged,
                                      response="sales",
                                      periods=1) <- 2.0,
               regexp=NA)
  period.1 <- (obj.ged[[kPeriod]] %in% 1)
  response <- obj.ged[["sales"]]
  response[period.1] <- (response + 2 * obj.ged[[kSpendChange]])[period.1]
  expect_equal(obj.ged[[kResponse]], response)
  # Now change period 2.
  response <- obj.ged[[kResponse]]
  expect_error(SetIncrementalResponse(obj.ged,
                                      response=kResponse,
                                      periods=2) <- 3.0,
               regexp=NA)
  period.2 <- (obj.ged[[kPeriod]] %in% 2)
  response[period.2] <- (response +
                         3.0 * obj.ged[[kSpendChange]])[period.2]
  expect_equal(obj.ged[[kResponse]], response)
})
