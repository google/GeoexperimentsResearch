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

context("SetSpendChange<-")
context("SetSpendChange<-.GeoExperimentData")

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


test_that("an error occurs without any treatment assignment", {
  obj.ged <- GeoExperimentData(obj.gts, periods=periods, geo.assignment=obj.ga,
                               treat.assignment=NULL)
  expect_error(SetSpendChange(obj.ged, prop.to="sales") <- 1.0,
               regexp=paste0("'", kAssignment, "' column must not be all NAs"))
  # Treatment assignment is unknown also if there is no geo assignment.
  obj.ged <- GeoExperimentData(obj.gts, periods=periods, geo.assignment=NULL,
                               treat.assignment=DefaultTreatmentAssignment())
  expect_error(SetSpendChange(obj.ged, prop.to="sales") <- 1.0,
               regexp=paste0("'", kAssignment, "' column must not be all NAs"))
  # Treatment assignment is unknown also if there is no period information.
  obj.ged <- GeoExperimentData(obj.gts, periods=NULL, geo.assignment=obj.ga,
                               treat.assignment=DefaultTreatmentAssignment())
  expect_error(SetSpendChange(obj.ged, prop.to="sales") <- 1.0,
               regexp=paste0("'", kAssignment, "' column must not be all NAs"))
})


test_that("an error does not occur if 'assignment' has no NAs", {
  date.range <- c(periods[1, "Start"], periods[3, "End"])
  gts <- obj.gts[obj.gts[[kDate]] >= date.range[1] &
                 obj.gts[[kDate]] <= date.range[2], , drop=FALSE]
  obj.ged <- GeoExperimentData(gts, periods=periods, geo.assignment=obj.ga,
                               treat.assignment=DefaultTreatmentAssignment())
  expect_true(!anyNA(obj.ged[[kAssignment]]))
  expect_error(SetSpendChange(obj.ged, prop.to="sales") <- 1.0, regexp=NA)
})


obj.ged <- GeoExperimentData(obj.gts, periods=periods, geo.assignment=obj.ga,
                             treat.assignment=DefaultTreatmentAssignment())


test_that(paste(sprintf("column '%s'", kSpendChange),
                "is inserted into the object and is a metric"), {
  SetSpendChange(obj.ged, prop.to="sales") <- 2.0
  expect_true(kSpendChange %in% names(obj.ged))
  expect_true(kSpendChange %in% GetInfo(obj.ged, "metrics"))
})

test_that("the total spend change matches", {
  SetSpendChange(obj.ged, prop.to="sales") <- 2.0
  expect_equal(2.0, sum(obj.ged[[kSpendChange]], na.rm=TRUE))
})

test_that("zero spend change is not allowed", {
  expect_error(SetSpendChange(obj.ged, prop.to="sales") <- 0,
               regexp="Zero spend change is not allowed")
})

test_that("negative spend change is not allowed", {
  expect_error(SetSpendChange(obj.ged, prop.to="sales") <- (-2.0),
               regexp="Negative spend change is not allowed")
})

test_that("nonexistent periods are not allowed", {
  expect_error(SetSpendChange(obj.ged, prop.to="sales", periods=3) <- 1,
               regexp="Unknown periods: 3")
  expect_error(SetSpendChange(obj.ged, prop.to="sales", periods=3:4) <- 1,
               regexp="Unknown periods: 3, 4")
})

test_that("error occurs if there is no period with spend change assignment", {
  expect_error(SetSpendChange(obj.ged,
                              prop.to="sales", periods=2) <- 1.0,
               regexp="There was no spend change during the given period")
})

test_that("spend change is reset to 0 for all periods with no spend change", {
  SetSpendChange(obj.ged, prop.to="sales", periods=1) <- 2.5
  # Aggregate the spend for each period. The DefaultTreatmentAssignment applies
  # all spend change to period 1 only. Others remain at 0.
  spend <- aggregate(obj.ged, by=kPeriod)[[kSpendChange]]
  expect_identical(spend, c(0, 2.5, 0))
})

test_that("spend change must have no NAs", {
  SetSpendChange(obj.ged, prop.to="sales", periods=1) <- 2.5
  expect_false(anyNA(obj.ged[[kSpendChange]]))
})

test_that("spend change is reset to 0 for the undefined period", {
  SetSpendChange(obj.ged, prop.to="sales", periods=1) <- 2.5
  obj.ged[[kPeriod]][which(is.na(obj.ged[[kPeriod]]))] <- 9L  # Dummy period.
  spend <- aggregate(obj.ged, by=kPeriod, FUN=sum, na.rm=FALSE)[[kSpendChange]]
  expect_identical(spend, c(0, 2.5, 0, 0))
})

test_that("spend chg is distributed in proportion to the specified column", {
  SetSpendChange(obj.ged, prop.to="sales") <- 1.0
  a <- aggregate(obj.ged, by=c(kGeo, kGeoGroup, kPeriod, kAssignment))
  change <- (a[[kAssignment]] == kTreatmentAssignment["change"])
  prop <- a[["sales"]][change]
  spend <- a[[kSpendChange]]
  expect_equal(spend[change], prop / sum(prop))
  expect_equal(0, sum(spend[!change]))
})

test_that("changing the proportionality works", {
  SetSpendChange(obj.ged, prop.to="other") <- 1.0
  a <- aggregate(obj.ged, by=c(kGeo, kGeoGroup, kPeriod, kAssignment))
  change <- (a[[kAssignment]] == kTreatmentAssignment["change"])
  prop <- a[["other"]][change]
  spend <- a[[kSpendChange]]
  expect_equal(spend[change], prop / sum(prop))
  expect_equal(0, sum(spend[!change]))
})


test_that("decreasing spend change is taken into account", {
  df.trt <- data.frame(geo.group=2,
                       period=1,
                       assignment=kTreatmentAssignment["decrease"])
  trt.assign <- TreatmentAssignment(df.trt)
  SetTreatmentAssignment(obj.ged) <- trt.assign
  abs.change <- 2.5
  SetSpendChange(obj.ged, prop.to="sales") <- abs.change
  a <- aggregate(obj.ged, by=c(kGeo, kGeoGroup, kPeriod, kAssignment))
  change <- (a[[kAssignment]] == kTreatmentAssignment["decrease"])
  prop <- a[["sales"]][change]
  spend <- a[[kSpendChange]]
  expect_equal(spend[change], -abs.change * prop / sum(prop))
  expect_equal(0, sum(spend[!change]))
})

test_that("NA resets the column to NA_real_", {
  obj.ged[[kSpendChange]] <- 0.0
  SetSpendChange(obj.ged) <- NA
  spend <- obj.ged[[kSpendChange]]
  expect_true(is.numeric(spend) && all(is.na(spend)))
})
