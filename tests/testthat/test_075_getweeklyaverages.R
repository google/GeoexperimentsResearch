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

context("GetWeeklyAverages.GeoTimeseries")

first.date <- as.Date("2016-09-26")
df.gts <- expand.grid(date=first.date + c(0:13),
                      geo=c("01", "02", "03", "04"))
daily.avg.sales <- 1000
daily.avg.other <- 10
df.gts[["sales"]] <- (as.integer(df.gts[[kGeo]]) * daily.avg.sales)
df.gts[["other"]] <- (as.integer(df.gts[[kGeo]]) * daily.avg.other)
obj.gts <- GeoTimeseries(df.gts, metrics=c("sales", "other"))

test_that("output is a d.f. with weekly avg of sales, other for each geo", {
  expect_is(obj <- GetWeeklyAverages(obj.gts), "data.frame")
  expect_identical(obj[["sales"]],
                   7 * daily.avg.sales * as.integer(obj[[kGeo]]))
  expect_identical(obj[["other"]],
                   7 * daily.avg.other * as.integer(obj[[kGeo]]))
})

test_that("NAs are removed, also by default", {
  obj.gts[1, "sales"] <- NA
  expect_is(obj1 <- GetWeeklyAverages(obj.gts, na.rm=TRUE), "data.frame")
  expect_true(!anyNA(obj1[["sales"]]))
  expect_is(obj2 <- GetWeeklyAverages(obj.gts), "data.frame")
  expect_identical(obj1, obj2)
})

test_that("na.rm=FALSE gives a sum == NA", {
  geo1.gts <- (obj.gts[[kGeo]] %in% "01")
  obj.gts[which(geo1.gts)[1], "sales"] <- NA  # Insert a single NA.
  expect_is(obj <- GetWeeklyAverages(obj.gts, na.rm=FALSE), "data.frame")
  geo1 <- (obj[[kGeo]] %in% "01")
  expect_true(is.na(obj[["sales"]][geo1]))
  expect_true(!anyNA(obj[["sales"]][!geo1]))
})
