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

context("ExperimentPeriods")

period.lengths <- c(35L, 28L, 7L)
period.dates <- as.Date(kDateFrom) +
    cumsum(c(0, period.lengths[-3], period.lengths[3] - 1))
period.numbers <- (seq_along(period.lengths) - 1L)

test_that("Default arguments produce a proper ExperimentPeriods object", {
  expect_is(obj <- ExperimentPeriods(period.dates), "ExperimentPeriods")
  expect_is(obj, "data.frame")
  pn <- seq_along(period.lengths)
  period.numbers <- (pn - 1L)
  expect_identical(obj[["Period"]], period.numbers)
  start.dates <- period.dates[pn]
  end.dates <- (period.dates[-1L][pn] - 1L)
  end.dates[length(end.dates)] <- end.dates[length(end.dates)] + 1L
  expect_identical(obj[["Start"]], start.dates)
  expect_identical(obj[["End"]], end.dates)
  expect_identical(obj[["Length"]], period.lengths)
  period.names <- c("Pretest", paste0("Test", period.numbers[-1L]))
  expect_identical(obj[["Name"]], period.names)
})

test_that("character-valued period dates are accepted", {
  expect_is(obj1 <- ExperimentPeriods(as.character(period.dates)),
                                      "ExperimentPeriods")
  expect_is(obj2 <- ExperimentPeriods(period.dates), "ExperimentPeriods")
  expect_identical(obj1, obj2)
})

test_that("character-valued period dates converted properly", {
  fmt <- "%m/%d/%Y"
  new.dates <- as.character(strftime(period.dates, format=fmt))
  expect_is(obj1 <- ExperimentPeriods(new.dates, date.format=fmt),
            "ExperimentPeriods")
  expect_is(obj2 <- ExperimentPeriods(period.dates), "ExperimentPeriods")
  expect_identical(obj1, obj2)
})

test_that("period names pass through", {
  period.names <- c("PRETEST", "TEST1", "TEST2")
  obj <- ExperimentPeriods(period.dates, period.names=period.names)
  expect_identical(obj[["Name"]], period.names)
})

test_that("period.dates is mandatory", {
  expect_error(ExperimentPeriods(),
               regexp='argument "period.dates" is missing, with no default')
})

test_that("bad period dates are rejected", {
  bad.dates <- as.numeric(period.dates)
  expect_error(ExperimentPeriods(bad.dates),
               regexp=paste0("period.dates is not a character vector ",
                   "or period.dates is not a Date object"))
})

test_that("non-consecutive period.dates are rejected", {
  expect_error(ExperimentPeriods(rev(period.dates)),
               regexp="Experiment dates must be consecutive")
})

test_that("periods with zero length are rejected", {
  period.dates[2L] <- period.dates[1L]
  expect_error(ExperimentPeriods(period.dates),
               regexp="Experiment dates must be at least of length 1")
})


context("ExtractExperimentPeriods.GeoTimeseries")

obj.gts <- GeoTimeseries(dfged1, metrics=metric.df1)

test_that(sprintf("Error is returned by default if '%s' does not exist",
                  kPeriod), {
  obj.gts[[kPeriod]] <- NULL
  expect_error(ExtractExperimentPeriods(obj.gts),
               regexp=paste(sprintf("The required column '%s'", kPeriod),
                            "is not in the data frame"))
})

test_that(sprintf("strict=FALSE => NULL is returned if '%s' does not exist",
                  kPeriod), {
  obj.gts[[kPeriod]] <- NULL
  expect_true(is.null(ExtractExperimentPeriods(obj.gts, strict=FALSE)))
})

test_that("the periods match to those in the data frame", {
  x <- as.data.frame(obj.gts)
  start.pre <- min(x[[kDate]][x[[kPeriod]] %in% 0])
  start.test1 <- min(x[[kDate]][x[[kPeriod]] %in% 1])
  start.test2 <- min(x[[kDate]][x[[kPeriod]] %in% 2])
  end.test2 <- max(x[[kDate]][x[[kPeriod]] %in% 2])
  expect_is(obj <- ExtractExperimentPeriods(obj.gts), "ExperimentPeriods")
  expect_identical(obj[["Period"]], 0:2)
  expect_identical(obj[["Start"]],
                   c(start.pre, start.test1, start.test2))
  expect_identical(obj[["End"]],
                   c(start.test1 - 1L, start.test2 - 1L, end.test2))
})
