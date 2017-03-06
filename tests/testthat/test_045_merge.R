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

context("merge.GeoTimeseries")

x <- GeoTimeseries(dfged3, metrics="sales")
y <- GeoTimeseries(dfged2, metrics=c("sales", "conversions"))

dfged4 <- dfged3
colnames(dfged4)[3] <- "foo"
w <- GeoTimeseries(dfged4, metrics="foo")

test_that(paste("an error is thrown when the second object is not a",
                "GeoTimeseries"), {
  to.test <- list(0, "a", dfged2)
  for (bad.y in to.test) {
    expect_error(z <- merge(x, bad.y),
                 regexp=paste("Both objects must inherit from",
                              "class GeoTimeseries"))
  }
})

test_that("the object created is a GeoTimeseries", {
  obj.gts.1 <- GeoTimeseries(dfged3[, c(kDate, kGeo, "sales")], metrics="sales")
  obj.gts.2 <- GeoTimeseries(dfged4[, c(kDate, kGeo, "foo")], metrics="foo")
  expect_warning(z <- merge(obj.gts.1, obj.gts.2), regexp=NA)
  expect_true(inherits(z, "GeoTimeseries"))
})

test_that(paste("a warning is shown when both objects share some common",
                "column names"), {
  x <- GeoTimeseries(dfged3[, c(kDate, kGeo, "sales")],
                     metrics="sales")
  y <- GeoTimeseries(dfged2[, c(kDate, kGeo, "conversions")],
                     metrics="conversions")
  expect_warning(z <- merge(x, y), regexp=NA)

  y <- GeoTimeseries(dfged2[, c(kDate, kGeo, "sales", "conversions")],
                     metrics=c("sales", "conversions"))
  expect_warning(z <- merge(x, y),
                 regexp="Column 'sales' is shared by both GeoTimeseries")

  x <- GeoTimeseries(dfged3[, c(kDate, kGeo, kGeoGroup, "sales")],
                     metrics="sales")
  y <- GeoTimeseries(dfged2[, c(kDate, kGeo, kGeoGroup,
                                "sales", "conversions")],
                     metrics=c("sales", "conversions"))
  expect_warning(merge(x, y),
                 regexp=paste(sprintf("Columns 'sales', '%s'", kGeoGroup),
                              "are shared by both GeoTimeseries"))
})

test_that("the columns in the resulting merged object are unique", {
  expect_warning(z <- merge(x, y),
                 regexp=paste("Columns 'sales', 'geo.group', 'period',",
                              "'assignment' are shared by both GeoTimeseries"))
  expect_false(any(duplicated(names(z))))
})

test_that("columns in y already present in x are ignored", {
  expect_warning(z <- merge(x, y),
                 regexp=paste("Columns 'sales', 'geo.group', 'period',",
                              "'assignment' are shared by both GeoTimeseries"))
  expect_equal(z[["sales"]], x[["sales"]])
})

test_that("the result has the same number of rows as x", {
  expect_warning(z <- merge(x, y),
                 regexp=paste("Columns 'sales', 'geo.group', 'period',",
                              "'assignment' are shared by both GeoTimeseries"))
  expect_equal(nrow(z), nrow(x))
})

test_that("the result inherits the metrics correctly", {
  expect_warning(z <- merge(x, y),
                 regexp=paste("Columns 'sales', 'geo.group', 'period',",
                              "'assignment' are shared by both GeoTimeseries"))
  expect_true(setequal(GetInfo(z, "metrics"), c("sales", "conversions")))
})


context("merge.GeoExperimentData")

x <- as.GeoExperimentData(GeoTimeseries(dfged2, metrics=metric.df1))
y <- as.GeoExperimentData(GeoTimeseries(dfged3, metrics=metric.df1))
w <- as.GeoExperimentData(GeoTimeseries(dfged4, metrics="foo"))

test_that(paste("an error is thrown when the second object is not a",
                "GeoExperimentData"), {
  to.test <- list(0, "a", dfged2)
  for (bad.y in to.test) {
    expect_error(merge(x, bad.y),
                 regexp=paste("Both objects must inherit from",
                              "class GeoExperimentData"))
  }
})

test_that("the object created is a GeoExperimentData", {
  expect_error(z <- merge(w, x), regexp=NA)
  expect_true(inherits(z, "GeoExperimentData"))
})


test_that(paste("a warning is shown when both objects share some common",
                "column names"), {
  dfged4 <- dfged2
  dfged4[["sales"]] <- NULL
  x <- as.GeoExperimentData(GeoTimeseries(dfged4, metrics="conversions"))
  y <- as.GeoExperimentData(GeoTimeseries(dfged3, metrics="sales"))
  expect_warning(merge(x, y), regexp=NA)

  x <- as.GeoExperimentData(GeoTimeseries(dfged2, metrics="sales"))
  y <- as.GeoExperimentData(GeoTimeseries(dfged3, metrics="sales"))
  expect_warning(merge(x, y),
                 regexp="Column 'sales' is shared by both GeoExperimentData")

  x <- as.GeoExperimentData(GeoTimeseries(dfged2, metrics="sales"))
  y <- as.GeoExperimentData(GeoTimeseries(dfged2, metrics="sales"))
  y[["sales"]] <- runif(nrow(y))
  y[["conversions"]] <- runif(nrow(y))
  y[["other.column"]] <- NULL
  y[["another"]] <- NULL
  expect_warning(merge(x, y),
                 regexp=paste("Columns 'sales', 'conversions'",
                              "are shared by both GeoExperimentData"))
})

test_that("the columns in the resulting merged object are unique", {
  expect_warning(z <- merge(x, y),
                 regexp="Column 'sales' is shared by both GeoExperimentData")
  expect_false(any(duplicated(names(z))))
})

test_that("columns in y already present in x are ignored", {
  expect_warning(z <- merge(x, y),
                 regexp="Column 'sales' is shared by both GeoExperimentData")
  expect_equal(z[["sales"]], x[["sales"]])
})

test_that("the result has the same number of rows as x", {
  expect_warning(z <- merge(x, y),
                 regexp="Column 'sales' is shared by both GeoExperimentData")
  expect_equal(nrow(z), nrow(x))
})

test_that("the result inherits the metrics correctly", {
  dfged4 <- dfged2
  dfged4[["sales"]] <- NULL
  x <- as.GeoExperimentData(GeoTimeseries(dfged4, metrics="conversions"))
  expect_error(z <- merge(x, y), regexp=NA)
  expect_true(setequal(GetInfo(z, "metrics"), c("sales", "conversions")))
})
