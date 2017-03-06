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

context("Geos")

geos.df <- data.frame(geo=c("A", "B", "C", "D"), sales=1:4, other=4:1)

test_that("the resulting object is of class Geos", {
  expect_is(obj <- Geos(geos.df, volume="sales"), "Geos")
})

test_that(sprintf("'%s' must be of type character or factor", kGeo), {
  geos.df[[kGeo]] <- seq_len(nrow(geos.df))
  expect_error(Geos(geos.df, volume="sales"),
               regexp=sprintf("Column '%s' is not a character vector", kGeo))
  geos.df[[kGeo]] <- as.factor(geos.df[[kGeo]])
  expect_is(Geos(geos.df, volume="sales")[[kGeo]], "character")
  geos.df[[kGeo]] <- as.character(geos.df[[kGeo]])
  expect_is(Geos(geos.df, volume="sales")[[kGeo]], "character")
})

test_that(sprintf("'%s' must not have NAs", kGeo), {
  geos.df[[kGeo]][1] <- NA
  expect_error(Geos(geos.df, volume="sales"),
               regexp=sprintf("Column '%s' has one missing value in row 1",
                              kGeo))
})

test_that("Other columns must not have NAs", {
  geos.df[["sales"]][2] <- NA
  expect_error(Geos(geos.df, volume="sales"),
               regexp="Column 'sales' has one missing value in row 2")
})

obj <- Geos(geos.df, volume="sales")

test_that("volume and proportion is correctly computed", {
  expect_equal(obj[[kVolume]], obj[["sales"]])
  expect_equal(obj[[kProportion]], obj[[kVolume]] / sum(obj[[kVolume]]))
})

test_that("if 'volume' is not specified, it will be set to '1'", {
  obj <- Geos(geos.df)
  expect_equal(obj[[kVolume]], rep(1, length.out=nrow(obj)))
  expect_equal(obj[[kProportion]], obj[[kVolume]] / sum(obj[[kVolume]]))
})

bad.sales.geos.df <- geos.df
bad.sales.geos.df[["sales"]][1] <- -1
test_that("negative values within the volume column are not allowed", {
  expect_error(Geos(bad.sales.geos.df, volume="sales"),
               regexp="Column 'sales' has one negative value in row 1")
})
test_that("negative values outside the volume column are allowed.", {
  expect_error(Geos(bad.sales.geos.df, volume="other"), regexp=NA)
})
test_that("negative values are allowed when 'volume' is not specified", {
  expect_error(Geos(bad.sales.geos.df), regexp=NA)
})

test_that("a zero volume is allowed, if volume is not all zero", {
  bad.sales.geos.df[["sales"]][1] <- 0
  expect_error(Geos(bad.sales.geos.df, volume="sales"), regexp=NA)
})
test_that("all zero volume is not allowed, however", {
  bad.sales.geos.df[["sales"]][1:4] <- rep(0, 4)
  expect_error(Geos(bad.sales.geos.df, volume="sales"),
               regexp=paste("To be defined as a volume, column 'sales'",
                            "needs to sum up to a positive value"))
})


test_that("other columns are included as well", {
  expect_true("other" %in% names(obj))
})

test_that("duplicate geos are not allowed", {
  geos.df.bad <- rbind(geos.df,
                       data.frame(geo="A", sales=100, other=100))
  expect_error(Geos(geos.df.bad, volume="sales"))
})


context("ExtractGeos.GeoTimeseries")

first.date <- as.Date("2016-09-26")
df.gts <- expand.grid(date=first.date + c(0:13),
                      geo=c("01", "02", "03", "04"))
daily.avg.sales <- 1000
daily.avg.other <- 10
df.gts[["sales"]] <- (as.integer(df.gts[[kGeo]]) * daily.avg.sales)
df.gts[["other"]] <- (as.integer(df.gts[[kGeo]]) * daily.avg.other)
obj.gts <- GeoTimeseries(df.gts, metrics=c("sales", "other"))

test_that("result is an object of class Geos", {
  expect_is(obj <- ExtractGeos(obj.gts), "Geos")
})

test_that("the metric columns equal the corresponding weekly average sales", {
  expect_is(obj <- ExtractGeos(obj.gts), "Geos")
  expect_identical(obj[["sales"]],
                   7 * daily.avg.sales * as.integer(obj[[kGeo]]))
  expect_identical(obj[["other"]],
                   7 * daily.avg.other * as.integer(obj[[kGeo]]))
})

bad.sales <- obj.gts
bad.sales[["sales"]][1] <- -1
test_that("negative values within the volume column are not allowed.", {
  expect_error(ExtractGeos(bad.sales, volume="sales"),
               regexp="Column 'sales' has one negative value in row 1")
})
test_that("negative values outside the volume column are allowed.", {
  expect_error(ExtractGeos(bad.sales, volume="other"), regexp=NA)
})
