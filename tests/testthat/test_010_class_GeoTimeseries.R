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

context("GeoTimeseries")

test_that("GeoTimeseries() returns a such an object", {
  obj <- GeoTimeseries(df1, metrics=metric.df1)
  expect_is(obj, "GeoTimeseries")
  expect_is(obj, "data.frame")
  expect_is(obj[[kDate]], "Date")
  expect_true(is.character(obj[[kGeo]]))
})

test_that("object and the input data frame have the same structure", {
  expect_is(obj <- GeoTimeseries(df1, metrics=metric.df1),
            "GeoTimeseries")
  expect_identical(as.list(df1), as.list(obj)[names(df1)])
})

test_that("object is coerced to data.frame", {
  class(df1) <- c("Foo", class(df1))
  expect_is(obj <- GeoTimeseries(df1, metrics=metric.df1), "GeoTimeseries")
  expect_equal(class(obj), c("GeoTimeseries", "data.frame"))
})

test_that("there are no duplicate class definitions", {
  class(df1) <- c("GeoTimeseries", class(df1))
  expect_error(obj <- GeoTimeseries(df1, metrics=metric.df1), regexp=NA)
  expect_equal(class(obj), class(df1))
})

test_that("GeoTimeseries has columns .weekday, .weeknum, .weekindex", {
  obj <- GeoTimeseries(df1, metrics=metric.df1)
  expect_true(all(c(".weekday", ".weeknum", ".weekindex") %in%
                  names(obj)))
  dfw <- obj[c(".weekday", ".weeknum", ".weekindex")]
  expect_true(all(sapply(dfw, is.integer)))
  expect_true(!anyNA(dfw))
  expect_true(all(dfw[[".weekday"]] %in% 1:7))
  expect_true(all(dfw[[".weeknum"]] %in% 0:53))
  all.possible.weekindex <- seq(from=1L, to=max(dfw[[".weekindex"]]))
  expect_true(all(dfw[[".weekindex"]] %in% all.possible.weekindex))
})

test_that("non-data frame objects are rejected", {
  class(df1) <- "foo"
  expect_error(GeoTimeseries(df1, metrics=metric.df1),
               regexp="'x' must be a data\\.frame")
})

test_that("objects coercible to data.frames are allowed", {
  class(df1) <- c("foo", class(df1))
  obj <- GeoTimeseries(df1, metrics=metric.df1)
})

test_that("columns with names starting with a dot are removed", {
  df1[[".foo"]] <- 0
  expect_is(obj <- GeoTimeseries(df1, metrics=metric.df1), "GeoTimeseries")
  expect_false(".foo" %in% names(obj))
})

test_that("at least one Metric is required", {
  expect_error(GeoTimeseries(df1),
               regexp="At least one metric must be specified")
  expect_error(GeoTimeseries(df1, metric=character(0)),
               regexp="At least one metric must be specified")
  for (metric in list("", NA_character_, c("sales", NA_character_))) {
    expect_error(GeoTimeseries(df1, metric=metric),
                 regexp=paste0("'metrics' must consist of non-empty strings ",
                     "with no missing values"))
  }
})

for (column in c(kDate, kGeo)) {
  test_that(paste0("a nonexisting '", column, "' is detected"), {
    df1[[column]] <- NULL
    expect_error(GeoTimeseries(df1, metrics=metric.df1),
                 regexp=paste0("The required column '", column, "' is not ",
                     "in the data frame"))
  })
}

test_that("several nonexisting columns are detected simultaneously", {
  df1[[kDate]] <- NULL
  df1[[kGeo]] <- NULL
  expect_error(GeoTimeseries(df1, metrics="foo"),
               regexp=paste0("The required columns ",
                   sprintf("'%s', '%s' are not in the data frame",
                           kDate, kGeo)))
})

test_that(sprintf("a '%s' that is not 'Date' nor character/factor is detected",
                  kDate), {
  df1[[kDate]] <- as.integer(df1[[kDate]])
  expect_error(GeoTimeseries(df1, metrics=metric.df1),
               regexp=sprintf("Column '%s' is not a Date object", kDate))
})

test_that(sprintf("an integer-valued '%s' is silently accepted", kGeo), {
  df1[[kGeo]] <- as.integer(df1[[kGeo]])
  expect_is(obj <- GeoTimeseries(df1, metrics=metric.df1), "GeoTimeseries")
  expect_identical(obj[[kGeo]], as.character(df1[[kGeo]]))
})

test_that(sprintf("a factor-valued '%s' is silently accepted", kGeo), {
  df1[[kGeo]] <- as.factor(df1[[kGeo]])
  expect_is(obj <- GeoTimeseries(df1, metrics=metric.df1), "GeoTimeseries")
  expect_identical(obj[[kGeo]], as.character(df1[[kGeo]]))
})

test_that(paste(sprintf("a '%s'", kGeo),
                "that is not character, factor, or integer is detected"), {
  df1[[kGeo]] <- as.integer(df1[[kGeo]]) + 0.1
  expect_error(GeoTimeseries(df1, metrics=metric.df1),
               regexp=sprintf("Column '%s' is not a character vector", kGeo))
})

test_that("character-formatted YYYY-MM-DD date is allowed by default", {
  date <- df1[[kDate]]
  df1[[kDate]] <- as.character(strftime(date, format="%Y-%m-%d"))
  obj <- GeoTimeseries(df1, metrics=metric.df1)
  expect_equal(obj[[kDate]], date)
})

test_that("conversion of other than default date formats works", {
  fmt <- "%d/%m/%Y"
  date <- df1[[kDate]]
  df1[[kDate]] <- as.character(strftime(date, format=fmt))
  expect_is(obj <- GeoTimeseries(df1, metrics=metric.df1, date.format=fmt),
            "GeoTimeseries")
  expect_equal(obj[[kDate]], date)
})

test_that(sprintf("a well-formed '%s' that is a factor is silently accepted",
                  kDate), {
  obj <- GeoTimeseries(df1, metrics=metric.df1)
  df1[[kDate]] <- as.factor(as.character(df1[[kDate]]))
  expect_identical(obj, GeoTimeseries(df1, metrics=metric.df1))
})

test_that(sprintf("an ill-formed '%s' is detected", kDate), {
  df1[[kDate]] <- as.character(strftime(df1[[kDate]], format="%m/%d/%Y"))
  expect_error(GeoTimeseries(df1, metrics=metric.df1),
               regexp=paste(sprintf("Column '%s'", kDate),
                            "has [0-9]+ invalid date values in rows 1"))
})

test_that(sprintf("missing values in '%s' are detected", kDate), {
  i <- sample.int(nrow(df1) - 1L, size=1L)
  df1[[i, kDate]] <- NA
  expect_error(GeoTimeseries(df1, metrics=metric.df1),
               regexp=paste(sprintf("Column '%s'", kDate),
                            "has one missing value in row", i))
  df1[[i + 1L, kDate]] <- NA
  expect_error(GeoTimeseries(df1, metrics=metric.df1),
               regexp=paste0(sprintf("Column '%s' ", kDate),
                             "has 2 missing values in rows ", i, ", ", i + 1L))
})

test_that(sprintf("missing values in '%s' are detected", kGeo), {
  i <- sample.int(nrow(df1) - 1L, size=1L)
  df1[[i, kGeo]] <- NA
  expect_error(GeoTimeseries(df1, metrics=metric.df1),
               regexp=paste(sprintf("Column '%s'", kGeo),
                            "has one missing value in row", i))
  df1[[i + 1L, kGeo]] <- NA
  expect_error(GeoTimeseries(df1, metrics=metric.df1),
               regexp=paste0(sprintf("Column '%s' ", kGeo),
                             "has 2 missing values in rows ", i, ", ", i + 1L))
})

test_that("duplicate date+geo entries are detected", {
  i <- sample.int(nrow(df1), size=1L)
  new.rows <- c(seq_len(i), i, setdiff(seq_len(nrow(df1)), seq_len(i)))
  df1.dup <- df1[new.rows, ]
  df1.dup[[i, metric.df1]] <- (2 * df1.dup[[i, metric.df1]])
  expect_error(GeoTimeseries(df1.dup, metrics=metric.df1),
               regexp=paste0("There is one duplicate ",
                             sprintf("%s[+]%s ", kDate, kGeo),
                             "combination in row ", i + 1L))
  df1.dup2 <- rbind(df1.dup, df1.dup[i, , drop=FALSE])
  expect_error(GeoTimeseries(df1.dup2, metrics=metric.df1),
               regexp=paste0("There are 2 duplicate ",
                             sprintf("%s[+]%s ", kDate, kGeo),
                             "combinations in rows ", i + 1, ", ",
                             nrow(df1.dup2)))
  df1.dup3 <- rbind(df1, df1)
  max.output <- getOption("FormatTextMaxOutput", default=7L)
  rows <- paste0(paste0(nrow(df1) + 1:7, collapse=", "), ", ...")
  expect_error(GeoTimeseries(df1.dup3, metrics=metric.df1),
               regexp=paste0("There are ", nrow(df1),
                   " duplicate ",
                   sprintf("%s[+]%s ", kDate, kGeo),
                   "combinations in rows ", rows))
})

test_that("other columns with arbitrary values (even NAs) are allowed", {
  i <- sample.int(nrow(df1), size=1L)
  df2[[i, other.df2[1L]]] <- NA
  j <- sample.int(nrow(df1), size=1L)
  df2[[j, other.df2[2L]]] <- NA_integer_
  expect_is(obj <- GeoTimeseries(df2, metric=metrics.df2), "GeoTimeseries")
  expect_true(all(names(df2) %in% names(obj)))
})

test_that("a metric cannot be all NAs", {
  df1[[metric.df1[[1]]]] <- rep(NA, nrow(df1))
  expect_error(GeoTimeseries(df1, metrics=metric.df1),
               regexp="Specified metric 'sales' is all NA")
  df2[[metrics.df2[[1]]]] <- rep(NA, nrow(df2))
  expect_error(GeoTimeseries(df2, metrics=metrics.df2),
               regexp="Specified metric 'sales' is all NA")
  df2[[metrics.df2[[2]]]] <- rep(NA, nrow(df2))
  expect_error(GeoTimeseries(df2, metrics=metrics.df2),
               regexp="Specified metrics 'sales', 'conversions' are all NA")
})

test_that("a column that is not a metric can be all NA", {
  df2[[metrics.df2[[1]]]] <- rep(NA, nrow(df2))
  expect_error(GeoTimeseries(df2, metrics=metrics.df2[-1]), regexp=NA)
})

test_that("multiple metrics are allowed", {
  obj <- GeoTimeseries(df2, metrics=metrics.df2)
})

test_that("non-numeric metrics are disallowed", {
  expect_error(GeoTimeseries(df2, metrics=c("other.column")),
               regexp="Specified metric 'other.column' is not numeric")
  df2[["conversions"]] <- as.character(df2[["conversions"]])
  expect_error(GeoTimeseries(df2, metrics=c("conversions", "other.column")),
               regexp=paste0("Specified metrics 'conversions', ",
                   "'other.column' are not numeric"))
})

test_that("order of columns is date, geo, metrics, others", {
  new.column.order <- sample(names(df2))
  df2.new <- df2[new.column.order]
  obj <- GeoTimeseries(df2, metrics=metrics.df2)
  cols <- c(kDate, kGeo, metrics.df2, other.df2)
  expect_true(all(cols == names(obj)[seq_along(cols)]))
})

test_that("order of rows is sorted by date + geo", {
  new.order <- sample.int(nrow(df1))
  obj <- GeoTimeseries(df1[new.order, ], metrics=metric.df1)
  ord1 <- do.call(order, as.list(obj[c(kDate, kGeo)]))
  expect_equal(ord1, seq_along(ord1))
})

test_that("appropriate information is stored in attr(obj, 'info')", {
  obj <- GeoTimeseries(df2, metrics=metrics.df2)
  info <- list(metrics=metrics.df2,
               other=other.df2)
  expect_equal(attr(obj, "info"), info)
})
