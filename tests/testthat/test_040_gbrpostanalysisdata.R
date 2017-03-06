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

context("GBRROASAnalysisData")

gbr.key.columns <- kGeo
gbr.num.columns <- c(kRespTest, kRespPre, kCostTest, kCostPre)
gbr.required.columns <- c(gbr.key.columns, kControl, gbr.num.columns)

test_that("GBRROASAnalysisData returns a properly formatted object", {
  expect_is(obj <- GBRROASAnalysisData(dfgbr1), "GBRROASAnalysisData")
  expect_is(obj, "data.frame")
  expect_true(all(gbr.required.columns %in% names(obj)))
  expect_true(all(sapply(obj[gbr.num.columns], is.numeric)))
})

test_that(paste("the resulting object and the input data frame",
                "have the same structure"), {
                  obj <- GBRROASAnalysisData(dfgbr1)
                  expect_identical(as.list(dfgbr1), as.list(obj)[names(dfgbr1)])
                })


test_that("non-data frame objects are rejected", {
  class(dfgbr1) <- "foo"
  expect_error(GBRROASAnalysisData(dfgbr1),
               regexp="'x' must be a data\\.frame")
})

test_that("objects coercible to data.frames are allowed", {
  class(dfgbr1) <- c("foo", class(dfgbr1))
  obj <- GBRROASAnalysisData(dfgbr1)
})

test_that("columns with names starting with a dot are disallowed", {
  dfgbr1[[".foo"]] <- (0)
  expect_error(GBRROASAnalysisData(dfgbr1),
               regexp="dot are not allowed: '\\.foo'")
})

for (column.name in gbr.required.columns) {
  test_that(sprintf("a nonexisting column '%s' is detected", column.name), {
    dfgbr1[[column.name]] <- NULL
    expect_error(GBRROASAnalysisData(dfgbr1),
                 regexp=paste0("The specified column '", column.name,
                     "' is not in the data frame"))
  })
}

test_that("several nonexisting columns are detected simultaneously", {
  dfgbr1[[kGeo]] <- NULL
  dfgbr1[[kControl]] <- NULL
  expect_error(GBRROASAnalysisData(dfgbr1),
               regexp=paste0("The specified columns ",
                   "'", kGeo, "', '", kControl, "'",
                   " are not in the data frame"))
})

for (column.name in gbr.num.columns) {
  test_that(sprintf(
      "column '%s' that is not numeric is detected", column.name), {
        dfgbr1[[column.name]] <- as.character(dfgbr1[[column.name]])
        expect_error(GBRROASAnalysisData(dfgbr1),
                     regexp=paste0("Column '", column.name, "' is not ",
                         "a numeric vector"))
      })
}

test_that(sprintf("column '%s' that is not character-valued is detected",
                  kGeo), {
  dfgbr1[[kGeo]] <- (1L)
  expect_error(GBRROASAnalysisData(dfgbr1),
               regexp=sprintf("Column '%s' is not a character vector", kGeo))
})

for (column.name in gbr.required.columns) {
  test_that(sprintf("missing values in '%s' are detected", column.name), {
    i <- sample.int(nrow(dfgbr1) - 1L, size=1L)
    dfgbr1[[i, column.name]] <- NA
    expect_error(GBRROASAnalysisData(dfgbr1),
                 regexp=paste0("Column '", column.name,
                     "' has one missing value in row ", i))
    dfgbr1[[i + 1L, column.name]] <- NA
    expect_error(GBRROASAnalysisData(dfgbr1),
                 regexp=paste0("Column '", column.name,
                     "' has 2 missing values in rows ", i, ", ", i + 1L))
  })
}

test_that("duplicate key entries are detected", {
  i <- sample.int(nrow(dfgbr1), size=1L)
  new.rows <- c(seq_len(i), i, setdiff(seq_len(nrow(dfgbr1)), seq_len(i)))
  dfgbr1.dup <- dfgbr1[new.rows, ]
  expect_error(GBRROASAnalysisData(dfgbr1.dup),
               regexp=paste0("There is one duplicate value in column ",
                             sprintf("'%s'", kGeo), " in row ", i + 1L))
  dfgbr1.dup2 <- rbind(dfgbr1.dup, dfgbr1.dup[i, , drop=FALSE])
  expect_error(GBRROASAnalysisData(dfgbr1.dup2),
               regexp=paste0("There are 2 duplicate values in column ",
                             sprintf("'%s'", kGeo),
                             " in rows ", i + 1, ", ", nrow(dfgbr1.dup2)))
  dfgbr1.dup3 <- rbind(dfgbr1, dfgbr1)
  max.output <- getOption("FormatTextMaxOutput", default=7L)
  rows <- paste0(paste0(nrow(dfgbr1) + 1:7, collapse=", "), ", ...")
  expect_error(GBRROASAnalysisData(dfgbr1.dup3),
               regexp=paste0("There are ", nrow(dfgbr1),
                   " duplicate values in column ",
                   sprintf("'%s' in rows ", kGeo), rows))
})

test_that("other columns with arbitrary values (even NAs) are allowed", {
  i <- sample.int(nrow(dfgbr1), size=1L)
  df2[[i, other.df2[1L]]] <- NA
  j <- sample.int(nrow(dfgbr1), size=1L)
  df2[[j, other.df2[2L]]] <- NA_integer_
  expect_is(obj <- GBRROASAnalysisData(dfgbr1),
            "GBRROASAnalysisData")
  expect_true(all(names(dfgbr1) %in% names(obj)))
})

test_that("non-numeric metrics are disallowed", {
  dfgbr1[["resp.test"]] <- as.character(dfgbr1[["resp.test"]])
  expect_error(GBRROASAnalysisData(dfgbr1))
})

test_that("order of columns is date, geo, metrics, others", {
  new.column.order <- sample(names(dfgbr1))
  dfgbr1.new <- dfgbr1[new.column.order]
  obj <- GBRROASAnalysisData(dfgbr1.new)
  cols <- c(gbr.required.columns, setdiff(names(obj), gbr.required.columns))
  expect_true(setequal(cols, names(obj)))
  expect_true(all(cols == names(obj)))
})

test_that("order of rows is sorted by the key columns", {
  new.order <- sample.int(nrow(dfgbr1))
  obj <- GBRROASAnalysisData(dfgbr1[new.order, ])
  ord1 <- do.call(order, as.list(obj[gbr.required.columns]))
  expect_equal(ord1, seq_along(ord1))
})

test_that("appropriate information is stored in attr(obj, 'info')", {
  obj <- GBRROASAnalysisData(dfgbr1)
  info <- list(keys=gbr.key.columns,
               attr="control",
               metrics=gbr.num.columns)
  expect_equal(attr(obj, "info"), info)
})
