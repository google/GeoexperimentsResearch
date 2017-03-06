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

context("TBRAnalysisData")

tbr.key.columns <- c(kDate, kPeriod)
tbr.num.columns <- c(kY, kX)
tbr.required.columns <- c(tbr.key.columns, tbr.num.columns)

dftbr1 <- data.frame(as.Date("2016-05-01") + 0:20,
                     c(-1, -1, rep(0, 8), NA, rep(1, 8), 2, 3),
                     abs(rnorm(21)),
                     abs(rnorm(21)),
                     0)
names(dftbr1) <- c(kDate, kPeriod, kY, kX, "z")

test_that("TBRAnalysisData returns a properly formatted object", {
  expect_is(obj <- TBRAnalysisData(dftbr1), "TBRAnalysisData")
  expect_is(obj, "data.frame")
  expect_true(all(names(dftbr1) %in% names(obj)))
})

obj <- TBRAnalysisData(dftbr1)

test_that(paste("the resulting object and the input data frame",
                "have the same structure"), {
                  expect_identical(as.list(dftbr1), as.list(obj)[names(dftbr1)])
                })


test_that("non-data frame objects are rejected", {
  class(dftbr1) <- "foo"
  expect_error(TBRAnalysisData(dftbr1),
               regexp="'x' must be a data\\.frame")
})

test_that("objects coercible to data.frames are allowed", {
  class(dftbr1) <- c("foo", class(dftbr1))
  expect_is(TBRAnalysisData(dftbr1), "TBRAnalysisData")
})

test_that("columns with names starting with a dot are disallowed", {
  dftbr1[[".foo"]] <- (0)
  expect_error(TBRAnalysisData(dftbr1),
               regexp=paste("dot are not allowed: '\\.foo'"))
})

for (column.name in tbr.required.columns) {
  test_that(sprintf("a nonexisting column '%s' is detected", column.name), {
    dftbr1[[column.name]] <- NULL
    expect_error(TBRAnalysisData(dftbr1),
                 regexp=paste0("The specified column '", column.name,
                     "' is not in the data frame"))
  })
}

test_that("several nonexisting columns are detected simultaneously", {
  dftbr1[[kDate]] <- NULL
  dftbr1[[kPeriod]] <- NULL
  expect_error(TBRAnalysisData(dftbr1),
               regexp=sprintf(paste0("The specified columns '%s', '%s'",
                   " are not in the data frame"), kDate, kPeriod))
})

for (column.name in tbr.num.columns) {
  test_that(sprintf(
      "column '%s' that is not numeric is detected", column.name), {
        dftbr1[[column.name]] <- as.character(dftbr1[[column.name]])
        expect_error(TBRAnalysisData(dftbr1),
                     regexp=paste0("Column '", column.name, "' is not ",
                         "a numeric vector"))
      })
}

test_that(sprintf("column '%s' that is not a 'Date' is detected", kDate), {
  dftbr1[[kDate]] <- as.character(dftbr1[[kDate]])
  expect_error(TBRAnalysisData(dftbr1),
               regexp=sprintf("Column '%s' is not a Date", kDate))
})

test_that(sprintf("NAs in '%s' disallowed anywhere", kDate), {
  in.preexperiment <- (dftbr1[[kPeriod]] %in%
                       kStandardPeriods[["preexperiment"]])
  in.posttest <- (dftbr1[[kPeriod]] %in% kStandardPeriods[["posttest"]])
  i <- c(which(in.preexperiment)[1], which(in.posttest)[1])
  dftbr1[i, kDate] <- NA
  expect_error(TBRAnalysisData(dftbr1),
               regexp=paste0("Column '", kDate,
                   "' has 2 missing values in rows ", i[1], ", ", i[2]))
})

for (column in tbr.num.columns) {
  test_that(sprintf("NAs in '%s' disallowed in analysis period", column), {
    i <- which(dftbr1[[kPeriod]] %in% kStandardPeriods[["analysis"]])[1]
    dftbr1[[i, column]] <- NA
    expect_error(TBRAnalysisData(dftbr1),
                 regexp=paste0("Column '", column,
                     "' has one missing value in row ", i))
  })
}

for (column in tbr.num.columns) {
  test_that(sprintf("NAs in '%s' allowed outside analysis period", column), {
    i <- which(dftbr1[[kPeriod]] %in% kStandardPeriods[["preexperiment"]])[1]
    dftbr1[[i, column]] <- NA
    expect_error(TBRAnalysisData(dftbr1), regexp=NA)
  })
}

test_that("duplicate key entries are detected", {
  i <- sample.int(nrow(dftbr1), size=1L)
  new.rows <- c(seq_len(i), i, setdiff(seq_len(nrow(dftbr1)), seq_len(i)))
  dftbr1.dup <- dftbr1[new.rows, ]
  expect_error(TBRAnalysisData(dftbr1.dup),
               regexp=paste0("There is one duplicate value in column ",
                             sprintf("'%s'", kDate), " in row ", i + 1L))
  dftbr1.dup2 <- rbind(dftbr1.dup, dftbr1.dup[i, , drop=FALSE])
  expect_error(TBRAnalysisData(dftbr1.dup2),
               regexp=paste0("There are 2 duplicate values in column ",
                             sprintf("'%s'", kDate),
                             " in rows ", i + 1, ", ", nrow(dftbr1.dup2)))
  dftbr1.dup3 <- rbind(dftbr1, dftbr1)
  max.output <- getOption("FormatTextMaxOutput", default=7L)
  rows <- paste0(paste0(nrow(dftbr1) + 1:7, collapse=", "), ", ...")
  expect_error(TBRAnalysisData(dftbr1.dup3),
               regexp=paste0("There are ", nrow(dftbr1),
                   " duplicate values in column ",
                   sprintf("'%s'", kDate), " in rows ", rows))
})

test_that("other columns with arbitrary values (even NAs) are allowed", {
  dftbr1[["foo"]] <- NA
  expect_is(obj <- TBRAnalysisData(dftbr1), "TBRAnalysisData")
  expect_true(all(names(dftbr1) %in% names(obj)))
})

test_that("non-numeric y is disallowed", {
  dftbr1[[kY]] <- as.character(dftbr1[[kY]])
  expect_error(TBRAnalysisData(dftbr1))
})

test_that("non-numeric x is disallowed", {
  dftbr1[[kX]] <- as.character(dftbr1[[kX]])
  expect_error(TBRAnalysisData(dftbr1))
})

test_that("order of columns is date, period, y, x, others", {
  new.column.order <- sample(names(dftbr1))
  dftbr1.new <- dftbr1[new.column.order]
  obj <- TBRAnalysisData(dftbr1.new)
  cols <- c(tbr.required.columns, setdiff(names(obj), tbr.required.columns))
  expect_true(setequal(cols, names(obj)))
  expect_true(all(cols == names(obj)))
})

test_that("order of rows is sorted by the key columns", {
  obj1 <- TBRAnalysisData(dftbr1)
  period <- dftbr1[[kPeriod]]
  new.order <- sample.int(nrow(dftbr1))
  dftbr2 <- dftbr1[new.order, ]
  dftbr2[[kPeriod]] <- period  # To keep the periods in sequential order.
  obj2 <- TBRAnalysisData(dftbr2)
  expect_equal(obj2[[kDate]], obj1[[kDate]])
})

test_that("intervention period < pretest period gives an error", {
  in.pretest <- dftbr1[[kPeriod]] %in% kStandardPeriods[["pretest"]]
  period <- dftbr1[[kPeriod]]
  dftbr1[[kPeriod]][in.pretest][1] <- kStandardPeriods[["intervention"]]
  expect_error(TBRAnalysisData(dftbr1),
               regexp="Periods must be consecutive")
})

test_that("both pretest period and intervention periods must be present", {
  dftbr1[[kPeriod]] <- NA_integer_
  dftbr1[[kPeriod]][2:3] <- kStandardPeriods[["pretest"]]
  expect_error(TBRAnalysisData(dftbr1),
               regexp="Both pretest and intervention periods must be present")
  dftbr1[[kPeriod]][2:3] <- kStandardPeriods[["intervention"]]
  expect_error(TBRAnalysisData(dftbr1),
               regexp="Both pretest and intervention periods must be present")
})

test_that("minimum length of pretest and intervention periods is 1", {
  dftbr1[[kPeriod]] <- NA_integer_
  dftbr1[[kPeriod]][2] <- kStandardPeriods[["pretest"]]
  dftbr1[[kPeriod]][3] <- kStandardPeriods[["intervention"]]
  expect_is(TBRAnalysisData(dftbr1), "TBRAnalysisData")
})
