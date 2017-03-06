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

context("ROASAnalysisResults")

iROAS <- 4.0
ad.spend.diff.daily <- 100

obj.gbr <- .MakeGBRDataObject(dfged3, iroas=iROAS, ad.spend.daily=0,
                              ad.spend.diff.daily=ad.spend.diff.daily)
obj <- DoGBRROASAnalysis(obj.gbr)

good.args <- list(estimate=-1, lower=0.2, upper=1, level=0.95,
                  incr.resp=1, incr.cost=2, threshold=0, post.prob=0.5,
                  model=kGBRModel1)

test_that("good arguments pass", {
  expect_is(do.call(ROASAnalysisResults, good.args),
            "ROASAnalysisResults")
})

test_that("'precision' must be calculated correctly", {
  expect_is(r <- do.call(ROASAnalysisResults, good.args),
            "ROASAnalysisResults")
  expect_equal(r[["precision"]],
               0.5 * (good.args[["upper"]] - good.args[["lower"]]))
  good.args[["upper"]] <- Inf
  r <- do.call(ROASAnalysisResults, good.args)
  expect_equal(r[["precision"]],
               (good.args[["estimate"]] - good.args[["lower"]]))
})

test_that("Inf is allowed for 'upper'", {
  good.args[["upper"]] <- Inf
  expect_is(do.call(ROASAnalysisResults, good.args), "ROASAnalysisResults")
})

test_that("negative values are not allowed for 'post.prob'", {
  good.args[["post.prob"]] <- (-0.5)
  bad.args <- good.args
  expect_error(do.call(ROASAnalysisResults, bad.args))
})

test_that("NA is not allowed for threshold", {
  good.args[["threshold"]] <- NA_real_
  bad.args <- good.args
  expect_error(do.call(ROASAnalysisResults, bad.args))
})

test_that("errors are thrown for invalid numbers", {
  # An NA or an -Inf when is an error.
  for (name in names(good.args)) {
    value <- good.args[[name]]
    bad.args <- good.args
    if (is.numeric(value)) {
      bad.args[[name]] <- (-Inf) ##if (name == "lower") -Inf else
      expect_error(do.call(ROASAnalysisResults, bad.args),
                   regexp=sprintf("%s is not a real number", name))
      bad.args[[name]] <- as.numeric(NA)
      expect_error(do.call(ROASAnalysisResults, bad.args),
                   regexp=sprintf("%s is not a real number", name))
    } else {
      # 'method'.
      bad.args[[name]] <- NA_character_
      expect_error(do.call(ROASAnalysisResults, bad.args),
                   regexp=sprintf("%s is not a nonempty string", name))
    }
  }
  # level is not between 0 and 1.
  bad.args <- good.args
  bad.args[["level"]] <- 0
  expect_error(do.call(ROASAnalysisResults, bad.args),
               regexp="level not greater than 0")
  bad.args[["level"]] <- 1
  expect_error(do.call(ROASAnalysisResults, bad.args),
               regexp="level not less than 1")
  # lower <= upper.
  bad.args <- good.args
  bad.args[["upper"]] <- (bad.args[["lower"]] - 1)
  expect_error(do.call(ROASAnalysisResults, bad.args),
               regexp="lower not less than or equal to upper")
  # lower == upper is allowed.
  good.args[["upper"]] <- good.args[["lower"]]
  expect_is(do.call(ROASAnalysisResults, good.args),
            "ROASAnalysisResults")
})

test_that("rounding is done correctly", {
  obj <- ROASAnalysisResults(estimate=0.123, lower=0.123, upper=0.1234,
                             level=0.975, incr.resp=0.123, incr.cost=0.123,
                             threshold=0, post.prob=0.5,
                             model=kGBRModel1)
  r <- round(obj, digits=2)
  # The object class must stay the same.
  expect_is(r, "ROASAnalysisResults")
  cols <- c("estimate", "lower", "upper", "incr.resp", "incr.cost")
  for (col in cols) {
    expect_equal(r[[col]], 0.12)
  }
  # Test 'precision' separately.
  expect_equal(r[["precision"]], 0.00)
  # 'level' must not be affected.
  expect_equal(r[["level"]], 0.975)
})

test_that("rounding to significant digits is done correctly", {
  obj <- ROASAnalysisResults(estimate=0.123, lower=0.123, upper=0.1234,
                             level=0.975, incr.resp=123.456,
                             threshold=0, post.prob=0.5,
                             incr.cost=123.456, model=kGBRModel1)
  r <- signif(obj, digits=2)
  # The object class must stay the same.
  expect_is(r, "ROASAnalysisResults")
  cols <- c("estimate", "lower", "upper")
  expect_equal(r[["estimate"]], 0.12)
  expect_equal(r[["lower"]], 0.12)
  expect_equal(r[["upper"]], 0.12)
  expect_equal(r[["incr.resp"]], 120)
  expect_equal(r[["incr.cost"]], 120)
  # Test 'precision' separately.
  expect_equal(r[["precision"]], 0.0002)
  # 'level' must not be affected.
  expect_equal(r[["level"]], 0.975)
})
