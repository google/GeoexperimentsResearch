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

context("RenameColumns")

d <- dfged2[c(kDate, kGeo, "sales")]

test_that("if 'map' is not given or is NULL, 'x' is returned unchanged", {
  expect_identical(d, RenameColumns(d))
  expect_identical(d, RenameColumns(d, map=NULL))
  expect_identical(d, RenameColumns(d, map=character(0)))
})

test_that("errors are thrown if 'x' is not a data frame", {
  m <- as.matrix(d)
  for (x in list(m, as.list(d), NULL)) {
    expect_error(RenameColumns(x),
                 msg="x is not a data frame")
  }
})

test_that("errors are thrown if 'map' is not a named character vector", {
  for (map in list(NA_character_, "", kDate, c(date=1))) {
    expect_error(RenameColumns(d, map=map),
                 msg="'map' must be a named character vector")
  }
})

test_that("column name is changed if it is not a duplicate", {
  expect_is(x <- RenameColumns(d, map=c(date2=kDate)), "data.frame")
  expect_identical(names(x), c("date2", names(d)[2:3]))
  # Nothing else has changed:
  names(x) <- names(d)
  expect_identical(x, d)
})

test_that("column names can be swapped", {
  expect_identical(c(kGeo, kDate, "sales"),
                   names(RenameColumns(d, map=c(geo=kDate, date=kGeo))))
})

test_that("an error is thrown if a column name is changed to a duplicate", {
  expect_error(RenameColumns(d, map=c(date=kGeo)))
})


context("GetModelIds")

test_that("the returned object is identical to kModels", {
  expect_identical(GetModelIds(), kModels)
})
