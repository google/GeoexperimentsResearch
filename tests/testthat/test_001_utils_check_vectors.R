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

context("is.treatment.assignment")

test_that("all NAs is allowed and returns all NAs", {
  expect_true(all(is.na(is.treatment.assignment(NA))))
  expect_true(all(is.na(is.treatment.assignment(c(NA, NA)))))
})

test_that("treatment assignments return TRUE ", {
  values <- as.vector(kTreatmentAssignment)
  expect_equal(is.treatment.assignment(values),
               rep(TRUE, length(values)))
})

test_that("bad treatment assignments return FALSE ", {
  bad.assignment <- (max(kTreatmentAssignment) + 1L)
  expect_equal(is.treatment.assignment(bad.assignment), FALSE)
})

test_that("NAs map to NAs", {
  k <- as.vector(kTreatmentAssignment["change"])
  expect_equal(is.treatment.assignment(c(NA, k, NA)),
               c(NA, TRUE, NA))
})

test_that("nonintegers cause an error", {
  expect_error(is.treatment.assignment(1.2))
  expect_error(is.treatment.assignment(-1.2))
  expect_error(is.treatment.assignment("1"))
  expect_error(is.treatment.assignment(TRUE))
})


context("is.geo.group.number")

test_that("all NAs is allowed and returns all NAs", {
  expect_true(all(is.na(is.geo.group.number(NA))))
  expect_true(all(is.na(is.geo.group.number(c(NA, NA)))))
})

test_that("nonnegative integers return TRUE ", {
  expect_equal(is.geo.group.number(0:10), rep(TRUE, 11))
})

test_that("negative integers return FALSE ", {
  expect_equal(is.geo.group.number(c(-(1:10))), rep(FALSE, 10))
})

test_that("mixtures return TRUE or FALSE ", {
  expect_equal(is.geo.group.number(c(-2, -1, 0, 1, 2)),
               c(FALSE, FALSE, TRUE, TRUE, TRUE))
})

test_that("NAs map to NAs", {
  expect_equal(is.geo.group.number(c(NA, -1, 0, NA)), c(NA, FALSE, TRUE, NA))
})

test_that("nonintegers cause an error", {
  expect_error(is.geo.group.number(1.2))
  expect_error(is.geo.group.number(-1.2))
  expect_error(is.geo.group.number("1"))
  expect_error(is.geo.group.number(TRUE))
})


context("is.period.number")

test_that("all NAs is allowed and returns all NAs", {
  expect_true(all(is.na(is.period.number(NA))))
  expect_true(all(is.na(is.period.number(c(NA, NA)))))
})

test_that("positive integers and zero return TRUE ", {
  expect_equal(is.period.number(0:10), rep(TRUE, 11))
})

test_that("negative integers return TRUE ", {
  expect_equal(is.period.number(c(-(1:10))), rep(TRUE, 10))
})

test_that("mixtures return TRUE ", {
  expect_equal(is.period.number(c(-2, -1, 0, 1, 2)),
               c(TRUE, TRUE, TRUE, TRUE, TRUE))
})

test_that("NAs map to NAs", {
  expect_equal(is.period.number(c(NA, -1, 0, NA)), c(NA, TRUE, TRUE, NA))
})

test_that("nonintegers cause an error", {
  expect_error(is.period.number(1.2))
  expect_error(is.period.number(-1.2))
  expect_error(is.period.number("1"))
  expect_error(is.period.number(TRUE))
})
