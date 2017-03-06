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

context("GeoExperimentData")

obj.gts <- GeoTimeseries(dfged2, metrics=metric.df1)
obj.trt <- ExtractTreatmentAssignment(obj.gts)
obj.ga <- ExtractGeoAssignment(obj.gts)
obj.per <- ExtractExperimentPeriods(obj.gts)
obj.gts2 <- obj.gts
obj.gts2[[kGeoGroup]] <- NA_integer_
obj.gts2[[kPeriod]] <- NA_integer_
obj.gts2[[kAssignment]] <- NA_integer_

test_that("a valid set of objects results in a GeoExperimentData object", {
  expect_is(obj <- GeoExperimentData(obj.gts,
                                     periods=obj.per,
                                     geo.assignment=obj.ga,
                                     treat.assignment=obj.trt),
            "GeoExperimentData")
  expect_is(obj, "GeoTimeseries")
  expect_is(obj, "data.frame")
})

test_that("object keeps its original class structure", {
  class(obj.gts) <- c("Foo", class(obj.gts))
  expect_error(obj <- GeoExperimentData(obj.gts,
                                        periods=obj.per,
                                        geo.assignment=obj.ga,
                                        treat.assignment=obj.trt),
               regexp=NA)
  expect_equal(class(obj), c("GeoExperimentData", class(obj.gts)))
})

test_that("there are no duplicate class definitions", {
  class(obj.gts) <- c("GeoExperimentData", class(obj.gts))
  expect_error(obj <- GeoExperimentData(obj.gts,
                                        periods=obj.per,
                                        geo.assignment=obj.ga,
                                        treat.assignment=obj.trt),
               regexp=NA)
  expect_equal(class(obj), class(obj.gts))
})

test_that("the three columns are re-constructed properly", {
  obj.orig <- obj.gts
  obj <- GeoExperimentData(obj.gts2,
                           periods=obj.per,
                           geo.assignment=obj.ga,
                           treat.assignment=obj.trt)
  columns <- c(kDate, kGeo, kGeoGroup, kPeriod)
  for (column in columns) {
    expect_identical(obj[[column]], obj.orig[[column]])
  }
  # Treatment assignment is special: the original data may have NAs in
  # either 'geo.group' or 'period' (or both) and they might have been
  # "mapped" to some value. However these mappings are considered to
  # be "outside of the experiment" and ignored. In the reconstructed
  # object, 'assignment' will appear as 'NA'.

  # Rows with both 'period' and 'geo.group' specified (non-NA)...
  rows <- !(is.na(obj.orig[[kPeriod]]) | is.na(obj.orig[[kGeoGroup]]))
  # ... should have a non-NA 'assignment'...
  expect_true(all(is.na(obj[[kAssignment]][!rows])))
  # ...and other rows else should have an NA in 'assignment'...
  expect_true(all(is.na(obj[[kAssignment]][!rows])))
  # ...in fact they should be equal to the original values.
  expect_identical(obj[[kAssignment]][rows], obj.orig[[kAssignment]][rows])
})

test_that("the deconstructed pieces match the original ones", {
  obj <- GeoExperimentData(obj.gts2,
                           periods=obj.per,
                           geo.assignment=obj.ga,
                           treat.assignment=obj.trt)
  expect_identical(ExtractGeoAssignment(obj), obj.ga)
  expect_identical(ExtractExperimentPeriods(obj), obj.per)
  expect_identical(ExtractTreatmentAssignment(obj), obj.trt)
})

obj.ged <- GeoExperimentData(obj.gts2,
                             periods=obj.per,
                             geo.assignment=obj.ga,
                             treat.assignment=obj.trt)

test_that("'periods' can be NULL", {
  # Construct an object that has no 'period' information.
  expect_is(obj <- GeoExperimentData(obj.gts2,
                                     periods=NULL,
                                     geo.assignment=obj.ga,
                                     treat.assignment=obj.trt),
            "GeoExperimentData")
  expect_true(all(is.na(obj[[kPeriod]])))
  # Should also have no treatment assignment information in 'assignment'.
  expect_true(all(is.na(obj[[kAssignment]])))
  # No information about experiment periods in the slot.
  expect_true(is.null(GetInfo(obj, "periods")))
})

test_that("'geo.assignment' can be NULL", {
  # Construct an object that has no 'geo.group' information.
  expect_is(obj <- GeoExperimentData(obj.gts2,
                                     periods=obj.per,
                                     geo.assignment=NULL,
                                     treat.assignment=obj.trt),
            "GeoExperimentData")
  expect_true(all(is.na(obj[[kGeoGroup]])))
  # Should also have no treatment assignment information in 'assignment'.
  expect_true(all(is.na(obj[[kAssignment]])))
  # No information about geo assignment in the slot.
  expect_true(is.null(GetInfo(obj, "geo.assignment")))
})

test_that("'treat.assignment' can be NULL", {
  # Construct an object that has no 'assignment' information.
  expect_is(obj <- GeoExperimentData(obj.gts2,
                                     periods=obj.per,
                                     geo.assignment=obj.ga,
                                     treat.assignment=NULL),
            "GeoExperimentData")
  # No treatment assignment information in 'assignment'.
  expect_true(all(is.na(obj[[kAssignment]])))
  # No information about treatment assignment in the slot.
  expect_true(is.null(GetInfo(obj, "treat.assignment")))
})

test_that("'assignment' column can be missing in the GeoTimeseries object", {
  # Construct a GeoTimeseries object that has no 'assignment' column.
  obj.gts2[[kAssignment]] <- NULL
  # Don't specify treat.assignment.
  expect_is(obj <- GeoExperimentData(obj.gts2,
                                     periods=obj.per,
                                     geo.assignment=obj.ga),
            "GeoExperimentData")
  # The column exists, but is all NA.
  expect_true(kAssignment %in% names(obj))
  expect_true(all(is.na(obj[[kAssignment]])))
  # No information about treatment assignment in the slot.
  expect_true(is.null(GetInfo(obj, "treat.assignment")))
})
