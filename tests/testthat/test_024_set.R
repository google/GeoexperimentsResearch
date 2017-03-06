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

context("SetExperimentPeriods<-.GeoExperimentData")

obj.gts <- GeoTimeseries(dfged2, metrics=metric.df1)
obj.per <- ExtractExperimentPeriods(obj.gts)
obj.ga <- ExtractGeoAssignment(obj.gts)
obj.trt <- ExtractTreatmentAssignment(obj.gts)
obj.ged <- GeoExperimentData(obj.gts,
                             periods=obj.per,
                             geo.assignment=obj.ga,
                             treat.assignment=obj.trt)
obj.gts2 <- obj.gts
obj.gts2[[kGeoGroup]] <- NA_integer_
obj.gts2[[kPeriod]] <- NA_integer_
obj.gts2[[kAssignment]] <- NA_integer_

test_that("'periods' is associated with the GED object properly", {
  # Construct an object that has no 'period' information.
  obj <- GeoExperimentData(obj.gts2,
                           periods=NULL,
                           geo.assignment=obj.ga,
                           treat.assignment=obj.trt)
  expect_false(identical(obj.ged[[kAssignment]], obj[[kAssignment]]))
  SetExperimentPeriods(obj) <- obj.per
  expect_identical(obj.ged, obj)
})

test_that("an error may be thrown if there is no data for a period", {
  omit.period <- 1L
  # Verify that 'obj.per' has the period to omit.
  stopifnot(isTRUE(omit.period %in% obj.trt[[kPeriod]]))
  # Omit the period from the data.
  obj.gts0 <- Subset(obj.gts, !(obj.gts[[kPeriod]] %in% omit.period))
  obj <- GeoExperimentData(obj.gts0)
  # When 'strict' is not specified (=TRUE), an error is thrown...
  expect_error(SetExperimentPeriods(obj) <- obj.per,
               regexp=paste0("No data available for period ", omit.period))
  # ...but, if strict=FALSE, there will be no error.
  expect_identical(SetExperimentPeriods(obj, strict=FALSE) <- obj.per, obj.per)
})


context("SetGeoAssignment<-.GeoExperimentData")

test_that("'geo.assignment' is associated with the GED object properly", {
  # Construct an object that has no 'geo.assignment' information.
  obj <- GeoExperimentData(obj.gts2,
                           periods=obj.per,
                           geo.assignment=NULL,
                           treat.assignment=obj.trt)
  expect_false(identical(obj.ged[[kGeoGroup]], obj[[kGeoGroup]]))
  expect_false(identical(obj.ged[[kAssignment]], obj[[kAssignment]]))
  SetGeoAssignment(obj) <- obj.ga
  expect_identical(obj.ged, obj)
})

test_that("assigning NULL causes the geo assignment to disappear", {
  # Construct an object that has no 'geo.assignment' information.
  obj <- GeoExperimentData(obj.gts2,
                           periods=obj.per,
                           geo.assignment=NULL,
                           treat.assignment=obj.trt)
  SetGeoAssignment(obj.ged) <- NULL
  expect_identical(obj.ged, obj)
  expect_true(all(is.na(obj.ged[[kAssignment]])))
})

test_that("new geo assignment is properly applied", {
  g <- max(obj.ged[[kGeoGroup]], na.rm=TRUE) + 1L
  obj.ga[obj.ga[[kGeo]] %in% "1", kGeoGroup] <- g
  SetGeoAssignment(obj.ged) <- obj.ga
  expect_true(all(obj.ged[obj.ged[[kGeo]] %in% "1", kGeoGroup] %in% g))
})

test_that("mapping to NA is allowed and is properly applied", {
  obj.ga[obj.ga[[kGeo]] %in% "1", kGeoGroup] <- NA_integer_
  SetGeoAssignment(obj.ged) <- obj.ga
  expect_true(all(is.na(obj.ged[obj.ged[[kGeo]] %in% "1", kGeoGroup])))
})

test_that("trt. assignment is done after periods and groups are there ", {
  # Construct an object that has no 'period' information.
  obj <- GeoExperimentData(obj.gts2,
                           periods=NULL,
                           geo.assignment=NULL,
                           treat.assignment=obj.trt)
  SetExperimentPeriods(obj) <- obj.per
  SetGeoAssignment(obj) <- obj.ga
  expect_identical(obj.ged, obj)
})

test_that("an error may be thrown if a geo in geo.assignment is not in data", {
  omit.geo <- "1"
  # Verify that the geo exists in the data and the mapping.
  stopifnot(isTRUE(omit.geo %in% obj.ga[[kGeo]]),
            isTRUE(omit.geo %in% obj.gts[[kGeo]]))
  # Omit the period from the data.
  obj.gts0 <- Subset(obj.gts, !(obj.gts[[kGeo]] %in% omit.geo))
  obj <- GeoExperimentData(obj.gts0)
  # When 'strict' is not specified (=TRUE), an error is thrown...
  expect_error(SetGeoAssignment(obj) <- obj.ga,
               regexp=paste0(
                   "The following geo was not found in the data: ",
                   "'", omit.geo, "'"))
  # ...but, if strict=FALSE, there will be no error.
  expect_identical(SetGeoAssignment(obj, strict=FALSE) <- obj.ga, obj.ga)
})

test_that("an error may be thrown if a geo in data is not in geo.assignment", {
  omit.geo <- "1"
  # Verify that the geo exists in the data and the mapping.
  stopifnot(isTRUE(omit.geo %in% obj.ga[[kGeo]]),
            isTRUE(omit.geo %in% obj.gts[[kGeo]]))
  # Omit the period from the geo assignment.
  obj.ga <- obj.ga[!(obj.ga[[kGeo]] %in% omit.geo), , drop=FALSE]
  obj <- obj.ged
  # When 'strict' is not specified (=TRUE), an error is thrown...
  expect_error(SetGeoAssignment(obj) <- obj.ga,
               regexp=paste0(
                   "The following geo was not found in the geo assignment: ",
                   "'", omit.geo, "'"))
  # ...but, if strict=FALSE, there will be no error.
  expect_identical(SetGeoAssignment(obj, strict=FALSE) <- obj.ga, obj.ga)
})


context("SetTreatmentAssignment<-.GeoExperimentData")

test_that("'treat.assignment' is associated with the GED object properly", {
  # Construct an object that has no 'period' information.
  obj <- GeoExperimentData(obj.gts2,
                           periods=obj.per,
                           geo.assignment=obj.ga,
                           treat.assignment=NULL)
  expect_false(identical(obj.ged[[kAssignment]], obj[[kAssignment]]))
  SetTreatmentAssignment(obj) <- obj.trt
  expect_identical(obj.ged, obj)
})

test_that("missing assignments are translated as 'none'", {
  obj <- GeoExperimentData(obj.gts2,
                           periods=obj.per,
                           geo.assignment=obj.ga,
                           treat.assignment=NULL)
  # Omit all mappings that map to 'none' ('no change')
  rows.with.no.change <- (obj.trt[[kAssignment]]
                          %in% kTreatmentAssignment["none"])
  obj.trt <- obj.trt[!rows.with.no.change, , drop=FALSE]

  SetTreatmentAssignment(obj) <- obj.trt
  expect_identical(obj.ged[[kAssignment]], obj[[kAssignment]])
})
