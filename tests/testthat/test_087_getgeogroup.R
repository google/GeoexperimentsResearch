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

context("GetGeoGroup")

geos.df <- data.frame(geo=base::LETTERS[1:12], sales=12:1)
geos <- Geos(geos.df, volume="sales")
geos[[kGeoGroup]] <- 1:2
obj.ga <- GeoAssignment(geos)
obj.gs <- GeoStrata(geos, n.groups=2)
obj.gs[[kGeoGroup]] <- 2:1
obj.gs[[kGeoGroup]][1] <- NA_integer_


context("GetGeoGroup.GeoAssignment")

map0 <- structure(obj.ga[[kGeoGroup]], names=obj.ga[[kGeo]])

test_that("by default the complete mapping is returned.", {
  expect_identical(map0, GetGeoGroup(obj.ga))
})

test_that("a single mapping can be returned.", {
  expect_identical(map0["A"], GetGeoGroup(obj.ga, geo="A"))
})

test_that("multiple mappings can be returned.", {
  expect_identical(map0[c("A", "B")], GetGeoGroup(obj.ga, geo=c("A", "B")))
})


context("GetGeoGroup.GeoStrata")

map0 <- structure(obj.gs[[kGeoGroup]], names=obj.gs[[kGeo]])

test_that("by default the complete mapping is returned.", {
  expect_identical(map0, GetGeoGroup(obj.gs))
})

test_that("a single mapping can be returned.", {
  expect_identical(map0["A"], GetGeoGroup(obj.gs, geo="A"))
})

test_that("multiple mappings can be returned.", {
  expect_identical(map0[c("A", "B")], GetGeoGroup(obj.gs, geo=c("A", "B")))
})


context("GetGeoGroup.GeoExperimentData")

obj.gts <- GeoTimeseries(dfged2, metrics=metric.df1)
obj.trt <- ExtractTreatmentAssignment(obj.gts)
obj.ga <- ExtractGeoAssignment(obj.gts)
obj.per <- ExtractExperimentPeriods(obj.gts)
obj.ged <- GeoExperimentData(obj.gts,
                             periods=obj.per,
                             geo.assignment=obj.ga,
                             treat.assignment=obj.trt)

map0 <- structure(obj.ga[[kGeoGroup]], names=obj.ga[[kGeo]])

test_that("by default the complete mapping is returned.", {
  expect_identical(map0, GetGeoGroup(obj.ged))
})

test_that("a single mapping can be returned.", {
  expect_identical(map0["1"], GetGeoGroup(obj.ged, geo="1"))
})

test_that("multiple mappings can be returned.", {
  expect_identical(map0[c("1", "5")], GetGeoGroup(obj.ged, geo=c("1", "5")))
})

obj.ged <- GeoExperimentData(obj.gts,
                             periods=obj.per,
                             geo.assignment=NULL,
                             treat.assignment=obj.trt)

test_that("an error is thrown if no geo.assignment was set in the object.", {
  expect_error(GetGeoGroup(obj.ged),
               regexp="No geo.assignment found")
})
