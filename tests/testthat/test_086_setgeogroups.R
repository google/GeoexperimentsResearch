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

context("SetGeoGroup<-")

geos.df <- data.frame(geo=base::LETTERS[1:12], sales=12:1)  # A=highest volume.
geos <- Geos(geos.df, volume="sales")


context("SetGeoGroup<-.GeoStrata")

obj.ga <- GeoAssignment(data.frame(geo=c("A", "C"), geo.group=c(2, 1)))

test_that("an assignment of single mapping succeeds", {
  obj.gs <- GeoStrata(geos, n.groups=2)  # geo.group is all NA.
  new.ga <- GeoAssignment(data.frame(geo="A", geo.group=2))
  expect_error(SetGeoGroup(obj.gs) <- new.ga,
               regexp=NA)
  expect_is(obj.gs, "GeoStrata")
  gg <- structure(obj.gs[[kGeoGroup]], names=obj.gs[[kGeo]])
  expect_true(gg["A"] == 2)
  expect_true(sum(!is.na(gg)) == 1)
})

test_that("an assignment of 2 mappings succeeds", {
  obj.gs <- GeoStrata(geos, n.groups=2)
  new.ga <- GeoAssignment(data.frame(geo=c("A", "B"), geo.group=1:2))
  expect_error(SetGeoGroup(obj.gs) <- new.ga,
               regexp=NA)
  expect_is(obj.gs, "GeoStrata")
  gg <- structure(obj.gs[[kGeoGroup]], names=obj.gs[[kGeo]])
  expect_true(gg["A"] == 1)
  expect_true(gg["B"] == 2)
  expect_true(sum(!is.na(gg)) == 2)
})

test_that("a geo can be assigned to group 0", {
  obj.gs <- GeoStrata(geos, n.groups=2)
  new.ga <- GeoAssignment(data.frame(geo="A", geo.group=0))
  expect_error(SetGeoGroup(obj.gs) <-new.ga,
               regexp=NA)
  expect_is(obj.gs, "GeoStrata")
  gg <- structure(obj.gs[[kGeoGroup]], names=obj.gs[[kGeo]])
  expect_true(gg["A"] == 0)
  expect_true(sum(!is.na(gg)) == 1)
})

test_that("the geos are restratified if geo is assigned to group 0", {
  obj.gs <- GeoStrata(geos, n.groups=2)
  new.ga <- GeoAssignment(data.frame(geo="A", geo.group=0))
  expect_error(SetGeoGroup(obj.gs) <- new.ga,
               regexp=NA)
  st <- structure(obj.gs[[kStratum]], names=obj.gs[[kGeo]])
  expect_true(st["A"] == 0)
  expect_true(all(st[c("B", "C", "D", "E")] == c(1, 1, 2, 2)))
})

test_that("the geos are restratified if 2 geos are assigned to group 0", {
  obj.gs <- GeoStrata(geos, n.groups=2)
  new.ga <- GeoAssignment(data.frame(geo=c("A", "C"), geo.group=c(0, 0)))
  expect_error(SetGeoGroup(obj.gs) <- new.ga,
               regexp=NA)
  st <- structure(obj.gs[[kStratum]], names=obj.gs[[kGeo]])
  expect_true(all(st[c("A", "C")] == c(0, 0)))
  expect_true(all(st[c("B", "D", "E", "F")] == c(1, 1, 2, 2)))
})


context("SetGeoGroup<-.GeoAssignment")

geos.ga <- geos.df
geos.ga[[kGeoGroup]] <- c(1, 2)
obj.ga <- GeoAssignment(geos.ga)

test_that("an assignment of single mapping succeeds", {
  gg.orig <- GetGeoGroup(obj.ga)
  new.ga <- GeoAssignment(data.frame(geo="A", geo.group=3))
  expect_error(SetGeoGroup(obj.ga) <- new.ga,
               regexp=NA)
  expect_is(obj.ga, "GeoAssignment")
  gg <- GetGeoGroup(obj.ga)
  expect_true(gg["A"] == 3)
  gg["A"] <- 1L
  expect_identical(gg, gg.orig)
})

test_that("an assignment of 2 mappings succeeds", {
  gg.orig <- GetGeoGroup(obj.ga)
  new.ga <- GeoAssignment(data.frame(geo=c("D", "F"), geo.group=c(3, 4)))
  expect_error(SetGeoGroup(obj.ga) <- new.ga,
               regexp=NA)
  expect_is(obj.ga, "GeoAssignment")
  gg <- GetGeoGroup(obj.ga)
  expect_true(gg["D"] == 3)
  expect_true(gg["F"] == 4)
  gg["D"] <- 2L
  gg["F"] <- 2L
  expect_identical(gg, gg.orig)
})

test_that("a geo can be assigned to group 0", {
  gg.orig <- GetGeoGroup(obj.ga)
  new.ga <- GeoAssignment(data.frame(geo="B", geo.group=0))
  expect_error(SetGeoGroup(obj.ga) <- new.ga,
               regexp=NA)
  expect_is(obj.ga, "GeoAssignment")
  gg <- GetGeoGroup(obj.ga)
  expect_true(gg["B"] == 0)
  gg["B"] <- 2L
  expect_identical(gg, gg.orig)
})


context("SetGeoGroup<-.GeoExperimentData")

obj.gts <- GeoTimeseries(dfged2, metrics="sales")
obj.ged <- GeoExperimentData(obj.gts)
obj.ga <- ExtractGeoAssignment(obj.ged)
new.group <- (1L + max(obj.ga[[kGeoGroup]]))
df.ga <- obj.ga[1, , drop=FALSE]
df.ga[[kGeoGroup]] <- new.group
new.ga <- GeoAssignment(df.ga)

test_that("the geo assignment object is changed", {
  expect_error(SetGeoGroup(obj.ged) <- new.ga,
               regexp=NA)
  ex.ga <- GetInfo(obj.ged, "geo.assignment")
  expect_error(SetGeoGroup(obj.ga) <- new.ga,
               regexp=NA)
  expect_identical(obj.ga, ex.ga)
})

test_that("the geo assignment object and the mapping are changed", {
  expect_error(SetGeoGroup(obj.ged) <- new.ga,
               regexp=NA)
  expect_error(SetGeoGroup(obj.ga) <- new.ga,
               regexp=NA)
  ex.ga2 <- ExtractGeoAssignment.GeoTimeseries(obj.ged)
  expect_identical(obj.ga, ex.ga2)
})
