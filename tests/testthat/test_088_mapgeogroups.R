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

context("MapGeoGroups<-.GeoAssignment")

geos.df <- data.frame(geo=c("A", "B", "C", "D"), geo.group=c(1 ,2, 2, 1))
obj.ga <- GeoAssignment(geos.df)

test_that("[2 groups] geo groups are swapped when value is 2:1", {
  expect_error(MapGeoGroups(obj.ga) <- 2:1, regexp=NA)
  expect_identical(obj.ga[[kGeoGroup]], c(2L, 1L, 1L, 2L))
})

test_that("[2 groups] nothing happens when value is 1:2", {
  old.obj.ga <- obj.ga
  expect_error(MapGeoGroups(obj.ga) <- 1:2, regexp=NA)
  expect_identical(old.obj.ga, obj.ga)
})

test_that("[2 groups] groups can be merged into one", {
  expect_error(MapGeoGroups(obj.ga) <- c(1, 1), regexp=NA)
  expect_identical(rep(1L, 4), obj.ga[[kGeoGroup]])
})

test_that("[2 groups] groups can be mapped into NAs", {
  expect_error(MapGeoGroups(obj.ga) <- c(1, NA), regexp=NA)
  expect_identical(obj.ga[[kGeoGroup]], c(1L, NA_integer_, NA_integer_, 1L))
  expect_error(MapGeoGroups(obj.ga) <- NA, regexp=NA)
  expect_identical(obj.ga[[kGeoGroup]], rep(NA_integer_, 4))
})

test_that("geo group number 0 is allowed", {
  expect_error(MapGeoGroups(obj.ga) <- c(0, 2), regexp=NA)
  expect_identical(obj.ga[[kGeoGroup]], c(0L, 2L, 2L, 0L))
})

test_that("[2 groups] groups can be mapped into any positive integer", {
  expect_error(MapGeoGroups(obj.ga) <- c(1, 200), regexp=NA)
  expect_identical(obj.ga[[kGeoGroup]], c(1L, 200L, 200L, 1L))
})

test_that("map must be equal to the maximum number of groups in the map", {
  expect_error(MapGeoGroups(obj.ga) <- 1,
               regexp="Expected a vector of length 2")
  expect_error(MapGeoGroups(obj.ga) <- 1:3,
               regexp="Expected a vector of length 2")
})

test_that("negative geo group numbers are not allowed", {
  expect_error(MapGeoGroups(obj.ga) <- c(-1, 2),
               regexp="Geo group numbers must be NA, 0, or positive")
})

test_that("negative geo group numbers are not allowed", {
  expect_error(MapGeoGroups(obj.ga) <- c(-1, 2),
               regexp="Geo group numbers must be NA, 0, or positive")
})

geos.df <- data.frame(geo=c("A", "B", "C", "D"), geo.group=c(1 ,2, 3, 1))
obj.ga3 <- GeoAssignment(geos.df)

test_that("[3 groups] geo groups mapped 1->3, 2->2, 3->1", {
  expect_error(MapGeoGroups(obj.ga3) <- 3:1, regexp=NA)
  expect_identical(obj.ga3[[kGeoGroup]], c(3:1, 3L))
})

test_that("[3 groups] geo groups mapped 1->3, 2->2, 3->1", {
  expect_error(MapGeoGroups(obj.ga3) <- 3:1, regexp=NA)
  expect_identical(obj.ga3[[kGeoGroup]], c(3:1, 3L))
})

geos.df <- data.frame(geo=c("A", "B", "C", "D"), geo.group=c(1 ,NA, NA, 1))
obj.ga1 <- GeoAssignment(geos.df)

test_that("[1 group] geo groups mapped 1->2, NAs unchanged", {
  expect_error(MapGeoGroups(obj.ga1) <- 2, regexp=NA)
  expect_identical(obj.ga1[[kGeoGroup]], c(2L, NA_integer_, NA_integer_, 2L))
})

geos.df <- data.frame(geo=c("A", "B", "C", "D"),
                      geo.group=as.integer(c(NA ,NA, NA, NA)))
obj.ga.none <- GeoAssignment(geos.df)

test_that("[0 groups] error occurs if a nonempty mapping is attempted", {
  expect_error(MapGeoGroups(obj.ga.none) <- 1,
               msg="There are no geo group numbers in the data")
  expect_error(MapGeoGroups(obj.ga.none) <- NA,
               msg="There are no geo group numbers in the data")
})

test_that("[0 groups] no effect occurs for an empty NULL or numeric mapping", {
  expect_error(MapGeoGroups(obj.ga.none) <- NULL, regexp=NA)
  expect_error(MapGeoGroups(obj.ga.none) <- integer(0), regexp=NA)
  expect_error(MapGeoGroups(obj.ga.none) <- numeric(0), regexp=NA)
  expect_error(MapGeoGroups(obj.ga.none) <- character(0))
})

# Testing geo assignments with omitted geos.

geos.df <- data.frame(geo=c("A", "B", "C", "D"),
                      geo.group=as.integer(c(0 ,0, 0, 0)))
obj.ga.all.zeros <- GeoAssignment(geos.df)


test_that("[all geos excluded] error occurs for nonempty mapping", {
  expect_error(MapGeoGroups(obj.ga.all.zeros) <- 1,
               msg="There are no geo group numbers in the data")
  expect_error(MapGeoGroups(obj.ga.all.zeros) <- NA,
               msg="There are no geo group numbers in the data")
})

test_that("[all geos excluded] no effect for empty, NULL or numeric mapping", {
  expect_error(MapGeoGroups(obj.ga.all.zeros) <- NULL, regexp=NA)
  expect_error(MapGeoGroups(obj.ga.all.zeros) <- integer(0), regexp=NA)
  expect_error(MapGeoGroups(obj.ga.all.zeros) <- numeric(0), regexp=NA)
  expect_error(MapGeoGroups(obj.ga.all.zeros) <- character(0))
})

# Check that geos mapped to group 0 have

geos.df <- data.frame(geo=c("A", "B", "C", "D"), geo.group=c(1 ,0, 2, 1))
obj.ga <- GeoAssignment(geos.df)

test_that("zero is left unchanged", {
  expect_error(MapGeoGroups(obj.ga) <- 2:1, regexp=NA)
  expect_identical(obj.ga[[kGeoGroup]], c(2L, 0L, 1L, 2L))
  expect_error(MapGeoGroups(obj.ga) <- 2:1, regexp=NA)
  expect_identical(obj.ga[[kGeoGroup]], c(1L, 0L, 2L, 1L))
})

test_that("missing groups can be mapped to by 0", {
  expect_error(MapGeoGroups(obj.ga) <- c(0, 2), regexp=NA)
  expect_identical(obj.ga[[kGeoGroup]], c(0L, 0L, 2L, 0L))
  expect_error(MapGeoGroups(obj.ga) <- c(0, 2), regexp=NA)
  expect_identical(obj.ga[[kGeoGroup]], c(0L, 0L, 2L, 0L))
})

test_that("excluding all geos is possible", {
  expect_error(MapGeoGroups(obj.ga) <- c(0, 0), regexp=NA)
  expect_identical(obj.ga[[kGeoGroup]], c(0L, 0L, 0L, 0L))
})
