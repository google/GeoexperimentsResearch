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

context("GeoAssignment")

set.seed(1L)
n.geos.ga <- 10L
geos <- as.character(1:10L)
geo.group <- rep(1:2, length.out=n.geos.ga)
df.ga <- data.frame(geo=geos, geo.group=geo.group)

test_that("GeoAssignment() returns a GeoAssignment object", {
  expect_is(obj <- GeoAssignment(df.ga), "GeoAssignment")
  expect_is(obj, "data.frame")
})

test_that("'x' must be a data frame", {
  expect_error(GeoAssignment(as.matrix(df.ga)),
               regexp="'x' must be a data\\.frame")
})

test_that("mappings with duplicate geos are rejected", {
  geos[1L] <- geos[2L]
  geo.group <- rep(1:2, length.out=n.geos.ga)
  df.ga <- data.frame(geo=geos, geo.group=geo.group)
  df.ga[[kGeo]][1L] <- df.ga[[kGeo]][2L]
  expect_error(obj <- GeoAssignment(df.ga),
               regexp=sprintf("There is one duplicate value in column '%s'",
                              kGeo))
})

test_that("bad geos are rejected", {
  df.ga[[kGeo]] <- (as.integer(geos) + 0.1)
  expect_error(obj <- GeoAssignment(df.ga),
               regexp=sprintf("Column '%s' is not a character vector", kGeo))
  df.ga[[kGeo]] <- geos
  df.ga[[kGeo]][1L] <- NA_character_
  expect_error(obj <- GeoAssignment(df.ga),
               regexp=sprintf("Column '%s' has one missing value in row 1",
                              kGeo))
})

test_that("geos coercible to character are not rejected", {
  # Only integers and factors are coercible.
  df.ga[[kGeo]] <- as.integer(geos)
  expect_is(GeoAssignment(df.ga), "GeoAssignment")
  df.ga[[kGeo]] <- as.factor(geos)
  expect_is(GeoAssignment(df.ga), "GeoAssignment")
})

test_that("bad geo.groups are rejected", {
  df.ga[[kGeoGroup]] <- as.character(geo.group)
  expect_error(
      obj <- GeoAssignment(df.ga),
      regexp=sprintf("Column '%s' is not an integer-valued numeric vector",
                     kGeoGroup))
  df.ga[[kGeoGroup]] <- geo.group
  df.ga[[kGeoGroup]][1L] <- Inf
  expect_error(
      obj <- GeoAssignment(df.ga),
      regexp=sprintf("Column '%s' is not an integer-valued numeric vector",
                     kGeoGroup))
  df.ga[[kGeoGroup]][1L] <- (-1L)
  expect_error(
      obj <- GeoAssignment(df.ga),
      regexp=sprintf("Column '%s' has one bad value in row 1", kGeoGroup))
})

test_that("geo group 0 is allowed", {
  df.ga[[kGeoGroup]][1L] <- 0L
  expect_is(GeoAssignment(df.ga), "GeoAssignment")
})

test_that(sprintf("missing values in '%s' are accepted", kGeoGroup),  {
  df.ga[[kGeoGroup]][1L] <- NA_integer_
  expect_is(GeoAssignment(df.ga), "GeoAssignment")
})


context("ExtractGeoAssignment.GeoTimeseries")

obj.gts <- GeoTimeseries(dfged1, metrics=metric.df1)

test_that(sprintf("Error is returned by default if '%s' does not exist",
                  kGeoGroup), {
  obj.gts[[kGeoGroup]] <- NULL
  expect_error(
      ExtractGeoAssignment(obj.gts),
      regexp=sprintf("The required column '%s' is not in the data frame",
                     kGeoGroup))
})

test_that(sprintf("strict=FALSE => NULL is returned if '%s' does not exist",
                  kGeoGroup), {
  obj.gts[[kGeoGroup]] <- NULL
  expect_true(is.null(ExtractGeoAssignment(obj.gts, strict=FALSE)))
})

test_that("all possible mappings are present in the geo.assignment object", {
  dfa <- as.data.frame(unique(obj.gts[c(kGeo, kGeoGroup)]))
  dfa <- dfa[order(dfa[[kGeo]]), , drop=FALSE]
  obj1 <- GeoAssignment(dfa)
  obj2 <- ExtractGeoAssignment(obj.gts)
  expect_identical(obj1, obj2)
})


context("ExtractGeoAssignment.GeoExperimentData")

obj.gts <- GeoTimeseries(dfged1, metrics=metric.df1)

test_that("info attribute 'geo.assignment' is returned", {
  obj.ged <- obj.gts
  class(obj.ged) <- c("GeoExperimentData", class(obj.gts))
  # Make sure that ExtractGeoAssignment.GeoTimeseries is not used.
  whatever <- list(123)
  obj.ged <- SetInfo(obj.ged, geo.assignment=whatever)
  obj.ga <- ExtractGeoAssignment(obj.ged)
  expect_identical(GetInfo(obj.ged, "geo.assignment"), whatever)
})
