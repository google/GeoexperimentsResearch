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

context("plot.GeoTimeseries")

obj <- GeoTimeseries(dfged2, metrics=metrics.df2)
sales <- metrics.df2[1L]
obj.ged <- GeoExperimentData(obj)

test_that("by default y == first metric", {
  p1 <- plot(obj)
  p2 <- plot(obj, y=metrics.df2[1L])
  expect_equal(p1, p2)
})

test_that("error is thrown if y is not a vector of nonempty strings", {
  expect_error(plot(obj, y=""))
  expect_error(plot(obj, y=NA_character_))
  expect_error(plot(obj, y=character(0)))
  expect_error(plot(obj, y=1))
})

test_that("error is thrown if 'subset' is not a vector or NULL", {
  expect_error(plot(obj, subset=NA_character_),
               regexp="No rows selected")
  expect_error(plot(obj, subset=""),
               regexp="No rows selected")
  expect_error(plot(obj, subset=base::mean))
  expect_is(plot(obj, subset=NULL), "ggplot")
})

test_that("'subset' selects subsets correctly", {
  p1 <- plot(obj, subset=1)
  s1 <- Subset(obj, obj[[kGeo]] %in% 1)
  expect_equal(p1[["data"]][[sales]], s1[[sales]])
  # Try the same with GeoExperimentData objects, column kGeoGroup.
  p2 <- plot(obj.ged, by=kGeoGroup, subset=1)
  obj.ged.sub <- Subset(obj.ged, obj.ged[[kGeoGroup]] %in% 1)
  a2 <- aggregate(obj.ged.sub, by=c(kDate, kGeoGroup))
  expect_equal(p2[["data"]][[sales]], a2[[sales]])
})

test_that("no error is thrown if 'subset' has integers matching the geos", {
  expect_is(plot(obj, subset=1L), "ggplot")
})

test_that("'aggregate' works", {
  choose.geos <- c(1:3, 10)
  p1 <- plot(obj, by=kGeo, subset=choose.geos, aggregate=TRUE)
  a1 <- aggregate(Subset(obj, obj[[kGeo]] %in% choose.geos), by=kDate)
  expect_equal(p1[["data"]][[sales]], a1[[sales]])
})

test_that("'title' works", {
  title <- "some string"
  p1 <- plot(obj, title=title)
  expect_identical(p1[["labels"]][["title"]], title)
})


context("plot.Geos")

obj <- GeoTimeseries(dfged2, metrics=metrics.df2)
geos <- ExtractGeos(obj)

test_that("by default y == kVolume", {
  p1 <- plot(geos)
  p2 <- plot(geos, y=kVolume)
  expect_equal(p1, p2)
})

test_that("error is thrown if 'y' is not a proper column name", {
  expect_error(plot(geos, y=""),
               regexp="'y' must be the name of a column in the data frame")
  expect_error(plot(geos, y="foo"),
               regexp="'y' must be the name of a column in the data frame")
  expect_error(plot(geos, y=character(0)),
               regexp="'y' must be the name of a column in the data frame")
  expect_error(plot(geos, y=1),
               regexp="'y' must be the name of a column in the data frame")
})

test_that("error is thrown if 'y' is not numerical", {
  expect_error(plot(geos, y=kGeo),
               regexp=paste("'y' must be the name of numeric column",
                            "in the data frame"))
})
