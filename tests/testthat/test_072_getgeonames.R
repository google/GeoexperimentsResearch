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

context("GetGeoNames")

unique.geos <- c("G11", "G21", "G12", "G22")
df.ga <- structure(data.frame(unique.geos,
                              c(1, 2, 1, 2)),
                   names=c(kGeo, kGeoGroup))
obj.ga <- GeoAssignment(df.ga)

first.date <- as.Date("2016-09-26")
df.gts <- expand.grid(date=first.date + c(0:13),
                      geo=unique.geos)
df.gts[["sales"]] <- abs(rnorm(nrow(df.gts)))
obj.gts <- GeoTimeseries(df.gts, metrics="sales")

obj.ged <- GeoExperimentData(obj.gts, periods=NULL,
                             geo.assignment=obj.ga,
                             treat.assignment=NULL)
df.geos <- data.frame(geo=unique.geos, sales=seq_along(unique.geos))
names(df.geos)[1] <- kGeo
geos <- Geos(df.geos, volume="sales")

test_that("'groups' must be a valid geo group number", {
  # The test should be done in the dispatcher.
  expect_error(GetGeoNames(NULL, groups="1"))
  expect_error(GetGeoNames(NULL, groups="all"))
  expect_error(GetGeoNames(NULL, groups=integer(0)))
  expect_error(GetGeoNames(NULL, groups=FALSE))
})

context("GetGeoNames.Geos")

test_that("an unique list of geos is returned", {
  expect_is(GetGeoNames(geos), "character")
  expect_true(setequal(GetGeoNames(geos), unique.geos))
})

test_that("the geos are in sorted order", {
  expect_identical(GetGeoNames(geos), sort(unique.geos))
})

test_that("an error is thrown if groups!=NULL", {
  expect_error(GetGeoNames(geos, groups=1),
               regexp="No geo group available")
  obj.gts[[kGeoGroup]] <- 1L
  expect_error(GetGeoNames(geos, groups=1),
               regexp="No geo group available")
})

context("GetGeoNames.GeoTimeseries")

test_that("an unique list of geos is returned", {
  expect_is(GetGeoNames(obj.gts), "character")
  expect_true(setequal(GetGeoNames(obj.gts), unique.geos))
})

test_that("the geos are in sorted order", {
  expect_identical(GetGeoNames(obj.gts), sort(unique.geos))
})

test_that("an error is thrown if groups!=NULL", {
  expect_error(GetGeoNames(obj.gts, groups=1),
               regexp="No geo group available")
  obj.gts[[kGeoGroup]] <- 1L
  expect_error(GetGeoNames(obj.gts, groups=1),
               regexp="No geo group available")
})

context("GetGeoNames.GeoExperimentData")

test_that("an unique list of geos is returned", {
  expect_true(setequal(GetGeoNames(obj.ged), unique.geos))
})

test_that("the geos are in sorted order", {
  expect_identical(GetGeoNames(obj.ged), sort(unique.geos))
})

test_that("'groups' selects the right set of geos", {
  expect_identical(GetGeoNames(obj.ged, groups=1),
                   df.ga[[kGeo]][df.ga[[kGeoGroup]] %in% 1])
  expect_identical(GetGeoNames(obj.ged, groups=2),
                   df.ga[[kGeo]][df.ga[[kGeoGroup]] %in% 2])
})

test_that("a nonexisting 'groups' returns an empty set of geos", {
  expect_identical(GetGeoNames(obj.ged, group=3), character(0))
})

test_that("error is thrown if geo assignment doesn't exist & groups != NULL", {
  obj.ged2 <- SetInfo(obj.ged, geo.assignment=NULL)
  expect_error(GetGeoNames(obj.ged2, group=1),
               regexp="Cannot match groups: there is no geo assignment")
})

test_that("'groups' can be NA", {
  expect_error(GetGeoNames(obj.ged, groups=NA),
               regexp=NA)
})

context("GetGeoNames.GeoAssignment")

test_that("an unique list of geos is returned by default", {
  expect_is(GetGeoNames(obj.ga), "character")
  expect_true(setequal(GetGeoNames(obj.ga), obj.ga[[kGeo]]))
})

test_that("the geos are in sorted order", {
  expect_identical(GetGeoNames(obj.ga), sort(obj.ga[[kGeo]]))
})

test_that("geos can be extracted by group", {
  expect_identical(GetGeoNames(obj.ga, group=1), c("G11", "G12"))
  expect_identical(GetGeoNames(obj.ga, group=2), c("G21", "G22"))
})

test_that("nonexisting group reference yields an empty set of geos", {
  expect_identical(GetGeoNames(obj.ga, group=3), character(0))
})


context("GetGeoNames.GeoStrata")

geo.strata <- ExtractGeoStrata(obj.ged)

test_that("an unique list of geos is returned by default", {
  expect_is(GetGeoNames(geo.strata), "character")
  expect_true(setequal(GetGeoNames(geo.strata), geo.strata[[kGeo]]))
})

test_that("the geos are in sorted order", {
  expect_identical(GetGeoNames(geo.strata), sort(geo.strata[[kGeo]]))
})

test_that("geo assignments that are fixed can be extracted by group", {
  SetGeoGroup(geo.strata) <- obj.ga
  expect_identical(GetGeoNames(geo.strata, group=1), c("G11", "G12"))
  expect_identical(GetGeoNames(geo.strata, group=2), c("G21", "G22"))
})

test_that("geos can be extracted by group, including NA", {
  # Assign G11 to group 1, others remain NA.
  SetGeoGroup(geo.strata) <- GeoAssignment(data.frame(geo="G11", geo.group=1))
  expect_identical(GetGeoNames(geo.strata, group=1), "G11")
  expect_identical(GetGeoNames(geo.strata, group=2), character(0))
  expect_identical(GetGeoNames(geo.strata, group=NA),
                   sort(setdiff(unique.geos, "G11")))
})

test_that("nonexisting group reference yields an empty set of geos", {
  expect_identical(GetGeoNames(geo.strata, group=3), character(0))
})
