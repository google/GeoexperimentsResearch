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

context("GeoExperimentPreanalysisData")

first.date <- as.Date("2016-09-26")
n.days <- 28L
n.geos <- 4L
all.geos <- c("01", "02", "03", "04")
df.gts <- expand.grid(date=first.date + seq(from=0, to=n.days - 1),
                      geo=all.geos)
df.gts[["sales"]] <- runif(nrow(df.gts), min=0, max=100)
obj.gts <- GeoTimeseries(df.gts, metrics="sales")
df.ga <- data.frame(geo=all.geos, 1L + (seq_along(all.geos) %% 2L))
names(df.ga) <- c(kGeo, kGeoGroup)
obj.ga <- GeoAssignment(df.ga)

period.lengths <- c(14L, 7L, 7L)

test_that("data set with exactly the length of experiment has i.max==1", {
  expect_is(obj <- GeoExperimentPreanalysisData(obj.gts,
                                                period.lengths=period.lengths,
                                                geos=obj.ga,
                                                recycle=FALSE),
            "GeoExperimentPreanalysisData")
  expect_is(obj, "GeoExperimentData")
  expect_equal(1, GetInfo(obj, "i.max"))
  extracted.ga <- ExtractGeoAssignment(obj)
  expect_identical(extracted.ga, obj.ga)
})

test_that("can plug in a GeoStrata object", {
  geo.strata <- ExtractGeoStrata(obj.gts)
  obj <- GeoExperimentPreanalysisData(obj.gts, period.lengths=period.lengths,
                                      geos=geo.strata, recycle=TRUE)
  expect_true(all(is.na(obj[[kGeoGroup]])))
  expect_equal(geo.strata, GetInfo(obj, "geos"))
})

test_that("geos must be specified", {
  expect_error(GeoExperimentPreanalysisData(obj.gts,
                                            period.lengths=period.lengths,
                                            recycle=TRUE))
  expect_error(GeoExperimentPreanalysisData(obj.gts,
                                            period.lengths=period.lengths,
                                            geos=NULL, recycle=TRUE))
})


test_that("[recycle=TRUE] the data set is doubled", {
  obj <- GeoExperimentPreanalysisData(obj.gts, period.lengths=period.lengths,
                                      geos=obj.ga, recycle=TRUE)
  n <- (nrow(obj.gts) * 2 - n.geos)  # Last date is omitted.
  expect_equal(n, nrow(obj))
  expect_equal(28, GetInfo(obj, "i.max"))
})


test_that("[recycle=TRUE] the weekdays match and the gap <= 7 days", {
  first.weekday <- as.integer(strftime(first.date, format="%u"))
  # Create data sets with different ending weekdays.
  for (weekday.diff in c(0, -2, 2, 7, -7)) {
    n <- (n.days + 7 + weekday.diff)
    df.gts <- expand.grid(date=first.date + seq(from=0, to=n - 1),
                          geo=c("01", "02", "03", "04"))
    df.gts[["sales"]] <- 0
    obj.gts <- GeoTimeseries(df.gts, metrics="sales")
    date.range <- range(obj.gts[[kDate]])
    weekday.range <- as.integer(strftime(date.range, format="%u"))
    obj <- GeoExperimentPreanalysisData(obj.gts, period.lengths=period.lengths,
                                        geos=obj.ga, recycle=TRUE)
    next.date <- obj[[kDate]][obj[[kDate]] > date.range[2]][1]
    expect_true(date.range[1] != next.date)
    expect_equal(strftime(next.date, format="%u"),
                 strftime(date.range[1], format="%u"))
    expect_true(next.date - date.range[2] <= 7)
  }
})

test_that("cooldown period can be omitted by setting it to length 0", {
  pl <- c(14L, 7L, 0L)
  expect_error(obj <- GeoExperimentPreanalysisData(obj.gts, period.lengths=pl,
                                                   geos=obj.ga, recycle=TRUE),
               regexp=NA)
  expect_identical(pl[-3], GetInfo(obj, "period.lengths"))
})

test_that("cooldown period can be omitted by giving only 2 period lengths", {
  pl <- c(14L, 7L)
  expect_error(obj <- GeoExperimentPreanalysisData(obj.gts, period.lengths=pl,
                                                   geos=obj.ga, recycle=TRUE),
               regexp=NA)
  expect_identical(pl, GetInfo(obj, "period.lengths"))
})


context("SimulateGeoExperimentData.GeoExperimentPreanalysisData")

obj <- GeoExperimentPreanalysisData(obj.gts, period.lengths=period.lengths,
                                    geos=obj.ga, recycle=TRUE)

test_that("object does not inherit from GeoExperimentPreanalysisData", {
  expect_error(obj.ged <- SimulateGeoExperimentData(obj), regexp=NA)
  expect_equal(class(obj.ged),
               setdiff(class(obj), "GeoExperimentPreanalysisData"))
})

test_that("period lengths are correct", {
  expect_is(obj.ged <- SimulateGeoExperimentData(obj), "GeoExperimentData")
  obj.per <- ExtractExperimentPeriods(obj.ged)
  expect_identical(period.lengths, obj.per[["Length"]])
})

test_that("period names are correct", {
  expect_is(obj.ged <- SimulateGeoExperimentData(obj), "GeoExperimentData")
  obj.per <- GetInfo(obj.ged, "periods")
  expect_identical(obj.per[["Name"]], c("Pretest", "Test", "Cooldown"))
})

test_that("geo assignment is not empty, and it matches the original", {
  expect_is(obj.ged <- SimulateGeoExperimentData(obj), "GeoExperimentData")
  expect_true(!all(is.na(obj.ged[[kGeoGroup]])))
  extracted.ga <- ExtractGeoAssignment(obj.ged)
  expect_identical(extracted.ga, obj.ga)
})

test_that("a geoStrata object, when specified, is used for randomization", {
  geo.strata <- ExtractGeoStrata(obj.gts)
  obj <- GeoExperimentPreanalysisData(obj.gts, period.lengths=period.lengths,
                                      geos=geo.strata, recycle=TRUE)
  # Check that the obtained randomization is the same as that from a call to
  # 'Randomize'.
  set.seed(1)
  map1 <- GetGeoGroup(Randomize(geo.strata))
  set.seed(1)
  expect_is(obj.ged <- SimulateGeoExperimentData(obj, i=1), "GeoExperimentData")
  map2 <- GetGeoGroup(ExtractGeoAssignment(obj.ged))
  expect_equal(map2, map1[names(map2)])
})

test_that("when i is not NA, the data set # is not drawn at random", {
  expect_is(obj.ged1 <- SimulateGeoExperimentData(obj, i=1),
            "GeoExperimentData")
  expect_is(obj.ged2 <- SimulateGeoExperimentData(obj, i=1),
            "GeoExperimentData")
  expect_identical(obj.ged1, obj.ged2)
})

test_that("i must be positive and it cannot exceed i.max", {
  expect_error(SimulateGeoExperimentData(obj, i=-1))
  expect_error(SimulateGeoExperimentData(obj, i=0))
  i.max <- GetInfo(obj, "i.max")
  expect_error(SimulateGeoExperimentData(obj, i=i.max),
               regexp=NA)
  expect_error(SimulateGeoExperimentData(obj, i=i.max + 1),
               regexp=paste0("i must be >=1 and <= ", i.max))
})

test_that("no cooldown period exists if its length is set to 0", {
  pl <- c(14, 7, 0)
  expect_error(obj <- GeoExperimentPreanalysisData(obj.gts, period.lengths=pl,
                                                   geos=obj.ga, recycle=TRUE),
               regexp=NA)
  expect_error(obj.ged <- SimulateGeoExperimentData(obj), regexp=NA)
  obj.per <- GetInfo(obj.ged, "periods")
  expect_identical(obj.per[["Period"]], 0:1)
  expect_identical(obj.per[["Name"]], c("Pretest", "Test"))
  expect_equal(obj.per[["Length"]], pl[1:2])
})

test_that("no cooldown period exists if it is omitted", {
  pl <- c(14, 7)
  expect_error(obj <- GeoExperimentPreanalysisData(obj.gts, period.lengths=pl,
                                                   geos=obj.ga, recycle=TRUE),
               regexp=NA)
  expect_error(obj.ged <- SimulateGeoExperimentData(obj), regexp=NA)
  obj.per <- GetInfo(obj.ged, "periods")
  expect_identical(obj.per[["Period"]], 0:1)
  expect_identical(obj.per[["Name"]], c("Pretest", "Test"))
  expect_equal(obj.per[["Length"]], pl)
})
