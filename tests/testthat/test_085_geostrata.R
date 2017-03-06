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

geos.df <- data.frame(geo=base::LETTERS[1:12], sales=12:1)  # A=highest volume.
geos <- Geos(geos.df, volume="sales")


context(".GenerateStrata")

test_that("result==rep(i, sum(group.ratios)) for each stratum i", {
  # No incomplete strata here.
  geo.group <- rep(NA_integer_, length.out=24)
  n.geos <- length(geo.group)
  for (n.groups in 2:3) {
    for (i in 1:3) {
      # Group ratios 1:1, 2:2, 3:3...
      group.ratios <- rep(i, length.out=n.groups)
      expect_is(x <- .GenerateStrata(geo.group, group.ratios=group.ratios), "integer")
      expect_equal(length(x), length(geo.group))
      expect_true(!anyNA(x))
      n.strata <- ceiling(n.geos / sum(group.ratios))
      strata <- rep(seq_len(n.strata), each=sum(group.ratios),
                    length.out=length(geo.group))
      expect_identical(x, strata)
    }
  }
})

test_that("incomplete strata are generated if necessary", {
  geo.group <- rep(NA_integer_, length.out=11)
  x <- .GenerateStrata(geo.group, group.ratios=c(1, 1))
  expect_equal(2, sum(x %in% 5))
  expect_equal(1, sum(x %in% 6))
  x <- .GenerateStrata(geo.group, group.ratios=c(2, 2))
  expect_equal(4, sum(x %in% 2))
  expect_equal(3, sum(x %in% 3))
})


context("GeoStrata")

test_that("the resulting object is of class GeoStrata", {
  expect_is(GeoStrata(geos, n.groups=2), "GeoStrata")
})

test_that("the resulting object inherits from Geos", {
  expect_is(GeoStrata(geos, n.groups=2), "Geos")
})

test_that(sprintf("the columns '%s', '%s', '%s' are preserved",
                  kGeo, kVolume, kProportion), {
  expect_is(obj <- GeoStrata(geos, n.groups=2), "Geos")
  expect_identical(obj[[kGeo]], geos[[kGeo]])
  expect_identical(obj[[kProportion]], geos[[kProportion]])
  expect_identical(obj[[kVolume]], geos[[kVolume]])
})

test_that("geos are correctly assigned to strata (2, 3, 5 groups)", {
  n.geos <- nrow(geos)
  for (n.groups in c(2L, 3L, 5L)) {
    expect_is(obj <- GeoStrata(geos, n.groups=n.groups), "GeoStrata")
    n.strata <- ceiling(n.geos / n.groups)
    strata <- rep(seq_len(n.strata), each=n.groups, length.out=nrow(obj))
    expect_identical(strata, obj[[kStratum]])
  }
})

test_that("geo.group is NA by default", {
  expect_is(obj <- GeoStrata(geos, n.groups=2), "GeoStrata")
  expect_true(kGeoGroup %in% names(obj))
  expect_true(all(is.na(obj[[kGeoGroup]])))
})

test_that("n.groups must be >= 2", {
  expect_error(obj <- GeoStrata(geos, n.groups=1))
  expect_error(obj <- GeoStrata(geos, n.groups=0))
})


test_that("group.ratios is 1,1,...by default", {
  expect_is(obj <- GeoStrata(geos, n.groups=2), "GeoStrata")
  group.ratios <- GetInfo(obj, "group.ratios")
  expect_true(length(group.ratios) == 2 && all(group.ratios == 1))
  expect_is(obj <- GeoStrata(geos, n.groups=3), "GeoStrata")
  group.ratios <- GetInfo(obj, "group.ratios")
  expect_true(length(group.ratios) == 3 && all(group.ratios == 1))
})

test_that("stratum size == sum(group.ratios)", {
  expect_is(obj <- GeoStrata(geos, n.groups=2, group.ratios=c(1, 2)),
            "GeoStrata")
  stratum.size <- sum(obj[[kStratum]] == 1)
  expect_true(stratum.size == 3)
  expect_is(obj <- GeoStrata(geos, n.groups=2, group.ratios=c(1, 3)),
            "GeoStrata")
  stratum.size <- sum(obj[[kStratum]] == 1)
  expect_true(stratum.size == 4)
})

test_that("sum(group.ratios) cannot exceed number of geos", {
  expect_error(obj <- GeoStrata(geos, n.groups=2, group.ratios=c(1, 11)),
               regexp=NA)  # No error, 1+11 <= 12 == n.geos.
  expect_error(obj <- GeoStrata(geos, n.groups=2, group.ratios=c(1, 12)))
})

test_that("all group.ratios must be >= 1", {
  expect_error(obj <- GeoStrata(geos, n.groups=2, group.ratios=c(0, 2)))
})

test_that("group.ratios must be integer", {
  expect_error(obj <- GeoStrata(geos, n.groups=2, group.ratios=c(0.5, 1)))
})

test_that("group.ratios must of length n.groups", {
  expect_error(obj <- GeoStrata(geos, n.groups=2, group.ratios=1))
  expect_error(obj <- GeoStrata(geos, n.groups=3, group.ratios=1:2))
})


context("ExtractGeoStrata.GeoTimeseries")

first.date <- as.Date("2016-09-26")
df.gts <- expand.grid(date=first.date + c(0:13),
                      geo=c("01", "02", "03", "04"))
df.gts[["sales"]] <- runif(nrow(df.gts), 0, 10)
obj.gts <- GeoTimeseries(df.gts, metrics="sales")

test_that("object is a combination of calls to ExtractGeos and GeoStrata", {
  obj.geos <- ExtractGeos(obj.gts)
  obj.geostrata <- GeoStrata(obj.geos)
  expect_identical(ExtractGeoStrata(obj.gts), obj.geostrata)
})


context("ExtractGeoStrata.GeoTimeseries")

obj.gts <- GeoTimeseries(dfged2, metrics=metric.df1)
obj.trt <- ExtractTreatmentAssignment(obj.gts)
obj.ga <- ExtractGeoAssignment(obj.gts)
obj.per <- ExtractExperimentPeriods(obj.gts)

test_that(paste("ExtractGeoStrata.GeoExperimentData is equivalent to",
          "ExtractGeoStrata.GeoTimeseries when no geos are excluded"), {
  obj.ged <- GeoExperimentData(obj.gts,
                               periods=obj.per,
                               geo.assignment=obj.ga,
                               treat.assignment=obj.trt)
  expect_identical(ExtractGeoStrata(obj.ged), ExtractGeoStrata(obj.gts))
})

test_that(paste("ExtractGeoStrata.GeoExperimentData excludes geos that were",
                "originally excluded in the assignment"), {
  excluded.geo <- obj.ga[[kGeo]][1]
  obj.ga[[kGeoGroup]][1] <- kExcludeGeoGroup
  obj.ged <- GeoExperimentData(obj.gts,
                               periods=obj.per,
                               geo.assignment=obj.ga,
                               treat.assignment=obj.trt)
  expect_error(gs <- ExtractGeoStrata(obj.ged), regexp=NA)
  expect_equal(gs[gs[[kGeo]] == excluded.geo, kGeoGroup], kExcludeGeoGroup)
})

obj.ga[[kGeoGroup]][1] <- kExcludeGeoGroup
first.date <- as.Date("2016-09-26")
df.gts <- expand.grid(date=first.date + c(0:13),
                      geo=c("01", "02", "03", "04"))
df.gts[["sales"]] <- runif(nrow(df.gts), 0, 10)
obj.gts <- GeoTimeseries(df.gts, metrics="sales")

test_that("object is a combination of calls to ExtractGeos and GeoStrata", {
  obj.geos <- ExtractGeos(obj.gts)
  obj.geostrata <- GeoStrata(obj.geos)
  expect_identical(ExtractGeoStrata(obj.gts), obj.geostrata)
})


context("CountRandomizations")

test_that("CountRandomizations works as intended on GeoStrata.", {
  gs <- GeoStrata(geos, n.groups=2)
  expect_equal(CountRandomizations(gs), rep(2, 6))

  # 6 geos, all in different strata, are pre-assigned.
  SetGeoGroup(gs) <- GeoAssignment(
      data.frame(geo=c("A", "C", "E", "G", "I", "K"),
                 geo.group=rep(1, 6)))
  expect_equal(CountRandomizations(gs), rep(1, 6))

  # 5 geos, all in different strata, are pre-assigned; one is excluded.
  SetGeoGroup(gs) <- GeoAssignment(
      data.frame(geo=c("A", "C", "E", "G", "I", "K"),
                 geo.group=c(rep(1, 5), 0)))
  expect_equal(CountRandomizations(gs), c(rep(1, 5), 2))

  # One stratum is pre-assigned to the same group.
  SetGeoGroup(gs) <- GeoAssignment(
      data.frame(geo=c("A", "B"), geo.group=c(1, 1)))
  expect_warning(CountRandomizations(gs, show.warnings=TRUE),
                 regexp="Stratum 1 is not compatible with group.ratios")
  expect_warning(CountRandomizations(gs, show.warnings=FALSE),
                 regexp=NA)

  # Two strata are fully pre-assigned, each pair to the same group.
  SetGeoGroup(gs) <- GeoAssignment(
      data.frame(geo=c("A", "B", "C", "D"), geo.group=c(1, 1, 2, 2)))
  expect_warning(CountRandomizations(gs, show.warnings=TRUE),
                 regexp="Strata 1, 2 are not compatible with group.ratios")
  expect_warning(CountRandomizations(gs, show.warnings=FALSE),
                 regexp=NA)

  # 3 groups, one stratum has 1+2+3=6 slots.
  gs <- GeoStrata(geos, n.groups=3, group.ratios=c(1, 2, 3))
  expect_equal(CountRandomizations(gs), c(60, 60))  # 6!/(3!2!1!)

  # Preassign 3 of the 6 geos in stratum 1, groups 1,2,3 are left to
  # be randomized into 3 slots.
  SetGeoGroup(gs) <- GeoAssignment(
      data.frame(geo=c("A", "B", "C"), geo.group=c(2, 3, 3)))
  expect_equal(CountRandomizations(gs), c(6, 60))  # 3!

  # Preassign 3 of the 6 geos in stratum 1, only group 3 geos are left over.
  SetGeoGroup(gs) <- GeoAssignment(
      data.frame(geo=c("A", "B", "C"), geo.group=c(1, 2, 2)))
  expect_equal(CountRandomizations(gs), c(1, 60))

  # Preassign all available group 1 and 2 geos in both strata. There are only
  # group 3 geos left over.
  SetGeoGroup(gs) <- GeoAssignment(
      data.frame(geo=c("A", "B", "C", "G", "J", "K"),
                 geo.group=c(1, 2, 2, 1, 2 ,2)))
  expect_equal(CountRandomizations(gs), c(1, 1))

  # Mess with the object to create incompatible group ids.
  gs[1, kGeoGroup] <- 100
  expect_error(CountRandomizations(gs),
               regexp=paste("Group 100 is not compatible with the",
                            "specified group.ratios"))

  gs[2, kGeoGroup] <- 10
  expect_error(CountRandomizations(gs),
               regexp=paste("Groups 100, 10 are not compatible with the",
                            "specified group.ratios"))
})


context("IsFixedRandomization")

test_that("IsFixedRandomization works as intended on GeoStrata.", {
  gs <- GeoStrata(geos, n.groups=2)
  expect_false(IsFixedRandomization(gs))

  # Preassign one geo in each stratum, all to group 1.
  SetGeoGroup(gs) <- GeoAssignment(
      data.frame(geo=c("A", "C", "E", "G", "I", "K"),
                 geo.group=rep(1, 6)))
  expect_true(IsFixedRandomization(gs))

  # Preassign one geo in 5 first strata in group 1, and exclude one geo.
  SetGeoGroup(gs) <- GeoAssignment(
      data.frame(geo=c("A", "C", "E", "G", "I", "K"),
                 geo.group=c(rep(1, 5), 0)))
  expect_false(IsFixedRandomization(gs))

  gs <- GeoStrata(geos, n.groups=3, group.ratios=c(1, 2, 3))
  expect_false(IsFixedRandomization(gs))

  # Preassign 3 of the 6 geos in stratum 1, groups 1,2,3 are left to
  # be randomized into 3 slots.
  SetGeoGroup(gs) <- GeoAssignment(
      data.frame(geo=c("A", "B", "C"), geo.group=c(2, 3, 3)))
  expect_false(IsFixedRandomization(gs))

  # Preassign 3 of the 6 geos in stratum 1, only group 3 geos are left over.
  SetGeoGroup(gs) <- GeoAssignment(
      data.frame(geo=c("A", "B", "C"), geo.group=c(1, 2, 2)))
  expect_false(IsFixedRandomization(gs))

  # Preassign 3 geos in each stratum so that only group 3 is left over.
  SetGeoGroup(gs) <- GeoAssignment(
      data.frame(geo=c("A", "B", "C", "G", "J", "K"),
                 geo.group=c(1, 2, 2, 1, 2 ,2)))
  expect_true(IsFixedRandomization(gs))

  # Mess with the object to create incompatible group ids.
  gs[1, kGeoGroup] <- 100
  expect_error(IsFixedRandomization(gs),
               regexp=paste("Group 100 is not compatible with the",
                            "specified group.ratios"))

  gs[2, kGeoGroup] <- 10
  expect_error(IsFixedRandomization(gs),
               regexp=paste("Groups 100, 10 are not compatible with the",
                            "specified group.ratios"))
})
