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

context("Randomize.GeoStrata (default group ratio)")

# 'A' has the highest volume.
geos.df <- data.frame(geo=base::LETTERS[1:12], sales=12:1)
geos <- Geos(geos.df, volume="sales")
obj.gs <- GeoStrata(geos, n.groups=2)  # 6 strata, length 2 each.

test_that("for n.groups=2, each stratum has groups 1 and 2", {
  obj.gs <- GeoStrata(geos, n.groups=2)
  obj.ga <- Randomize(obj.gs)
  x <- rep(1:2, length.out=nrow(obj.ga))
  # Totals by geo.group are 1 + 2 = 3. No other combination produces 3.
  a <- aggregate(obj.ga[kGeoGroup], by=obj.ga[kStratum], FUN=sum)
  expect_true(all(a[[kGeoGroup]] == 3))
  map <- structure(obj.ga[[kGeoGroup]], names=obj.ga[[kGeo]])
})

test_that("Randomization should change as the function is called again", {
  set.seed(1)
  obj.ga1 <- Randomize(obj.gs)
  obj.ga2 <- Randomize(obj.gs)
  # (There is 1.5% chance (1/2^6) that the randomization is the same.)
  expect_true(!all(obj.ga1[[kGeoGroup]] == obj.ga2[[kGeoGroup]]))
})

test_that("existing assignments are taken into account", {
  SetGeoGroup(obj.gs) <- GeoAssignment(data.frame(geo=c("A", "C"), geo.group=1:2))
  obj.ga1 <- Randomize(obj.gs)
  for (i in 1:6) {
    # In repeated randomizations, the fixed mapping cannot change,
    # although the other mappings must change.
    obj.ga2 <- Randomize(obj.gs)
    map <- structure(obj.ga2[[kGeoGroup]], names=obj.ga2[[kGeo]])
    expect_true(map["A"] == 1 && map["C"] == 2)
    expect_true(!all(obj.ga1[[kGeoGroup]] == obj.ga2[[kGeoGroup]]))
    obj.ga1 <- obj.ga2
  }
})

test_that("the other geos in a strata cannot take the preassigned group id", {
  SetGeoGroup(obj.gs) <- GeoAssignment(data.frame(geo=c("A"), geo.group=2))
  # "B" should be always geo.group==1 since "A" is fixed at geo.group==2.
  n <- 10
  for (i in seq_len(n)) {
    obj.ga <- Randomize(obj.gs)
    map <- structure(obj.ga[[kGeoGroup]], names=obj.ga[[kGeo]])
    expect_true(map["A"] == 2 && map["B"] == 1)
  }
})

test_that("it is possible to have all assignments pre-assigned", {
  obj.gs[[kGeoGroup]] <- 1L
  for (i in 1:6) {
    # In repeated randomizations, the mapping cannot change.
    obj.ga <- Randomize(obj.gs)
    expect_true(all(obj.ga[[kGeoGroup]] == 1L))
  }
})

test_that("group id can be any of 1:n.groups if preassignment==0 (omit geo)", {
  SetGeoGroup(obj.gs) <- GeoAssignment(data.frame(geo=c("A"), geo.group=0))
  # "B" should be assigned group 1 or 2 since "A" maps neither to 1 nor to 2.
  n <- 6
  b <- integer(n)
  for (i in seq_len(n)) {
    obj.ga <- Randomize(obj.gs)
    map <- structure(obj.ga[[kGeoGroup]], names=obj.ga[[kGeo]])
    b[i] <- map["B"]
  }
  expect_true(1L %in% b && 2L %in% b)
})

test_that("incomplete strata can be assigned any of the group numbers", {
  n.groups <- 5
  obj.gs <- GeoStrata(geos, n.groups=n.groups)  # 3 strata, lengths 5, 5, 2.
  all.groups <- seq_len(n.groups)
  groups <- integer(0)
  set.seed(1)
  for (i in 1:5) {
    obj.ga <- Randomize(obj.gs)
    # Last stratum is incomplete, of size 2.
    geo.groups.stratum <- obj.ga[[kGeoGroup]][obj.ga[[kStratum]] %in% 3]
    groups <- c(groups, geo.groups.stratum)
  }
  expect_true(all(all.groups %in% groups))
})


context("Randomize.GeoStrata (non-default group ratio)")

test_that("for ratio 1:2 each stratum has 1x group 1 & 2x group 2", {
  obj.gs <- GeoStrata(geos, n.groups=2, group.ratio=c(1, 2))
  obj.ga <- Randomize(obj.gs)
  x <- rep(1:2, length.out=nrow(obj.ga))
  # Totals by geo.group are 1 + 2 + 2 = 5.
  a <- aggregate(obj.ga[kGeoGroup], by=obj.ga[kStratum], FUN=sum)
  expect_true(all(a[[kGeoGroup]] == 5))
  expect_equal(4, sum(obj.ga[[kGeoGroup]] %in% 1))
  expect_equal(8, sum(obj.ga[[kGeoGroup]] %in% 2))
})

test_that("for ratio 2:1 each stratum has 2x group 1 & 1x group 2", {
  obj.gs <- GeoStrata(geos, n.groups=2, group.ratio=c(2, 1))
  obj.ga <- Randomize(obj.gs)
  # Totals by geo.group are 1 + 1 + 2 = 4.
  a <- aggregate(obj.ga[kGeoGroup], by=obj.ga[kStratum], FUN=sum)
  expect_true(all(a[[kGeoGroup]] == 4))
  expect_equal(8, sum(obj.ga[[kGeoGroup]] %in% 1))
  expect_equal(4, sum(obj.ga[[kGeoGroup]] %in% 2))
})

test_that("for ratio 3:3 each stratum has 3x group 1 & 3x group 2", {
  obj.gs <- GeoStrata(geos, n.groups=2, group.ratio=c(3, 3))
  obj.ga <- Randomize(obj.gs)
  # Totals by geo.group are 1 + 1 + 1 + 2 + 2 + 2 = 9.
  a <- aggregate(obj.ga[kGeoGroup], by=obj.ga[kStratum], FUN=sum)
  expect_true(all(a[[kGeoGroup]] == 9))
  expect_equal(6, sum(obj.ga[[kGeoGroup]] %in% 1))
  expect_equal(6, sum(obj.ga[[kGeoGroup]] %in% 2))
})

test_that("incomplete strata can have any of the group numbers", {
  # Check that the randomization method doesn't mistakenly fix the geo
  # assignment of the incomplete final stratum.
  obj.gs <- GeoStrata(geos, n.groups=2, group.ratio=c(2, 3))
  # Last stratum (3) is of size 2.
  set.seed(1)
  obj.ga <- Randomize(obj.gs)
  geo.groups.stratum <- obj.ga[[kGeoGroup]][obj.ga[[kStratum]] %in% 3]
  expect_equal(2, length(geo.groups.stratum))  # Double-check.
  # In this particular randomization, both are 1.
  expect_true(all(geo.groups.stratum %in% 1))
  obj.ga <- Randomize(obj.gs)
  obj.ga <- Randomize(obj.gs)
  geo.groups.stratum <- obj.ga[[kGeoGroup]][obj.ga[[kStratum]] %in% 3]
  # The randomization has changed: now, also group 2 is there.
  expect_true(2 %in% geo.groups.stratum)
})
