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

context("TreatmentAssignment")

trt1 <- data.frame(period=1L, geo.group=2L,
                   assignment=kTreatmentAssignment["change"])
rownames(trt1) <- NULL
trt2 <- data.frame(period=0:1, geo.group=1:2,
                   assignment=kTreatmentAssignment[c("none", "change")])
rownames(trt2) <- NULL
trt3 <- data.frame(period=seq_along(kTreatmentAssignment) - 1L,
                   geo.group=seq_along(kTreatmentAssignment),
                   assignment=kTreatmentAssignment)
rownames(trt3) <- NULL

test_that("a default input produces a proper TreatmentAssignment object", {
  expect_is(obj <- TreatmentAssignment(trt1), "TreatmentAssignment")
  expect_identical(as.data.frame(obj), trt1)
})

test_that("'x' must be a data frame", {
  expect_error(TreatmentAssignment(as.matrix(trt1)),
               regexp="'x' must be a data\\.frame")
})


to.test <- c(kPeriod, kGeoGroup, kAssignment)
for (column in to.test) {
  test_that(sprintf("column '%s' must exist", column), {
    trt1[[column]] <- NULL
    expect_error(TreatmentAssignment(trt1),
                 regexp=sprintf("required column '%s' is not in the data frame",
                                column))
  })
}

test_that(sprintf("'%s' must be integer-valued, with no missing values",
                  kPeriod), {
  trt1[[kPeriod]][1L] <- 0.99
  expect_error(TreatmentAssignment(trt1),
               regexp="")
  trt1[[kPeriod]][1L] <- NA_integer_
  expect_error(TreatmentAssignment(trt1),
               regexp="")
})

test_that(sprintf("'%s' must be integer-valued, with no missing values",
                  kGeoGroup), {
  trt1[[kGeoGroup]][1L] <- 0.99
  expect_error(TreatmentAssignment(trt1),
               regexp="")
  trt1[[kGeoGroup]][1L] <- NA_integer_
  expect_error(TreatmentAssignment(trt1),
               regexp="")
  trt1[[kGeoGroup]][1L] <- 1
  trt1[[kGeoGroup]] <- as.character(trt1[[kGeoGroup]])
  expect_error(TreatmentAssignment(trt1),
               regexp="")
})

test_that("'assign' must have only valid values", {
  # No error if all values are valid.
  expect_is(TreatmentAssignment(trt3), "TreatmentAssignment")
  trt3[[kAssignment]][1L] <- "1"
  expect_error(TreatmentAssignment(trt3),
               regexp="")
  trt3[[kAssignment]][1L] <- NA_integer_
  expect_error(TreatmentAssignment(trt3),
               regexp="")
  trt2[[kAssignment]] <- seq_len(nrow(trt2))
  expect_error(TreatmentAssignment(trt3),
               regexp="")
})


context("ExtractTreatmentAssignment.GeoTimeseries")

obj.gts <- GeoTimeseries(dfged2, metrics=metrics.df2)

test_that("A valid object is generated from a valid GeoTimeseries", {
  expect_is(ExtractTreatmentAssignment(obj.gts), "TreatmentAssignment")
})

to.test <- c(kPeriod, kGeoGroup, kAssignment)
for (column in to.test) {
  test_that(sprintf("Error is thrown if the required column %s does not exist",
                    column), {
    obj <- obj.gts
    obj[[column]] <- NULL
    expect_error(
        ExtractTreatmentAssignment(obj),
        regexp=sprintf("The required column '%s' is not in the data frame",
                       column))
  })
}

to.test <- c(kPeriod, kGeoGroup, kAssignment)
for (column in to.test) {
  test_that(sprintf("strict=FALSE => NULL if '%s' does not exist", column), {
    obj <- obj.gts
    obj[[column]] <- NULL
    expect_true(is.null(ExtractTreatmentAssignment(obj, strict=FALSE)))
  })
}

test_that("error is thrown if the mapping is not consistent", {
  obj <- obj.gts
  p1g1 <- which(obj[[kPeriod]] %in% 1 & obj[[kGeoGroup]] %in% 1)
  # This data frame has only '0' and '*'. Change one of them to different.
  obj[[kAssignment]][p1g1[1]] <- kTreatmentAssignment["decrease"]
  expect_error(ExtractTreatmentAssignment(obj),
               regexp=paste0(sprintf("Mapping from '%s'\\+'%s' to '%s'",
                                     kPeriod, kGeoGroup, kAssignment),
                             " is not consistent"))
})

test_that("the treatment assignments match to those in the data frame", {
  obj <- ExtractTreatmentAssignment(obj.gts)
  # Only look at those assignments that are not 'none'.
  x <- as.data.frame(obj[!obj[[kAssignment]]
                         %in% kTreatmentAssignment["none"], ])
  rownames(x) <- NULL
  expect_identical(trt1, x)
})

test_that("default treatment assignment is correct", {
  # Period 1 of Group 2 should be a 'change'.
  obj <- DefaultTreatmentAssignment()
  expect_true(nrow(obj) == 1L)
  x <- obj[[obj[[kPeriod]] %in% 1L & obj[[kGeoGroup]] %in% 2L, kAssignment]]
  expect_equal(x, kTreatmentAssignment[["change"]])
})
