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

context("SetInfo")

test_that("SetInfo rejects 'obj' that are not objects", {
  info <- list(a=1L, b=2L)
  obj <- list(foo=1)
  # 'obj' must not be a plain list or other non-object.
  expect_error(SetInfo(obj, info=info),
               regexp="is\\.object\\(obj\\) is not TRUE")
  # 'obj' can be any object.
  obj <- as.data.frame(obj)
  expect_is(SetInfo(obj, info=info), "data.frame")
})

test_that("SetInfo rejects 'info' that are not plain, named lists", {
  obj <- data.frame(foo=1)
  expect_error(SetInfo(obj, info=data.frame(a=1)),
               regexp="'info' is not a named list or an empty list")
  expect_error(SetInfo(obj, info=list(1)),
               regexp="'info' is not a named list or an empty list")
})

test_that("SetInfo changes the 'info' attribute as expected", {
  info <- list(a=1L, b=2L)
  x <- data.frame(x=1)
  obj <- SetInfo(x, info=info)
  expect_is(obj, "data.frame")
  expect_identical(attr(obj, "info"), info)
  info2 <- list(c=3L, d=4L)
  obj2 <- SetInfo(obj, info=info2)
  expect_identical(attr(obj2, "info"), info2)
})

test_that("SetInfo changes key-value pairs of the 'info' attribute", {
  x <- data.frame(x=1)
  attr(x, "info") <- list(a=1L, b=2L)
  obj <- SetInfo(x, a=0L)
  expect_identical(attr(obj, "info"), list(a=0L, b=2L))
  obj <- SetInfo(x, a=3L, b=4L)
  expect_identical(attr(obj, "info"), list(a=3L, b=4L))
})

test_that("SetInfo creates a new 'info' if it does not exist", {
  x <- data.frame(x=1)
  obj <- SetInfo(x, a=0L)
  expect_identical(attr(obj, "info"), list(a=0L))
})

test_that("SetInfo creates a new key if it does not exist", {
  x <- data.frame(x=1)
  attr(x, "info") <- list(a=1L, b=2L)
  obj <- SetInfo(x, c=3L)
  expect_identical(attr(obj, "info"), list(a=1L, b=2L, c=3L))
})


context("GetInfo")

test_that("GetInfo retrieves the 'info' attribute if no key is specified", {
  info <- list(a=1L, b=2L)
  x <- data.frame(x=1)
  attr(x, "info") <- info
  expect_identical(GetInfo(x), info)
})

test_that("GetInfo retrieves the value by key", {
  info <- list(a=1L, b=2L)
  x <- data.frame(x=1)
  attr(x, "info") <- info
  expect_identical(GetInfo(x, "a"), 1L)
})

test_that("GetInfo throws an error if the key does not exist", {
  info <- list(a=1L, b=2L)
  x <- data.frame(x=1)
  attr(x, "info") <- info
  expect_error(GetInfo(x, "c"),
               regexp=paste0("Field 'c' does not exist in this object of ",
                   "class 'data.frame'"))
})
