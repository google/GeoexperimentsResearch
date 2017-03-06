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

context("CheckThat")

test_that("CheckThat rejects bad arguments", {
  xyz <- 0
  expect_error(CheckThat(is.string(xyz), name=character(0)),
               regexp="name is not NULL or name is not a nonempty string")
  expect_error(CheckThat(is.string(xyz), name=1),
               regexp="name is not NULL or name is not a nonempty string")
})

test_that("CheckThat produces the custom message", {
  xyz <- 0
  expect_error(CheckThat(is.string(xyz), name="Foo"),
               regexp="Foo is not a string")
  expect_error(CheckThat(is.string(xyz)),
               regexp="xyz is not a string")
  expect_true(CheckThat(is.string("xyz")))
})


context("CheckForAllTrue")

test_that("CheckForAllTrue outputs error messages for bad arguments", {
  xyz <- (1:10)
  xyz <- (xyz <= 5L)
  expect_error(CheckForAllTrue(1, "=$x="),
               regexp="x is not a logical vector")
  expect_error(CheckForAllTrue(xyz, "=$x="),
               regexp="names\\(x\\) is not a vector of nonempty strings")
  names(xyz) <- base::letters[seq_along(xyz)]
  names(xyz)[2L] <- names(xyz)[1L]
  expect_error(CheckForAllTrue(xyz, "=$x="),
               regexp="names\\(x\\) has duplicate values")
})

test_that("CheckForAllTrue outputs the custom error message", {
  xyz <- (1:10)
  xyz <- (xyz <= 5L)
  names(xyz) <- base::letters[seq_along(xyz)]
  expect_error(CheckForAllTrue(xyz, "=$w="), "=6, 7, 8, 9, 10=")
  expect_error(CheckForAllTrue(xyz, "=$x="), "=f, g, h, i, j=")
})


context("CheckForMissingColumns")

test_that("CheckForMissingColumns outputs error messages for bad arguments", {
  dfx <- data.frame(a=1:2, b=1:2)
  for (bad in list("", NA, c("a", ""))) {
    expect_error(CheckForMissingColumns(bad, dataframe=dfx),
                 regexp=paste0("'bad' must consist of non-empty strings with ",
                     "no missing values"))
  }
})

test_that("CheckForMissingColumns returns TRUE for columns in the data frame", {
  dfx <- data.frame(a=1:2, b=1:2)
  expect_true(CheckForMissingColumns("a", dataframe=dfx))
  expect_true(CheckForMissingColumns("b", dataframe=dfx))
  expect_true(CheckForMissingColumns(names(dfx), dataframe=dfx))
})

test_that("CheckForMissingColumns outputs the custom error message", {
  dfx <- data.frame(a=1:2, b=c(1, NA))
  for (bad in list("c", c("a", "c"))) {
    expect_error(CheckForMissingColumns(bad, dataframe=dfx),
                 regexp="The specified column 'c' is not in the data frame")
  }
  bad <- c("c", "d")
  expect_error(CheckForMissingColumns(bad, dataframe=dfx),
               regexp=paste0("The specified columns 'c', 'd' are ",
                   "not in the data frame"))
})


context("CheckForBadValues")

test_that("CheckForBadValues outputs error messages for bad arguments", {
  dfx <- data.frame(a=1:2, b=1:2)
  dfx[["b"]][2L] <- NA_integer_
  expect_error(CheckForBadValues(dfx, columns="x"))
  #regexp=paste0("Column 'a' has one missing value in row 2"))
})

test_that("CheckForBadValues returns TRUE if no failure occurs", {
  dfx <- data.frame(a=1:2, b=1:2)
  expect_true(CheckForBadValues(dfx, columns="a", CHECK=is.na, good=FALSE,
                                what="missing"))
})

test_that("CheckForBadValues outputs the special error message", {
  dfx <- data.frame(a=1:2, b=1:2, c=c("", "foo"))
  dfx[["b"]][2L] <- NA_integer_
  expect_error(CheckForBadValues(dfx, columns="b", CHECK=is.na, good=FALSE,
                                 what="missing"),
               regexp=paste0("Column 'b' has one missing value in row 2"))
  expect_error(CheckForBadValues(dfx, columns="c", CHECK=is.empty.string,
                                 good=FALSE, what="empty"),
               regexp=paste0("Column 'c' has one empty value in row 1"))
})


context("CheckForTypes")

df.cft <- data.frame(a=1:2, b=1:2 + 0.1, c=c("", "foo"))

test_that("CheckForTypes outputs error messages for bad arguments", {
  for (cl in list(NULL, 1, list(), list(1))) {
    expect_error(CheckForTypes(df.cft, checklist=cl),
                 regexp="checklist is not a named list")
  }
})

test_that("CheckForTypes returns TRUE if no failure occurs", {
  cl <- list(a=is.integer.valued, b=is.numeric.vector, c=is.character.vector)
  expect_true(CheckForTypes(df.cft, checklist=cl))
})

test_that("CheckForTypes outputs the special error message", {
  cl <- list(a=is.integer.valued, b=is.numeric.vector, c=is.nonempty.string)
  expect_error(CheckForTypes(df.cft, checklist=cl),
               regexp=paste0("Column 'c' is not a nonempty string"))
})


context("CheckForDuplicateRows")

df.cfdv <- data.frame(a=1:3, b=1:3, c=c("", "foo", "bar"))

test_that("CheckForDuplicateRows outputs error messages for bad arguments", {
  for (cl in list(NULL, 1, "", c(NA_character_), c(NA_character_, "a"))) {
    expect_error(CheckForDuplicateRows(df.cfdv, columns=cl),
                 regexp="columns is not a vector of nonempty strings")
  }
})

test_that("CheckForDuplicateRows returns TRUE if no failure occurs", {
  for (col in c("a", "b")) {
    expect_true(CheckForDuplicateRows(df.cfdv, columns="a"))
  }
})

test_that("CheckForDuplicateRows outputs the special error message", {
  df.cfdv[2L, "a"] <- 1L
  expect_error(CheckForDuplicateRows(df.cfdv, columns="a"),
               regexp="There is one duplicate value in column 'a' in row 2")
  df.cfdv[2L, "b"] <- 1L
  expect_error(CheckForDuplicateRows(df.cfdv, columns=c("a", "b")),
               regexp="There is one duplicate a\\+b combination in row 2")
})


context("CheckForMapping")

df.map <- data.frame(a=1:3, b=1:3, c=c("", "foo", "bar"))

test_that("CheckForMapping outputs error messages for bad arguments", {
  expect_error(CheckForMapping(1, from="a", to="b"),
               regexp="x is not a data frame")
  expect_error(CheckForMapping(df.map, from="a", to=""),
               regexp="to is not a vector of nonempty strings")
  expect_error(CheckForMapping(df.map, from="", to="b"),
               regexp="from is not a vector of nonempty strings")
  expect_error(CheckForMapping(df.map, from="a", to="a"),
               regexp="'from' and 'to' must not have common elements")
})

test_that("CheckForMapping returns TRUE if no failure occurs", {
  expect_true(CheckForMapping(df.map, from="a", to="b"))
  expect_true(CheckForMapping(df.map, from=c("a", "b"), to="c"))
})

test_that("CheckForMapping outputs the special error message", {
  df.map[["a"]] <- c(1, 1, 2)
  df.map[["b"]] <- c(1, 2, 3)
  expect_error(CheckForMapping(df.map, from="a", to="b"),
               regexp="Mapping from 'a' to 'b' is not consistent")
  expect_true(CheckForMapping(df.map, from=c("a", "b"), to="c"))
  df.map[["a"]] <- c(1, 1, 2)
  df.map[["b"]] <- c(1, 1, 2)
  df.map[["c"]] <- c("x", "x", "z")
  expect_true(CheckForMapping(df.map, from=c("a", "b"), to="c"))
  df.map[["c"]] <- c("x", "y", "z")
  expect_error(CheckForMapping(df.map, from=c("a", "b"), to="c"),
               regexp="Mapping from 'a'\\+'b' to 'c' is not consistent")
})
