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

context("ConcatItems")

test_that("missing 'x' or non-atomic 'x' or NULL gives an error", {
  expect_error(ConcatItems())
  expect_error(ConcatItems(list()))
  expect_error(ConcatItems(list(1, 2)))
  expect_error(ConcatItems(NULL))
})

test_that("bad values for arguments cause an error", {
  expect_error(ConcatItems(base::letters, quote=NA))
  expect_error(ConcatItems(base::letters, quote=NA, collapse=NA))
  expect_error(ConcatItems(base::letters[1:5], max.output=0L))
  expect_error(ConcatItems(base::letters[1:5], max.output=NA))
  expect_error(ConcatItems(base::letters[1:5], max.output=1:2))
})


test_that("length zero 'x' returns an empty string", {
  expect_identical(ConcatItems(character(0)), "")
  expect_identical(ConcatItems(integer(0)), "")
})

test_that("default case works", {
  expect_identical(ConcatItems(base::letters[1:3]), "'a', 'b', 'c'")
})

test_that("'x' can be non-character", {
  expect_identical(ConcatItems(1:2), "'1', '2'")
})

test_that("'quote' works as expected", {
  expect_identical(ConcatItems(1:2), "'1', '2'")
  expect_identical(ConcatItems(1:2, quote=""), "1, 2")
  expect_identical(ConcatItems(1:2, quote="\""), '"1", "2"')
})

test_that("'collapse' works as expected", {
  expect_identical(ConcatItems(base::letters[1:3], collapse=":"),
                   "'a':'b':'c'")
  expect_identical(ConcatItems(base::letters[1:3], collapse=""),
                   "'a''b''c'")
})

test_that("'quote' and 'collapse' work together", {
  expect_identical(ConcatItems(base::letters[1:3], quote="", collapse=":"),
                   "a:b:c")
  expect_identical(ConcatItems(base::letters[1:3], quote=",", collapse="'"),
                   ",a,',b,',c,")
  expect_identical(ConcatItems(base::letters[1:3], quote="", collapse=""),
                   "abc")
})

test_that("'max.output' works as expected", {
  expect_identical(ConcatItems(base::letters[1:5], quote="", max.output=Inf),
                   "a, b, c, d, e")
  expect_identical(ConcatItems(base::letters[1:5], quote="", max.output=6L),
                   "a, b, c, d, e")
  expect_identical(ConcatItems(base::letters[1:5], quote="", max.output=5L),
                   "a, b, c, d, e")
  expect_identical(ConcatItems(base::letters[1:5], quote="", max.output=4L),
                   "a, b, c, d, e")
  expect_identical(ConcatItems(base::letters[1:5], quote="", max.output=3L),
                   "a, b, c, ...")
  expect_identical(ConcatItems(base::letters[1:5], quote="", max.output=2L),
                   "a, b, ...")
  expect_identical(ConcatItems(base::letters[1:5], quote="", max.output=1L),
                   "a, ...")
})


context("FormatText")

test_that("only integer-valued numeric and logical vectors pass", {
  # Must not be missing.
  expect_error(FormatText(),
               regexp="missing")
  # Must specify at least one character vector.
  expect_error(FormatText(1),
               regexp="One or more character vectors expected")
  expect_error(FormatText(1, character(0)),
               regexp="One or more character vectors expected")
  # Empty string gives an empty string.
  expect_identical(FormatText(1, ""), "")
  # Strings are collapsed.
  expect_identical(FormatText(1, "", "a", "b", ""), "ab")
  # Multiple-component vectors are expanded and collapsed.
  expect_identical(FormatText(1, c("a", "b"), c("c", "d")), "abcd")
  # 'x' can be integer-valued but nonnegative and not NA.
  expect_error(FormatText(NULL))
  expect_error(FormatText(-1, ""))
  expect_error(FormatText(Inf, ""))
  expect_error(FormatText(NA_integer_, ""))
  # 'x' can be logical but not NA.
  expect_identical(FormatText(TRUE, ""), "")
  expect_identical(FormatText(FALSE, ""), "")
  expect_error(FormatText(NA, ""))
  expect_identical(FormatText(1:10 %in% 1:5, ""), "")
})

test_that("pattern {a|b} works", {
  txt <- "There {is|are} {one|many} item{|s}"
  expect_identical(FormatText(0, txt), "There are many items")
  expect_identical(FormatText(1, txt), "There is one item")
  expect_identical(FormatText(2, txt), "There are many items")
  expect_identical(FormatText(3, txt), "There are many items")
})

test_that("pattern {z|a|b} works", {
  txt <- "There {is|is|are} {nothing|one thing|many things}"
  expect_identical(FormatText(0, txt), "There is nothing")
  expect_identical(FormatText(1, txt), "There is one thing")
  expect_identical(FormatText(2, txt), "There are many things")
  expect_identical(FormatText(3, txt), "There are many things")
})

test_that("patterns {a|b} and {z|a|b} work together", {
  txt <- "There {is|are} {zero|only one|many} thing{|s}"
  expect_identical(FormatText(0, txt), "There are zero things")
  expect_identical(FormatText(1, txt), "There is only one thing")
  expect_identical(FormatText(2, txt), "There are many things")
  expect_identical(FormatText(3, txt), "There are many things")
})

test_that("logical 'x' works similarly", {
  txt <- "There {is|are} {zero|only one|many} thing{|s}"
  expect_identical(FormatText(FALSE, txt), "There are zero things")
  expect_identical(FormatText(TRUE, txt), "There is only one thing")
  expect_identical(FormatText(c(TRUE, FALSE), txt), "There is only one thing")
  expect_identical(FormatText(c(TRUE, TRUE), txt), "There are many things")
  expect_identical(FormatText(c(TRUE, TRUE, TRUE), txt),
                   "There are many things")
})

test_that("patterns $N and $L work", {
  expect_identical(FormatText(1, "=$N/$L="), "=1/1=")
  expect_identical(FormatText(2, "=$N/$L="), "=2/1=")
  x1 <- sample(c(rep(TRUE, 3), rep(FALSE, 7)))
  expect_identical(FormatText(x1, "=$N/$L="), "=3/10=")
})

test_that("patterns $N and $L work with the patterns {...}", {
  txt <- "There {is|are} {zero|only one ($N)|$N} thing{|s} (out of {$L|$L})"
  x1 <- sample(c(rep(TRUE, 1), rep(FALSE, 9)))
  expect_identical(FormatText(x1, txt),
                   "There is only one (1) thing (out of 10)")
  expect_identical(FormatText(!x1, txt),
                   "There are 9 things (out of 10)")
})

test_that("pattern $P works", {
  x1 <- sample(c(rep(TRUE, 1), rep(FALSE, 9)))
  txt <- "$P% of the values are TRUE"
  expect_identical(FormatText(x1, txt), "10.0% of the values are TRUE")
  expect_identical(FormatText(!x1, txt), "90.0% of the values are TRUE")
})

test_that("patterns $x and $X and $w and $W work but only with logical 'x'", {
  x <- structure(c(TRUE, FALSE, TRUE, FALSE, TRUE), names=base::letters[1:5])
  expect_identical(FormatText(x, "=$x="), "=a, c, e=")
  expect_identical(FormatText(x, "=$X="), "='a', 'c', 'e'=")
  expect_identical(FormatText(x, "=$w="), "=1, 3, 5=")
  expect_identical(FormatText(x, "=$W="), "='1', '3', '5'=")
  x <- (1L)
  expect_error(FormatText(x, "=$x="), regexp="'x' must be logical")
  expect_error(FormatText(x, "=$X="), regexp="'x' must be logical")
  expect_error(FormatText(x, "=$w="), regexp="'x' must be logical")
  expect_error(FormatText(x, "=$W="), regexp="'x' must be logical")
})

test_that("patterns $x and $X work only with names(x) properly set", {
  x <- c(TRUE, FALSE, TRUE, FALSE, TRUE)
  expect_error(FormatText(x, "=$x="),
               regexp="'x' must have the names attribute set")
  expect_error(FormatText(x, "=$X="),
               regexp="'x' must have the names attribute set")
  names(x) <- c("", "", "", NA, "")
  expect_error(FormatText(x, "=$x="),
               regexp="'x' must have the names attribute set")
  expect_error(FormatText(x, "=$X="),
               regexp="'x' must have the names attribute set")
})

test_that("patterns $x and $X and $w and $W work", {
  x <- structure(c(TRUE, FALSE, TRUE, FALSE, TRUE), names=base::letters[1:5])
  expect_identical(FormatText(x, "=$x="), "=a, c, e=")
  expect_identical(FormatText(x, "=$X="), "='a', 'c', 'e'=")
  expect_identical(FormatText(x, "=$w="), "=1, 3, 5=")
  expect_identical(FormatText(x, "=$W="), "='1', '3', '5'=")
})
