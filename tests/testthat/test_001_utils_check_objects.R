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

context("is.character.vector")

test_that("is.character.vector works correctly", {
  # Can be a single string.
  expect_true(is.character.vector(""))
  # Can be a vector of any length.
  expect_true(is.character.vector(c("", "a")))
  # Must be a character vector.
  expect_false(is.character.vector(NULL))
  # Must have positive length.
  expect_false(is.character.vector(character(0)))
})


context("is.nonempty.string")

test_that("is.nonempty.string works correctly", {
  # Can be a single string.
  expect_true(is.nonempty.string("foo"))
  # Must not be an empty vector.
  expect_false(is.nonempty.string(character(0)))
  # Must not be an empty string.
  expect_false(is.nonempty.string(""))
  # Must not be NA.
  expect_false(is.nonempty.string(NA_character_))
  expect_false(is.nonempty.string(NA))
  # Must be a character vector of length 1.
  expect_false(is.nonempty.string(c("a", "b")))
  expect_false(is.nonempty.string(1))
  expect_false(is.nonempty.string(NULL))
})

context("is.vector.of.nonempty.string")

test_that("is.vector.of.nonempty.strings works correctly", {
  # Usual case.
  expect_true(is.vector.of.nonempty.strings("foo"))
  expect_true(is.vector.of.nonempty.strings(c("a", "b")))
  # Must be a nonempty vector.
  expect_false(is.vector.of.nonempty.strings(character(0)))
  # Must be a nonempty string.
  expect_false(is.vector.of.nonempty.strings(""))
  # Must not be NA.
  expect_false(is.vector.of.nonempty.strings(NA_character_))
  expect_false(is.vector.of.nonempty.strings(NA))
  # Must be a character vector of length 1.
  expect_false(is.vector.of.nonempty.strings(1))
  expect_false(is.vector.of.nonempty.strings(NULL))
})


context("is.plain.list")

test_that("is.plain.list works correctly", {
  # Can be a nonempty list.
  expect_true(is.plain.list(list(1)))
  # Can be an empty list.
  expect_true(is.plain.list(list()))
  # Can be a named list.
  expect_true(is.plain.list(list(a=1, b=2)))
  # Must not be any object.
  expect_false(is.plain.list(structure(list(a=1, b=2), class="foo")))
})


context("is.named.list")

test_that("is.named.list works correctly", {
  # Can be a a named plain list.
  expect_true(is.named.list(list(a=1, b=2)))
  # Must not be any object except a 'plain list'.
  expect_false(is.named.list(structure(list(a=1, b=2), class="foo")))
  # Must not be a list of objects without names.
  expect_false(is.named.list(list(1)))
  # Must not be an empty list.
  expect_false(is.named.list(list()))
  # Must not be a non-list.
  expect_false(is.named.list(c(a=1)))
  # Must not be NULL.
  expect_false(is.named.list(NULL))
})


context("is.integer.valued")

test_that("is.integer.valued works correctly", {
  # Can be an integer or floating-pointing numeric vector.
  expect_true(is.integer.valued(2L))
  expect_true(is.integer.valued(0))
  expect_true(is.integer.valued(-2))
  expect_true(is.integer.valued(.Machine$integer.max))
  # Can be of any length.
  expect_true(is.integer.valued(1:101))
  # Must be numeric.
  expect_false(is.integer.valued("1"))
  # Numeric values exceeding integer.max produce FALSE.
  expect_false(is.integer.valued(.Machine$integer.max + 1))
  # Does not deal with complex numbers.
  expect_false(is.integer.valued(complex(1L, real=1.0)))
  # Can be NA_integer_ or NA_real_ but not NA_logical_.
  expect_false(is.integer.valued(NA))
  expect_true(is.integer.valued(NA_real_))
  expect_true(is.integer.valued(NA_integer_))
})
