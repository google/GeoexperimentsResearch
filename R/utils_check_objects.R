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

is.character.vector <- function(x) {
  # Tests whether 'x' is a nonempty character vector.
  #
  # Args:
  #   x : object to test.
  #
  # Returns:
  #   TRUE if and only if 'x' is a character vector of positive length,
  #   otherwise FALSE.

  pass <- (is.character(x) && length(x) > 0L)
  return(pass)
}

.is.character.vector.fail <- function(call, env) {
  # Function used by assert_that in case of assertion failure.
  #
  # Args:
  #   call: call.
  #   env: environment.
  #
  # Returns:
  #   A message string.

  msg <- paste0(deparse(call$x),
                " is not a character vector of positive length")
  return(msg)
}

on_failure(is.character.vector) <- .is.character.vector.fail

is.nonempty.string <- function(x) {
  # Tests whether 'x' is a nonempty string (and not NA).
  #
  # Args:
  #   x : object to test.
  #
  # Returns:
  #   TRUE if and only if 'x' is a character vector of length 1 with a
  #   component that is nonempty and non-NA, otherwise FALSE.

  pass <- (is.string(x) && !is.na(x) && nchar(x) > 0)
  return(pass)
}


.is.nonempty.string.fail <- function(call, env) {
  # Function used by assert_that in case of assertion failure.
  #
  # Args:
  #   call: call.
  #   env: environment.
  #
  # Returns:
  #   A message string.

  msg <- paste0(deparse(call$x), " is not a nonempty string")
  return(msg)
}

on_failure(is.nonempty.string) <- .is.nonempty.string.fail

is.vector.of.nonempty.strings <- function(x) {
  # Tests whether 'x' is a character vector of nonempty strings.
  #
  # Args:
  #   x : object to test.
  #
  # Returns:
  #   TRUE if and only if 'x' is a vector of nonempty strings
  #   none of which is an NA, otherwise FALSE. Empty vectors yield FALSE.

  pass <- (is.character(x) && length(x) > 0L &&
           isTRUE(all(nzchar(x, keepNA=TRUE))))
  return(pass)
}

.is.vector.of.nonempty.strings.fail <- function(call, env) {
  # Function used by assert_that in case of assertion failure.
  #
  # Args:
  #   call: call.
  #   env: environment.
  #
  # Returns:
  #   A message string.

  msg <- paste0(deparse(call$x), " is not a vector of nonempty strings")
  return(msg)
}

on_failure(is.vector.of.nonempty.strings) <- .is.vector.of.nonempty.strings.fail

is.plain.list <- function(x) {
  # Tests whether 'x' is a plain list (not an object).
  #
  # Args:
  #   x : object to test.
  #
  # Returns:
  #   TRUE if and only if 'x' is a list but not an object.
  #   otherwise FALSE.

  pass <- (is.list(x) && (!is.object(x)))
  return(pass)
}

.is.plain.list.fail <- function(call, env) {
  # Function used by assert_that in case of assertion failure.
  #
  # Args:
  #   call: call.
  #   env: environment.
  #
  # Returns:
  #   A message string.

  msg <- paste0(deparse(call$x), " is not a plain list")
  return(msg)
}

on_failure(is.plain.list) <- .is.plain.list.fail

is.named.list <- function(x) {
  # Tests whether 'x' is a named list.
  #
  # Args:
  #   x : object to test.
  #
  # Returns:
  #   TRUE if and only if 'x' is a plain list with a names
  #   attribute with non-empty strings for names,
  #   otherwise FALSE.

  pass <- (is.plain.list(x) &&
           is.vector.of.nonempty.strings(names(x)) &&
           (!any(duplicated(names(x)))))
  return(pass)
}

.is.named.list.fail <- function(call, env) {
  # Function used by assert_that in case of assertion failure.
  #
  # Args:
  #   call: call.
  #   env: environment.
  #
  # Returns:
  #   A message string.

  msg <- paste0(deparse(call$x), " is not a named list")
  return(msg)
}

on_failure(is.named.list) <- .is.named.list.fail

is.integer.valued <- function(x) {
  # Tests whether 'x' is an integer-valued numeric vector.
  #
  # Args:
  #   x : object to test.
  #
  # Notes:
  #   NAs are allowed.
  #
  # Returns:
  #   TRUE if and only if 'x' is either of type integer, or
  #   numeric and all components coercible to integer,
  #   otherwise FALSE.

  pass <- (is.integer(x) ||
           (is.numeric(x) &&
            isTRUE(suppressWarnings(all(x[!is.na(x)] ==
                                        as.integer(x[!is.na(x)]))))))
  return(pass)
}

.is.integer.valued.fail <- function(call, env) {
  # Function used by assert_that in case of assertion failure.
  #
  # Args:
  #   call: call.
  #   env: environment.
  #
  # Returns:
  #   A message string.

  msg <- paste0(deparse(call$x), " is not an integer-valued numeric vector")
  return(msg)
}

on_failure(is.integer.valued) <- .is.integer.valued.fail

is.logical.vector <- function(x) {
  # Tests whether 'x' is a nonempty logical vector.
  #
  # Args:
  #   x : object to test.
  #
  # Returns:
  #   TRUE if and only if 'x' is a logical vector of positive length,
  #   otherwise FALSE.

  pass <- (is.logical(x) && length(x) > 0L)
  return(pass)
}

.is.logical.vector.fail <- function(call, env) {
  # Function used by assert_that in case of assertion failure.
  #
  # Args:
  #   call: call.
  #   env: environment.
  #
  # Returns:
  #   A message string.

  msg <- paste0(deparse(call$x),
                " is not a logical vector of positive length")
  return(msg)
}

on_failure(is.logical.vector) <- .is.logical.vector.fail

is.empty.string <- function(x) {
  # Tests each component of 'x' to see whether the string is empty.
  #
  # Args:
  #   x : (character) a vector to test.
  #
  # Notes:
  #   NA maps to NA.
  #
  # Returns:
  #   A logical vector of the same length as 'x', with 'TRUE' if and
  #   only if the string is empty, NA if the string is NA, otherwise FALSE.
  #   If 'x' is not character, throws an error.

  assert_that(is.character(x))
  pass <- (nzchar(x, keepNA=TRUE) == 0L)
  return(pass)
}

is.numeric.vector <- function(x) {
  # Tests whether 'x' is a nonempty numeric vector.
  #
  # Args:
  #   x : object to test.
  #
  # Returns:
  #   TRUE if and only if 'x' is a numeric vector of positive length,
  #   otherwise FALSE.

  pass <- (is.numeric(x) && length(x) > 0L)
  return(pass)
}

.is.numeric.vector.fail <- function(call, env) {
  # Function used by assert_that in case of assertion failure.
  #
  # Args:
  #   call: call.
  #   env: environment.
  #
  # Returns:
  #   A message string.

  msg <- paste0(deparse(call$x),
                " is not a numeric vector of positive length")
  return(msg)
}

on_failure(is.numeric.vector) <- .is.numeric.vector.fail

is.real.number <- function(x) {
  # Tests whether 'x' is a real number.
  #
  # Args:
  #   x : object to test.
  #
  # Returns:
  #   TRUE if and only if 'x' is a numeric scalar, not an NA, and not
  #   infinite, otherwise FALSE.

  pass <- (is.numeric(x) && length(x) == 1L && !is.na(x) && !is.infinite(x))
  return(pass)
}

.is.real.number.fail <- function(call, env) {
  # Function used by assert_that in case of assertion failure.
  #
  # Args:
  #   call: call.
  #   env: environment.
  #
  # Returns:
  #   A message string.

  msg <- paste0(deparse(call$x), " is not a real number")
  return(msg)
}

on_failure(is.real.number) <- .is.real.number.fail

is.inf <- function(x) {
  # Tests whether 'x' is Inf or -Inf.
  #
  # Args:
  #   x : object to test.
  #
  # Returns:
  #   TRUE if and only if 'x' is Inf or -Inf.

  pass <- (is.numeric(x) && length(x) == 1L && is.infinite(x))
  return(pass)
}

.is.inf.fail <- function(call, env) {
  # Function used by assert_that in case of assertion failure.
  #
  # Args:
  #   call: call.
  #   env: environment.
  #
  # Returns:
  #   A message string.

  msg <- paste0(deparse(call$x), " is not Inf or -Inf")
  return(msg)
}

on_failure(is.inf) <- .is.inf.fail
