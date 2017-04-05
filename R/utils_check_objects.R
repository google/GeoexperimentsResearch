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

#' Various tests.
#'
#' @name assert_tests
#' @param x object to test.
#'
#' @return
#' These are typically used in statements with \code{assert_that(...)}.
#' Each function returns either \code{TRUE} or \code{FALSE}; some functions
#' may return \code{NA}.
#'
#' #' \itemize{
#'   \item{\code{is.character.vector}}: \code{TRUE} if and only if \code{x} is a
#'     character vector of positive length, otherwise \code{FALSE}.
#'   \item{\code{is.nonempty.string}}: \code{TRUE} if and only if \code{x} is a
#'     character vector of length 1 with a component that is nonempty and non-NA,
#'     otherwise \code{FALSE}.
#'   \item{\code{is.vector.of.nonempty.strings}}: \code{TRUE} if and only if
#'     \code{x} is a vector of nonempty strings none of which is an NA, otherwise
#'     \code{FALSE}.  Empty vectors yield \code{FALSE}.
#'   \item{\code{is.plain.list}}: \code{TRUE} if and only if \code{x} is a list
#'     but does not have the object class set. Otherwise \code{FALSE}.
#'   \item{\code{is.plain.list}}: \code{TRUE} if and only if \code{x} is a list
#'     but does not have the object class set. Otherwise \code{FALSE}.
#'   \item{\code{is.named.list}}: \code{TRUE} if and only if \code{x} is a plain
#'     list with a names attribute with non-empty strings for names, otherwise
#'     \code{FALSE}.
#'   \item{\code{is.integer.valued}}: \code{TRUE} if and only if \code{x} is
#'     either of type integer, or numeric and all components coercible to
#'     integer, otherwise \code{FALSE}.  NAs are allowed.
#'   \item{\code{is.logical.vector}}: \code{TRUE} if and only if \code{x} is a
#'     logical vector of positive length, otherwise \code{FALSE}.
#'   \item{\code{is.empty.string}}: A logical vector of the same length as
#'     \code{x}, #' with '\code{TRUE}' if and only if the string is empty,
#'     NA if the string is NA, otherwise \code{FALSE}. If \code{x} is not
#'     character, throws an error.  NA maps to NA.
#'   \item{\code{is.numeric.vector}}: \code{TRUE} if and only if \code{x} is a
#'     numeric vector of positive length, otherwise \code{FALSE}.
#'   \item{\code{is.real.number}}: \code{TRUE} if and only if \code{x} is a
#'     numeric scalar, not an NA, and not infinite, otherwise \code{FALSE}.
#'   \item{\code{is.inf}}: \code{TRUE} if and only if \code{x} is \code{Inf} or
#'     \code{-Inf}.
#' }
#' @rdname utils_check_objects
NULL

#' @rdname utils_check_objects
is.character.vector <- function(x) {
  pass <- (is.character(x) && length(x) > 0L)
  return(pass)
}

.is.character.vector.fail <- function(call, env) {
  # Function used by assert_that in case of assertion failure.
  msg <- paste0(deparse(call$x),
                " is not a character vector of positive length")
  return(msg)
}

on_failure(is.character.vector) <- .is.character.vector.fail


#' @rdname utils_check_objects
is.nonempty.string <- function(x) {
  pass <- (is.string(x) && !is.na(x) && nchar(x) > 0)
  return(pass)
}

.is.nonempty.string.fail <- function(call, env) {
  # Function used by assert_that in case of assertion failure.
  msg <- paste0(deparse(call$x), " is not a nonempty string")
  return(msg)
}

on_failure(is.nonempty.string) <- .is.nonempty.string.fail


#' @rdname utils_check_objects
is.vector.of.nonempty.strings <- function(x) {
  pass <- (is.character(x) && length(x) > 0L &&
           isTRUE(all(nzchar(x, keepNA=TRUE))))
  return(pass)
}

.is.vector.of.nonempty.strings.fail <- function(call, env) {
  # Function used by assert_that in case of assertion failure.
  msg <- paste0(deparse(call$x), " is not a vector of nonempty strings")
  return(msg)
}

on_failure(is.vector.of.nonempty.strings) <- .is.vector.of.nonempty.strings.fail


#' @rdname utils_check_objects
is.plain.list <- function(x) {
  pass <- (is.list(x) && (!is.object(x)))
  return(pass)
}

.is.plain.list.fail <- function(call, env) {
  # Function used by assert_that in case of assertion failure.
  msg <- paste0(deparse(call$x), " is not a plain list")
  return(msg)
}

on_failure(is.plain.list) <- .is.plain.list.fail


#' @rdname utils_check_objects
is.named.list <- function(x) {
  pass <- (is.plain.list(x) &&
           is.vector.of.nonempty.strings(names(x)) &&
           (!any(duplicated(names(x)))))
  return(pass)
}

.is.named.list.fail <- function(call, env) {
  # Function used by assert_that in case of assertion failure.
  msg <- paste0(deparse(call$x), " is not a named list")
  return(msg)
}

on_failure(is.named.list) <- .is.named.list.fail


#' @rdname utils_check_objects
is.integer.valued <- function(x) {
  pass <- (is.integer(x) ||
           (is.numeric(x) &&
            isTRUE(suppressWarnings(all(x[!is.na(x)] ==
                                        as.integer(x[!is.na(x)]))))))
  return(pass)
}

.is.integer.valued.fail <- function(call, env) {
  # Function used by assert_that in case of assertion failure.
  msg <- paste0(deparse(call$x), " is not an integer-valued numeric vector")
  return(msg)
}

on_failure(is.integer.valued) <- .is.integer.valued.fail


#' @rdname utils_check_objects
is.logical.vector <- function(x) {
  pass <- (is.logical(x) && length(x) > 0L)
  return(pass)
}

.is.logical.vector.fail <- function(call, env) {
  # Function used by assert_that in case of assertion failure.
  msg <- paste0(deparse(call$x),
                " is not a logical vector of positive length")
  return(msg)
}

on_failure(is.logical.vector) <- .is.logical.vector.fail


#' @rdname utils_check_objects
is.empty.string <- function(x) {
  assert_that(is.character(x))
  pass <- (nzchar(x, keepNA=TRUE) == 0L)
  return(pass)
}

.is.empty.string.fail <- function(call, env) {
  # Function used by assert_that in case of assertion failure.
  msg <- paste0(deparse(call$x),
                " is not an empty string")
  return(msg)
}

on_failure(is.empty.string) <- .is.empty.string.fail


#' @rdname utils_check_objects
is.numeric.vector <- function(x) {
  pass <- (is.numeric(x) && length(x) > 0L)
  return(pass)
}

.is.numeric.vector.fail <- function(call, env) {
  # Function used by assert_that in case of assertion failure.
  msg <- paste0(deparse(call$x),
                " is not a numeric vector of positive length")
  return(msg)
}

on_failure(is.numeric.vector) <- .is.numeric.vector.fail


#' @rdname utils_check_objects
is.real.number <- function(x) {
  pass <- (is.numeric(x) && length(x) == 1L && !is.na(x) && !is.infinite(x))
  return(pass)
}

.is.real.number.fail <- function(call, env) {
  # Function used by assert_that in case of assertion failure.
  msg <- paste0(deparse(call$x), " is not a real number")
  return(msg)
}

on_failure(is.real.number) <- .is.real.number.fail


#' @rdname utils_check_objects
is.inf <- function(x) {
  pass <- (is.numeric(x) && length(x) == 1L && is.infinite(x))
  return(pass)
}

.is.inf.fail <- function(call, env) {
  # Function used by assert_that in case of assertion failure.
  msg <- paste0(deparse(call$x), " is not Inf or -Inf")
  return(msg)
}

on_failure(is.inf) <- .is.inf.fail
