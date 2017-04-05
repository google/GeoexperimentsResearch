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

#' An alternative version of assert_that, outputting a modified error
#' message.
#'
#' @param ... see 'assert_that'.
#' @param env see 'assert_that'.
#' @param name (string) name of the object to output in the error message.
#'
#' @note
#' Replaces anything up to ' is not ' in the error message with the
#' value of 'name'.
#'
#' @examples
#' # Outputs "Column 'foo' is not an integer-valued numeric vector"
#' # if 'x' fails the test.
#' \dontrun{
#' CheckThat(is.integer.valued(x), name="Column 'foo'")}
#' @return 'TRUE' if tests pass; otherwise throws an error.

CheckThat <- function(..., env = parent.frame(), name = NULL) {
  assert_that(is.null(name) || is.nonempty.string(name))
  res <- see_if(..., env = env)
  if (res) {
    return(TRUE)
  }
  msg <- attr(res, "msg")
  if (!is.null(name)) {
    msg <- sub("^.*?(?= is not)", replacement=name, x=msg, perl=TRUE)
  }
  # This is the same as assertthat:::assertError(Message(msg)).
  class <- c("assertError", "simpleError", "error", "condition")
  m <- structure(list(message = Message(msg), call = NULL), class = class)
  stop(m)
}

#' Checks whether all components are TRUE.
#'
#' @param x (logical) vector. A 'TRUE' denotes a success, and 'FALSE' and 'NA'
#'   a failure. The names attribute must be set, with nonempty, non-NA,
#'   non-duplicated names.
#' @param ... one or more character vectors, passed on to \code{FormatText}.
#' @return If all tests pass, returns TRUE invisibly. Otherwise, an
#'   'assertError' is thrown.

CheckForAllTrue <- function(x, ...) {
  assert_that(is.logical.vector(x),
              is.vector.of.nonempty.strings(names(x)))
  assert_that(!anyDuplicated(names(x)),
              msg="names(x) has duplicate values")
  # Ensure that NAs are FALSE.
  x[TRUE] <- (x %in% TRUE)
  assert_that(all(x), msg=Message(FormatText(!x, ...)))
  return(invisible(TRUE))
}

#' Checks whether specified column names exist in a data frame.
#'
#' @param x (character) names of the columns.
#' @param dataframe (data frame) data frame.
#' @param what (string) string to use in the error message.
#'
#' @note
#' Convenient for checking function arguments. Quotes the name of the
#' variable 'x'.
#' @return If all tests pass, returns TRUE invisibly. Otherwise, an
#'   'assertError' is thrown.

CheckForMissingColumns <- function(x, dataframe, what="specified") {
  x.name <- deparse(substitute(x))
  assert_that(is.vector.of.nonempty.strings(x),
              msg=Message("'", x.name, "' must consist of non-empty strings ",
                          "with no missing values"))
  assert_that(is.data.frame(dataframe))
  CheckForAllTrue(structure(x %in% names(dataframe), names=x),
                  msg=paste0("The ", what, " column{|s} $X {is|are} ",
                             "not in the data frame"))
  return(invisible(TRUE))
}

#' Checks for bad values in given columns.
#'
#' @param x (data frame) any data frame.
#' @param columns (character) columns to check.
#' @param CHECK (function) function that returns a logical vector (of length
#'   \code{nrow(x)}).
#' @param good (logical) the value or values that indicate a 'good' value.
#'   Other values returned by 'CHECK' will be 'bad' and throw an error.
#' @param what (string) a string to display in the informative message.
#' @param ... further arguments passed on to function 'CHECK'.
#' @return If all tests pass, returns NULL invisibly. Otherwise, an
#'   'assertError' is thrown.

CheckForBadValues <- function(x, columns, CHECK, good=TRUE, what="invalid",
                              ...) {
  assert_that(is.data.frame(x),
              all(columns %in% names(x)))
  assert_that(is.function(CHECK))
  assert_that(is.logical(good))
  assert_that(is.string(what))
  for (i in seq_along(columns)) {
    column.name <- columns[i]
    v <- x[[column.name]]
    good.values <- (CHECK(v, ...) %in% good)
    assert_that(length(good.values) == length(v))
    assert_that(all(good.values),
                msg=Message(
                    FormatText(!good.values,
                               "Column '", column.name, "' has {one|$N} ",
                               what, " value{|s} in row{|s} $w")))
  }
  return(invisible(TRUE))
}

#' Checks whether the vector values in the given list satisfies the specified
#' types.
#'
#' @param x a named list or a data frame.
#' @param checklist (named list) mapping from names to functions that check
#'   whether the functions are correct or not.
#' @return invisible NULL.

CheckForTypes <- function(x, checklist) {
  assert_that(is.list(x))
  assert_that(is.named.list(checklist))
  for (i in seq_along(checklist)) {
    column.name <- names(checklist)[i]
    .Check <- checklist[[i]]
    assert_that(is.function(.Check))
    v <- x[[column.name]]
    CheckThat(.Check(v), name=sprintf("Column '%s'", column.name))
  }
  return(invisible(TRUE))
}

#' Checks whether the values in rows of given columns are not duplicated.
#'
#' @param x a data frame.
#' @param columns (character) vector.
#' @return If all tests pass, returns TRUE invisibly. Otherwise, an
#'   'assertError' is thrown.

CheckForDuplicateRows <- function(x, columns) {
  assert_that(is.data.frame(x))
  assert_that(is.vector.of.nonempty.strings(columns))
  dups <- duplicated(x[columns])
  if (length(columns) == 1L) {
    assert_that(!any(dups),
                msg=Message(
                    FormatText(dups,
                               "There {is one|are $N} duplicate value{|s} in ",
                               "column '", columns, "' in row{|s} $w")))
  } else {
    assert_that(!any(dups),
                msg=Message(
                    FormatText(dups,
                               "There {is one|are $N} duplicate ",
                               paste0(columns, collapse="+"),
                               " combination{|s} in row{|s} $w")))
  }
  return(invisible(TRUE))
}

#' Checks whether the mapping is consistent within a data frame.
#'
#' @param x a data frame.
#' @param from (character) vector of column names.
#' @param to (character) vector of column names. Typically these are different
#'   from those in 'from'.
#'
#' @note
#' Checks that no rows in 'x[from]' map to two different values of
#' 'x[to]'.
#' @return If all tests pass, returns TRUE invisibly. Otherwise, an
#'   'assertError' is thrown.

CheckForMapping <- function(x, from, to) {
  assert_that(is.data.frame(x))
  assert_that(is.vector.of.nonempty.strings(from))
  assert_that(is.vector.of.nonempty.strings(to))
  assert_that(length(intersect(from, to)) == 0L,
              msg="'from' and 'to' must not have common elements")
  assert_that(all(c(from, to) %in% names(x)))
  dfu <- unique(x[unique(c(from, to))])
  dups <- duplicated(dfu[from])
  CheckForAllTrue(structure(!dups, names=rownames(dfu)),
                  "Mapping from ", ConcatItems(from, collapse="+"),
                  " to ", ConcatItems(to, collapse="+"),
                  " is not consistent")
  return(invisible(TRUE))
}

#' Check that the specified geo group number is present in the data.
#'
#' @param geo.group (an integer) geo group number (>= 1). (NA throws an error).
#' @param values (vector) values to check against.
#'
#' @note
#' For checking arguments within a function. Outputs the name of the
#' variable in error messages.
#' @return TRUE, invisibly; as a side effect may throw an assertion error.

CheckGeoGroupNumber <- function(geo.group, values) {
  name <- deparse(substitute(geo.group))
  assert_that(length(geo.group) >=1,
              isTRUE(all(is.geo.group.number(geo.group), na.rm=TRUE)),
              !anyNA(geo.group),
              msg=Messagef("'%s' does not contain valid geo group numbers",
                  name))
  assert_that(!isTRUE(all(is.na(values))),
              msg=Message("There are no geo group numbers in the data ",
                  "(all are NA)"))
  there <- structure(geo.group %in% values, names=paste(geo.group))
  assert_that(isTRUE(all(there)),
              msg=Message(FormatText(!there,
                  "Specified '", name, "' $x {is|are} ",
                  "not present in the data")))
  invisible(TRUE)
}

#' Check that the specified period numbers are present in the data.
#'
#' @param period (integer vector): one or more period numbers. (NA throws an
#'   error).
#' @param values (vector) values to check against.
#'
#' @note
#' For checking arguments within a function. Outputs the name of the
#' variable in error messages.
#' @return TRUE invisibly if the checks pass. As a side effect will terminate
#'   with an error if the checks fail.

CheckPeriodNumbers <- function(period, values) {
  name <- deparse(substitute(period))
  assert_that(length(period) >=1,
              isTRUE(all(is.period.number(period), na.rm=TRUE)),
              !anyNA(period),
              msg=Messagef("'%s' does not contain valid period numbers",
                  name))
  assert_that(!isTRUE(all(is.na(values))),
              msg=Message("There are no period numbers in the data ",
                  "(all are NA)"))
  there <- structure(period %in% values, names=paste(period))
  assert_that(isTRUE(all(there)),
              msg=Message(FormatText(!there,
                  "Specified '", name, "' $x {is|are} ",
                  "not present in the data")))
  invisible(TRUE)
}
