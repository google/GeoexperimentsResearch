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

#' Utilities to manipulate Date objects.
#'
#' @name date_utilities
#' @param x a Date object (vector).
#' @return
#' \itemize{
#'   \item{\code{.GetWeekdays}}: Indicator for weekday, given a
#'     date: an integer vector of the same length as \code{x}, indicating
#'     the number of the weekday, Monday=1, Tuesday=2, ..., Sunday=7.
#'   \item{\code{.GetWeekIndex}}: An indicator for an 'absolute' week number:
#'      an integer vector of the same length as \code{x}, indicating the week
#'      number, composed of the 4-digit year and the number of the week within
#'      the year.
#'   \item{\code{.GetWeekNumbers}}: Indicators for week numbers within the
#'      year: an integer vector of the same length as 'x', indicating the week
#'      number, 0 .. 53.
#'   \item{code{.GetLastDayOfWeek}}: Translates the given date(s) to the date
#'     of the last day of the corresponding week: a date object of the same
#'     length as 'x', with the corresponding dates shifted to the next (future)
#'     day given by 'last.day'.
#'   \item{\code{.GetLastDayOfMonth}}: Translates the given date(s) to the last
#'     day of the month of the corresponding month.  A date object of the same
#'     length as 'x', with the corresponding dates shifted to the last day of
#'     the month.
#' }
#' @note
#' As a convention, a week starts on Monday and ends on Sunday.
#'
#' @rdname utils_time
NULL

#' @rdname utils_time
.GetWeekdays <- function(x) {
  assert_that(is.date(x), length(x) >= 1L)
  weekdays <- as.integer(strftime(x, format="%u"))
  return(weekdays)
}

#' @rdname utils_time
.GetWeekIndex <- function(x) {
  assert_that(is.date(x), length(x) >= 1L)
  week.index <- as.integer(strftime(x, format="%Y%W"))
  return(week.index)
}

#' @rdname utils_time
.GetWeekNumbers <- function(x) {
  assert_that(is.date(x), length(x) >= 1L)
  weeknum <- as.integer(strftime(x, format="%W"))
  return(weeknum)
}

#' @param last.day (integer) number of the day to translate the date to. Monday
#'   is 1, Sunday is 7.
#'
#' @rdname utils_time
.GetLastDayOfWeek <- function(x, last.day=7L) {
  assert_that(is.date(x))
  assert_that(is.integer.valued(last.day),
              length(last.day) == 1,
              last.day %in% 1:7)
  weekdays <- as.integer(strftime(x, format="%u"))
  offset <- ((7L + last.day - weekdays) %% 7L)  # Always nonnegative.
  last.day.of.week <- (x + offset)
  return(last.day.of.week)
}

#' @rdname utils_time
.GetLastDayOfMonth <- function(x) {
  assert_that(is.date(x))
  year <- as.integer(strftime(x, "%Y"))
  next.month <- (1L + as.integer(strftime(x, "%m")))
  skip <- (next.month == 13L)
  year[skip] <- (year[skip] + 1L)
  next.month[skip] <- 1L
  first.day.of.next.month <- as.Date(sprintf("%s-%02d-01", year, next.month))
  last.day.of.this.month <- (first.day.of.next.month - 1L)
  return(last.day.of.this.month)
}
