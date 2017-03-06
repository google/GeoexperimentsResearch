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

monday.string <- "2016-08-01"
monday <- as.Date(monday.string)
two.weeks <- (monday + seq_len(14L) - 1L)


context(".GetWeekdays")

test_that("Monday to Sunday are mapped to 1 .. 7", {
  expect_identical(.GetWeekdays(monday + 0:6), 1:7)
})

test_that("a non-Date 'x' is rejected", {
  expect_error(.GetWeekdays(monday.string),
               regexp="is not a Date object")
})


context(".GetWeekIndex")

test_that("last days of the week in EOY are mapped to week 0", {
  last.day <- as.Date("2015-12-31")  # Thursday
  first.sunday <- last.day + 3L
  first.monday <- first.sunday + 1L
  expect_identical(.GetWeekIndex(last.day), 201552L)
  expect_identical(.GetWeekIndex(last.day + 1L), 201600L)
  expect_identical(.GetWeekIndex(first.sunday), 201600L)
  expect_identical(.GetWeekIndex(first.monday), 201601L)
})

test_that("GetWeekIndex rejects non-Date 'x'", {
  expect_error(.GetWeekIndex(monday.string),
               regexp="is not a Date object")
})


context(".GetWeekNumbers")

test_that("First Monday of the year is week 1", {
  week.1 <- as.Date("2016-01-04")  # First Monday.
  expect_identical(.GetWeekNumbers(week.1), 1L)
})

test_that("2nd ... 11th Monday of the year are 2 .. 11", {
  week.1 <- as.Date("2016-01-04")  # First Monday.
  n <- (1:10)
  expect_identical(.GetWeekNumbers(week.1 + n * 7L), 1L + n)
})

test_that("GetWeekNumbers rejects non-Date 'x'", {
  expect_error(.GetWeekNumbers(monday.string),
               regexp="is not a Date object")
})


context(".GetLastDayOfWeek")

test_that("a non-date 'x' is rejected", {
  expect_error(.GetLastDayOfWeek(monday.string))
})

test_that("in the default case, a day is moved to next Sunday", {
  next.sunday <- (monday + 6)
  for (shift in 0:6) {
    expect_identical(.GetLastDayOfWeek(monday + shift), next.sunday)
  }
})

test_that("days are shifted correctly", {
  for (i in 0:6) {
    d1 <- .GetLastDayOfWeek(two.weeks, last.day=i + 1)
    shift <- c(rep(i, i + 1), rep(7 + i, 7), rep(14 + i, 7))[1:14]
    d2 <- (monday + shift)
    expect_identical(d1, d2)
  }
})

test_that("last.day that is not within 1:7 is rejected", {
  for (day.number in list(0, -2, 0.5, 1.5, 8L, NA_real_, Inf, "1")) {
    expect_error(.GetLastDayOfWeek(monday, last.day=day.number))
  }
})


context(".GetLastDayOfMonth")

test_that("a non-date 'x' is rejected", {
  expect_error(.GetLastDayOfMonth(monday.string))
})

test_that("the last day of the current month  is returned", {
  days.2016 <- c("01"=31, "02"=29, "03"=31, "04"=30, "05"=31, "06"=30,
                 "07"=31, "08"=31, "09"=30, "10"=31, "11"=30, "12"=31)
  for (i in seq_along(days.2016)) {
    month <- names(days.2016)[i]
    last.day <- days.2016[i]
    date.from <- as.Date(sprintf("2016-%s-01", month))
    date.to <- as.Date(sprintf("2016-%s-%s", month, last.day))
    expect_identical(.GetLastDayOfMonth(date.from), date.to)
  }
})
