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

#' Constructs an ExperimentPeriods object.
#'
#' @param period.dates (a character or Date vector); start dates of each
#'   period, plus the last date of the experiment. The first period is the
#'   pretest period, after which there must be at least one test period
#'   (there can be more than one test period); the length must be at least
#'   3.
#' @param period.names (character or NULL or a vector of nonempty strings)
#'   optional names of the periods. By default, names 'Pretest' and 'Test'
#'   (or 'Test1', 'Test2', ...) are used.
#' @param date.format (string) format for the dates if provided in character
#'   format.
#' @return An ExperimentPeriods object.
#'
#' @note
#' The periods must be consecutive and each period must be at least of
#' length 1 (day). No gaps can be specified. It is however possible to
#' define a ('dummy') test period that is not included in the analyses.

ExperimentPeriods <- function(period.dates, period.names=NULL,
                              date.format="%Y-%m-%d") {
  kClassName <- "ExperimentPeriods"
  SetMessageContextString(kClassName)
  on.exit(SetMessageContextString())

  assert_that(is.character(period.dates) ||
              is.date(period.dates))
  if (!is.date(period.dates)) {
    period.dates <- as.Date(period.dates, format=date.format)
  }
  n <- length(period.dates)
  assert_that(n >= 3L,
              msg="At least 3 dates must be specified.")
  # Compute start and end dates of each period.
  start.dates <- period.dates[-n]
  end.date <- period.dates[n]
  end.dates <- c(period.dates[c(-1L, -n)] - 1L, end.date)
  period.lengths <- as.integer(diff(c(start.dates, end.date + 1L)))
  assert_that(all(period.lengths >= 0),
              msg="Experiment dates must be consecutive")
  assert_that(all(period.lengths >= 1),
              msg="Experiment dates must be at least of length 1")
  n.periods <- length(period.lengths)  # Includes the pre-test period.
  n.test.periods <- (n.periods - 1L)
  if (is.null(period.names)) {
    # Fill in the default names: Pretest, Test, (or Test1, Test2, ...).
    test.period.names <- rep("Test", n.test.periods)
    if (length(test.period.names) >= 2) {
      test.period.names <- paste0(test.period.names, seq_len(n.test.periods))
    }
    period.names <- c("Pretest", test.period.names)
  }
  assert_that(is.vector.of.nonempty.strings(period.names),
              length(period.names) == n.periods,
              !anyDuplicated(period.names))
  # First (the pre-test) period is always '0'.
  period.numbers <- as.integer(seq(from=0, length.out=n.periods))
  obj <- data.frame(Period=period.numbers,
                    Name=period.names,
                    Start=start.dates,
                    End=end.dates,
                    Length=period.lengths)
  obj <- SetInfo(obj, info=list())
  class(obj) <- c(kClassName, oldClass(obj))
  return(obj)
}
