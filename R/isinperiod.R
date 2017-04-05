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

#' [internal] Returns a logical vector indicating which of the rows are in
#' the specified period(s).
#'
#' @param obj some object.
#' @param periods names of the periods.
#'
#' @return A logical vector of length \code{nrow(obj)}, \code{TRUE} for a row
#' that is within the given period(s), \code{FALSE} otherwise. No \code{NA}s.
#'
#' @rdname IsInPeriod
IsInPeriod <- function(obj, periods) {
  assert_that(is.vector.of.nonempty.strings(periods))
  there <- structure(periods %in% names(kStandardPeriods), names=periods)
  assert_that(all(there),
              msg=Message(FormatText(!there, "Unknown periods: $X")))

  UseMethod("IsInPeriod")
}

#' @rdname IsInPeriod
IsInPeriod.TBRAnalysisData <- function(obj, periods) {
  SetMessageContextString("IsInPeriod.TBRAnalysisData")
  on.exit(SetMessageContextString())

  period <- obj[[kPeriod]]
  period.numbers <- unlist(kStandardPeriods[periods])
  in.period <- (period %in% period.numbers)

  return(in.period)
}

#' @rdname IsInPeriod
IsInPeriod.TBRAnalysisFitTbr1 <- function(obj, periods) {
  x <- IsInPeriod.TBRAnalysisData(obj, periods=periods)
  return(x)
}

#' @note \code{TBRROASAnalysisFit} consists of simulations with rows only for
#' the prediction period, not for the preanalysis period.
#'
#' @rdname IsInPeriod
IsInPeriod.TBRROASAnalysisFit <- function(obj, periods) {
  tbr.fit <- GetInfo(obj, "tbr.resp")
  in.prediction <- IsInPeriod(tbr.fit, periods="prediction")
  rows.in.periods <- IsInPeriod(tbr.fit, periods=periods)[in.prediction]
  return(rows.in.periods)
}

#' @note \code{TBRQuantiles} consists of simulations with rows only for the
#' prediction period, not for the preanalysis period.
#'
#' @rdname IsInPeriod
IsInPeriod.TBRQuantiles <- function(obj, periods) {
  rows.in.periods <- IsInPeriod.TBRAnalysisData(obj, periods=periods)
  return(rows.in.periods)
}
