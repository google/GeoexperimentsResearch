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

#' Sets an incremental response for simulation purposes.
#'
#' @param obj an object with the column '.spend'.
#' @param response (string) name of the column which acts as the response
#'   metric.
#' @param periods numbers of the periods which are affected; if this is "all",
#'   all periods are affected.
#' @param value the absolute value of the incremental return on ad spend. Can
#'   also be negative. Can also be NA, in the case of which the column
#'   \code{.response} will be reset to NA.
#'
#' @return The object, with the modfied spend change column \code{.response}.
#'
#' @note
#' Creates a column \code{.response} if it does not exist, and registers it
#' as a metric. The column \code{.response} contains the baseline response and
#' the incremental response, which is defined as the specified incremental
#' ROAS ('value') times the column '.spend'.
#'
#' @rdname SetIncrementalResponse

"SetIncrementalResponse<-" <- function(obj,
                                       response,
                                       periods="all",
                                       value) {
  UseMethod("SetIncrementalResponse<-")
}

#' @rdname SetIncrementalResponse
"SetIncrementalResponse<-.GeoExperimentData" <- function(obj, response,
                                                         periods="all", value) {
  SetMessageContextString("SetIncrementalResponse<-.GeoExperimentData")
  on.exit(SetMessageContextString())

  reset.column <- isTRUE(is.na(value))
  metrics <- GetInfo(obj, "metrics")

  if (reset.column) {
    obj[[kResponse]] <- NA_real_
  } else {
    assert_that(kSpendChange %in% metrics,
                msg=Messagef("No metric '%s' available", kSpendChange))
    assert_that(is.real.number(value))
    iroas <- value
    assert_that(is.string(response))
    assert_that(response %in% names(obj),
                msg=Messagef("Specified response '%s' is not a column",
                    response))
    assert_that(response %in% metrics,
                msg=Messagef("Specified response '%s' is not a metric",
                    response))
    assert_that(identical(periods, "all") ||
                all(is.period.number(periods)))
    available.periods <- unique(obj[[kPeriod]])
    if (identical(periods, "all")) {
      match.period <- TRUE
    } else {
      there <- structure(periods %in% available.periods, names=paste0(periods))
      assert_that(all(there),
                  msg=Message(FormatText(!there, "Unknown period{|s}: $x")))
      match.period <- (obj[[kPeriod]] %in% periods)
    }
    total.response <- obj[[response]]
    # If .spend has NAs then .response will have also NAs.
    spend.column <- obj[[kSpendChange]]
    total.response[match.period] <-
        (total.response + iroas * spend.column)[match.period]
    obj[[kResponse]] <- total.response
  }

  if (!(kResponse %in% metrics)) {
    obj <- SetInfo(obj, metrics=c(metrics, kResponse))
  }

  return(obj)
}
