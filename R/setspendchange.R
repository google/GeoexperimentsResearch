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

#' Sets the spend change in a geo experiment for simulation purposes.
#'
#' @param obj an object with the column 'assignment'.
#' @param prop.to (existing) name of the column in proportion to which the
#' spend change will be spread across the geos.
#' @param periods numbers of the periods which are affected; if this is
#' \code{"all"}, all periods are affected and the spend change will be spread
#' across all periods that have spend change (determined by the column
#' \code{assignment}).
#' @param value the total absolute value of the spend change. Can also be NA,
#' in the case of which the spend change column will be reset to NA.
#'
#' @return The object, with the modified spend change column \code{.spend}.
#'
#' @note
#' Creates a column \code{.spend}. The spend change type is determined by the
#' \code{assignment} column. The absolute value of the spend change will be
#' spread across geos in the proportion of the given column (\code{prop.to});
#' the sign of the change is determined by the type of change: for
#' treatment assignments set to 'decrease', the sign will be negative,
#' otherwise positive.
#'
#' @rdname SetSpendChange
"SetSpendChange<-" <- function(obj,
                               prop.to,
                               periods="all",
                               value) {
  UseMethod("SetSpendChange<-")
}

#' @rdname SetSpendChange
"SetSpendChange<-.GeoExperimentData" <- function(obj,
                                                 prop.to,
                                                 periods="all",
                                                 value) {
  SetMessageContextString("SetSpendChange<-.GeoExperimentData")
  on.exit(SetMessageContextString())

  reset.column <- isTRUE(is.na(value))
  if (reset.column) {
    obj[[kSpendChange]] <- NA_real_
    metrics <- GetInfo(obj, "metrics")
    if (!(kSpendChange %in% metrics)) {
      obj <- SetInfo(obj, metrics=c(metrics, kSpendChange))
    }
    return(obj)
  }

  assert_that(is.real.number(value))
  assert_that(value >= 0,
              msg=Message("Negative spend change is not allowed;",
                  " specify this in TreatmentAssignment instead"))
  assert_that(value != 0,
              msg=Message("Zero spend change is not allowed;",
                  " to reset the column, use NA instead"))

  assert_that(is.string(prop.to),
              prop.to %in% names(obj))
  assert_that(identical(periods, "all") ||
              is.integer.valued(periods) && all(periods >= 0))

  assert_that(!all(is.na(obj[[kAssignment]])),
              msg=Message("'assignment' column must not be all NAs"))

  total.abs.spend.change <- value

  add.column <- !(kSpendChange %in% names(obj))
  if (add.column) {
    obj[[kSpendChange]] <- 0
  }
  spend <- obj[[kSpendChange]]

  available.periods <- unique(obj[[kPeriod]])
  if (identical(periods, "all")) {
    match.period <- TRUE
  } else {
    there <- structure(periods %in% available.periods, names=paste0(periods))
    assert_that(all(there),
                msg=Message(FormatText(!there, "Unknown periods: $x")))
    match.period <- (obj[[kPeriod]] %in% periods)
  }

  assignment <- obj[[kAssignment]]
  increase <- (match.period &
               (assignment %in% kTreatmentAssignment["increase"]))
  decrease <- (match.period &
               (assignment %in% kTreatmentAssignment["decrease"]))
  change <- (increase | decrease)

  assert_that(any(change),
              msg=Message("There was no spend change during the given periods"))

  # Setting no spend change applies always to all periods.
  no.change <- (assignment %in% kTreatmentAssignment["none"])
  spend[no.change] <- 0

  geos <- ExtractGeos(obj, volume=prop.to)
  map <- structure(geos[[kProportion]], names=geos[[kGeo]])
  prop <- map[obj[[kGeo]]]
  p <- (prop[change] / sum(prop[change]))
  spend[change] <- (total.abs.spend.change * p)
  if (any(decrease)) {
    spend[decrease] <- (-spend[decrease])
  }

  obj[[kSpendChange]] <- spend

  if (add.column) {
    metrics <- GetInfo(obj, "metrics")
    obj <- SetInfo(obj, metrics=c(metrics, kSpendChange))
  }

  return(obj)
}
