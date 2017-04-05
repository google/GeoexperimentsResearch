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

#' Associates the object with an ExperimentPeriods object.
#'
#' @param obj the object to change.
#' @param ... further arguments passed to methods.
#' @param value a ExperimentPeriods object.
#'
#' @return The object that has been modified in place.
#'
#' @rdname SetExperimentPeriods

"SetExperimentPeriods<-" <- function(obj, ..., value) {
  UseMethod("SetExperimentPeriods<-")
}

#' @param strict (flag) insist that data has all specified experiment periods?
#'
#' @note The 'date' column is mapped to the 'period' column. The 'period' slot
#' is assigned with the new value and the columns 'period' and 'assignment' are
#' changed to reflect the new date ranges.
#'
#' @rdname SetExperimentPeriods
"SetExperimentPeriods<-.GeoExperimentData" <- function(obj, strict=TRUE, ...,
                                                       value) {
  SetMessageContextString("SetExperimentPeriods<-.GeoExperimentData")
  on.exit(SetMessageContextString())

  assert_that(is.flag(strict))
  assert_that(is.null(value) || inherits(value, "ExperimentPeriods"),
              msg=Message("An ExperimentPeriods object or NULL is required"))

  obj.periods <- value
  if (is.null(obj.periods)) {
    period.numbers <- NA_integer_
  } else {
    column.order <- names(obj)
    dates <- obj[[kDate]]
    period.numbers <- rep(NA_integer_, length.out=nrow(obj))
    for (i in seq_len(nrow(obj.periods))) {
      period.number <- obj.periods[i, "Period"]
      start.period <- obj.periods[i, "Start"]
      end.period <- obj.periods[i, "End"]
      dates.in.range <- (dates >= start.period & dates <= end.period)
      if (strict) {
        assert_that(any(dates.in.range),
                    msg=paste0("No data available for period ", period.number))
      }
      period.numbers[dates.in.range] <- period.number
    }
  }
  obj[[kPeriod]] <- period.numbers
  obj <- SetInfo(obj, periods=obj.periods)
  # Re-generate the treatment assignment vector, as 'geo.groups' may
  # have changed.
  SetTreatmentAssignment(obj) <- GetInfo(obj, "treat.assignment")
  return(obj)
}
