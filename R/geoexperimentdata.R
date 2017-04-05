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

#' Constructs a GeoExperimentData object.
#'
#' @param geo.timeseries a GeoTimeseries object.
#' @param periods an \code{ExperimentPeriods} object, specifying the start and
#' end dates of each period in the experiment. Or, \code{NULL} if the dates are yet
#' unknown. Or, leave unspecified to extract this information from the column
#' 'periods' of the \code{geo.timeseries}; in this case the column \emph{must}
#' exist in the data frame.
#' @param geo.assignment a \code{GeoAssignment} object, specifying the mapping
#' from a geo to a geo group. Or, \code{NULL} if the geo assignment is yet
#' unknown.  Or, leave unspecified to extract this information from the column
#' 'geo.group' of \code{geo.timeseries}; in this case the column \emph{must}
#' exist in the data frame.
#' @param treat.assignment a \code{TreatmentAssignment} object, specifying the
#' mapping from (period, geo) to a treatment assignment condition (treatment
#' intervention type). Or, \code{NULL} if the treatment assignment is yet
#' unknown. Or, leave unspecified to extract this information from the column
#' \code{assignment} of \code{geo.timeseries}.  If the column \code{assignment}
#' does not exist, the resulting object will have the column \code{assignment}
#' filled with \code{NA}s.
#'
#' @return A GeoExperimentData object.
#'
#' @details If any of the arguments \code{periods}, \code{geo.assignment}, or
#' \code{treat.assignment} is \code{NULL}, the corresponding column
#' (\code{period}, \code{geo.group}, \code{assignment}) in the resulting
#' \code{GeoExperimentData} object will have only \code{NA}s.\cr
#'
#' If \code{geo.timeseries} has columns \code{period}, \code{geo.group}, or
#' \code{assignment}, they will be overwritten by the values specified by
#' \code{periods}, \code{geo.assignment}, and \code{treat.assignment}.\cr
#'
#' The resulting object \emph{may} have undefined \code{periods},
#' \code{geo.group}, or \code{assignment}.  The missing parts may be filled in
#' by using the functions \code{SetExperimentPeriods}, \code{SetGeoAssignment},
#' and \code{SetTreatmentAssignment}.

GeoExperimentData <- function(geo.timeseries,
                              periods,
                              geo.assignment,
                              treat.assignment) {
  kClassName <- "GeoExperimentData"
  SetMessageContextString(kClassName)
  on.exit(SetMessageContextString())

  assert_that(inherits(geo.timeseries, "GeoTimeseries"))
  if (missing(periods)) {
    periods <- ExtractExperimentPeriods(geo.timeseries)
  } else {
    assert_that(is.null(periods) ||
                inherits(periods, "ExperimentPeriods"))
  }
  if (missing(geo.assignment)) {
    geo.assignment <- ExtractGeoAssignment(geo.timeseries)
  } else {
    assert_that(is.null(geo.assignment) ||
                inherits(geo.assignment, "GeoAssignment"))
  }
  if (missing(treat.assignment)) {
    # The assignment column can be missing; in this case,
    # treat.assignment will be NULL and the resulting data frame
    # will have the column filled with NAs.
    treat.assignment <- ExtractTreatmentAssignment(geo.timeseries,
                                                   strict=FALSE)
  } else {
    assert_that(is.null(treat.assignment) ||
                inherits(treat.assignment, "TreatmentAssignment"))
  }

  metrics <- GetInfo(geo.timeseries, "metrics")
  # Add the new columns. If they exist, they will be overwritten and
  # filled by the Set*<- functions.
  obj <- geo.timeseries
  obj[[kPeriod]] <- NA_integer_
  obj[[kGeoGroup]] <- NA_integer_
  obj[[kAssignment]] <- NA_character_
  internal <- c(kDate, kGeo, kPeriod, kGeoGroup, kAssignment)
  new.column.order <- union(c(internal, metrics), names(obj))
  obj <- obj[new.column.order]
  # Add the attributes.
  obj <- SetInfo(obj,
                 metrics=metrics,
                 periods=NULL,
                 geo.assignment=NULL,
                 treat.assignment=NULL)
  # The new class inherits from GeoTimeseries.
  if (!inherits(obj, kClassName)) {
    class(obj) <- c(kClassName, class(geo.timeseries))
  }
  # Use the 'Set' functions to set the values of the three columns.
  SetExperimentPeriods(obj) <- periods
  SetGeoAssignment(obj) <- geo.assignment
  SetTreatmentAssignment(obj) <- treat.assignment
  return(obj)
}
