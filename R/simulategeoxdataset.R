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

#' Returns a geo experiment data set.
#'
#' @param obj an object.
#' @param ... further arguments passed to or from other methods.
#'
#' @return A GeoExperimentData object with the experiment periods, geo
#' assignment, and the treatment assignment set.
#'
#' @rdname SimulateGeoExperimentData
SimulateGeoExperimentData <- function(obj, ...) {
  UseMethod("SimulateGeoExperimentData")
}

#' Returns a set number i from the GeoExperimentPreanalysisData object.
#'
#' @param i (positive integer or NA) number of the pseudo data set; if NA, the
#' number will be drawn from a uniform discrete distribution.
#'
#' @note If the embedded 'geos' object is GeoStrata, a randomization is done
#' and the geo assignment applied.
#'
#' @rdname SimulateGeoExperimentData
SimulateGeoExperimentData.GeoExperimentPreanalysisData <- function(obj,
    i=NA_integer_, ...) {

  SetMessageContextString(
      "SimulateGeoExperimentData.GeoExperimentPreanalysisData")
  on.exit(SetMessageContextString())

  i.max <- GetInfo(obj, "i.max")
  assert_that((is.count(i) && !isTRUE(is.na(i)) && i <= i.max) ||
              isTRUE(is.na(i)),
              msg=Messagef("i must be >=1 and <= %d", i.max))

  if (is.na(i)) {
    i <- sample.int(i.max, size=1)
  }

  period.lengths <- GetInfo(obj, "period.lengths")
  # There may be 2 or 3 periods.
  n.periods <- length(period.lengths)
  cum.pl <- cumsum(period.lengths)
  period.offset <- c(0L, cum.pl[-n.periods], cum.pl[n.periods] - 1L)

  dates <- GetInfo(obj, "dates")
  date.start <- dates[i]

  period.names <- c("Pretest", "Test", "Cooldown")[seq_len(n.periods)]
  period.dates <- (date.start + period.offset)
  obj.periods <- ExperimentPeriods(period.dates,
                                   period.names=period.names)
  geos <- GetInfo(obj, "geos")
  if (inherits(geos, "GeoAssignment")) {
    geo.assignment <- GetInfo(obj, "geo.assignment")  # GeoAssignment.
  } else {
    geo.assignment <- Randomize(geos)  # GeoStrata.
  }

  treatment.assignment <- GetInfo(obj, "treat.assignment")

  kClassName <- "GeoExperimentPreanalysisData"
  class(obj) <- setdiff(class(obj), kClassName)

  obj.result <- GeoExperimentData(obj,
                                  periods=obj.periods,
                                  geo.assignment=geo.assignment,
                                  treat.assignment=treatment.assignment)
  return(obj.result)
}
