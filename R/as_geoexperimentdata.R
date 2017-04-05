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

#' Coerces an object to a GeoExperimentData object.
#'
#' @param obj an object.
#' @param ... further arguments passed to methods.
#'
#' @return A GeoExperimentData object.
#'
#' @rdname as.GeoExperimentData

as.GeoExperimentData <- function(obj, ...) {
  UseMethod("as.GeoExperimentData")
}

#' Coerces a GeoTimeseries object to a GeoExperiment object.
#'
#' @param strict (flag) if FALSE, the additional columns are optional and no
#'   error is thrown if any of them is missing;
#'
#' @return A GeoExperimentData object.
#'
#' @note The GeoTimeseries object is supposed to have the columns 'period',
#'   'geo.group', and 'assignment'. If any of these columns are missing,
#'   the corresponding columns in the resulting object will be 'NA'.
#'
#' @rdname as.GeoExperimentData
as.GeoExperimentData.GeoTimeseries <- function(obj, strict=TRUE, ...) {
  SetMessageContextString("as.GeoExperimentData.GeoTimeseries")
  on.exit(SetMessageContextString())

  obj.ga <- ExtractGeoAssignment(obj, strict=strict)
  obj.periods <- ExtractExperimentPeriods(obj, strict=strict)
  obj.trt <- ExtractTreatmentAssignment(obj, strict=strict)
  obj.ged <- GeoExperimentData(obj,
                               periods=obj.periods,
                               geo.assignment=obj.ga,
                               treat.assignment=obj.trt)
  return(obj.ged)
}
