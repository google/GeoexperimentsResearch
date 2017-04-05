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

#' Extract a GeoStrata object from an object.
#'
#' @param obj an object.
#' @param ... further arguments passed to or from other methods.
#'
#' @return A GeoStrata object.
#'
#' @rdname ExtractGeoStrata

ExtractGeoStrata <- function(obj, ...) {
  UseMethod("ExtractGeoStrata")
}

#' Extract a GeoStrata object from a GeoTimeseries.
#'
#' @param volume (string) name of a metric from which to generate the
#' \code{volume} column.
#'
#' @rdname ExtractGeoStrata
ExtractGeoStrata.GeoTimeseries <- function(obj, volume=NULL, ...) {
  SetMessageContextString("ExtractGeoStrata.GeoTimeseries")
  on.exit(SetMessageContextString())

  obj.geos <- ExtractGeos(obj, volume=volume)
  obj <- GeoStrata(obj.geos, ...)
  return(obj)
}

#' @rdname ExtractGeoStrata
ExtractGeoStrata.GeoExperimentData <- function(obj, volume=NULL, ...) {
  SetMessageContextString("ExtractGeoStrata.GeoExperimentData")
  on.exit(SetMessageContextString())

  obj.geos <- ExtractGeos(obj, volume=volume)
  obj.ga <- GetInfo(obj, "geo.assignment")

  excluded.geos.ga <- obj.ga[obj.ga[[kGeoGroup]] == kExcludeGeoGroup, ,
                             drop=FALSE]
  obj <- GeoStrata(obj.geos, ...)
  if (nrow(excluded.geos.ga) > 0) {
    SetGeoGroup(obj) <- excluded.geos.ga
  }
  return(obj)
}
