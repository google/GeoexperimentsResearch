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

#' Returns the geo-to-geo group mapping.
#'
#' @param obj an object with the columns 'geo' and 'geo.group'.
#' @param geo (character vector or NULL) names of geos for which to obtain the
#'   geo group ids.
#' @param ... other arguments passed on to the methods.
#' @return A named integer-valued vector, with the geo names in the names
#'   attribute, mapping geos to geo group ids.
#' @rdname getgeogroup
GetGeoGroup <- function(obj, geo=NULL, ...) {
  UseMethod("GetGeoGroup")
}

#' @note
#' If a specified 'geo' does not exist in the GeoAssignment object, the
#' corresponding geo group will be an NA.
#' @rdname getgeogroup
GetGeoGroup.GeoAssignment <- function(obj, geo=NULL, ...) {
  SetMessageContextString("GetGeoGroup.GeoAssignment")
  on.exit(SetMessageContextString())

  assert_that(is.null(geo) || is.character(geo))

  map <- structure(as.integer(obj[[kGeoGroup]]), names=obj[[kGeo]])

  if (length(geo) == 0) {
    return(map)
  }
  return(map[geo])
}

#' @rdname getgeogroup
GetGeoGroup.GeoExperimentData <- function(obj, geo=NULL, ...) {
  SetMessageContextString("GetGeoGroup.GeoExperimentData")
  on.exit(SetMessageContextString())

  ga <- GetInfo(obj, "geo.assignment")
  assert_that(!is.null(ga), msg="No geo.assignment found")

  map <- GetGeoGroup.GeoAssignment(ga, geo=geo)
  return(map)
}

#' @rdname getgeogroup
GetGeoGroup.GeoStrata <- function(obj, geo=NULL, ...) {
  SetMessageContextString("GetGeoGroup.GeoStrata")
  on.exit(SetMessageContextString())

  map <- GetGeoGroup.GeoAssignment(obj, geo=geo)
  return(map)
}
