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

#' Extracts the names of the geos from the object.
#'
#' @param obj an object.
#' @param groups (NULL, or integer vector) id number(s) of the groups whose
#'   geos to obtain, or NULL for all geos. \code{NA} is allowed.
#'
#' @return A character vector of unique geo identifiers, sorted. Can be empty
#'   if there are no geos matching 'groups'. Will be empty if the geo
#'   assignment object does not exist in the object.
#'
#' @rdname GetGeoNames

GetGeoNames <- function(obj, groups=NULL) {
  assert_that(is.null(groups) ||
              all(is.geo.group.number(groups) %in% c(TRUE, NA)))

  UseMethod("GetGeoNames")
}

#' @rdname GetGeoNames
GetGeoNames.GeoTimeseries <- function(obj, groups=NULL) {
  SetMessageContextString("GetGeoNames.GeoTimeseries")
  on.exit(SetMessageContextString())

  assert_that(is.null(groups),
              msg=Message("No geo group available in this object"))

  geo.names <- sort(unique(obj[[kGeo]]))

  return(geo.names)
}

#' @rdname GetGeoNames
GetGeoNames.GeoExperimentData <- function(obj, groups=NULL) {
  SetMessageContextString("GetGeoNames.GeoExperimentData")
  on.exit(SetMessageContextString())
  
  if (!is.null(groups)) {
    obj.ga <- ExtractGeoAssignment(obj)
    assert_that(!is.null(obj.ga),
                msg=Message("Cannot match groups: there is no geo assignment"))
    geo.names <- GetGeoNames(obj.ga, groups=groups)
  } else {
    geo.names <- GetGeoNames.GeoTimeseries(obj)
  }

  return(geo.names)
}

#' @rdname GetGeoNames
GetGeoNames.GeoAssignment <- function(obj, groups=NULL) {
  all.geo.names <- obj[[kGeo]]
  geo.group <- obj[[kGeoGroup]]
  if (is.null(groups)) {
    geo.names <- all.geo.names
  } else {
    match <- (geo.group %in% groups)
    geo.names <- all.geo.names[match]
  }

  return(sort(geo.names))
}

#' @rdname GetGeoNames
GetGeoNames.Geos <- function(obj, groups=NULL) {
  SetMessageContextString("GetGeoNames.Geos")
  on.exit(SetMessageContextString())

  assert_that(is.null(groups),
              msg=Message("No geo group available in this object"))

  geo.names <- sort(unique(obj[[kGeo]]))
  return(geo.names)
}

#' Extracts the unique names of the geos from a GeoStrata object.
#'
#' @param obj a GeoStrata object.
#' @param groups (NULL, integer vector) id number(s) of the groups whose geos
#'   to obtain, or NULL for all geos. NA is allowed.
#' @return A character vector of unique geo identifiers, sorted.

GetGeoNames.GeoStrata <- function(obj, groups=NULL) {
  geo.names <- GetGeoNames.GeoAssignment(obj, groups=groups)

  return(geo.names)
}
