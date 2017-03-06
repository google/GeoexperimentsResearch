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

GetGeoGroup <- function(obj, geo=NULL, ...) {
  # Returns the geo-to-geo group mapping.
  #
  # Args:
  #   obj: an object with the columns 'geo' and 'geo.group'.
  #   geo: (character vector or NULL) names of geos for which to obtain the geo
  #     group ids.
  #   ...: other arguments passed on to the methods.
  #
  # Returns:
  #   A named integer-valued vector, with the geo names in the names attribute,
  #   mapping geos to geo group ids.

  UseMethod("GetGeoGroup")
}

GetGeoGroup.GeoAssignment <- function(obj, geo=NULL, ...) {
  # Returns the geo group of a geo in a GeoStrata object.
  #
  # Args:
  #   obj: a GeoAssignment object.
  #   geo: (character vector or NULL) names of geos for which to obtain the geo
  #     group ids.
  #   ...: ignored.
  #
  # Returns:
  #   A named integer-valued vector, with the geo names in the names attribute,
  #   mapping geos to geo group ids.
  #
  # Notes:
  #   If a specified 'geo' does not exist in the GeoAssignment object, the
  #   corresponding geo group will be an NA.

  SetMessageContextString("GetGeoGroup.GeoAssignment")
  on.exit(SetMessageContextString())

  assert_that(is.null(geo) || is.character(geo))

  map <- structure(as.integer(obj[[kGeoGroup]]), names=obj[[kGeo]])

  if (length(geo) == 0) {
    return(map)
  }
  return(map[geo])
}

GetGeoGroup.GeoExperimentData <- function(obj, geo=NULL, ...) {
  # Returns the geo group of a geo in a GeoExperimentData object.
  #
  # Args:
  #   obj: a GeoExperiment object.
  #   geo: (character vector or NULL) names of geos for which to obtain the geo
  #     group ids.
  #   ...: ignored.
  #
  # Returns:
  #   A named integer-valued vector, with the geo names in the names attribute,
  #   mapping geos to geo group ids.

  SetMessageContextString("GetGeoGroup.GeoExperimentData")
  on.exit(SetMessageContextString())

  ga <- GetInfo(obj, "geo.assignment")
  assert_that(!is.null(ga), msg="No geo.assignment found")

  map <- GetGeoGroup.GeoAssignment(ga, geo=geo)
  return(map)
}

GetGeoGroup.GeoStrata <- function(obj, geo=NULL, ...) {
  # Returns the geo group of a geo in a GeoStrata object.
  #
  # Args:
  #   obj: a GeoStrata object.
  #   geo: (character vector or NULL) names of geos for which to obtain the geo
  #     group ids.
  #   ...: ignored.
  #
  # Returns:
  #   A named integer-valued vector, with the geo names in the names attribute,
  #   mapping geos to geo group ids.

  SetMessageContextString("GetGeoGroup.GeoStrata")
  on.exit(SetMessageContextString())

  map <- GetGeoGroup.GeoAssignment(obj, geo=geo)
  return(map)
}
