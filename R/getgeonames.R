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

GetGeoNames <- function(obj, groups=NULL) {
  # Extracts the names of the geos from the object.
  #
  # Args:
  #   obj: an object.
  #   groups: (NULL, or integer vector) id number(s) of the groups whose geos to
  #     obtain, or NULL for all geos. NA is allowed.
  #
  # Returns:
  #   A character vector of unique geo identifiers.

  assert_that(is.null(groups) ||
              all(is.geo.group.number(groups) %in% c(TRUE, NA)))

  UseMethod("GetGeoNames")
}

GetGeoNames.GeoTimeseries <- function(obj, groups=NULL) {
  # Extracts the unique names of the geos from a GeoAssignment object.
  #
  # Args:
  #   obj: a GeoTimeseries object.
  #   groups: should be NULL, otherwise an error occurs.
  #
  # Returns:
  #   A character vector of unique geo identifiers, sorted. Can be empty if
  #   there are no geos matching 'groups'.

  SetMessageContextString("GetGeoNames.GeoTimeseries")
  on.exit(SetMessageContextString())
  
  assert_that(is.null(groups),
              msg=Message("No geo group available in this object"))

  geo.names <- sort(unique(obj[[kGeo]]))

  return(geo.names)
}

GetGeoNames.GeoExperimentData <- function(obj, groups=NULL) {
  # Extracts the unique names of the geos from a GeoAssignment object.
  #
  # Args:
  #   obj: a GeoTimeseries (including GeoExperimentData) object.
  #   groups: (NULL or integer vector) id number(s) of the groups whose geos to
  #     obtain, or NULL for all geos. NA is allowed.
  #
  # Returns:
  #   A character vector of unique geo identifiers, sorted. Can be empty if
  #   there are no geos matching 'groups'.  Will be empty if the geo assignment
  #   object does not exist in the object.

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

GetGeoNames.GeoAssignment <- function(obj, groups=NULL) {
  # Extracts the names of the geos from a GeoAssignment object.
  #
  # Args:
  #   obj: a GeoAssignment object.
  #   groups: (NULL or integer vector) id number(s) of the groups whose geos to
  #     obtain, or NULL for all geos.  NA is allowed.
  #
  # Returns:
  #   A character vector of unique geo identifiers, sorted by their name. May
  #   be empty if none of the groups specified are found in the object.

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

GetGeoNames.Geos <- function(obj, groups=NULL) {
  # Extracts the unique names of the geos from a Geos object.
  #
  # Args:
  #   obj: a Geos object.
  #   groups: should be NULL, otherwise an error occurs.
  #
  # Returns:
  #   A character vector of unique geo identifiers, sorted. Cannot be empty.

  SetMessageContextString("GetGeoNames.Geos")
  on.exit(SetMessageContextString())

  assert_that(is.null(groups),
              msg=Message("No geo group available in this object"))

  geo.names <- sort(unique(obj[[kGeo]]))
  return(geo.names)
}

GetGeoNames.GeoStrata <- function(obj, groups=NULL) {
  # Extracts the unique names of the geos from a GeoStrata object.
  #
  # Args:
  #   obj: a GeoStrata object.
  #   groups: (NULL, integer vector) id number(s) of the groups whose geos to
  #     obtain, or NULL for all geos. NA is allowed.
  #
  # Returns:
  #   A character vector of unique geo identifiers, sorted.

  geo.names <- GetGeoNames.GeoAssignment(obj, groups=groups)

  return(geo.names)
}
