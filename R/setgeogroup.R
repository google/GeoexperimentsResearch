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

"SetGeoGroup<-" <- function(obj, value) {
  # Modifies the geo assignment in an object.
  #
  # Args:
  #   obj: an object with the columns 'geo' and 'geo.group'.
  #   value: a GeoAssignment object.
  #
  # Returns:
  #   The object with the modfied geo-to-geo.group mapping.
  #
  # Notes:
  #   See also: SetGeoAssignment<-.

  UseMethod("SetGeoGroup<-")
}

"SetGeoGroup<-.GeoStrata" <- function(obj, value) {
  # Modifies the geo assignment in a GeoStrata object.
  #
  # Args:
  #   obj: a GeoStrata object.
  #   value: a GeoAssignment object.
  #
  # Returns:
  #   The same object, with the geo.group column modified.

  SetMessageContextString("SetGeoGroup<-.GeoStrata")
  on.exit(SetMessageContextString())

  assert_that(inherits(value, "GeoAssignment"))

  geo.assignment <- value
  match.row <- match(geo.assignment[[kGeo]], table=obj[[kGeo]], nomatch=NA)
  geos.not.there <- structure(is.na(match.row),
                              names=geo.assignment[[kGeo]])
  assert_that(!any(geos.not.there),
              msg=Message(FormatText(geos.not.there,
                  "The geos $X are not in the GeoAssignment object.")))

  n.groups <- GetInfo(obj, "n.groups")
  groups.in.geoassignment <- unique(value[[kGeoGroup]])
  allowed.groups <- seq(from=0L, to=n.groups)
  groups.not.there <- groups.in.geoassignment[!(groups.in.geoassignment
                                                %in% allowed.groups)]
  assert_that(length(groups.not.there) == 0,
              msg=Message("The following group ids are not compatible with ",
                  "the GeoStrata object: ",
                  paste0(groups.not.there, collapse=", ")))

  obj[[kGeoGroup]][match.row] <- geo.assignment[[kGeoGroup]]

  # Redo stratification: the omitted geos (if any) must not belong to any
  # strata.
  group.ratios <- GetInfo(obj, "group.ratios")
  new.strata <- .GenerateStrata(obj[[kGeoGroup]],
                                group.ratios=group.ratios)
  obj[[kStratum]] <- new.strata

  return(obj)
}


"SetGeoGroup<-.GeoAssignment" <- function(obj, value) {
  # Modifies a GeoAssignment object.
  #
  # Args:
  #   obj: a GeoAssignment object.
  #   value: a GeoAssignment object.
  #
  # Returns:
  #   The same object, with the geo.group column modified.

  SetMessageContextString("SetGeoGroup<-.GeoAssignment")
  on.exit(SetMessageContextString())

  assert_that(inherits(value, "GeoAssignment"))

  geo.assignment <- value
  match.row <- match(geo.assignment[[kGeo]], table=obj[[kGeo]], nomatch=NA)
  geos.not.there <- structure(is.na(match.row),
                              names=geo.assignment[[kGeo]])

  assert_that(!any(geos.not.there),
              msg=Message(FormatText(geos.not.there,
                  "The geos $X are not in the GeoAssignment object.")))

  obj[[kGeoGroup]][match.row] <- geo.assignment[[kGeoGroup]]

  return(obj)
}

"SetGeoGroup<-.GeoExperimentData" <- function(obj, value) {
  # Modifies the GeoAssignment object in the GeoExperimentData object.
  #
  # Args:
  #   obj: a GeoExperimentData object.
  #   value: a GeoAssignment object.
  #
  # Returns:
  #   The same object, with the geo assignment (and therefore also the
  #   geo.group column) modified.
  #
  # Notes:
  #   The GeoExperimentData object must contain a valid GeoAssignment
  #   object. Use SetGeoAssignment<- to associate a GeoAssignment object to the
  #   GeoExperimentData object.

  SetMessageContextString("SetGeoGroup<-.GeoExperimentData")
  on.exit(SetMessageContextString())

  assert_that(inherits(value, "GeoAssignment"))

  geo.assignment <- ExtractGeoAssignment(obj)
  assert_that(inherits(geo.assignment, "GeoAssignment"),
              msg=Message("The GeoExperimentData object does not ",
                  "contain a GeoAssignment object (Set it using ",
                  "SetGeoAssignment<- first)"))

  SetGeoGroup(geo.assignment) <- value
  SetGeoAssignment(obj) <- geo.assignment
  return(obj)
}
