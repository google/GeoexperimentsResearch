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

#' Modifies the geo assignment in an object.
#'
#' @aliases SetGeoGroup
#'
#' @param obj an object with the columns 'geo' and 'geo.group'.
#' @param value a GeoAssignment object.
#' @return The object with the modfied geo-to-geo.group mapping.
#'
#' @seealso \code{\link{SetGeoAssignment<-}}, \code{\link{MapGeoGroups<-}}.
#'
#' @rdname setgeogroup
"SetGeoGroup<-" <- function(obj, value) {
  UseMethod("SetGeoGroup<-")
}

#' @rdname setgeogroup
"SetGeoGroup<-.GeoStrata" <- function(obj, value) {
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

#' @rdname setgeogroup
"SetGeoGroup<-.GeoAssignment" <- function(obj, value) {
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

#' @note
#' The \code{\link{GeoExperimentData}} object must contain a valid
#' \code{\link{GeoAssignment}} object. (Use \code{\link{SetGeoAssignment<-}} to
#' associate a \code{\link{GeoAssignment}} object to the
#' \code{\link{GeoExperimentData}} object.)
#' @rdname setgeogroup
"SetGeoGroup<-.GeoExperimentData" <- function(obj, value) {
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
