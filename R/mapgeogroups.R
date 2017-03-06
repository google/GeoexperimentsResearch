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

"MapGeoGroups<-" <- function(obj, value) {
  # Map or merge geo group numbers into new group ids.
  #
  # Args:
  #   obj: an object.
  #   value: (integer vector, NA allowed, NULL or empty integer vector allowed)
  #     mapping of old group numbers to new ones. The length must be exactly
  #     equal to the number of old groups in the object.  The old numbers match
  #     the positions of the index, and the new ones are the values in the
  #     vector. For example, c(2, 1) maps 1->2 and 2->1; c(3, 2, 1) maps 1->3,
  #     3->1 and 2 stays unchanged; '1:2' has no effect; NOTE: c(1, 1) merges
  #     groups 1 and 2 into 1! If the 'geo.group' is NA in the object, its
  #     value will be NOT changed. If the data has only NAs, the only mapping
  #     that is allowed is an empty vector, resulting in no change in the
  #     original object. The special value '0' (group of excluded geos) will
  #     also be unchanged.
  #
  # Returns:
  #   An object identical to 'obj' except the 'geo.groups' column has
  #   (possibly) changed.
  #

  UseMethod("MapGeoGroups<-")
}


"MapGeoGroups<-.GeoAssignment" <- function(obj, value) {
  # Map or merge geo group numbers into new group ids.
  #
  # Args:
  #   obj: a GeoAssignment object.
  #   value: (integer vector, NA allowed, NULL or empty integer vector allowed)
  #     mapping of old group numbers to new ones. The length must be exactly
  #     equal to the number of old groups in the object.  The old numbers match
  #     the positions of the index, and the new ones are the values in the
  #     vector. For example, c(2, 1) maps 1->2 and 2->1; c(3, 2, 1) maps 1->3,
  #     3->1 and 2 stays unchanged; '1:2' has no effect; NOTE: c(1, 1) merges
  #     groups 1 and 2 into 1! If the 'geo.group' is NA in the object, its
  #     value will be NOT changed. If the data has only NAs, the only mapping
  #     that is allowed is an empty vector or NULL, resulting in no change in
  #     original object. The special value '0' (group of excluded geos) will
  #     also be unchanged.
  #
  # Returns:
  #   A GeoAssignment object, identical to 'obj' except the 'geo.groups' column
  #   has (possibly) changed.
  #
  # Notes:
  #   If all group ids in 'geo.group' are NA, nothing changes as 'NAs' cannot
  #   be mapped. Try 'SetGeoGroup<-' instead, to map specific geos first into
  #   group ids.\cr
  #
  #   The group number '0' (group of excluded geos) remains unchanged. It is
  #   not possible to map this group to another. For that purpose, map geos
  #   explicitly to desired groups using the replacement method
  #   'SetGeoGroup<-'.\cr
  #
  #   It is however possible to exclude a complete group by mapping it to 0 by
  #   including 0 in the map, for example c(1, 2, 0) maps group 3 to 0, leaving
  #   group numbers 1 and 2 unchanged.

  SetMessageContextString("MapGeoGroups<-.GeoAssignment")
  on.exit(SetMessageContextString())

  group.map <- value
  assert_that(is.null(group.map) ||
              (length(group.map) == 0 && is.numeric(group.map)) ||
              all(is.geo.group.number(group.map), na.rm=TRUE),
              msg=Message("Geo group numbers must be NA, 0, or positive."))
  group.map <- as.integer(group.map)

  geo.group.column <- obj[[kGeoGroup]]
  all.old.group.ids <- sort(na.omit(unique(geo.group.column)))
  n.old.groups <- max(all.old.group.ids)
  if (length(all.old.group.ids) == 0 || n.old.groups == 0) {
    assert_that(length(group.map) == 0,
                msg=Message("Nothing to map: no geo group numbers in the data"))
    return(obj)
  }

  assert_that(length(group.map) == n.old.groups,
              msg=Message("Expected a vector of length ", n.old.groups))

  # Re-map temporarily all groups to group + 1 to map group 0 to 1. For
  # example, c(2, 1) maps 0, 1, 2 first to 1, 3, 2, then gets shifted back to
  # 0, 2, 1.
  group.map.shifted <- c(1L, group.map + 1L)
  geo.group.shifted <- group.map.shifted[(geo.group.column + 1L)]
  obj[[kGeoGroup]] <- (geo.group.shifted - 1L)
  return(obj)
}

"MapGeoGroups<-.GeoExperimentData" <- function(obj, value) {
  # Map or merge geo group numbers into new group ids.
  #
  # Args:
  #   obj: a GeoExperimentData object.
  #   value: (integer vector or NA) mapping of old group numbers to new
  #     ones. See 'MapGeoGroups<-.GeoAssignment' for details.
  #
  # Returns:
  #   The GeoExperimentData object, with the column 'geo.group' affected.

  obj.ga <- ExtractGeoAssignment(obj)
  MapGeoGroups(obj.ga) <- value
  SetGeoAssignment(obj) <- obj.ga
  return(obj)
}
