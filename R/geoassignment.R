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

GeoAssignment <- function(x) {
  # Constructs a GeoAssignment object.
  #
  # Args:
  #   x: a data frame with columns 'geo' (character) and 'geo.group'
  #     (integer-valued, positive).  'geo.group' indicates the group
  #     (1, 2, ...) the corresponding geo belongs to. Other columns
  #     are allowed but not checked. If 'geo' is an integer-valued
  #     numeric or factor, it is coerced to character.
  #
  # Returns:
  #   An object of class 'GeoAssignment'.
  #
  # Notes:
  #   The group numbers must be integers (integer-valued numeric are
  #   allowed as input), starting with 1.
  #
  #   The column 'geo.group' *can* have missing values (NA), unlike
  #   'geo', which must have no missing values, and no duplicates.
  #
  #   A missing value in geo.group simply indicates that the
  #   mapping from a geo is not available. When associating the geo
  #   assignment with a GeoExperimentData object, the effect will be the
  #   same as if the geo with 'geo.group'==NA was not in the mapping
  #   in the first place.

  kClassName <- "GeoAssignment"
  SetMessageContextString(kClassName)
  on.exit(SetMessageContextString())

  # Make sure 'x' is a plain data frame.
  assert_that(is.data.frame(x),
              msg=Message("'x' must be a data.frame"))
  # Required columns.
  required <- c(geo=kGeo, group=kGeoGroup)
  # Ensure that all required columns are in the given data frame.
  CheckForMissingColumns(required, dataframe=x, what="required")
  # Coerce 'geo' to character.
  if (is.factor(x[[kGeo]]) || is.integer.valued(x[[kGeo]])) {
    x[[kGeo]] <- as.character(x[[kGeo]])
  }
  # Ensure that the required columns are of the specified type.
  checklist <- list(geo=is.character.vector,
                    group=is.integer.valued)
  names(checklist) <- required[names(checklist)]
  CheckForTypes(x, checklist=checklist)
  # Do not allow missing values in 'geo'.
  CheckForBadValues(x, columns=kGeo, CHECK=is.na, good=FALSE,
                    what="missing")
  # Check for specific bad values in 'geo.group'. Note: NAs are OK.
  CheckForBadValues(x, columns=kGeoGroup, CHECK=is.geo.group.number,
                    good=c(TRUE, NA), what="bad")
  x[[kGeoGroup]] <- as.integer(x[[kGeoGroup]])

  # Check for duplicate 'geo' columns.
  CheckForDuplicateRows(x, columns=kGeo)
  obj <- x
  obj <- SetInfo(obj, info=list())
  class(obj) <- c(kClassName, oldClass(obj))
  return(obj)
}
