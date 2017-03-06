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

"SetGeoAssignment<-" <- function(obj, ..., value) {
  # Associates the object with a GeoAssignment object.
  #
  # Args:
  #   obj: the object to change.
  #   ...: further arguments passed to methods.
  #   value: a GeoAssignment object.
  #
  # Returns:
  #   The object (that has been modified in place).
  #
  # Documentation:
  #   seealso: SetGeoAssignment.GeoExperimentData.

  UseMethod("SetGeoAssignment<-")
}

"SetGeoAssignment<-.GeoExperimentData" <- function(obj, strict=TRUE, ...,
                                                   value) {
  # Associates the object with a GeoAssignment object, mapping geos
  # into geo group numbers.
  #
  # Args:
  #   obj: the GeoExperimentData object to change.
  #   strict: (flag) insist that all geos in the data are mapped?
  #     Also, warn if some geos are not found in the data?
  #   ...: ignored.
  #   value: a GeoAssignment object to associate with the
  #     GeoExperimentData object.
  #
  # Returns:
  #   The same object that has been modified in place; the
  #   'geo.assignment' slot is assigned with the new value and the
  #   columns 'geo.group' and 'assignment' are changed to reflect the
  #   new geo assignment.
  #
  # Notes:
  #   If 'value' is 'NULL', the geo assignment and the treatment
  #   assignment are removed, and their corresponding columns in the
  #   data frame are reset to 'NA'.
  #
  #   An error is thrown if any geo in data cannot be mapped to a
  #   group (due to the GeoAssignment object not having such a
  #   mapping). Setting 'strict' to FALSE will avert this.
  #
  #   An error is thrown if some of the geos in the mapping were
  #   not present in the data. Setting 'strict' to FALSE will avert
  #   this.
  #
  # Documentation:
  #   seealso: SetGeoAssignment<- (generic).

  kClassName <- "SetGeoAssignment<-.GeoExperimentData"
  SetMessageContextString(kClassName)
  on.exit(SetMessageContextString())

  assert_that(is.flag(strict))
  assert_that(is.null(value) || inherits(value, "GeoAssignment"),
              msg=Message("A GeoAssignment object or NULL is required"))

  geo.assignment <- value
  if (is.null(geo.assignment)) {
    geo.groups <- NA_integer_
   } else {
    df.ged <- as.data.frame(obj)[kGeo]
    df.ged[[kTmpi]] <- seq_len(nrow(df.ged))
    df.ged.m <- merge(df.ged, geo.assignment[c(kGeo, kGeoGroup)],
                      by=kGeo, all.x=TRUE, all.y=FALSE, sort=FALSE)
    # Restore original order.
    df.ged.m <- df.ged.m[order(df.ged.m[[kTmpi]]), , drop=FALSE]
    geo.groups <- df.ged.m[[kGeoGroup]]
    if (strict) {
      geos.in.data <- unique(obj[[kGeo]])
      geos.in.map <- geo.assignment[[kGeo]]
      nonexisting.map.geos <- structure(!(geos.in.map %in% geos.in.data),
                                        names=geos.in.map)
      assert_that(!any(nonexisting.map.geos),
                  msg=Message(
                      FormatText(nonexisting.map.geos,
                                 "The following geo{|s} w{as|ere} not found ",
                                 "in the data: $X")))
      nonexisting.data.geos <- structure(!(geos.in.data %in% geos.in.map),
                                         names=geos.in.data)
      assert_that(!any(nonexisting.data.geos),
                  msg=Message(
                      FormatText(nonexisting.data.geos,
                                 "The following geo{|s} w{as|ere} not found ",
                                 "in the geo assignment: $X")))
    }
  }
  obj[[kGeoGroup]] <- geo.groups
  obj <- SetInfo(obj, geo.assignment=geo.assignment)
  # Re-generate the treatment assignment vector, as geo groups may
  # have changed.
  SetTreatmentAssignment(obj) <- GetInfo(obj, "treat.assignment")
  return(obj)
}
