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

# Constructor and S3 methods for TreatmentAssignment objects.
# Constructor:
# - TreatmentAssignment
# Generic methods:
# - ExtractTreatmentAssignment, SetTreatmentAssignment<-
# Other functions:
#  - DefaultTreatmentAssignment

TreatmentAssignment <- function(x) {
  # Constructs a TreatmentAssignment object.
  #
  # Args:
  #   x: (data frame) a mapping from (period, group) to treatment
  #     assignment condition. The data frame must have the columns
  #     'period' (integer-valued), 'geo.group' (integer-valued,
  #     positive), and 'assignment' (integer-valued, one of 0, 1, -1),
  #     Each row specifies the mapping of one (period, group) pair. No
  #     missing values are allowed.
  #
  # Returns:
  #   An object of class 'TreatmentAssignment'.

  kClassName <- "TreatmentAssignment"
  SetMessageContextString(kClassName)
  on.exit(SetMessageContextString())

  assert_that(is.data.frame(x),
              msg=Message("'x' must be a data.frame"))
  rownames(x) <- NULL
  # Required columns.
  required <- c(period=kPeriod, group=kGeoGroup, assignment=kAssignment)
  # Ensure that all required columns are in the given data frame.
  CheckForMissingColumns(required, dataframe=x, what="required")
  # Ensure that the required columns are of the specified type.
  required.type <- list(period=is.integer.valued,
                        group=is.integer.valued,
                        assignment=is.integer.valued)
  names(required.type) <- required[names(required.type)]
  CheckForTypes(x, checklist=required.type)
  # Do not allow missing values in the required columns.
  CheckForBadValues(x, columns=required, CHECK=is.na, good=FALSE,
                    what="missing")
  # Ensure that the geos are not duplicated.
  CheckForDuplicateRows(x, columns=c(kPeriod, kGeoGroup))
  # Check for invalid values.
  CheckForBadValues(x, columns=kAssignment, CHECK=is.treatment.assignment)
  CheckForBadValues(x, columns=kGeoGroup, CHECK=is.geo.group.number)
  CheckForBadValues(x, columns=kPeriod, CHECK=is.period.number)
  obj <- x
  class(obj) <- c(kClassName, oldClass(obj))
  return(obj)
}

DefaultTreatmentAssignment <- function() {
  # Generates the default treatment assignment condition.
  #
  # Returns:
  #   A TreatmentAssignment object.
  #
  # Notes:
  #   Period 1 of group 2 is assigned a change (some intervention).

  required <- c(period=kPeriod, group=kGeoGroup, assignment=kAssignment)
  df.assign <- data.frame(period=1L, group=2L,
                          assignment=kTreatmentAssignment["change"],
                          stringsAsFactors=FALSE)
  names(df.assign) <- required[names(df.assign)]
  obj <- TreatmentAssignment(df.assign)
  return(obj)
}

ExtractTreatmentAssignment <- function(obj, ...) {
  # Extracts a TreatmentAssignment object.
  #
  # Args:
  #   obj: an object.
  #   ...: further arguments passed on to methods.
  #
  # Returns:
  #   A TreatmentAssignment object.
  #
  # Notes:
  #   A generic S3 method.
  #
  # Documentation:
  #   seealso: SetTreatmentAssignment<-.GeoExperimentData.

  UseMethod("ExtractTreatmentAssignment")
}

"SetTreatmentAssignment<-" <- function(obj, ..., value) {
  # Associates the object with an TreatmentAssignment object.
  #
  # Args:
  #   obj: the object to change.
  #   ...: further arguments passed to methods.
  #   value: a TreatmentAssignment object.
  #
  # Returns:
  #   The object (that has been modified in place).
  #
  # Documentation:
  #   seealso: SetTreatmentAssignment<-.GeoExperimentData.

  UseMethod("SetTreatmentAssignment<-")
}
