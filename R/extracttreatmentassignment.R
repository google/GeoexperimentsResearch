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

ExtractTreatmentAssignment.GeoTimeseries <- function(obj, strict=TRUE, ...) {
  # Extracts a TreatmentAssignment object from a GeoTimeseries object.
  #
  # Args:
  #   obj: a GeoTimeseries object with the columns 'period',
  #     'geo.group', and 'assignment'.
  #   strict: (flag) if FALSE, the function returns NULL if either of
  #     the columns 'geo.group' or 'period' does not exist. Otherwise,
  #     throws an error.
  #   ...: ignored.
  #
  # Returns:
  #   A TreatmentAssignment object.
  #
  # Notes:
  #   A well-defined (period, group) pair (i.e., neither of them is
  #   missing) in the data frame implies that the corresponding (date,
  #   geo) pair is part of the experiment and *must* be therefore
  #   associated a treatment condition. Otherwise, if a date or a geo
  #   is not part of the experiment, the (date, geo) pair *must* have *no*
  #   treatment condition assignment.
  #
  #   In other words, any (period, group) pair that maps to a
  #   non-missing treatment assignment must have no missing
  #   values. Conversely, any (period, group) pair with a missing
  #   value must map to a missing treatment assignment.
  #
  # Documentation:
  #   seealso: ExtractTreatmentAssignment (generic).

  SetMessageContextString("ExtractTreatmentAssignment.GeoTimeseries")
  on.exit(SetMessageContextString())

  assert_that(is.flag(strict))
  # If no required columns are there, return NULL (if strict is FALSE).
  if (!strict && !all(c(kGeoGroup, kPeriod, kAssignment) %in% names(obj))) {
    return(NULL)
  }
  # Ensure that the required columns are in the given data frame.
  keys <- c(kPeriod, kGeoGroup)
  required <- c(keys, kAssignment)
  CheckForMissingColumns(required, dataframe=obj, what="required")
  # Check that (group, period) -> (treatment assignment) mapping
  # is unique.
  CheckForMapping(obj, from=keys, to=kAssignment)
  # Find the unique combinations; strip other class attributes.
  df.uniq <- as.data.frame(unique(obj[required]))
  order.rows <- do.call(order, as.list(df.uniq[keys]))
  df.uniq <- df.uniq[order.rows, , drop=FALSE]
  # Check mappings for missing values.
  nas <- is.na(df.uniq)  # A matrix.
  if (any(nas)) {
    if (all(nas)) {
      # If everything is NA, return NULL => No mapping avaiable.
      return(NULL)
    }
    spec <- (!nas)
    #                          Treatment assignment specified?
    #                           Yes                      No
    #                 +----------------------+------------------------+
    # Both        Yes | a proper assignment  |  missing assignment    |
    # Period &        |        (OK)          |        (error)         |
    # Group           +----------------------+------------------------+
    # Specified?   No | ambiguous assignment | outside of experiment  |
    #                 |       (error)        |       (ignore)         |
    #                 +-------------------+---------------------------+
    treatment.specified <- spec[, kAssignment]
    period.and.group.specified <- (spec[, kPeriod] & spec[, kGeoGroup])
    good.mapping <- (treatment.specified & period.and.group.specified)
    ignorable.mapping <- (!treatment.specified & !period.and.group.specified)
    bad.mapping <- (!good.mapping & !ignorable.mapping)
    if (any(bad.mapping)) {
      df.uniq[, "INVALID"] <- bad.mapping
      print(df.uniq)
      stop(Message(FormatText(sum(bad.mapping),
                              "$N invalid treatment assignment{|s} ",
                              "encountered in the GeoTimeseries")))
    }
    df.uniq <- df.uniq[good.mapping, , drop=FALSE]
  }
  obj.trt <- TreatmentAssignment(df.uniq)
  return(obj.trt)
}
