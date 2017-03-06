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

ExtractExperimentPeriods <- function(obj, ...) {
  # Extracts an ExperimentPeriods object.
  #
  # Args:
  #   obj: an object.
  #   ...: further arguments passed on to methods.
  #
  # Returns:
  #   An ExperimentPeriods object.
  #
  # Notes:
  #   A generic S3 method.
  #
  # Documentation:
  #   seealso: ExtractExperimentPeriods.GeoTimeseries.

  UseMethod("ExtractExperimentPeriods")
}

ExtractExperimentPeriods.GeoTimeseries <- function(obj, strict=TRUE, ...) {
  # Extracts a ExperimentPeriods object from a GeoTimeseries.
  #
  # Args:
  #   obj: a GeoTimeseries object with the column 'period'.
  #   strict: (flag) if FALSE, the function returns NULL if the column
  #     'period' does not exist. Otherwise, throws an error.
  #   ...: ignored.
  #
  # Returns:
  #   An ExperimentPeriods object.
  #
  # Documentation:
  #   seealso: ExtractExperimentPeriods (generic).

  SetMessageContextString("ExtractExperimentPeriods.GeoTimeseries")
  on.exit(SetMessageContextString())

  assert_that(is.flag(strict))
  if (!strict && !(kPeriod %in% names(obj))) {
    return(NULL)
  }
  # Ensure that the required column 'period' is in the given data frame.
  CheckForMissingColumns(kPeriod, dataframe=obj, what="required")
  # Find the dates that mark the limits of the periods.
  keys <- c(date=kDate, group=kPeriod)
  df.agg <- unique(obj[keys])
  # R drops the Date class attribute when unlisting, so keep the dates
  # temporarily as character.
  date.ranges <- tapply(df.agg[[kDate]], df.agg[[kPeriod]],
                        FUN=function(x) as.character(range(x)))
  start.dates <- as.Date(sapply(date.ranges, "[", 1L))
  end.date <- max(as.Date(unlist(date.ranges)))
  period.dates <- c(start.dates, end.date)
  obj.ga <- ExperimentPeriods(period.dates)
  return(obj.ga)
}
