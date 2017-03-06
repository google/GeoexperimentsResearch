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

GeoExperimentData <- function(geo.timeseries,
                              periods,
                              geo.assignment,
                              treat.assignment) {
  # Constructs a GeoExperimentData object.
  #
  # Args:
  #   geo.timeseries: a GeoTimeseries object.
  #   periods: an ExperimentPeriods object, specifying the start dates
  #     of each period in the experiment. Or, 'NULL' if the dates are
  #     yet unknown. Or, leave unspecified to extract this information
  #     from the column 'periods' of the geo.timeseries object; in
  #     this case the column *must* exist in the data frame.
  #   geo.assignment: a GeoAssignment object, specifying the mapping
  #     from a geo to a geo group. Or, 'NULL' if the geo assignment is
  #     yet unknown. Or, leave unspecified to extract this information
  #     from the column 'geo.group' of the geo.timeseries object; in
  #     this case the column *must* exist in the data frame.
  #   treat.assignment: a TreatmentAssignment object, specifying the
  #     mapping from (period, geo) to a treatment assignment condition
  #     (treatment intervention type). Or, 'NULL' if the treatment
  #     assignment is yet unknown. Or, leave unspecified to extract
  #     this information from the column 'assignment' of the
  #     geo.timeseries object; in this case the column *must* exist in
  #     the data frame.
  #
  # Returns:
  #   A GeoExperimentData object.
  #
  # Notes:
  #   If any of the arguments 'periods', 'geo.assignment', or
  #   'treat.assignment' is not specified, a column 'period',
  #   'geo.group', or 'assignment', respectively, *must* be present in the
  #   'geo.timeseries' object.
  #
  #   If any of the arguments 'periods', 'geo.assignment', or
  #   'treat.assignment' is NULL, the corresponding column ('period',
  #   'geo.group', or 'assignment') in the resulting GeoExperimentData
  #   data frame will have only 'NA's.
  #
  #   If the GeoTimeseries object has columns 'period', 'geo.group',
  #   or 'assignment', they will be overwritten by the values given by
  #   the objects 'periods', 'geo.assignment', and 'treat.assignment'.
  #
  #   The resulting object *may* have undefined 'periods',
  #   'geo.assignment', or 'treat.assignment'.  The missing parts may
  #   be filled in by using the functions 'SetExperimentPeriods',
  #   'SetGeoAssignment', and 'SetTreatmentAssignment'.

  kClassName <- "GeoExperimentData"
  SetMessageContextString(kClassName)
  on.exit(SetMessageContextString())

  assert_that(inherits(geo.timeseries, "GeoTimeseries"))
  if (missing(periods)) {
    periods <- ExtractExperimentPeriods(geo.timeseries)
  } else {
    assert_that(is.null(periods) ||
                inherits(periods, "ExperimentPeriods"))
  }
  if (missing(geo.assignment)) {
    geo.assignment <- ExtractGeoAssignment(geo.timeseries)
  } else {
    assert_that(is.null(geo.assignment) ||
                inherits(geo.assignment, "GeoAssignment"))
  }
  if (missing(treat.assignment)) {
    # The assignment column can be missing; in this case,
    # treat.assignment will be NULL and the resulting data frame
    # will have the column filled with NAs.
    treat.assignment <- ExtractTreatmentAssignment(geo.timeseries,
                                                   strict=FALSE)
  } else {
    assert_that(is.null(treat.assignment) ||
                inherits(treat.assignment, "TreatmentAssignment"))
  }

  metrics <- GetInfo(geo.timeseries, "metrics")
  # Add the new columns. If they exist, they will be overwritten and
  # filled by the Set*<- functions.
  obj <- geo.timeseries
  obj[[kPeriod]] <- NA_integer_
  obj[[kGeoGroup]] <- NA_integer_
  obj[[kAssignment]] <- NA_character_
  internal <- c(kDate, kGeo, kPeriod, kGeoGroup, kAssignment)
  new.column.order <- union(c(internal, metrics), names(obj))
  obj <- obj[new.column.order]
  # Add the attributes.
  obj <- SetInfo(obj,
                 metrics=metrics,
                 periods=NULL,
                 geo.assignment=NULL,
                 treat.assignment=NULL)
  # The new class inherits from GeoTimeseries.
  if (!inherits(obj, kClassName)) {
    class(obj) <- c(kClassName, class(geo.timeseries))
  }
  # Use the 'Set' functions to set the values of the three columns.
  SetExperimentPeriods(obj) <- periods
  SetGeoAssignment(obj) <- geo.assignment
  SetTreatmentAssignment(obj) <- treat.assignment
  return(obj)
}
