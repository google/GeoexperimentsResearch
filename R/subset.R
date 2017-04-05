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

#' Returns a subset of the object, ensuring that the resulting object is of
#' the same class.
#'
#' @param obj an object.
#' @param ... further arguments passed to or from other methods.
#' @return An object of the same class as '\code{obj}'.
#'
#' @rdname Subset
Subset <- function(obj, ...) {
  UseMethod("Subset")
}

#' Extract a subset of GeoTimeseries.
#'
#' @param rows logical vector indicating rows to keep. No missing values are
#'   allowed.
#' @param columns character vector indicating columns of the data frame to
#'   keep. Columns such as 'date', 'geo', 'period', 'geo.group' and
#'   'assignment', if they exist, they are always included.
#'
#' @note
#' The arguments 'rows' and 'columns' are *not* evaluated in the
#' environment of the columns of 'obj'; the expressions must be evaluable
#' in the enclosing environment. This behavior is deliberately different
#' from that of 'subset'. The original experiment configuration (periods,
#' geo assignment, treatment assignment) is preserved.
#'
#' @rdname Subset
Subset.GeoTimeseries <- function(obj, rows, columns, ...) {
  SetMessageContextString("Subset.GeoTimeseries")
  on.exit(SetMessageContextString())

  if (missing(rows)) {
    rows <- rep_len(TRUE, nrow(obj))
  }
  assert_that(is.logical(rows),
              msg=Message("Argument 'rows' must be logical"))
  assert_that(!anyNA(rows),
              msg=Message(
                  FormatText(is.na(rows),
                             "Argument 'rows' has {one|$N} ",
                             "missing value{|s} ",
                             "(of total $L) in row{|s} $w")))
  assert_that(any(rows),
              msg=Message("No rows selected"))
  metrics <- GetInfo(obj, "metrics")
  if (missing(columns)) {
    columns <- names(obj)
  } else {
    CheckThat(is.vector.of.nonempty.strings(columns), name="Argument 'columns'")
    CheckForMissingColumns(columns, dataframe=obj, what="specified")
    internal <- intersect(c(kDate, kGeo, kPeriod, kGeoGroup, kAssignment),
                          names(obj))
    metrics <- intersect(metrics, columns)
    assert_that(length(metrics) > 0L,
                msg=Message("Must include at least one metric in 'columns'"))
    columns <- union(internal, columns)
  }
  df.subset <- as.data.frame(obj[rows, columns, drop = FALSE])
  # as.GeoTimeseries removes all columns whose names contain dots,
  # then the constructor GeoTimeseries(...) adds the default ones.
  obj.subset <- GeoTimeseries(df.subset, metrics=metrics)
  return(obj.subset)
}

#' @rdname Subset
Subset.GeoExperimentData <- function(obj, rows, columns, ...) {
  SetMessageContextString("Subset.GeoExperimentData")
  on.exit(SetMessageContextString())

  if (missing(rows)) {
    rows <- rep_len(TRUE, nrow(obj))
  }
  assert_that(is.logical(rows),
              msg=Message("Argument 'rows' must be logical"))
  assert_that(!anyNA(rows),
              msg=Message(
                  FormatText(is.na(rows),
                             "Argument 'rows' has {one|$N} ",
                             "missing value{|s} ",
                             "(of total $L) in row{|s} $w")))
  assert_that(any(rows),
              msg=Message("No rows selected"))
  metrics <- GetInfo(obj, "metrics")
  if (missing(columns)) {
    columns <- names(obj)
  } else {
    CheckThat(is.vector.of.nonempty.strings(columns), name="Argument 'columns'")
    CheckForMissingColumns(columns, dataframe=obj, what="specified")
    internal <- intersect(c(kDate, kGeo, kPeriod, kGeoGroup, kAssignment),
                          names(obj))
    metrics <- intersect(metrics, columns)
    assert_that(length(metrics) > 0L,
                msg=Message("Must include at least one metric in 'columns'"))
    columns <- union(internal, columns)
  }
  df.subset <- as.data.frame(obj[rows, columns, drop = FALSE])
  # as.GeoTimeseries data removes all columns whose names contain dots.
  obj.gts <- GeoTimeseries(df.subset, metrics=metrics)
  # Attempt to keep the original experiment configuration.
  obj.result <- GeoExperimentData(obj.gts)
  SetExperimentPeriods(obj.result, strict=FALSE) <- GetInfo(obj, "periods")
  SetGeoAssignment(obj.result, strict=FALSE) <- GetInfo(obj, "geo.assignment")
  SetTreatmentAssignment(obj.result, strict=FALSE) <- GetInfo(obj,
                                         "treat.assignment")
  return(obj.result)
}
