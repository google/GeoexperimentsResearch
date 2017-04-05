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

#' Merges two GeoExperimentData objects.
#'
#' @param x a GeoExperimentData object.
#' @param y a GeoExperimentData object.
#' @param ... ignored.
#'
#' @return A GeoExperimentData object.
#'
#' @note
#' The object created is a GeoExperimentData that includes all the
#' columns of x, and all the columns of y that were not in x. Columns of y
#' that have the same name as columns of x are ignored and a warning is
#' issued whenever y and x share some column name(s). The merging is done
#' by kDate, kGeo, kPeriod, kGeoGroup, kAssignment.

merge.GeoExperimentData <- function(x, y, ...) {
  SetMessageContextString("merge.GeoExperimentData")
  on.exit(SetMessageContextString())

  assert_that(inherits(y, "GeoExperimentData"),
              msg=Message(paste("Both objects must inherit from class",
                          "GeoExperimentData")))

  # Remove internal columns.
  xx <- as.data.frame(x)[, !grepl(pattern="^\\.", x=names(x)), drop=FALSE]
  yy <- as.data.frame(y)[, !grepl(pattern="^\\.", x=names(y)), drop=FALSE]

  missing.columns <- setdiff(names(yy), names(xx))
  already.there <- structure(names(yy) %in% names(xx), names=names(yy))
  keys <- c(kDate, kGeo, kPeriod, kGeoGroup, kAssignment)
  already.there[keys] <- FALSE

  if (any(already.there)) {
    warning(Message(FormatText(already.there,
                               "Column{|s} $X {is|are} shared by ",
                               "both GeoExperimentData")))
  }

  if (length(missing.columns) == 0) {
    return(x)
  }

  all.metrics <- union(GetInfo(x, "metrics"), GetInfo(y, "metrics"))

  merged <- merge(xx,
                  yy[, c(keys, missing.columns), drop=FALSE],
                  by=keys, all.x=TRUE, all.y=FALSE)
  obj.result <- as.GeoExperimentData(GeoTimeseries(merged, metrics=all.metrics))

  return(obj.result)
}

#' Merges two GeoTimeseries objects.
#'
#' @param x a GeoTimeseries object.
#' @param y a GeoTimeseries object.
#' @param ... ignored.
#'
#' @return A GeoTimeseries object.
#'
#' @note
#' The object created is a GeoTimeseries that includes all the columns
#' of x, and all the columns of y that were not in x. Columns of y that
#' have the same name as columns of x are ignored and a warning is issued
#' whenever y and x share some column name(s). The merging is done by
#' kDate, kGeo.

merge.GeoTimeseries <- function(x, y, ...) {
  SetMessageContextString("merge.GeoTimeseries")
  on.exit(SetMessageContextString())

  assert_that(inherits(y, "GeoTimeseries"),
              msg=Message(paste("Both objects must inherit from class",
                          "GeoTimeseries")))

  # Remove internal columns.
  xx <- as.data.frame(x)[, !grepl(pattern="^\\.", x=names(x)), drop=FALSE]
  yy <- as.data.frame(y)[, !grepl(pattern="^\\.", x=names(y)), drop=FALSE]

  missing.columns <- setdiff(names(yy), names(xx))
  already.there <- structure(names(yy) %in% names(xx), names=names(yy))
  keys <- c(kDate, kGeo)
  already.there[keys] <- FALSE

  if (any(already.there)) {
    warning(Message(FormatText(already.there,
                               "Column{|s} $X {is|are} shared by ",
                               "both GeoTimeseries")))
  }

  if (length(missing.columns) == 0) {
    return(x)
  }

  all.metrics <- union(GetInfo(x, "metrics"), GetInfo(y, "metrics"))

  merged <- merge(xx,
                  yy[, c(keys, missing.columns), drop=FALSE],
                  by=keys, all.x=TRUE, all.y=FALSE)
  obj.result <- GeoTimeseries(merged, metrics=all.metrics)

  return(obj.result)
}
