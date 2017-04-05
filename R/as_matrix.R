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

#' Coerces a response metric of a GeoTimeseries to a matrix representation
#' with geos in rows, dates in columns.
#'
#' @param x a GeoTimeseries object.
#' @param response (string) name of the response metric to use.
#' @param ... ignored.
#' @return A numeric matrix of the response metric, geos in rows, dates in
#'   columns.

as.matrix.GeoTimeseries <- function(x, response, ...) {
  SetMessageContextString("as.matrix.GeoTimeseries")
  on.exit(SetMessageContextString())

  obj <- x
  metrics <- GetInfo(obj, "metrics")
  assert_that(is.string(response),
              response %in% GetInfo(obj, "metrics"),
              msg=paste0("The specified response variable '",
                  response, "' is not available"))

  # Create a matrix with geos in rows and date numbers in columns.
  form <- as.formula(sprintf("%s ~ %s + %s", response, kGeo, kDate))
  x <- xtabs(form, data=obj)

  # As for R 3.2.2, as.matrix(x) does not yet produce a plain matrix,
  # so we must do unclass first.
  obj.result <- as.matrix(unclass(x))

  return(obj.result)
}

#' Extracts the real-valued matrix of quantiles from a TBRQuantiles object.
#'
#' @param x a TBRQuantiles object.
#' @param ... ignored.
#'
#' @return A real-valued matrix of the quantiles. The column names are the
#'   same as those in the TBRQuantiles object.

as.matrix.TBRQuantiles <- function(x, ...) {
  m <- as.matrix(as.data.frame(x)[!(names(x) %in% c(kDate, kPeriod))])
  rownames(m) <- as.character(x[[kDate]])
  return(m)
}
