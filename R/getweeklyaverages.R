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

#' Calculates the weekly averages of the metrics per geo.
#'
#' @param obj an object.
#' @param ... further arguments passed to or from other methods.
#'
#' @return A data frame with one row per 'geo', and the weekly averages of
#'   each metric in the columns.
#'
#' @rdname GetWeeklyAverages

GetWeeklyAverages <- function(obj, ...) {
  UseMethod("GetWeeklyAverages")
}

#' @param na.rm (flag) remove NAs?
#'
#' @note
#' Does not handle incomplete weeks. Each week is supposed to have the
#' complete total sales of the week. If for example the first and last
#' weeks of a daily data set are incomplete, the weekly average will be
#' underestimated. Works for both weekly and daily data. The averages are
#' calculated by first calculating the weekly sums for each geo, and then
#' taking the averages over the weeks. \code{NA}s are removed by default.

#' @rdname GetWeeklyAverages
GetWeeklyAverages.GeoTimeseries <- function(obj, na.rm=TRUE, ...) {
  assert_that(is.flag(na.rm) && !is.na(na.rm))

  metrics <- GetInfo(obj, "metrics")
  df.agg <- aggregate(obj, by=c(kGeo, kWeekindex), FUN=base::sum, na.rm=na.rm)
  df.result <- aggregate(df.agg[metrics], by=df.agg[kGeo], FUN=base::mean)
  df.result <- df.result[order(df.result[[kGeo]]), ]
  return(df.result)
}
