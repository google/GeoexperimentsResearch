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

#' Aggregate the Metrics of a GeoTimeseries.
#'
#' @param x a GeoTimeseries object.
#' @param by (character vector) name(s) of column(s) by which to group.
#' @param FUN (function) function to apply to each metric column.
#' @param metrics (character vector) metrics to aggregate. Default is all
#'   metrics.
#' @param ... optional arguments passed to FUN.
#'
#' @return A data.frame object.
#'
#' @note
#' Uses \code{aggregate.data.frame} to do the aggregation. This function
#' omits rows that have missing values in the '\code{by}' columns.
#'
#' @seealso AggregateTimeseries.

aggregate.GeoTimeseries <- function(x, by=kGeo, FUN=base::sum,
                                    metrics=NULL, ...) {
  SetMessageContextString("aggregate.GeoTimeseries")
  on.exit(SetMessageContextString())

  assert_that(is.function(FUN))
  all.metrics <- GetInfo(x, "metrics")
  if (is.null(metrics)) {
    metrics <- all.metrics
  }
  # Ensure that all 'metrics' are there.
  CheckForMissingColumns(metrics, dataframe=x)
  # Ensure that all 'by' columns are there.
  CheckForMissingColumns(by, dataframe=x)
  # 'metrics' and 'by' cannot intersect.
  assert_that(length(intersect(metrics, by)) == 0L,
              msg=Message("'metrics' and 'by' cannot intersect"))
  dfx <- as.data.frame(x)[metrics]
  dfb <- as.data.frame(x)[by]
  dfa <- aggregate(dfx, by=dfb, FUN=FUN, ...)
  return(dfa)
}
