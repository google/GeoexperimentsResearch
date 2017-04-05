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

#' Aggregate the metrics of a time series object over specified time
#' intervals.
#'
#' @param obj an object.
#' @param freq (string) 'weekly' or 'monthly' aggregation.
#' @param ... further arguments passed to or from other methods.
#'
#' @return An object of the same class as 'obj'.
#'
#' @note
#' \itemize{
#'   \item{'Weekly' frequency}: each day in the time series is mapped
#'     to the next Sunday, then aggregated.
#'   \item{'Monthly' frequency}: each day in the time series is mapped to
#'     the last day of the month, then aggregated. No check is made about
#'     the current frequency. This works best on daily data. So it is
#'     possible to attempt to change a 'monthly' frequency back to 'weekly',
#'     but this simply re-maps the last day of the month to the next Sunday.
#' }
#'
#' @seealso \code{\link{aggregate.GeoTimeseries}}.
#'
#' @rdname AggregateTimeseries

AggregateTimeseries <- function(obj, freq=c("weekly", "monthly"), ...) {
  UseMethod("AggregateTimeseries")
}

#' @rdname AggregateTimeseries

AggregateTimeseries.GeoTimeseries <- function(obj, freq, ...) {
  SetMessageContextString("AggregateTimeseries.GeoTimeseries")
  on.exit(SetMessageContextString())

  dates <- obj[[kDate]]
  # Last day of week is Sunday (=7).
  new.dates <- switch(freq,
                      weekly=.GetLastDayOfWeek(dates, last.day=7L),
                      monthly=.GetLastDayOfMonth(dates),
                      stop(Message(paste0("Possible values for 'freq' are ",
                                          "'weekly' and 'monthly'"))))
  obj[[kDate]] <- new.dates
  internal <- intersect(c(kDate, kGeo, kPeriod, kGeoGroup, kAssignment),
                        names(obj))
  df.agg <- aggregate(obj, by=internal)
  metrics <- GetInfo(obj, "metrics")
  obj.gts <- GeoTimeseries(df.agg, metrics=metrics)
  return(obj.gts)
}
