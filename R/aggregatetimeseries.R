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

AggregateTimeseries <- function(obj, freq=c("weekly", "monthly"), ...) {
  # Aggregate the metrics of a time series object over specified time
  # intervals.
  #
  # Args:
  #   obj: an object.
  #   freq: (string) 'weekly' or 'monthly' aggregation.
  #   ...: further arguments passed to or from other methods.
  #
  # Returns:
  #   An object of the same class as 'obj'.
  #
  # Notes:
  #   A generic S3 method.
  #
  # Documentation:
  #   seealso: AggregateTimeseries.GeoTimeseries.

  UseMethod("AggregateTimeseries")
}

AggregateTimeseries.GeoTimeseries <- function(obj, freq, ...) {
  # Convert a GeoTimeseries to a weekly or monthly
  # GeoTimeseries. (Note: intended to transform daily data.)
  #
  # Args:
  #   obj: a GeoTimeseries object.
  #   freq: desired frequency: 'weekly' or 'monthly'.
  #   ...: ignored.
  #
  # Returns:
  #   A GeoTimeseries object with the weekly or monthly metrics
  #   aggregated. The sums are associated with the last day of the
  #   week (which is by our definition _Sunday_), or the last day of
  #   the month.
  #
  # Notes:
  #   'Weekly' frequency: each day in the time series is mapped to the
  #   next Sunday, then aggregated.  'Monthly' frequency: each day in
  #   the time series is mapped to the last day of the month, then
  #   aggregated.
  #
  #   No check is made about the current frequency. This works best on
  #   daily data.  So it is possible to attempt to change a 'monthly'
  #   frequency back to 'weekly', but this simply re-maps the last day
  #   of the month to the next Sunday.
  #
  # Documentation:
  #   seealso: AggregateTimeseries (generic), aggregate.GeoTimeseries.

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
