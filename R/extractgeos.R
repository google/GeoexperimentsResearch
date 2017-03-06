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

ExtractGeos <- function(obj, ...) {
  # Extract a Geos object from an object.
  #
  # Args:
  #   obj: an object.
  #   ...: other arguments passed to the methods.
  #
  # Returns:
  #   A 'Geos' object.

  UseMethod("ExtractGeos")
}

ExtractGeos.GeoTimeseries <- function(obj, volume=NULL, ...) {
  # Extract a Geos object from a GeoTimeseries.
  #
  # Args:
  #   obj: a GeoTimeseries object.
  #   volume: (string) name of a metric in the GeoTimeseries over which to
  #     generate the 'volume' column. If omitted, the first metric is used.
  #     The volumes have to be non-negative.
  #   ...: ignored.
  #
  # Returns:
  #   A Geos object, with the average weekly volume of all metrics in 'obj'.

  SetMessageContextString("ExtractGeos.GeoTimeseries")
  on.exit(SetMessageContextString())

  metrics <- GetInfo(obj, "metrics")
  if (length(volume) == 0) {
    volume <- metrics[1]
  }
  assert_that(is.nonempty.string(volume),
              volume %in% metrics)
  CheckForBadValues(obj, columns=volume,
                    CHECK=function(z) { z < 0 }, good=FALSE, what="negative")
  geos <- GetWeeklyAverages(obj)
  obj.result <- Geos(geos, volume=volume)
  return(obj.result)
}
