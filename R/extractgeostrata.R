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

ExtractGeoStrata <- function(obj, ...) {
  # Extract a GeoStrata object from an object.
  #
  # Args:
  #   obj: some object for which the method 'ExtractGeos' exists.
  #   ...: other arguments passed to the methods.
  #
  # Returns:
  #   A GeoStrata object.

  UseMethod("ExtractGeoStrata")
}

ExtractGeoStrata.GeoTimeseries <- function(obj, volume=NULL, ...) {
  # Extract a GeoStrata object from a GeoTimeseries.
  #
  # Args:
  #   obj: a GeoTimeseries object.
  #   volume: (string) name of a metric in the GeoTimeseries over which to
  #     generate the 'volume' column.
  #   ...: arguments passed to 'GeoStrata'.
  #
  # Returns:
  #   A GeoStrata object, obtained by first extracting a Geos object from 'obj'.
  SetMessageContextString("ExtractGeoStrata.GeoTimeseries")
  on.exit(SetMessageContextString())

  obj.geos <- ExtractGeos(obj, volume=volume)
  obj <- GeoStrata(obj.geos, ...)
  return(obj)
}

ExtractGeoStrata.GeoExperimentData <- function(obj, volume=NULL, ...) {
  # Extract a GeoStrata object from a GeoExperimentData object.
  #
  # Args:
  #   obj: a GeoExperimentData object.
  #   volume: (string) name of a metric in the GeoExperimentData over which to
  #     generate the 'volume' column.
  #   ...: arguments passed to 'GeoStrata'.
  #
  # Returns:
  #   A GeoStrata object, obtained by first extracting a Geos object from 'obj'.
  SetMessageContextString("ExtractGeoStrata.GeoExperimentData")
  on.exit(SetMessageContextString())

  obj.geos <- ExtractGeos(obj, volume=volume)
  obj.ga <- GetInfo(obj, "geo.assignment")

  excluded.geos.ga <- obj.ga[obj.ga[[kGeoGroup]] == kExcludeGeoGroup, ,
                             drop=FALSE]
  obj <- GeoStrata(obj.geos, ...)
  if (nrow(excluded.geos.ga) > 0) {
    SetGeoGroup(obj) <- excluded.geos.ga
  }
  return(obj)
}
