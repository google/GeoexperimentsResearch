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

# Constants used in the functions.
#
# Column names in various data frame-based objects.

kDate <- "date"
kGeo <- "geo"
kGeoGroup <- "geo.group"
kGroup <- "group"
kBlock <- "block"
kWeekday <- ".weekday"
kWeeknum <- ".weeknum"
kWeekindex <- ".weekindex"
kAssignment <- "assignment"
kPeriod <- "period"
kControl <- "control"
kRespPre <- "resp.pre"
kRespTest <- "resp.test"
kCostPre <- "cost.pre"
kCostTest <- "cost.test"
kTmpi <- ".i"
kTmpj <- ".j"
kY <- "y"
kX <- "x"
kYpredMean <- "pred"
kYpredSd <- "predsd"
kYpredDif <- "dif"
kYpredCumdif <- "cumdif"
kYpredCumdifSd <- "cumsd"
kYestSd <- "estsd"
kDefault <- "default"
# Geos.
kVolume <- "volume"
kProportion <- "proportion"
# GeoStrata.
kStratum <- "stratum"
# SetSpendChange<-
kSpendChange <- ".spend"
# SetIncrementalResponse<-
kResponse <- ".response"
# DoGBRROASAnalysis, EstimateIncremental.
kGbr1MinObs <- 4L
kIncrCost <- "incr.cost"
kWeight <- "weight"
# Group ID indicating a geo to be omitted.
kExcludeGeoGroup <- 0L

# IDs for various models.

kGBRModel1 <- "gbr1"
kTBRModel1 <- "tbr1"
# kModels: all available models.
kModels <- c(gbr=kGBRModel1, tbr=kTBRModel1)
# kModelToMethod: maps model to the corresponding methodology id.
kModelToMethod <- structure(names(kModels), names=kModels)

# kTreatmentAssignment: map from labels to actual symbols used in the
# 'assignment' column GeoExperimentData objects.

kTreatmentAssignment <- c(none=0L, increase=1L, decrease=-1L, change=1L,
                          exclude=-9)

kStandardPeriods <- list(pretest=0L, intervention=1L, cooldown=2L,
                         posttest=3L, preexperiment=-1L,
                         experiment=c(0L, 1L, 2L),
                         excluded=NA_integer_, analysis=c(1L, 2L),
                         prediction=c(1L, 2L, 3L))

#' Returns the list of available model identifiers.
#'
#' @return A character vector of the available model IDs. The 'names'
#'   attribute shows the methodology ID (gbr, tbr).

GetModelIds <- function() {
  return(kModels)
}

# plot.TBRPlotData.

kPanel <- "panel"
kPanelLabel <- "panel.label"

kTBRPlotPanels <- list(counterfactual=
                       c(observed=kY,
                         panel.label="Observed + counterfactual"),
                       pointwise=
                       c(observed=NA,
                         panel.label="Pointwise differences"),
                       cumulative=
                       c(observed=NA,
                         panel.label="Cumulative effect"))

kTBRROASPlotPanels <- list(pointwise=
                           c(observed=NA,
                             panel.label="Pointwise iROAS"),
                           cumulative=
                           c(observed=NA,
                             panel.label="Cumulative iROAS"))

#' Returns the names of available TBR panels in the default order of
#' plotting.
#'
#' @return A character vector of the available panel IDs.

GetTBRPlotPanelNames <- function() {
  return(names(kTBRPlotPanels))
}
