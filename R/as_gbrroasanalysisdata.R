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

as.GBRROASAnalysisData.GeoExperimentData <- function(obj,
                                                     response=character(0),
                                                     cost=character(0),
                                                     pretest.period=0L,
                                                     intervention.period=1L,
                                                     cooldown.period=NULL,
                                                     control.group=1L,
                                                     treatment.group=2L,
                                                     ...) {
  # Coerces an object to a GBRROASAnalysisData object.
  #
  # Args:
  #   obj: a GeoExperimentData object.
  #   response: (string) name of the response variable column.
  #   cost: (string) name of the cost variable column.
  #   pretest.period: (vector of non-negative integers) number(s) of the
  #     period(s) forming the pretest period.
  #   intervention.period: (vector of non-negative integers) number(s) of the
  #     period(s) forming the intervention period. All must be larger than the
  #     largest period in the pretest period.
  #   cooldown.period: (NULL or vector of non-negative integers) number(s) of
  #     the period(s) forming the cooldown period. All must be larger than the
  #     largest period in the intervention period.
  #   control.group: (NULL or a vector of positive integers) number(s) of geo
  #     groups forming the control group.
  #   treatment.group: (NULL or a vector of positive integers) number(s) of geo
  #     groups forming the control group.
  #   ...: ignored.
  #
  # Returns:
  #   A GBRROASAnalysisData object.
  #
  # Documentation:
  #   seealso: as.GBRROASAnalysisData (generic).

  SetMessageContextString("as.GBRROASAnalysisData.GeoExperimentData")
  on.exit(SetMessageContextString())

  assert_that(is.string(response),
              is.string(cost))
  CheckForMissingColumns(c(response, cost), dataframe=obj)

  geo.group <- obj[[kGeoGroup]]
  CheckGeoGroupNumber(control.group, values=geo.group)
  CheckGeoGroupNumber(treatment.group, values=geo.group)
  assert_that(length(intersect(control.group, treatment.group)) == 0,
              msg=Message("Control and treatment groups must not overlap"))

  period <- obj[[kPeriod]]
  CheckPeriodNumbers(pretest.period, values=period)
  CheckPeriodNumbers(intervention.period, values=period)
  assert_that(max(pretest.period) < min(intervention.period),
              msg=Message("Pretest period must occur ",
                  "before the intervention period"))
  if (!is.null(cooldown.period)) {
    CheckPeriodNumbers(cooldown.period, values=period)
    assert_that(max(intervention.period) < min(cooldown.period),
                msg=Message("Intervention period must occur ",
                    "before the cooldown period"))
  }

  # Ignore geo.groups and periods that are not part of the analysis.
  within.experiment <- (geo.group %in% c(control.group, treatment.group) &
                        period %in% c(pretest.period, intervention.period,
                                      cooldown.period))

  # No missing values in the response columns allowed in the given periods
  # for the given groups.
  .IsMissing <- function(x) {
    return(within.experiment & is.na(x))
  }
  CheckForBadValues(obj, columns=c(response, cost), CHECK=.IsMissing,
                    good=FALSE, what="missing")

  obj <- obj[within.experiment, , drop=FALSE]
  geo.group <- obj[[kGeoGroup]]
  period <- obj[[kPeriod]]

  kResp <- ".resp"  # Temporary column name.
  kCost <- ".cost"  # Temporary column name.

  pretest <- (period %in% pretest.period)
  test <- (period %in% c(intervention.period, cooldown.period))

  obj[[kResp]] <- NA_character_
  obj[[kCost]] <- NA_character_

  obj[[kResp]][pretest] <- kRespPre
  obj[[kCost]][pretest] <- kCostPre

  obj[[kResp]][test] <- kRespTest
  obj[[kCost]][test] <- kCostTest

  control <- (geo.group %in% control.group)
  obj[[kControl]] <- NA
  obj[[kControl]][control] <- TRUE
  obj[[kControl]][!control] <- FALSE

  formula <- as.formula(sprintf("%s + %s ~ %s", kGeo, kControl, kResp))
  df.resp <- dcast(obj, formula=formula, value.var=response,
                  fun.aggregate=base::sum)

  formula <- as.formula(sprintf("%s + %s ~ %s", kGeo, kControl, kCost))
  df.cost <- dcast(obj, formula=formula, value.var=cost,
                  fun.aggregate=base::sum)

  df.result <- merge(df.resp, y=df.cost, by=c(kGeo, kControl), all=TRUE)
  obj.result <- GBRROASAnalysisData(df.result)
  return(obj.result)
}
