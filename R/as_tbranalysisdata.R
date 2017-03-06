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

# S3 methods for coercing objects to TBRAnalysisData objects.
# Methods:
# - as.TBRAnalysisData (generic)
# - as.TBRAnalysisData.GeoExperimentData

as.TBRAnalysisData <- function(obj, ...) {
  # Coerces the object to a TBRAnalysisData object.
  #
  # Args:
  #   obj: an object.
  #   ...: further arguments to be passed to or from other methods.
  #
  # Returns:
  #   A TBRAnalysisData object.
  #
  # Notes:
  #   A generic S3 method.
  #
  # Documentation:
  #   seealso: as.TBRAnalysisData.GeoExperimentData.

  UseMethod("as.TBRAnalysisData")
}

as.TBRAnalysisData.GeoExperimentData <- function(obj,
                                                 response=character(0),
                                                 control.group=1L,
                                                 treatment.group=2L,
                                                 pretest.period=0L,
                                                 intervention.period=1L,
                                                 cooldown.period=NULL,
                                                 ...) {
  # Coerces a GeoExperimentData object to a TBRAnalysisData object.
  #
  # Args:
  #   obj: a GeoExperimentData object.
  #   response: (string) name of the response metric to analyze.
  #   control.group: (positive integer) number of the control group
  #     (matching one of the groups in the column 'geo.group').
  #     This is typically 1.
  #   treatment.group: (positive integer) number of the treatment group
  #     (matching one of the groups in the column 'geo.group').
  #     This is typically 2.
  #   pretest.period: (non-negative integers) number of the pretest
  #     period, typically 0. Can also be one or more numbers, if
  #     periods are to be collapsed.
  #   intervention.period: (vector of non-negative integers) number(s) of the
  #     period(s) forming the intervention period. All must be larger than the
  #     largest period in the pretest period.
  #   cooldown.period: (NULL or vector of non-negative integers) number(s) of
  #     the period(s) forming the cooldown period. All must be larger than the
  #     largest period in the intervention period.
  #   ...: ignored.
  #
  # Returns:
  #   A TBRAnalysisData object.
  #
  # Documentation:
  #   seealso: as.TBRAnalysisData (generic).

  SetMessageContextString("as.TBRAnalysisData.GeoExperimentData")
  on.exit(SetMessageContextString())

  assert_that(is.string(response))
  CheckForMissingColumns(response, dataframe=obj)

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
  CheckForBadValues(obj, columns=response, CHECK=.IsMissing,
                    good=FALSE, what="missing")

  experiment.groups.only <- (geo.group %in% c(control.group, treatment.group))
  obj <- obj[experiment.groups.only, , drop=FALSE]
  geo.group <- obj[[kGeoGroup]]
  period <- obj[[kPeriod]]

  # Reformat data to form a data frame with columns date, period, y, and x.
  kXY <- ".xy"  # Temporary column name. Not there in the final object.
  obj[[kXY]] <- NA_character_
  obj[[kXY]][geo.group %in% treatment.group] <- kY
  obj[[kXY]][geo.group %in% control.group] <- kX

  in.pretest <- (period %in% pretest.period)
  first.pretest.date <- head(which(in.pretest), 1)
  in.preexperiment <- (seq_along(period) < first.pretest.date)
  in.intervention <- (period %in% intervention.period)
  in.cooldown <- (period %in% cooldown.period)
  last.experiment.date <- max(obj[[kDate]][in.intervention | in.cooldown])
  in.posttest <- (obj[[kDate]] > last.experiment.date)

  obj[[kPeriod]] <- NA_integer_
  obj[[kPeriod]][in.preexperiment] <- kStandardPeriods[["preexperiment"]]
  obj[[kPeriod]][in.pretest] <- kStandardPeriods[["pretest"]]
  obj[[kPeriod]][in.intervention] <- kStandardPeriods[["intervention"]]
  obj[[kPeriod]][in.cooldown] <- kStandardPeriods[["cooldown"]]
  obj[[kPeriod]][in.posttest] <- kStandardPeriods[["posttest"]]

  # The data frame will have columns kY and kX.
  formula <- as.formula(sprintf("%s + %s ~ %s", kDate, kPeriod, kXY))
  df.tbr <- dcast(obj, formula=formula, value.var=response,
                  fun.aggregate=base::sum)
  obj.result <- TBRAnalysisData(df.tbr)
  return(obj.result)
}
