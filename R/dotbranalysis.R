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

# Functions to perform TBR analyses.
# - DoTBRAnalysis (generic method)
# - DoTBRAnalysis.TBRAnalysisData

DoTBRAnalysis <- function(obj, model, ...) {
  # Performs a TBR Causal Effect Analysis.
  #
  # Args:
  #   obj: an object.
  #   model: (string) model to use.
  #   ...: further arguments passed to or from other methods.
  #
  # Returns:
  #   A TBRAnalysisFit object.
  #
  # Notes:
  #   A generic S3 method.
  #
  # Documentation:
  #   seealso: DoTBRAnalysis.TBRAnalysisData, DoTBRAnalysis.GeoExperimentData.

  assert_that(is.nonempty.string(model))
  if (model %in% kDefault) {
    model <- getOption("geoexperiments.default.tbr.model",
                       default=kTBRModel1)
  }
  UseMethod("DoTBRAnalysis")
}

DoTBRAnalysis.GeoExperimentData <- function(obj, model, ...) {
  # Performs a TBR Causal Effect Analysis on a GeoExperimentData object.
  #
  # Args:
  #   obj: a GeoExperimentData object.
  #   model: (string) model to use. See the generic method for
  #     possible values.
  #   ...: arguments passed on to the method performing the analysis.
  #
  # Returns:
  #   A TBRAnalysisFit object.
  #
  # Notes:
  #   Calls 'as.TBRAnalysisData' and passes the 'TBRAnalysisData'
  #   object to the analysis method.
  #
  # Documentation:
  #   seealso: DoTBRAnalysis (generic), DoTBRAnalysis.TBRAnalysisData.

  obj <- as.TBRAnalysisData(obj, ...)
  obj.result <- DoTBRAnalysis(obj, model=model, ...)
  return(obj.result)
}

DoTBRAnalysis.TBRAnalysisData <- function(obj, model, ...) {
  # Performs a TBR Analysis on a TBRAnalysisData object.
  #
  # Args:
  #   obj: a TBRAnalysisData object.
  #   model: (string) model to use. See the generic method for
  #     possible values.
  #   ...: Arguments passed on to the method performing the analysis.
  #
  # Returns:
  #   A TBRAnalysisFit object.
  #
  # Documentation:
  #   seealso: DoTBRAnalysis (generic), DoTBRAnalysis.GeoExperimentData.

  SetMessageContextString("DoTBRAnalysis.TBRAnalysisData")
  on.exit(SetMessageContextString())

  if (model %in% kTBRModel1) {
    obj.result <- DoTBRAnalysisTbr1(obj, ...)
  } else {
    stop(Messagef("Unknown model '%s'", model))
  }
  return(obj.result)
}
