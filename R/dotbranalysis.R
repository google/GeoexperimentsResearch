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

#' Executes a TBR Causal Effect Analysis.
#'
#' @param obj an object.
#' @param model (string) model to use.
#' @param ... further arguments passed to or from other methods.
#'
#' @return A TBRAnalysisFit object.
#'
#' @note
#' Dispatcher of methods.
#' The actual method to execute the 'tbr1' method is \code{DoTBRAnalysisTbr1}.
#' @seealso \code{\link{DoTBRAnalysisTbr1}}, \code{\link{DoROASAnalysis}},
#' \code{\link{DoGBRROASAnalysis}}.
#' @rdname DoTBRAnalysis
DoTBRAnalysis <- function(obj, model, ...) {
  assert_that(is.nonempty.string(model))
  if (model %in% kDefault) {
    model <- getOption("geoexperiments.default.tbr.model",
                       default=kTBRModel1)
  }
  UseMethod("DoTBRAnalysis")
}

#' @rdname DoTBRAnalysis
DoTBRAnalysis.GeoExperimentData <- function(obj, model, ...) {
  obj <- as.TBRAnalysisData(obj, ...)
  obj.result <- DoTBRAnalysis(obj, model=model, ...)
  return(obj.result)
}

#' @rdname DoTBRAnalysis
DoTBRAnalysis.TBRAnalysisData <- function(obj, model, ...) {
  SetMessageContextString("DoTBRAnalysis.TBRAnalysisData")
  on.exit(SetMessageContextString())

  if (model %in% kTBRModel1) {
    obj.result <- DoTBRAnalysisTbr1(obj, ...)
  } else {
    stop(Messagef("Unknown model '%s'", model))
  }
  return(obj.result)
}
