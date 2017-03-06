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

DoROASAnalysis <- function(obj, model, ...) {
  # Performs a ROAS analysis using the given model.
  #
  # Args:
  #   obj: an object.
  #   model: (string) id of the model to use. Available models are 'gbr1',
  #     'tbr1'.
  #   ...: further arguments passed to or from other models.
  #
  # Returns:
  #   A GBRROASAnalysisFit (for 'gbr1') or a TBRROASAnalysisFit object
  #  (for 'tbr1').
  #
  # Notes:
  #   Dispatches the right model for the given model type.

  SetMessageContextString("DoROASAnalysis")
  on.exit(SetMessageContextString())

  assert_that(is.nonempty.string(model))
  assert_that(model %in% GetModelIds(),
              msg=Messagef("Unknown model: '%s'", model))

  method <- kModelToMethod[model]
  obj.result <- switch(method,
                       gbr=DoGBRROASAnalysis(obj, ...),
                       tbr=DoTBRROASAnalysis(obj, model=model, ...))
  return(obj.result)
}
