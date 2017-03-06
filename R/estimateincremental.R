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

EstimateIncremental <- function(obj, ...) {
  # Estimates the difference of a metric between the test and counterfactual
  # between the pre and the post periods.
  #
  # Args:
  #   obj: an object.
  #   ...: further arguments passed to or from other methods.
  #
  # Returns:
  #   An object representing the adjusted incremental values
  #
  # Notes:
  #   A generic S3 method.
  #
  # Documentation:
  #   seealso: EstimateIncremental.GBRROASAnalysisData.

  UseMethod("EstimateIncremental")
}

EstimateIncremental.GBRROASAnalysisData <- function(obj, variable=
                                                c("response", "cost"),
                                                ...) {
  # Estimates the incremental response or cost of the test geos during
  # the test period.
  #
  # Args:
  #   obj: a GBRROASAnalysisData object.
  #   variable: (string) 'response' or 'cost'.
  #   ...: ignored.
  #
  # Returns:
  #   A vector of (adjusted) incremental values, of length
  #   \code{nrow(obj)}.
  #
  # Notes:
  #   The incremental metric (response or cost) is calculated as the
  #   difference between the actual observed metric and the
  #   counterfactual during the test period. The counterfactual is
  #   defined as the prediction of the metric, using a model that uses
  #   only the control geos as the training data set.
  #
  #   The incremental cost is defined to be zero for the control geos.
  #
  #   In case there is not enough data to model the counterfactual
  #   (happens when all pre-test data are constant, usually zero), the
  #   counterfactual is defined to be simply the metric during the
  #   pre-test period.
  #
  # Documentation:
  #   seealso: EstimateIncremental (generic).

  # For some experiments, ad spend in all of the control geos should
  # be zero in the pre and test periods. But, there may be small
  # amounts of ad spend in some of these geos.
  variable <- match.arg(variable)
  df.control <- obj[obj[[kControl]], , drop=FALSE]
  pre <- switch(variable, response="resp.pre", cost="cost.pre")
  post <- switch(variable, response="resp.test", cost="cost.test")
  lmweights <- ComputeLinearModelWeights(df.control[[pre]])
  counterfactual.model <- lm(post ~ pre,
                             data = list(
                                 post=df.control[[post]],
                                 pre=df.control[[pre]]),
                             weights=lmweights)
  # If all pre-test variables are constants, estimation is not possible.
  if (is.na(coef(counterfactual.model)["pre"])) {
    # In case estimation is not possible, the counterfactual is simply 'pre'.
    counterfactual <- obj[[pre]]
  } else {
    # Compute the counterfactual: what 'post' would have been, given 'pre'.
    counterfactual <- predict(counterfactual.model,
                              newdata = list(pre=obj[[pre]]))
  }
  incremental <- (obj[[post]] - counterfactual)
  # For Control geos, set the differential to zero.
  is.control <- obj[["control"]]
  incremental[is.control] <- 0
  return(incremental)
}
