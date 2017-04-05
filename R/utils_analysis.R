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

#' Computes the weights to be used in the weighted linear model used to
#' estimate ROAS.
#'
#' @param response a vector of the response in the pre period. Length equal to
#'   the number of geos. Must be all nonnegative.
#' @param power default power to which 'response' is raised to. Can be
#'   overridden by setting the global option
#'   'geoexperiments.gbr1.weight.power'. Must be nonnegative.
#' @return A vector of weights of the same length as 'response'. Data points
#'   with response == 0 have weight NA (indicating these need to be taken
#'   special care of). There is an attribute 'power' corresponding to the
#'   exponent used.
#'
#' @note
#' If a component of 'response' tends to infinity, the corresponding
#' weight tends to 0 (i.e., the corresponding data point is ignored).

ComputeLinearModelWeights <- function(response, power=2.0) {
  assert_that(is.numeric(response), !anyNA(response), all(response >= 0))
  power <- getOption("geoexperiments.gbr1.weight.power", default=power)
  assert_that(is.numeric(power), !is.na(power), power >= 0)
  weights <- 1 / (response^power)
  weights[response == 0] <- NA_real_
  attr(weights, "power") <- power
  return(weights)
}
