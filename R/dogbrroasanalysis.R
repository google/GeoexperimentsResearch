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

#' Performs a GBR ROAS Analysis.
#'
#' @param obj an object.
#' @param ... further arguments passed to or from other methods.
#' @return A GBRROASAnalysisFit object.
#'
#' @rdname DoGBRROASAnalysis
DoGBRROASAnalysis <- function(obj, ...) {
  UseMethod("DoGBRROASAnalysis")
}

#' @rdname DoGBRROASAnalysis
DoGBRROASAnalysis.GBRROASAnalysisData <- function(obj, ...) {
  kClassName <- "GBRROASAnalysisFitGbr1"
  SetMessageContextString("DoGBRROASAnalysis.GBRROASAnalysisData")
  on.exit(SetMessageContextString())

  # Find ad spend differential for each geo.
  incremental.cost <- EstimateIncremental(obj, variable="cost")
  # Augment the original object with columns kIncrCost & kWeight.
  obj[[kIncrCost]] <- incremental.cost
  #
  lmweights <- ComputeLinearModelWeights(obj[[kRespPre]])
  power <- attr(lmweights, "power")
  missing.weights <- structure(is.na(lmweights), names=obj[[kGeo]])
  if (any(missing.weights)) {
    warning(FormatText(missing.weights,
                       "$N geo{|s} ($x) ha{s|ve} zero pretest response"))
  }
  obj[[kWeight]] <- lmweights
  data <- obj[!missing.weights, , drop=FALSE]
  n.data.points <- nrow(data)
  # Cannot fit if there are fewer than kGbr1MinObs geos available with weights.
  assert_that(n.data.points >= kGbr1MinObs,
              msg=Message(FormatText(n.data.points,
                  "Cannot fit GBR model:",
                  " {no|only one|only $N} data point{|s} available")))
  # Cannot fit if there are no test or control geos available.
  assert_that(sum(data[["control"]]) > 0,
              msg=Message(FormatText(n.data.points,
                  "Cannot fit GBR model: no control geos to fit")))
  assert_that(sum(!data[["control"]]) > 0,
              msg=Message(FormatText(n.data.points,
                  "Cannot fit GBR model: no treatment geos to fit")))
  # Fit the iROAS model.
  model <- as.formula(sprintf("%s ~ %s + %s", kRespTest, kRespPre, kIncrCost))
  lmfit <- lm(model, data=data, weights=data[[kWeight]])
  assert_that(!anyNA(coef(lmfit)),
              msg=Message("GBR model fit failed: NA in coefficient"))

  .PosteriorBeta2Tail <- function(x) {
    # Calculates the posterior of Pr(beta2 > x | data) with the uniform prior
    # on (beta, log sigma).
    #
    # Args:
    #   x: (numeric vector) tail probability threshold.
    # Returns:
    #   A numeric vector of tail probabilities, where
    #   components correspond to those in 'x'.
    # Notes:
    #   Reference: Gelman et al., Bayesian Data Analysis (3rd ed.).  See
    #   section 14.2, equations (14.2) through (14.7), about the basic linear
    #   regression model without weights and section 14.7 (weighted
    #   regression), page 372. The weighted regression can be implemented with
    #   the coefficient matrix of the basic linear regression model X replaced
    #   by W^(-0.5)X, where W is the n x n diagonal matrix of weights.  With
    #   the given noninformative prior, the location and scale of the
    #   t-distributed posterior of a coefficient beta2 matches its classical
    #   point estimate and its standard error, respectively.

    # 'lmfit' will be available from within the closure.
    coefs <- coef(summary(lmfit))
    beta2.hat <- coefs["incr.cost", "Estimate"]
    df.residual <- lmfit[["df.residual"]]
    beta2.se <- coefs["incr.cost", "Std. Error"]
    location <- beta2.hat
    q <- ((x - location) / beta2.se)
    prob <- pt(q, df=df.residual, lower.tail=FALSE)
    return(prob)
  }
  obj.result <- list(lmfit=lmfit,
                     data=data,
                     iroas.post=.PosteriorBeta2Tail,
                     power=power,
                     model=kGBRModel1)
  class(obj.result) <- c(kClassName, class(obj.result))
  return(obj.result)
}

#' @param response (string) name of the response variable column.
#' @param cost (string) name of the cost variable column.
#' @param pretest.period (non-negative integer) number of the pretest period,
#'   typically 0.
#' @param intervention.period (vector of non-negative integers) number(s) of
#'   the period(s) forming the intervention period. All must be larger
#'   than the largest period in the pretest period.
#' @param cooldown.period (vector of non-negative integers or NULL) number(s)
#'   of the period(s) forming the cooldown period. All must be larger than
#'   the largest period in the intervention period.
#' @param control.group (a vector of positive integers) number(s) of geo groups
#'   forming the control group.
#' @param treatment.group (a vector of positive integers) number(s) of geo
#'   groups forming the treatment group.
#'
#' @seealso \code{\link{as.GBRROASAnalysisData}}, \code{\link{DoROASAnalysis}}.
#'
#' @rdname DoGBRROASAnalysis
DoGBRROASAnalysis.GeoExperimentData <- function(obj,
                                                response=character(0),
                                                cost=character(0),
                                                pretest.period=0L,
                                                intervention.period=1L,
                                                cooldown.period=NULL,
                                                control.group=1L,
                                                treatment.group=2L,
                                                ...) {
  SetMessageContextString("DoGBRROASAnalysis.GeoExperimentData")
  on.exit(SetMessageContextString())

  obj.gbr <- as.GBRROASAnalysisData(obj, response=response, cost=cost,
                                    pretest.period=pretest.period,
                                    intervention.period=intervention.period,
                                    cooldown.period=cooldown.period,
                                    control.group=control.group,
                                    treatment.group=treatment.group,
                                    ...)
  obj.result <- DoGBRROASAnalysis(obj.gbr)
  return(obj.result)
}
