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

DoGBRROASAnalysis <- function(obj, ...) {
  # Performs a GBR ROAS Analysis.
  #
  # Args:
  #   obj: an object.
  #   ...: further arguments passed to or from other methods.
  #
  # Returns:
  #   A GBRROASAnalysisFit object.
  #
  # Notes:
  #   A generic S3 method.
  #
  #   See also: DoGBRROASAnalysis.GBRROASAnalysisData,
  #   DoGBRROASAnalysis.GeoExperimentData.

  UseMethod("DoGBRROASAnalysis")
}

DoGBRROASAnalysis.GBRROASAnalysisData <- function(obj, ...) {
  # Performs a ROAS Analysis on a GBRROASAnalysisData object.
  #
  # Args:
  #   obj: a GBRROASAnalysisData object.
  #   ...: ignored.
  #
  # Returns:
  #   A GBRROASAnalysisFit object.
  #
  # Documentation:
  #   seealso: DoGBRROASAnalysis (generic), DoGBRROASAnalysis.GeoExperimentData.

  kClassName <- "GBRROASAnalysisFitGbr1"
  SetMessageContextString("DoGBRROASAnalysis.GBRROASAnalysisData")
  on.exit(SetMessageContextString())

  # Find ad spend differential for each geo.
  incremental.cost <- EstimateIncremental(obj, variable="cost")
  # Augment the original object with these two columns.
  obj[["incr.cost"]] <- incremental.cost
  obj[["lmweights"]] <- ComputeLinearModelWeights(obj[["resp.pre"]])
  # Fit the iROAS model.
  lmfit <- lm(resp.test ~ resp.pre + incr.cost, data=obj,
              weights=obj[["lmweights"]])

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
                     data=obj,
                     iroas.post=.PosteriorBeta2Tail,
                     model=kGBRModel1)
  class(obj.result) <- c(kClassName, class(obj.result))
  return(obj.result)
}

DoGBRROASAnalysis.GeoExperimentData <- function(obj,
                                                response=character(0),
                                                cost=character(0),
                                                pretest.period=0L,
                                                intervention.period=1L,
                                                cooldown.period=NULL,
                                                control.group=1L,
                                                treatment.group=2L,
                                                ...) {
  # Performs a ROAS Analysis on a GeoExperimentData object.
  #
  # Args:
  #   obj: a GeoExperimentData object.
  #   response: (string) name of the response variable column.
  #   cost: (string) name of the cost variable column.
  #   pretest.period: (non-negative integer) number of the pretest period,
  #     typically 0.
  #   intervention.period: (vector of non-negative integers) number(s) of the
  #     period(s) forming the intervention period. All must be larger than the
  #     largest period in the pretest period.
  #   cooldown.period: (vector of non-negative integers or NULL) number(s) of
  #     the period(s) forming the cooldown period. All must be larger than the
  #     largest period in the intervention period.
  #   control.group: (a vector of positive integers) number(s) of geo
  #     groups forming the control group.
  #   treatment.group: (a vector of positive integers) number(s) of geo
  #     groups forming the treatment group.
  #   ...: further arguments passed to as.GBRROASAnalysisData.
  #
  # Returns:
  #   A GBRROASAnalysisFit object.
  #
  # Notes:
  #   See also: DoGBRROASAnalysis (generic),
  #   DoGBRROASAnalysis.GBRROASAnalysisData.

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
