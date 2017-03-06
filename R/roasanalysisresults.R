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

# Constructor and S3 methods for ROASAnalysisResults objects.
# Constructor:
# - ROASAnalysisResults
# Methods for ROASAnalysisResults objects:
# - round, signif

ROASAnalysisResults <- function(estimate, lower, upper, level, incr.resp,
                                incr.cost, threshold, post.prob,
                                model) {
  # Constructs a ROASAnalysisResults object.
  #
  # Args:
  #   estimate: (number) iROAS point estimate.
  #   lower: (number) lower bound of the interval estimate.
  #   upper: (number or Inf) upper bound of the interval estimate.
  #   level: (number between 0 and 1) confidence level.
  #   incr.resp: (number) point estimate of the total incremental response.
  #   incr.cost: (number) point estimate of the total incremental cost.
  #   threshold: (number) threshold for calculating the posterior probability.
  #   post.prob: (number) posterior probability Pr(beta2 > threshold|data).
  #   model: (string) id string indicating the model used.
  #
  # Returns:
  #   An object of class ROASAnalysisResults, which also inherits from
  #   'ROASAnalysisResults'. This is a data frame with the columns:
  #   \itemize{
  #      \item\code{estimate}: point estimate of the incremental Return On Ad
  #        Spend.
  #      \item\code{precision}: confidence/credible interval half-width.
  #         Calculated as 0.5 times the difference between upper and lower
  #         bounds; if upper is Inf, then precision is the distance between the
  #         estimate and the lower bound.
  #      \item\code{lower}: lower bound of the interval estimate.
  #      \item\code{upper}: upper bound of the interval estimate (can be Inf).
  #      \item\code{level}: confidence level.
  #      \item\code{incr.resp}: estimated incremental response.
  #      \item\code{incr.cost}: estimated incremental cost.
  #      \item\code{thres}: threshold for the posterior tail probability.
  #      \item\code{prob}: posterior tail probability Pr(beta2>thres|data).
  #      \item\code{model}: model id.}

  kClassName <- "ROASAnalysisResults"
  SetMessageContextString(kClassName)
  on.exit(SetMessageContextString())
  assert_that(is.real.number(estimate),
              is.real.number(lower),
              is.real.number(upper) || (is.inf(upper) && upper > 0),
              is.real.number(level),
              is.real.number(incr.resp),
              is.real.number(incr.cost),
              lower <= upper,
              level > 0 && level < 1,
              is.nonempty.string(model))
  assert_that(is.real.number(threshold))
  assert_that(is.real.number(post.prob),
              post.prob >= 0 && post.prob <= 1)

  # Confidence/Credible interval half-width.
  if (is.inf(upper)) {
    precision <- (estimate - lower)
  } else {
    precision <- (0.5 * (upper - lower))
  }
  obj <- data.frame(estimate=estimate, precision=precision, lower=lower,
                    upper=upper, level=level, incr.resp=incr.resp,
                    incr.cost=incr.cost, thres=threshold,
                    prob=post.prob, model=model)
  rownames(obj) <- "iROAS"
  class(obj) <- c(kClassName, oldClass(obj))

  obj <- SetInfo(obj, estimate.columns=c("estimate", "precision", "lower",
                          "upper", "incr.resp", "incr.cost"))
  return(obj)
}

round.ROASAnalysisResults <- function(x, digits=1) {
  # Rounds the estimates to a given number of decimal places.
  #
  # Args:
  #   x: a ROASAnalysisResults object.
  #   digits: number of digital places to round to.
  #
  # Returns:
  #   A ROASAnalysisResults object, with estimates appropriately
  #   rounded.

  cols <- GetInfo(x, "estimate.columns")
  x[cols] <- round(as.data.frame(x)[cols], digits=digits)
  return(x)
}

signif.ROASAnalysisResults <- function(x, digits=1) {
  # Rounds the estimates to a given number of significant digits.
  #
  # Args:
  #   x: a ROASAnalysisResults object.
  #   digits: number of significant digits to round to.
  #
  # Returns:
  #   A ROASAnalysisResults object, with estimates appropriately
  #   rounded.

  cols <- GetInfo(x, "estimate.columns")
  x[cols] <- signif(as.data.frame(x)[cols], digits=digits)
  return(x)
}
