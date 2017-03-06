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

# Methods for GBRROASAnalysisFit objects.
# - print, coef, summary, confint

print.GBRROASAnalysisFitGbr1 <- function(x, ...) {
  # Shows a summary of GBR analysis results.
  #
  # Args:
  #   x: a GBRROASAnalysisFitGbr1 object.
  #   ...: ignored.
  #
  # Returns:
  #   The object itself, invisibly. As a side effect, prints the
  #   default summary of 'x' on the console.

  print(summary(x))
  return(invisible(x))
}

coef.GBRROASAnalysisFitGbr1 <- function(object, ...) {
  # Returns the model coefficients.
  #
  # Args:
  #   object: a GBRROASAnalysisFitGbr1 object.
  #   ...: ignored.
  #
  # Returns:
  #   A named numeric vector with the model coefficients from the GBR
  #   analysis.

  coeff <- coef(summary(object[["lmfit"]]))
  return(coeff)
}

confint.GBRROASAnalysisFitGbr1 <- function(object, parm="incr.cost", level=0.95,
                                       ...) {
  # Returns the confidence intervals associated with the model
  # parameters.
  #
  # Args:
  #   object: a GBRROASAnalysisFitGbr1 object.
  #   parm: (character vector) a specification of which parameters are
  #     to be given confidence intervals, either a vector of numbers
  #     or a vector of names.
  #   level: (number between 0 and 1) the confidence level required.
  #   ...: ignored.
  #
  # Returns:
  #   A matrix with the confidence intervals from the 'confint.lm' method.

  assert_that(is.vector.of.nonempty.strings(parm))
  assert_that(is.real.number(level), level > 0 & level < 1)
  conf.int <- confint(object[["lmfit"]], parm=parm, level=level, ...)
  return(conf.int)
}

summary.GBRROASAnalysisFitGbr1 <- function(object, level=0.90,
                                           interval.type=c("one-sided",
                                               "two-sided"),
                                           threshold=0,
                                           ...) {
  # Returns a concise summary of the ROAS estimate and confidence
  # interval and the total incremental cost and incremental response.
  #
  # Args:
  #   object: a GBRROASAnalysisFitGbr1 object.
  #   level: (number between 0 and 1) confidence level.
  #   interval.type: (string) 'one-sided', or 'two-sided' (interval).
  #   threshold: (numeric vector) threshold(s) for the right-tail posterior
  #     probabilities of beta2.
  #   ...: ignored.
  #
  # Returns:
  #   A ROASAnalysisResults object.

  SetMessageContextString("summary.GBRROASAnalysisFitGbr1")
  on.exit(SetMessageContextString())

  assert_that(is.real.number(level),
              level > 0 && level < 1,
              msg=Message("'level' must be > 0 and < 1"))
  interval.type <- match.arg(interval.type)
  assert_that(is.real.number(threshold))

  if (interval.type %in% "one-sided") {
    p.two.sided <- (2 * level - 1)
  } else {
    p.two.sided <- level
  }
  # confint() computes 2-sided intervals.
  conf.int <- confint(object[["lmfit"]], parm="incr.cost",
                      level=p.two.sided)[1L, ]
  if (interval.type %in% "one-sided") {
    conf.int[2] <- Inf
  }
  iroas.estimate <- coef(object)["incr.cost", "Estimate"]
  incr.cost <- sum(object[["data"]][["incr.cost"]])
  incr.resp <- (iroas.estimate * incr.cost)
  post.prob <- object[["iroas.post"]](threshold)

  obj.res <- ROASAnalysisResults(estimate=iroas.estimate,
                                 lower=conf.int[1],
                                 upper=conf.int[2],
                                 level=level,
                                 incr.resp=incr.resp,
                                 incr.cost=incr.cost,
                                 threshold=threshold,
                                 post.prob=round(post.prob, digits=3),
                                 model=object[["model"]])
  return(obj.res)
}
