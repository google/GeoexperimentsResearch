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

summary.TBRAnalysisFitTbr1 <- function(object,
                                       level=0.90,
                                       interval.type=c("one-sided",
                                           "two-sided"),
                                       threshold=0, ...) {
  # Returns a concise summary of the incremental response estimate and
  # its confidence interval for the model 'tbr1'.
  #
  # Args:
  #   object: a TBRAnalysisFit object.
  #   level: (number between 0 and 1) confidence level.
  #   interval.type: (string) 'one-sided', 'two-sided' (interval).
  #   threshold: (numeric vector) threshold(s) for the right-tail posterior.
  #   ...: ignored.
  #
  # Returns:
  #   A TBRAnalysisResults object.

  kClassName <- "TBRAnalysisResults"
  SetMessageContextString("summary.TBRAnalysisFitTbr1")
  on.exit(SetMessageContextString())

  assert_that(is.real.number(level),
              level > 0 && level < 1)
  interval.type <- match.arg(interval.type)
  assert_that(is.real.number(threshold))

  day.in.analysis <- IsInPeriod(object, periods="analysis")
  last.day.of.test <- tail(which(day.in.analysis), 1)
  x <- object[last.day.of.test, , drop=TRUE]
  incr.estimate <- x[[kYpredCumdif]]
  df.residual <- GetInfo(object, "fit")[["df.residual"]]
  sd.estimate <- x[[kYpredCumdifSd]]
  if (interval.type %in% "two-sided") {
    p.left <- (0.5 * (1 - level))
    p <- c(p.left, 1 - p.left)
  } else {
    p <- c(1 - level, 1)  # Upper bound will be Inf.
  }
  t.alpha <- qt(p, df=df.residual)  # Vector of length 2.
  margins.of.error <- (t.alpha * sd.estimate)
  precision <- abs(margins.of.error[1])
  conf.int <- (incr.estimate +  margins.of.error)
  model <- GetInfo(object, "model")

  post.prob <- GetInfo(object, "tailprob")(threshold)

  obj <- data.frame(estimate=incr.estimate,
                    precision=precision,
                    lower=conf.int[1],
                    upper=conf.int[2],
                    se=sd.estimate,
                    level=level,
                    thres=threshold,
                    prob=round(post.prob, digits=3),
                    model=model)
  rownames(obj) <- "incremental"
  obj <- SetInfo(obj, estimate.columns=c("estimate",
                          "precision", "lower", "upper", "se"))
  class(obj) <- c(kClassName, oldClass(obj))
  return(obj)
}
