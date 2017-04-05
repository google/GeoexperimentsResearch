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

#' Computes quantiles of the cumulative, pointwise, or the counterfactual
#' (posterior) distribution of the causal lift, from the beginning of
#' pretest to the end of posttest.
#'
#' @param x a TBRAnalysisFitTbr1 object.
#' @param probs (numeric vector, each >= 0 and <= 1) quantiles to calculate.
#' @param distribution (string) either 'cumulative' or 'pointwise' or
#'   'counterfactual'.
#' @param ... ignored.
#'
#' @return A TBRQuantiles object, which is a data frame with each row
#'   corresponding to a day in the pretest and the prediction period, with
#'   columns 'date', 'test', and the columns for the quantiles.

quantile.TBRAnalysisFitTbr1 <- function(x, probs=c(0.1, 0.5, 0.9),
                                        distribution=c("cumulative",
                                            "pointwise", "counterfactual"),
                                        ...) {
  kClassName <- "TBRQuantiles"
  SetMessageContextString("quantile.TBRAnalysisFitTbr1")
  on.exit(SetMessageContextString())

  assert_that(is.numeric(probs),
              length(probs) >= 1,
              !anyNA(probs),
              all(probs >= 0 & probs <= 1))
  distribution <- match.arg(distribution)

  # For the Counterfactual quantiles, use the estimation s.d. in pretest period
  # and the prediction s.d. in the prediction period.
  kCfSd <- ".cf.sd"
  x[[kCfSd]] <- x[[kYpredSd]]
  in.pretest <- IsInPeriod(x, periods="pretest")
  x[[kCfSd]][in.pretest] <- x[[kYestSd]][in.pretest]

  column <- switch(distribution,
                   "counterfactual"=c(location=kYpredMean, scale=kCfSd),
                   "cumulative"=c(location=kYpredCumdif, scale=kYpredCumdifSd),
                   "pointwise"=c(location=kYpredDif, scale=kYpredSd))

  df.residual <- GetInfo(x, "fit")[["df.residual"]]
  n.probs <- length(probs)
  margins <- matrix(rep(x[, column["scale"]], each=n.probs) *
                    qt(probs, df=df.residual),
                    ncol=n.probs, byrow=TRUE)
  m <- x[, column["location"]] + margins
  colnames(m) <- sprintf("%g%%", probs * 100)

  obj.result <- cbind(x[, c(kDate, kPeriod)], as.data.frame(m))
  rownames(obj.result) <- NULL
  class(obj.result) <- c(kClassName, class(obj.result))
  return(obj.result)
}

#' Computes quantiles of the cumulative or pointwise distribution of the
#' incremental ROAS, for each of the days in the pretest and the
#' prediction period.
#'
#' @param x a TBRROASAnalysisFit object.
#' @param probs (numeric vector, each >= 0 and <= 1) quantiles to calculate.
#' @param distribution (string) either 'cumulative' or 'pointwise'.
#' @param ... ignored.
#' @return A TBRQuantiles object, which is a data frame with each row
#'   corresponding to a day in the pretest and the prediction period, with
#'   columns 'date', 'test', and the columns for the quantiles.
#'
#' @note
#' The pretest period is included solely for the purpose of obtaining
#' objects of conforming size from both quantile methods (that of
#' TBRAnalysisFitTbr1 and of this one).

quantile.TBRROASAnalysisFit <- function(x, probs=c(0.1, 0.5, 0.9),
                                        distribution=c("cumulative",
                                            "pointwise"), ...) {
  kClassName <- "TBRQuantiles"
  SetMessageContextString("quantile.TBRROASAnalysisFit")
  on.exit(SetMessageContextString())

  assert_that(is.numeric(probs),
              length(probs) >= 1,
              !anyNA(probs),
              all(probs >= 0 & probs <= 1))
  distribution <- match.arg(distribution)

  col.location <- switch(distribution,
                         "cumulative"=kYpredCumdif,
                         "pointwise"=kYpredDif)
  col.scale <- switch(distribution,
                      "cumulative"=kYpredCumdifSd,
                      "pointwise"=kYpredSd)
  resp.sim <- x[["response"]]
  cost.sim <- x[["cost"]]
  tbr.resp <- GetInfo(x, "tbr.resp")
  in.prediction <- IsInPeriod(tbr.resp, periods="prediction")

  cost.is.constant <- is.null(dim(cost.sim))

  if (cost.is.constant) {
    # No need to resort to simulations. Just scale the response instead.
    cost <- switch(distribution,
                   "cumulative"=cumsum(cost.sim),
                   "pointwise"=cost.sim)
    resp.q <- quantile(tbr.resp, probs=probs, distribution=distribution)
    iroas.q <- as.matrix(resp.q[in.prediction, ]) / cost
  } else {
    if (distribution == "cumulative") {
      cumul.resp <- cumsum(resp.sim)
      cumul.cost <- cumsum(cost.sim)
      iroas <- (cumul.resp / cumul.cost)
    } else {
      iroas <- (resp.sim / cost.sim)
    }
    iroas.q <- quantile(iroas, probs=probs, na.rm=TRUE)
  }
  # If incremental cost is zero then we get +/- Inf. Change these to NA.
  iroas.q[is.infinite(iroas.q)] <- NA_real_
  obj.result <- SetInfo(tbr.resp[, c(kDate, kPeriod)], info=list())
  obj.result[, colnames(iroas.q)] <- NA_real_
  obj.result[in.prediction, colnames(iroas.q)] <- iroas.q
  class(obj.result) <- c(kClassName, class(obj.result))
  return(obj.result)
}
