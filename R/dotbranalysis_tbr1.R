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

#' Performs a TBR Analysis using the 2-parameter linear model 'tbr1'.
#'
#' @param obj an object.
#' @param ... further arguments passed to or from other methods.
#' @return A TBRAnalysisFit object.
#'
#' @rdname DoTBRAnalysisTbr1
DoTBRAnalysisTbr1 <- function(obj, ...) {
  UseMethod("DoTBRAnalysisTbr1")
}

#' @rdname DoTBRAnalysisTbr1
DoTBRAnalysisTbr1.TBRAnalysisData <- function(obj, ...) {
  SetMessageContextString("DoTBRAnalysisTbr1.TBRAnalysisData")
  on.exit(SetMessageContextString())

  in.preexperiment <- IsInPeriod(obj, periods="preexperiment")
  in.pretest <- IsInPeriod(obj, periods="pretest")
  in.prediction <- IsInPeriod(obj, periods="prediction")
  in.excluded <- IsInPeriod(obj, periods="excluded")

  length.pretest <- sum(in.pretest)
  kMinPretest <- 3L
  assert_that(length.pretest >= kMinPretest,
              msg=Message("Need at least ", kMinPretest, " timepoints ",
                  "in pre-test period (there were only ", length.pretest, ")"))
  pretest.data <- obj[in.pretest, , drop=FALSE]
  if (sd(pretest.data[[kX]]) == 0) {
    # Drop the second parameter, assume the counterfactual is a constant.
    formula <- as.formula(sprintf("%s ~ 1", kY))
    lmfit <- lm(formula, data=pretest.data)
    newdata <- as.data.frame(x=rep(1, nrow(obj)))
    coef.x <- coef(lmfit)[1]
  } else {
    formula <- as.formula(sprintf("%s ~ %s", kY, kX))
    lmfit <- lm(formula, data=pretest.data)
    newdata <- obj[, kX, drop=FALSE]
    coef.x <- coef(lmfit)[kX]
  }
  # Do the pointwise predictions for both time periods.
  # Note: it is possible that some of the data points are excluded.
  lmpred <- predict(lmfit, newdata=newdata, se.fit=TRUE)
  # Get pointwise observations and predictions.
  # 'y' refers to the response, and 'x' to the covariate.
  y.hat <- lmpred[["fit"]]  # Point estimates of the mean of predictions.
  y.hat.sd <- lmpred[["se.fit"]]  # S.d. estimates of the y.pred.
  sigma <- lmpred[["residual.scale"]]  # Residual s.d.
  y.pred.sd <- sqrt(y.hat.sd^2 + sigma^2)  # Pointwise s.d. of the predictions.
  y <- obj[[kY]]
  y.pred.dif <- (y - y.hat)

  y.hat[in.preexperiment] <- NA_real_
  y.pred.dif[in.preexperiment] <- NA_real_
  y.pred.sd[in.preexperiment] <- NA_real_

  # Mean estimate of the cumulative incremental response.
  y.cum.dif <- rep(NA_real_, length(y.pred.dif))
  y.cum.dif[in.prediction] <- cumsum(y.pred.dif[in.prediction])

  # Compute the cumulative variance in the test period.
  x <- obj[[kX]]
  x.test <- x[in.prediction]
  x.seq <- seq_along(x.test)
  x.cum.mean <- (cumsum(x.test) / x.seq)
  covariance.matrix <- vcov(lmfit)  # Linear model coefficient cov. matrix.
  alpha.var <- covariance.matrix[1, 1]
  # Variance of cumulative mean (accounts for correlation).
  if (nrow(covariance.matrix) == 1) {
    # Only the intercept was estimated.
    y.pred.var.cum.test <- (x.seq^2 * alpha.var)
  } else {
    beta.var <- covariance.matrix[2, 2]
    alpha.beta.cov <- covariance.matrix[1, 2]
    y.pred.var.cum.test <-
        x.seq^2 * (alpha.var + x.cum.mean^2 * beta.var +
                   2 * x.cum.mean * alpha.beta.cov)
  }
  # Calculate the variance of a predicted new cumulative response by
  # adding the cumulative error terms in the prediction period.
  # In the pre-period, the predictive uncertainty is zero.
  y.pred.var.cum <- rep(NA_real_, length(y))
  y.pred.var.cum[in.pretest] <- 0
  y.pred.var.cum[in.prediction] <- (y.pred.var.cum.test + x.seq * sigma^2)
  y.pred.sd.cum <- sqrt(y.pred.var.cum)

  df.out <- as.data.frame(structure(list(y.hat,
                                         y.pred.sd,
                                         y.pred.dif,
                                         y.cum.dif,
                                         y.pred.sd.cum,
                                         y.hat.sd),
                                    names=c(kYpredMean, kYpredSd, kYpredDif,
                                        kYpredCumdif, kYpredCumdifSd,
                                        kYestSd)))
  df.out[in.pretest, kYpredSd] <- 0
  df.out[!in.pretest, kYestSd] <- NA_real_
  # Dates that have been excluded will not contribute.
  df.out[in.excluded, ] <- NA_real_
  obj.out <- cbind(obj[c(kDate, kPeriod, kY, kX)], df.out)
  class(obj.out) <- c("TBRAnalysisFitTbr1", class(obj.out))

  .GetTailProb <- function(x) {
    # Calculates the posterior of Pr(effect > x | data) with the uniform prior
    # on (alpha, beta, log sigma).
    #
    # Args:
    #   x: (numeric vector) tail probability threshold.
    # Returns:
    #   A numeric vector of tail probabilities, where
    #   components correspond to those in 'x'.
    # Notes:
    #   Reference: Gelman et al., Bayesian Data Analysis (3rd ed.).  See
    #   section 14.2, equations (14.2) through (14.7), about fitting a linear
    #   regression model without weights.
    #   With the given noninformative prior, the location and scale of the
    #   t-distributed posterior of a coefficient matches its classical
    #   point estimate and its standard error, respectively.

    # 'lmfit' will be available from within the closure.

    in.analysis <- IsInPeriod(obj.out, periods="analysis")
    last.day.of.analysis <- tail(which(in.analysis), 1)
    loc.sd <- obj.out[last.day.of.analysis, , drop=FALSE]
    location <- loc.sd[[kYpredCumdif]]
    se <- loc.sd[[kYpredCumdifSd]]
    q <- ((x - location) / se)
    df.residual <- lmfit[["df.residual"]]
    prob <- pt(q, df=df.residual, lower.tail=FALSE)
    return(prob)
  }

  obj.out <- SetInfo(obj.out, model=kTBRModel1, fit=lmfit, data=obj,
                     tailprob=.GetTailProb)
  return(obj.out)
}
