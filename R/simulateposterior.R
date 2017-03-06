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

SimulatePosterior <- function(obj, n.sims=1e5, ...) {
  # Draws a sample from a posterior distribution.
  #
  # Args:
  #   obj: an object.
  #   n.sims: (integer >= 2) number of simulations to draw.
  #   ...: other arguments passed on to the methods.
  #
  # Returns:
  #   A PosteriorSimulations object, which is a matrix with 'n.sims'
  #   columns. Each row corresponds to a variable.

  assert_that(is.integer.valued(n.sims), n.sims >= 2)

  UseMethod("SimulatePosterior")
}

SimulatePosterior.TBRAnalysisFitTbr1 <- function(obj, n.sims=1e5, ...) {
  # Draws from the posterior distribution of the pointwise incremental effect
  # over the test period.
  #
  # Args:
  #   obj: A TBRAnalysisFitTbr1 object.
  #   n.sims: (integer >= 2) number of simulations to draw.
  #   ...: ignored.
  #
  # Returns:
  #   A PosteriorSimulations object, which is a matrix with 'n.sims' columns
  #   and as many rows as there are time points in the test period. Each row
  #   corresponds to one time point in the test period, with the draws from the
  #   pointwise distribution of the incremental effect.
  #
  # Notes:
  #   The 'standard' noninformative prior is assumed: uniform on (beta, log sigma).
  #
  #   Reference: Gelman et al. Bayesian Data Analysis (2nd ed.), section 14.2,
  #   formula (14.8).
  #
  #   To obtain the simulations for the cumulative distribution, use the
  #   \code{cumsum} method.

  SetMessageContextString("SimulatePosterior.TBRAnalysisFitTbr1")
  on.exit(SetMessageContextString())

  in.prediction <- IsInPeriod(obj, periods="prediction")
  # Check if covariates are missing. Cannot simulate NAs, so we exclude them.
  exclude <- is.na(obj[[kX]])
  do.predict <- (in.prediction & !exclude)
  x <- obj[do.predict, , drop=TRUE]

  summ <- summary(GetInfo(obj, "fit"))
  df <- summ[["df"]]
  n.params <- df[1]
  d.o.f <- df[2]
  v.beta <- summ[["cov.unscaled"]]

  if (n.params == 1) {
    m <- cbind(rep(1, nrow(x)))
  } else {
    m <- cbind(1, x[[kX]])  # n.params == 2.
  }
  n.pred.days <- nrow(m)

  var.pred0 <- (m %*% (v.beta %*% t(m)))
  var.pred <- (diag(1, nrow=n.pred.days, ncol=n.pred.days) + var.pred0)
  # r: a matrix of dimensions (n.sims, n.pred.days)
  sims <- MASS::mvrnorm(n.sims, mu=rep(0, n.pred.days), Sigma=var.pred)
  dimnames(sims) <- list(NULL, as.character(x[[kDate]]))
  # Draw from the posterior of sigma.
  sigma.hat <- summ[["sigma"]]
  sigma <- (sigma.hat * sqrt((d.o.f) / rchisq(n.sims, df=d.o.f)))
  # Rely on recycling of the vector of size n.sims (using the same draw of
  # sigma for each simulation of y pred).
  sims <- t(sigma * sims)  # Draws from the multivariate t with center 0.
  # Again rely on recycling to set the mean of the simulations.
  sims <- (x[[kYpredDif]] + sims)  # Matrix (n.pred.days, n.sims matrix).
  if (any(exclude)) {
    # Ensure that all matrices have all dates in the prediction period.
    # Create a simulation matrix with NAs in rows that have not been simulated,
    # ensuring conformity between the TBR Cost and Response simulation matrices.
    n.prediction <- sum(in.prediction)
    m <- matrix(NA_real_, nrow=n.prediction, ncol=ncol(sims),
                dimnames=list(as.character(obj[in.prediction, kDate]), NULL))
    # 'predicted' is an indicator of which dates we have predicted (i.e.,
    # excluding dates that have NA in covariate 'x'). The first date of
    # 'predicted' is the first date of the prediction period.
    predicted <- do.predict[in.prediction]
    m[predicted, ] <- sims
    sims <- m
  }
  obj.result <- sims
  class(obj.result) <- c("PosteriorSimulations", class(sims))
  return(obj.result)
}

quantile.PosteriorSimulations <- function(x,
                                          probs=c(0.05, 0.1, 0.5, 0.9, 0.95),
                                          names=TRUE,
                                          ...) {
  # Compute the quantiles for the posterior simulations.
  #
  # Args:
  #   x: a PosteriorSimulations object.
  #   probs: (numeric vector) of probabilities.
  #   names: (flag) if TRUE, the result has a 'names' attribute.
  #   ...: possibly other arguments passed on to 'quantile'.
  #
  # Returns:
  #   A matrix with n rows and m columns, where n = rows in 'x' and m = length
  #   of 'probs'.

  assert_that(is.numeric(probs), length(probs) >= 1, !anyNA(probs),
              all(probs >= 0), all(probs <= 1))
  assert_that(is.flag(names) && !is.na(names))

  m <- apply(x, MARGIN=1, FUN=stats::quantile, probs=probs, names=names, ...)

  # Ensure that the output is always a matrix with nrow(x) rows.
  if (length(probs) == 1) {
    m <- matrix(m, nrow=nrow(x), ncol=1,
                dimnames=list(NULL, sprintf("%g%%", 100 * probs)))
  } else {
    m <- t(m)
  }
  return(m)
}

cumsum.PosteriorSimulations <- function(x) {
  # Returns simulations of the posterior cumulative joint distribution.
  #
  # Args:
  #   x: a PosteriorSimulations object.
  #
  # Returns:
  #   A PosteriorSimulations object.

  obj <- apply(x, MARGIN=2, FUN=cumsum)
  class(obj) <- class(x)
  return(obj)
}
