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

#' Performs a TBR ROAS Analysis.
#'
#' @param obj an object.
#' @param model (string) id of the TBR to use.
#' @param response (string) name of the column of the response variable.
#' @param cost (string) name of the column of the cost variable, or
#'   alternatively a real number specifying the exact incremental cost
#'   (spend change).
#' @param n.sims (integer) number of simulations to draw.
#' @param ... arguments passed to function 'DoTBRAnalysis'.
#' @return A TBRROASAnalysisFit object.
#'
#' @seealso DoTBRAnalysis, DoGBRROASAnalysis
#' @rdname DoTBRROASAnalysis
DoTBRROASAnalysis <- function(obj, model, response, cost, n.sims=1e5, ...) {
  assert_that(is.nonempty.string(model))
  assert_that(is.nonempty.string(response))
  assert_that(is.nonempty.string(cost) || is.real.number(cost))

  assert_that(is.count(n.sims) && !is.na(n.sims) && n.sims >= 1)

  UseMethod("DoTBRROASAnalysis")
}

#' @rdname DoTBRROASAnalysis
DoTBRROASAnalysis.GeoExperimentData <- function(obj, model, response, cost,
                                                n.sims=1e5, ...) {
  SetMessageContextString("DoTBRROASAnalysis")
  on.exit(SetMessageContextString())

  tbr.resp <- DoTBRAnalysis(obj, model=model, response=response, ...)
  in.prediction <- IsInPeriod(tbr.resp, periods="prediction")
  in.intervention <- IsInPeriod(tbr.resp, periods="intervention")

  if (is.real.number(cost)) {
    assert_that(cost != 0,
                msg=Message("'cost' must not be zero"))
    n.prediction <- sum(in.prediction)
    n.intervention <- sum(in.intervention)
    # Spread the cost evenly across the intervention period.
    # cost.sim will be a dimensionless vector of length sum(in.intervention).
    # Constant cost <=> dimension of cost.sim is NULL.
    cost.sim <- rep(0, length.out=n.prediction)
    cost.sim[seq_len(n.intervention)] <- cost / n.intervention
    tbr.cost <- NULL
  } else {
    tbr.cost <- DoTBRAnalysis(obj, model=model, response=cost, ...)
    cost.summ <- summary(tbr.cost, level=0.99, interval.type="two-sided")
    cost.bounds <- cost.summ[c("lower", "upper")]
    if (prod(sign(cost.bounds)) <= 0) {
      # 99% interval crosses zero.
      warning(Message("Unstable result: cost may be zero"))
    }
    pred.sd <- tbr.cost[in.intervention, kYpredSd]
    if (isTRUE(all.equal(range(pred.sd), c(0, 0)))) {
      # Cost is constant, do not generate simulations.
      cost.sim <- tbr.cost[in.prediction, kYpredDif, drop=TRUE]
    } else {
      assert_that(n.sims > 1,
                  msg=Message("Cannot estimate iROAS when n.sims == 1 as ",
                      "cost is not constant"))
      cost.sim <- SimulatePosterior(tbr.cost, n.sims=n.sims)
    }
  }
  if (n.sims == 1) {
    # Obtain only the point estimates.
    resp.sim <- tbr.resp[in.prediction, kYpredDif, drop=FALSE]
  } else {
    resp.sim <- SimulatePosterior(tbr.resp, n.sims=n.sims)
  }

  obj.result <- list(response=resp.sim, cost=cost.sim)
  class(obj.result) <- c("TBRROASAnalysisFit", class(obj.result))
  obj.result <- SetInfo(obj.result,
                        response=response,
                        cost=cost,
                        tbr.resp=tbr.resp,
                        tbr.cost=tbr.cost,
                        model=model)
  return(obj.result)
}

#' Shows a summary of TBR analysis results.
#'
#' @param x a TBRROASAnalysisFit object.
#' @param ... ignored.
#' @return The object itself, invisibly. As a side effect, prints the default
#'   summary of 'x' on the console.

print.TBRROASAnalysisFit <- function(x, ...) {
  print(summary(x, ...))
  return(invisible(x))
}

#' Returns a concise summary of the incremental response estimate and its
#' confidence interval for a TBR ROAS analysis.
#'
#' @param object a TBRROASAnalysisFit object.
#' @param level (number between 0 and 1) confidence level.
#' @param interval.type (string) 'one-sided', 'two-sided' (interval).
#' @param threshold (numeric vector) threshold(s) for the right-tail posterior
#'   probabilities of the total cumulative iROAS.
#' @param ... ignored.
#' @return A ROASAnalysisResults object.

summary.TBRROASAnalysisFit <- function(object,
                                       level=0.90,
                                       interval.type=c("one-sided",
                                           "two-sided"),
                                       threshold=0,
                                       ...) {
  kClassName <- "TBRAnalysisResults"
  SetMessageContextString("summary.TBRAnalysisFitTbr1")
  on.exit(SetMessageContextString())

  interval.type <- match.arg(interval.type)
  info <- GetInfo(object)

  resp.sim <- object[["response"]]
  cost.sim <- object[["cost"]]
  tbr.resp <- info[["tbr.resp"]]
  cost.is.constant <- is.null(dim(cost.sim))
  summ.resp <- summary(tbr.resp, level=level, interval.type=interval.type)
  incr.resp <- summ.resp[["estimate"]]
  in.analysis <- IsInPeriod(object, periods="analysis")
  last.analysis.date <- tail(which(in.analysis), 1)
  if (cost.is.constant) {
    # No need to resort to simulations.
    cumul.cost <- cumsum(cost.sim)
    incr.cost <- cumul.cost[last.analysis.date]
    iroas.estimate <- (incr.resp / incr.cost)
    iroas.se <- (summ.resp[["se"]] / incr.cost)
    iroas.lower <- (summ.resp[["lower"]] / incr.cost)
    iroas.upper <- (summ.resp[["upper"]] / incr.cost)

    lmfit <- GetInfo(tbr.resp, "fit")
    df.residual <- lmfit[["df.residual"]]
    location <- iroas.estimate
    q <- ((threshold - iroas.estimate) / iroas.se)
    post.prob <- pt(q, df=df.residual, lower.tail=FALSE)
  } else {
    # Recover the index of the last day of the analysis (test+cooldown) period.
    probs <- switch(interval.type,
                    "one-sided"=c(0.5, 1 - level, 1),
                    "two-sided"=c(0.5, 0.5 * c(1 - level, 1 + level)))
    cumul.resp <- cumsum(resp.sim)
    cumul.cost <- cumsum(cost.sim)
    iroas <- (cumul.resp / cumul.cost)
    iroas.quantiles <- quantile(iroas, probs=probs, na.rm=TRUE)

    total.iroas <- iroas.quantiles[last.analysis.date, , drop=FALSE]
    if (interval.type %in% "one-sided") {
      total.iroas[3] <- Inf  # Upper is 'Inf'.
    }
    iroas.estimate <- total.iroas[1]
    iroas.lower <- total.iroas[2]
    iroas.upper <- total.iroas[3]
    tbr.cost <- info[["tbr.cost"]]
    incr.cost <- summary(tbr.cost)[["estimate"]]
    post.prob <- mean(iroas[last.analysis.date, ] > threshold)
  }
  model <- info[["model"]]
  obj.result <- ROASAnalysisResults(estimate=iroas.estimate,
                                    lower=iroas.lower,
                                    upper=iroas.upper,
                                    level=level,
                                    incr.resp=incr.resp,
                                    incr.cost=incr.cost,
                                    threshold=threshold,
                                    post.prob=round(post.prob, digits=3),
                                    model=model)
  return(obj.result)
}
