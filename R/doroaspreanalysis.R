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

#' Predicts the standard deviation (standard error) of the posterior of the
#' iROAS estimator.
#'
#' @param obj an object.
#' @param ... further arguments passed to or from other methods.
#'
#' @return A ROASPreanalysisFit object.
#'
#' @rdname DoROASPreanalysis
DoROASPreanalysis <- function(obj, ...) {
  UseMethod("DoROASPreanalysis")
}

#' @param period.lengths a vector of length 3, denoting the lengths (in days)
#'   of pre-period, test, and the cooldown periods, respectively. The test
#'   period must be at least 7 days; the pre-period must be at least as
#'   long as the test period; the cooldown period can be 0 or more days.
#' @param geos (\code{GeoStrata} or \code{GeoAssignment} object) object
#'   to use for choosing the geo groups; if it is a \code{GeoAssignment}
#'   object, the geo assignment will be fixed. If it is a \code{GeoStrata}
#'   object, method \code{Randomize} will be used to obtain a geo assignment
#'   at each iteration.
#' @param recycle (flag) if TRUE, uses an augmented data set in order to create
#'   a larger time series, reusing data from the head of the time series.
#' @return A ROASPreanalysisFit object.
#'
#' @note \code{DoROASPreanalysis.GeoTimeseries} is a wrapper, combining
#' calls to \code{GeoExperimentPreanalysisData} and \code{DoROASPreanalysis}.\cr
#'
#' \code{DoROASPreanalysis.GeoTimeseries}: The extra arguments \code{...} are
#' passed to \code{DoROASPreanalysis.GeoExperimentPreanalysisData}.\cr
#'
#' @rdname DoROASPreanalysis
DoROASPreanalysis.GeoTimeseries <- function(obj, period.lengths, geos,
                                            recycle=TRUE, ...) {
  assert_that(is.integer.valued(period.lengths),
              length(period.lengths) == 3L,
              period.lengths[2] >= 7L,
              period.lengths[1] >= period.lengths[2],
              period.lengths[3] >= 0)
  period.lengths <- as.integer(period.lengths)

  obj.gpd <- GeoExperimentPreanalysisData(obj,
                                          period.lengths=period.lengths,
                                          geos=geos,
                                          recycle=recycle)
  obj.result <- DoROASPreanalysis(obj.gpd, ...)
  return(obj.result)
}


#' @param response (string) name of the metric column to use as the response
#'   variable.
#' @param prop.to (string) an existing name of the column in proportion to
#'   which the spend change will be distributed across the geos.
#' @param n.sims (integer or NA) number of simulations to do. Note: if 'geos'
#'   is a GeoAssignment object, NA is allowed, and the actual number of
#'   simulations will be the number of all possible pseudo-data sets; if
#'   n.sims is given, only the first n.sims data sets will be simulated
#'   (useful for testing only).
#' @param models (vector of nonempty strings) one or more analysis model ids,
#'   to apply to each simulated data set.
#' @param FUN (function) a lapply-compatible function to use for generating the
#'   simulations. Use this for a parallel version of lapply to speed up
#'   the process. Note: the function only needs to be able to iterate
#'   along a vector of integers.
#'
#' @note Simulates experiments without resorting to regenerating time series.
#' Segments of the original time series are used to create 'pseudo-timeseries'
#' of the length of the experiment. These simulated geo experiment data sets
#' are produced by calls to \code{SimulateGeoExperimentData}.\cr
#'
#' The object that is returned contains the simulated raw numbers. Use
#' the \code{summary} method for a more user-friendly output.\cr
#'
#' \code{DoROASPreanalysis.GeoExperimentPreanalysisData}: The extra arguments
#' \code{...} are passed to \code{FUN}.
#'
#' @rdname DoROASPreanalysis
DoROASPreanalysis.GeoExperimentPreanalysisData <- function(
    obj, response, prop.to, n.sims=1000,  models=GetModelIds(),
    FUN=base::lapply, ...) {
  kClassName <- "ROASPreanalysisFit"

  assert_that(is.nonempty.string(response))
  assert_that(is.vector.of.nonempty.strings(models),
              all(models %in% GetModelIds()))

  assert_that(is.string(prop.to),
              prop.to %in% names(obj))
  assert_that(is.count(n.sims) || isTRUE(is.na(n.sims)))
  assert_that(is.nonempty.string(response))
  assert_that(is.vector.of.nonempty.strings(models),
              all(models %in% GetModelIds()))

  geos <- GetInfo(obj, "geos")
  # 'geos' is either GeoAssignment or GeoStrata.
  randomize.i <- !inherits(geos, "GeoAssignment")
  if (randomize.i) {
    # A GeoStrata. Produce n.sims simulations, with randomization.
    assert_that(!is.na(n.sims))
    n.groups <- GetInfo(geos, "n.groups")
    swap.and.redo <- (n.groups == 2)
    if (swap.and.redo) {
      n.actual.sims <- ceiling(n.sims / 2)
    } else {
      n.actual.sims <- n.sims
    }
  } else {
    # A GeoAssignment. Produce i.max simulations, no randomization.
    i.max <- GetInfo(obj, "i.max")
    if (is.na(n.sims)) {
      n.actual.sims <- i.max
    } else {
      n.actual.sims <- min(n.sims, i.max)
    }
    swap.and.redo <- FALSE
  }

  sim.seq <- seq_len(n.actual.sims)
  result.list <- FUN(sim.seq, FUN=.SimulateROASAnalysis, obj=obj,
                     randomize.i=randomize.i,
                     response=response, prop.to=prop.to, cost=1,
                     models=models, swap.and.redo=swap.and.redo)

  if (!is.list(result.list)) {
    warning(Message("Result from the function was not a list"))
  }

  obj.result <- do.call(rbind, args=result.list)
  # One of the columns, 'geo.assignment', is a list of data frames. Extract it
  # and assign into the 'info' attribute so obj.result can be viewed more
  # conveniently.
  geo.assignments <- obj.result[["geo.assignment"]]
  obj.result[["geo.assignment"]] <- NULL
  rownames(obj.result) <- NULL
  obj.result <- SetInfo(obj.result,
                        "obj.gpd"=obj,
                        "geo.assignment"=geo.assignments)
  class(obj.result) <- c(kClassName, "data.frame")
  return(obj.result)
}

#' Computes a summary of the predicted ROAS estimate and associated
#' incremental cost.
#'
#' @param object a \code{ROASPreanalysisFit} object.
#' @param level (number between 0 and 1) confidence level.
#' @param interval.type (string) 'one-sided', 'two-sided'.
#' @param precision (number) target precision of the estimate; the distance
#'   between the lower bound of the confidence interval and the point
#'   estimate. Note: if \code{cost} is given, this will be ignored and the CI
#'   half-width will be computed based on \code{cost}.
#' @param cost (number) cost difference (ad spend difference); if missing,
#'   will be computed based on \code{precision}.
#' @param ... ignored.
#'
#' @return A ROASPreanalysisResults object.

summary.ROASPreanalysisFit <- function(object, level=0.90,
                                       interval.type=c("one-sided",
                                           "two-sided"),
                                       precision=1.0,
                                       cost=NA_real_,
                                       ...) {
  kClassName <- "ROASPreanalysisResults"
  SetMessageContextString("summary.ROASPreanalysisFit")
  on.exit(SetMessageContextString())

  assert_that(is.real.number(level), level > 0 & level < 1)
  interval.type <- match.arg(interval.type)
  assert_that(is.scalar(precision),
              is.na(precision) || (is.real.number(precision) && precision > 0))
  assert_that(is.scalar(cost),
              is.na(cost) || (is.real.number(cost) && cost > 0))

  alpha <- switch(interval.type,
                  "one-sided"=1 - level,
                  "two-sided"=0.5 * (1 - level),
                  stop("Internal error"))

  .GetPrecisionTable <- function(model) {
    df <- object[object[["model"]] %in% model, , drop=FALSE]
    sd <- df[["sd"]]
    dof <- df[["df"]]
    multiplier <- qt(1 - alpha, df=dof)
    precision <- (sd * multiplier)
    x <- data.frame(x=precision)
    names(x) <- model
    return(x)
  }
  .Summarize <- function(model) {
    df <- object[object[["model"]] %in% model, , drop=FALSE]
    se.pred <- median(df[["sd"]])
    dof <- df[["df"]][[1]]
    multiplier <- qt(1 - alpha, df=dof)
    if (is.na(cost)) {
      actual.precision <- (multiplier * se.pred)
      target.precision <- precision
      cost <- (actual.precision / target.precision)
    } else {
      precision <- (multiplier * se.pred / cost)
    }
    data.frame(model=model, precision=precision, total.cost=cost)
  }
  .GeoRatio <- function(geo.assignments) {
    d <- geo.assignments[object[["model"]] %in% models[1]]
    gg <- unlist(lapply(d, function(x) x[[kGeoGroup]]))
    gg <- gg[gg >= 1]
    frac <- (table(gg) / length(gg))
    prop <- (frac / min(frac))
    if (isTRUE(all.equal(prop, trunc(prop)))) {
      prop <- trunc(prop)
      gratio <- paste(prop, collapse=":")
    } else {
      gratio <- paste(round(prop, 1), collapse=":")
    }
    data.frame(cfrac=signif(frac[1]), gratio=gratio)
  }
  geo.assignments <- GetInfo(object, "geo.assignment")
  geos <- GetInfo(GetInfo(object, "obj.gpd"), "geos")
  fixed.assignment <- inherits(geos, "GeoAssignment")
  if (!fixed.assignment) {
    # Only for the purpose of counting geos below.
    geos <- geo.assignments[[1]]
  }
  n.all.geos <- sum(GetGeoGroup(geos) != 0, na.rm=TRUE)

  models <- unique(object[["model"]])
  prec.table.list <- lapply(models, .GetPrecisionTable)
  prec.table <- do.call(cbind, args=prec.table.list)

  result.list <- lapply(models, .Summarize)
  df.sd <- do.call(rbind, args=result.list)
  df.gratio <- .GeoRatio(geo.assignments)

  period.lengths <- GetInfo(GetInfo(object, "obj.gpd"), "period.lengths")

  obj.result <- data.frame(df.sd,
                           level=level,
                           interval=interval.type,
                           df.gratio,
                           n.geos=n.all.geos,
                           pretest=period.lengths[1L],
                           test=period.lengths[2L],
                           cooldown=period.lengths[3L],
                           fixed=fixed.assignment)
  class(obj.result) <- c(kClassName, class(obj.result))
  obj.result <- SetInfo(obj.result, precision.table=prec.table)
  return(obj.result)
}
