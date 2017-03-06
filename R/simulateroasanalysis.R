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

.SimulateROASAnalysis <- function(sim, randomize.i, obj, response, prop.to,
                                  cost=1, models=GetModelIds(),
                                  swap.and.redo=FALSE) {
  # [internal] Performs a ROAS analysis on one simulated data set drawn from
  # the GeoExperimentPreanalysisData object.
  #
  # Args:
  #   sim: (integer >= 1) number of the simulation; if 'randomize.i' is FALSE,
  #     'sim' will be used as the number of the data set to draw.
  #   randomize.i: (flag) if TRUE, the number of the data set will be drawn at
  #     random. If FALSE, the number of the data set will be set equal to
  #     'sim'.
  #   obj: a GeoExperimentPreanalysisData object.
  #   response: (string) name of the metric column to use as the response
  #     variable.
  #   prop.to: (string) an existing name of the column in proportion to which
  #     the spend change will be distributed across the geos.
  #   cost: (real number) incremental cost, assumed fixed and known.
  #   models: (vector of nonempty strings) one or more analysis model ids. The
  #     simulated data set is apply to simulated data set. By default, all
  #     possible models are fit.
  #   swap.and.redo: (flag) swap groups and redo the analysis? Note: number of
  #     groups must be 2 if this flag is TRUE. Since this produces two
  #     simulations, the indices of simulations that are registered will be 2 *
  #     sim - 1 and 2 * sim.
  #
  # Returns:
  #   A ROASAnalysisFit object, a data frame with one or more rows.
  #
  # Notes:
  #   This is an internal function, called from another function. No integrity
  #   checking of the parameters is done; this must be done by the enclosing
  #   function.

  kClassName <- "ROASAnalysisFit"

  i.max <- GetInfo(obj, "i.max")
  if (randomize.i) {
    i <- sample.int(i.max, size=1)
  } else {
    i <- sim
  }
  if (swap.and.redo) {
    sim <- (2L * sim - 1L)
  }

  obj.ged <- SimulateGeoExperimentData(obj, i=i)
  SetSpendChange(obj.ged, prop.to=prop.to) <- cost

  .AnalyzeOnce <- function(model, obj.ged, sim) {
    # Need only n.sims == 1 since 'cost' is a fixed quantity.
    fit <- DoROASAnalysis(obj.ged, model=model,
                          response=response, cost=kSpendChange,
                          pretest.period=0, intervention.period=1,
                          cooldown.period=2, control.group=1, treatment.group=2,
                          n.sims=1)
    geo.assignment <- GetInfo(obj.ged, "geo.assignment")
    summ <- .GetPreanalysisSummary(fit, i=i, geo.assignment=geo.assignment,
                                   sim=sim)
    return(summ)
  }

  result.list <- lapply(models, FUN=.AnalyzeOnce, obj.ged=obj.ged, sim=sim)

  if (swap.and.redo) {
    # Swap the group numbers and redo the analysis. This works ONLY when
    # n.groups==2. This must be checked in the calling routine.
    MapGeoGroups(obj.ged) <- (2:1)
    SetSpendChange(obj.ged, prop.to=prop.to) <- cost
    more.results <- lapply(models, FUN=.AnalyzeOnce,
                           obj.ged=obj.ged, sim=sim + 1L)
    result.list <- c(result.list, more.results)
  }
  obj.result <- do.call(rbind, args=result.list)
  rownames(obj.result) <- NULL
  class(obj.result) <- c(kClassName, "data.frame")
  return(obj.result)
}

.ROASPreanalysisFitRow <- function(sim, model, estimate, sd, df, i,
                                   geo.assignment) {
  # [internal] Constructs a ROASPreanalysisFit-compatible object with one
  # single row. To be bound together row-wise with other such objects.
  #
  # Args:
  #   sim: (integer >= 0) number of the simulation.
  #   model: (string) model id.
  #   estimate: (real number) point estimate.
  #   sd: (real number > 0) posterior s.d.
  #   df: (integer > 0) degrees of freedom of the t distribution that is the
  #     posterior of the iROAS estimate.
  #   i: (integer >= 1) index of the data set.
  #   geo.assignment: (a GeoAssignment object) the actual geo assignment used.
  #
  # Returns:
  #   A ROASPreanalysisFitRow object.

  obj.result <- data.frame(sim=sim, model=model, estimate=estimate, sd=sd,
                       df=df, i=i)
  obj.result[["geo.assignment"]] <- list(geo.assignment)
  rownames(obj.result) <- NULL
  class(obj.result) <- c("ROASAnalysisFitRow", class(obj.result))
  return(obj.result)
}

.GetPreanalysisSummary <- function(obj, i, geo.assignment, sim) {
  # [internal] Derives a short summary of the quantities for the preanalysis.
  #
  # Args:
  #   obj: some object.
  #   ...: arguments passed on to the models.
  #
  # Returns:
  #   A ROASAnalysisFit object.

  UseMethod(".GetPreanalysisSummary")
}

.GetPreanalysisSummary.TBRROASAnalysisFit <- function(obj, i, geo.assignment,
                                                      sim) {
  # [internal] Derives a short summary of the quantities of a TBR ROAS analysis
  # for the preanalysis.
  #
  # Args:
  #   obj: a TBRROASAnalysisFit object.
  #   i: (integer) index of the data set.
  #   geo.assignment: (a GeoAssignment object) the actual geo assignment of the
  #     data set.
  #   sim: (integer >= 0) number of the simulation.
  #
  # Returns:
  #   A ROASAnalysisFit object.
  #
  # Notes:
  #   No integrity checking of the arguments is done.

  info <- GetInfo(obj)
  summ <- summary(obj)
  tbr <- info[["tbr.resp"]]
  # The cost is always a constant, hence obj$cost is a vector.
  incr.estimate <- summ[["estimate"]]
  incr.cost <- summ[["incr.cost"]]
  df.residual <- GetInfo(tbr, "fit")[["df.residual"]]
  sd.estimate <- (summary(tbr)[["se"]] / incr.cost)
  model <- info[["model"]]
  obj.result <- .ROASPreanalysisFitRow(sim=sim, model=model,
                                       estimate=incr.estimate,
                                       sd=sd.estimate, df=df.residual, i=i,
                                       geo.assignment=geo.assignment)
  return(obj.result)
}

.GetPreanalysisSummary.GBRROASAnalysisFitGbr1 <- function(obj, i,
                                                          geo.assignment,
                                                          sim) {
  # [internal] Derives a short summary of the quantities of a GBR ROAS analysis
  # for the preanalysis.
  #
  # Args:
  #   obj: a GBRROASAnalysisFit object.
  #   i: (integer) index of the data set.
  #   geo.assignment: (a GeoAssignment object) the actual geo assignment of the
  #     data set.
  #   sim: (integer >= 0) number of the simulation.
  #
  # Returns:
  #   A ROASPreanalysisFit object.
  #
  # Notes:
  #   No integrity checking of the arguments is done.

  fit <- obj[["lmfit"]]
  summ <- summary(fit)
  coeff.fit <- coef(fit)
  incr.estimate <- coeff.fit["incr.cost"]
  if (is.na(incr.estimate)) {
    sd.estimate <- NA
  } else {
    sd.estimate <- coef(summ)["incr.cost", "Std. Error"]
  }
  df.residual <- summ[["df"]][2]
  obj.result <- .ROASPreanalysisFitRow(sim=sim, model=obj[["model"]],
                                       estimate=incr.estimate,
                                       sd=sd.estimate, df=df.residual, i=i,
                                       geo.assignment=geo.assignment)
  return(obj.result)
}

print.ROASPreanalysisFit <- function(x, ...) {
  # Prints a summary of the ROASPreanalysisFit method.
  #
  # Args:
  #   x: a ROASPreanalysisFit object.
  #   ...: arguments passed to the 'summary' method.
  #
  # Returns:
  #   Prints the summary of 'x' and returns the summary of 'x' invisibly.

  summ <- summary(x, ...)
  print(summ)
}
