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

# R package GeoexperimentsResearch: set-up file for unit tests.
#
# Functions to build data sets that are compatible with the GeoTimeseries
# and GeoExperimentData classes.

kNamespace <- "GeoexperimentsResearch"

if (!exists("DEBUG", .GlobalEnv)) {
  # The 'DEBUG' flag controls whether the non-exported objects should
  # be temporarily copied to .GlobalEnv during testing. Used only for
  # debugging / developing purposes in interactive mode and does not
  # have effect when the actual tests are run.
  DEBUG <- interactive()
}

SEED <- (20151124L)
kDateFrom <- "2015-01-01"
options(stringsAsFactors=FALSE)

.ImportFromNamespace <- function(regex, namespace=kNamespace,
                                 into.envir=.GlobalEnv) {
  # Copy an object from the namespace into an environment.
  #
  # Args:
  #   regex: (string) regular expression to match names in the environment.
  #   namespace: (string) name of the namespace.
  #   into.envir: (environment) environment into which to copy the objects.
  #
  # Returns:
  #   NULL, invisibly.

  stopifnot(is.string(regex))
  stopifnot(is.string(namespace))
  stopifnot(is.environment(into.envir))
  ns <- asNamespace(namespace)
  all.names <- names(as.list(ns, all.names=TRUE))
  match <- grepl(regex, x=all.names)
  stopifnot(any(match))
  match.names <- all.names[match]
  for (name in match.names) {
    value <- get(name, envir=ns)
    assign(name, value=value, envir=into.envir)
    if (interactive()) {
      cat("Importing ", name, "\n", sep="")
    }
  }
  invisible(NULL)
}

if (!DEBUG) {
  # The constants, utilities and helper functions are not exported and
  # thus are not visible during the tests without explicit reference
  # to the namespace (e.g., <package name>:::FormatText). To avoid
  # this verbose style, these objects are exported into .GlobalEnv so
  # that the tests can be run without explicit references to the
  # namespace.
  #
  # For debugging and development purposes, the user can avoid this
  # temporarily in the interactive mode by assigning 'DEBUG <- TRUE'
  # and sourcing these functions directly.

  .ImportFromNamespace("^k")
  .ImportFromNamespace("^is\\.")
  .ImportFromNamespace("^Check[A-Z]")
  .ImportFromNamespace("[GS]etInfo")
  .ImportFromNamespace("[GS]etMessageContextString")
  .ImportFromNamespace("Messagef?")
  .ImportFromNamespace("ConcatItems")
  .ImportFromNamespace("FormatText")
  .ImportFromNamespace("ComputeLinearModelWeights")
  .ImportFromNamespace("^\\.GetWeek")
  .ImportFromNamespace("^\\.GetLast")
}

.SimulateGeoTimeseries <- function(n.geos, n.days, cv=0.2, date.from=kDateFrom,
                                   daily.volume=1e4, date.col=kDate,
                                   geo.col=kGeo, metric.col="sales", seed=1L) {
  # Simulates geo experiment data.
  #
  # Args:
  #   n.geos: (count) number of geos.
  #   n.days: (count) number of days.
  #   cv: (scalar) daily coefficient of variation for the geo time series.
  #   date.from: (Date or 'YYYY-MM-DD') first date of the time series.
  #   daily.volume: (number) total average daily volume.
  #   date.col: (string) date column name.
  #   geo.col: (string) geo column name.
  #   metric.col: (string) metric column name.
  #   seed: (integer) random seed.
  #
  # Returns:
  #   A data frame with columns date.col. geo.col, and "sales" which is the
  #   metric.
  date <- as.Date(date.from) + (seq_len(n.days) - 1L)
  dfg <- expand.grid(x=as.character(date), y=1:n.geos)
  names(dfg) <- c(date.col, geo.col)
  geo.baseline <- rev(sort(rlnorm(n.geos)))
  geo.baseline <- (daily.volume * (geo.baseline / sum(geo.baseline)))
  df.bl <- data.frame(y=1:n.geos, z=geo.baseline)
  names(df.bl) <- c(geo.col, metric.col)
  dfx <- merge(dfg, df.bl, by=geo.col)
  set.seed(seed)
  metric <- rnorm(nrow(dfx), mean=dfx$sales, sd=cv * dfx$sales)
  dfx[[date.col]] <- as.Date(dfx[[date.col]])
  dfx[[geo.col]] <- as.character(dfx[[geo.col]])
  dfx[[metric.col]] <- metric
  dfx <- dfx[order(dfx$date, dfx$geo), ]
  rownames(dfx) <- NULL
  return(dfx)
}

.MakeGeoExperimentData <- function(dfx, geo.groups=list(),
                                   experiment.dates=list()) {
  # Builds a data frame with Geo experiment data.
  #
  # Args:
  #   dfx: (data frame) data frame conforming to GeoTimeseries format.
  #   geo.groups: (named list) list of 'control' and 'test' geo groups.
  #   experiment.dates: (date or 'YYYY-MM-DD') start of pre-test,
  #     start of test, start of cooldown, end of test.
  #
  # Returns:
  #   A data frame with columns period, geo.group added to the input
  #   data frame.
  dfx[[kGeoGroup]] <- NA_integer_
  geo.group <- dfx[[kGeoGroup]]
  for (i in seq_along(geo.groups)) {
    geo.group.id <- i
    group <- geo.groups[[i]]
    geo.group[dfx[[kGeo]] %in% group] <- geo.group.id
  }
  dfx[[kGeoGroup]] <- geo.group
  #
  dfx[[kPeriod]] <- NA_integer_
  period <- dfx[[kPeriod]]
  for (i in seq_along(experiment.dates)) {
    period.id <- (i - 1L)
    x <- experiment.dates[[i]]
    if (inherits(x, "Date")) {
      date <- as.Date(x)
    } else {
      date <- (date + x)
    }
    if (i < length(experiment.dates)) {
      period[dfx[[kDate]] >= date] <- period.id
    } else {
      period[dfx[[kDate]] > date] <- NA_integer_
    }
  }
  dfx[[kPeriod]] <- period
  return(dfx)
}

.AddSpendAndROAS <- function(x, iroas=4.0, ad.spend.diff.daily,
                             ad.spend.daily) {
  # Add ad spend and an effect to 'sales' based on the given ad spend
  # difference during test.
  #
  # Args:
  #   x: a GeoExperimentData compatible data frame with the metric
  #     'cost.clicks'.
  #  iroas: (scalar) incremental ROAS.
  #  ad.spend.daily: (number) average ad spend difference per day (across geos).
  #  ad.spend.diff.daily: (number) average ad spend difference during the test.
  #
  # Returns:
  #   A GeoExperimentData compatbile data frame with the
  #   adjusted cost and response, with the incremental
  #   change in reponse during the test period.

  # Calculate the total ad spend baseline.
  n.all.days <- (1L + as.integer(diff(range(x[[kDate]]))))
  total.ad.spend <- (n.all.days * ad.spend.daily)
  # Calculate the total ad spend difference for the duration of the test.
  is.test <- (x[["assignment"]] %in% kTreatmentAssignment["change"])
  n.test.days <- (1L + as.integer(diff(range(x[[kDate]][is.test]))))
  total.ad.spend.diff <- (n.test.days * ad.spend.diff.daily)
  # 'sales' == size of the geo. Larger geos have more clicks and
  # consequently larger cost of clicks.
  sales <- x[["sales"]]
  # Set baseline ad spend, proportional of the 'size' of each geo.
  weight <- (sales / sum(sales))
  cost.clicks.baseline <- round(weight * total.ad.spend, 2)
  # Set incremental ad spend.
  weight.test <- ((is.test * sales) / sum(is.test * sales))
  incr.cost.clicks <- (total.ad.spend.diff * weight.test)
  x[["cost.clicks"]] <- (cost.clicks.baseline + incr.cost.clicks)
  x[["sales"]] <- (sales + iroas * incr.cost.clicks)
  x[["bl.cost.clicks"]] <- cost.clicks.baseline
  x[["incr.cost.clicks"]] <- incr.cost.clicks
  return(x)
}

.MakeGBRDataObject <- function(dfged, iroas, ad.spend.diff.daily,
                               ad.spend.daily=0) {
  # Make a GBRROASAnalysisData object, with an effect.
  #
  # Args:
  #   dfged: a data frame compatible with GeoExperimentData.
  #   iroas: (scalar) incremental ROAS.
  #   ad.spend.daily: (number) baseline average ad spend daily.
  #   ad.spend.diff.daily: (number) average ad spend difference during the test.
  #
  # Returns:
  #   A GBRROASAnalysisData object.

  # By default, obj.gbr has all zeros in the pre-period cost.clicks.
  dfged0 <- .AddSpendAndROAS(dfged, iroas=iroas,
                             ad.spend.diff.daily=ad.spend.diff.daily,
                             ad.spend.daily=ad.spend.daily)
  obj.gts <- GeoTimeseries(dfged0, metrics=c("sales", "cost.clicks"))
  obj.ged <- GeoExperimentData(obj.gts)
  obj.gbr <- as.GBRROASAnalysisData(obj.ged, response="sales",
                                cost="cost.clicks")
  return(obj.gbr)
}
