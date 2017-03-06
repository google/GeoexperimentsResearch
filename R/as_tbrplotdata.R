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

as.TBRPlotData <- function(obj, panels,
                           periods,
                           quantiles=c(0.1, 0.9), ...) {
  # Coerces an object to a TBRPlotData object.
  #
  # Args:
  #   obj: an object to coerce.
  #   panels: (vector of strings or NULL) names of the panels to be
  #     plotted. By default, all available panels.
  #   periods: (vector of strings or NULL) names of the periods to show. By
  #     default, all default periods.
  #   quantiles: (real vector of length 2) lower and upper quantiles of the
  #     credible interval to show.
  #   ...: further arguments passed to the methods.
  #
  # Returns:
  #   A TBRPlotData object.

  assert_that(is.numeric(quantiles),
              length(quantiles) == 2,
              !anyNA(quantiles),
              quantiles[1] >= 0,
              quantiles[2] <= 1,
              quantiles[1] < quantiles[2])

  UseMethod("as.TBRPlotData")
}

as.TBRPlotData.TBRAnalysisFitTbr1 <- function(obj, panels=GetTBRPlotPanelNames(),
                                              periods=c("pretest", "prediction"),
                                              quantiles=c(0.1, 0.9), ...) {
  # Coerces a TBRAnalysisFitTbr1 object to a TBRPlotData object.
  #
  # Args:
  #   obj: an object to coerce.
  #   panels: (vector of strings) names of the panels to be plotted.
  #   periods: (vector of strings) names of the periods to show.
  #   quantiles: (real vector of length 2) lower and upper quantiles of the
  #     credible interval to show.
  #   ...: ignored.
  #
  # Returns:
  #   A TBRPlotData object.

  kClassName <- "TBRPlotData"
  SetMessageContextString("as.TBRPlotData.TBRAnalysisFitTbr1")
  on.exit(SetMessageContextString())

  obj.result <- .GetTBRDataFrameForGgplot(obj, panels=panels,
                                          lower=quantiles[1],
                                          upper=quantiles[2],
                                          periods=periods,
                                          panel.info=kTBRPlotPanels)

  class(obj.result) <- c(kClassName, class(obj.result))
  return(obj.result)
}


as.TBRPlotData.TBRROASAnalysisFit <- function(obj, panels="cumulative",
                                              periods="prediction",
                                              quantiles=c(0.1, 0.9), ...) {
  # Coerces a TBRROASAnalysisFit object to a TBRPlotData object.
  #
  # Args:
  #   obj: an object to coerce.
  #   panels: (vector of strings) names of the panels to be plotted.
  #   periods: (vector of strings) names of the periods to show.
  #   quantiles: (real vector of length 2) lower and upper quantiles of the
  #     credible interval to show.
  #   ...: ignored.
  #
  # Returns:
  #   A TBRPlotData object.

  kClassName <- "TBRPlotData"
  SetMessageContextString("as.TBRPlotData.TBRAnalysisFitTbr1")
  on.exit(SetMessageContextString())

  obj.result <- .GetTBRDataFrameForGgplot(obj, panels=panels,
                                          lower=quantiles[1],
                                          upper=quantiles[2],
                                          periods=periods,
                                          panel.info=kTBRROASPlotPanels)

  class(obj.result) <- c(kClassName, class(obj.result))
  return(obj.result)
}

.GetTBRDataFrameForGgplot <- function(obj, panels, periods, lower, upper,
                                      panel.info) {
  # [internal] Arranges a TBRPlotData or TBRROASPlotData object into a data
  # frame for plotting.
  #
  # Args:
  #   obj: a TBRPlotData or TBRROASPlotData object.
  #   panels: names of the panels to plot. Available names are
  #     those in \code{(kTBRPlotPanels)}.
  #   periods: (vector of strings) names of the periods to show.
  #   lower: (0 < number < upper) lower quantile.
  #   upper: (lower < number < 1) upper quantile.
  #   panel.info: (list) information about the data frame columns for each of
  #     the available panels.
  #
  # Returns:
  #   A data frame with the columns:
  #   \itemize{
  #      \item\code{date} date.
  #      \item\code{observed} an observed time series.
  #      \item\code{predicted} a predicted time series.
  #      \item\code{lower} lower bound of the predicted time series.
  #      \item\code{upper} lower bound of the predicted time series.
  #      \item\code{panel.label} panel label to be shown in the plot.}

  assert_that(is.vector.of.nonempty.strings(panels))
  there <- structure(panels %in% names(panel.info),
                     names=panels)
  assert_that(all(there),
              msg=Message(FormatText(!there,
                  "Unknown panel name{|s}: $X")))
  assert_that(is.vector.of.nonempty.strings(periods))
  there <- structure(periods %in% names(kStandardPeriods),
                     names=periods)
  assert_that(all(there),
              msg=Message(FormatText(!there,
                  "Unknown period name{|s}: $X")))

  list.df <- lapply(panels, FUN=.GetOneTBRDataFrameForGgplot, obj=obj,
                    lower=lower, upper=upper, panel.info=panel.info)
  plot.data <- do.call(rbind, args=list.df)

  panel.labels <- sapply(panel.info, "[[", kPanelLabel)
  plot.data[[kPanelLabel]] <- factor(plot.data[[kPanelLabel]],
                                     levels=panel.labels)

  in.periods <- IsInPeriod.TBRAnalysisData(plot.data, periods=periods)
  plot.data <- plot.data[in.periods, , drop=FALSE]

  return(plot.data)
}

.GetOneTBRDataFrameForGgplot <- function(panel, obj, lower, upper, panel.info) {
  # [internal] Arranges a TBRPlotData or TBRROASPlotData object into a data
  # frame for plotting.
  #
  # Args:
  #   panel: name of the panel to plot. Must be one of those in
  #     \code{Get(kTBRPlotPanels)}.
  #   obj: a TBRPlotData or TBRROASPlotData object.
  #   lower: (0 < number < upper) lower quantile.
  #   upper: (lower < number < 1) upper quantile.
  #   panel.info: (list) information about the data frame columns for each of
  #     the available panels.
  #
  # Returns:
  #   A data frame with the columns:
  #   \itemize{
  #      \item\code{date} date.
  #      \item\code{observed} an observed time series.
  #      \item\code{predicted} a predicted time series.
  #      \item\code{lower} lower bound of the predicted time series.
  #      \item\code{upper} lower bound of the predicted time series.
  #      \item\code{panel.label} panel label to be shown in the plot.}

  probs <- c("lower"=unname(lower), "predicted"=0.5, "upper"=unname(upper))
  df.panel <- quantile(obj, probs=probs, distribution=panel)
  # The columns 3, 4, 5 contain the lower, middle, and upper quantiles.
  names(df.panel)[3:5] <- names(probs)
  column.map <- panel.info[[panel]]
  old.column.name <- column.map[["observed"]]
  if (is.na(old.column.name)) {
    observed <- NA_real_
  } else {
    observed <- obj[[old.column.name]]
  }
  df.panel[["observed"]] <- observed
  df.panel[[kPanelLabel]] <- column.map[[kPanelLabel]]
  return(df.panel)
}
