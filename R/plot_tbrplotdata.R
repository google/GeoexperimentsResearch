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

plot.TBRPlotData <- function(x, y, highlight.weeks=TRUE, date.format="%Y-%m-%d",
                             ...) {
  # Creates a graph of the TBRPlotData object.
  #
  # Args:
  #   x: a TBRPlotData object.
  #   y: ignored.
  #   highlight.weeks: (flag), alternate background shading to highlight weeks?
  #   date.format: (string) date format.
  #   ...: ignored.
  #
  # Returns:
  #   A ggplot2 object.
  #
  # Documentation:
  #   method plot TBRPlotData

  SetMessageContextString("plot.TBRPlotData")
  on.exit(SetMessageContextString())

  assert_that(is.flag(highlight.weeks) && !is.na(highlight.weeks))
  assert_that(is.nonempty.string(date.format))

  obj <- x
  plot.data <- as.data.frame(obj)

  in.pretest <- IsInPeriod(obj, periods="pretest")
  in.intervention <- IsInPeriod(obj, periods="intervention")
  in.cooldown <- IsInPeriod(obj, periods="cooldown")
  in.analysis <- IsInPeriod(obj, periods="analysis")
  in.posttest <- IsInPeriod(obj, periods="posttest")

  # If no dates in a period, the dates will be set to NA.
  pretest.start <- head(obj[[kDate]][in.pretest], n=1)[1]
  intervention.start <- head(obj[[kDate]][in.intervention], n=1)[1]
  cooldown.start <- head(obj[[kDate]][in.cooldown], n=1)[1]
  test.end <- tail(obj[[kDate]][in.analysis], n=1)[1]
  posttest.end <- tail(obj[[kDate]][in.posttest], n=1)[1]

  gg <- ggplot()

  if (highlight.weeks) {
    rect <- .GetWeeklyShadedBackgroundDataFrameForGgplot(plot.data)
    gg <- gg + geom_rect(data=rect, aes(xmin=xmin, xmax=xmax,
                                        ymin=ymin, ymax=ymax),
                         fill="grey50", alpha=0.05)
  }

  # No y-axis label.
  gg <- gg + ylab("")

  # Horizontal line at zero.
  gg <- gg + geom_hline(data=plot.data,
                        aes(yintercept=0), linetype="solid", color="darkgrey")

  # Intervals.
  gg <- gg + geom_ribbon(data=plot.data,
                         aes(x=date, ymin=lower, ymax=upper), alpha=0.6,
                         fill="lightblue")

  # Predicted values.
  gg <- gg + geom_line(data=plot.data,
                       aes(x=date, y=predicted, colour="Predicted",
                           linetype="Predicted"))

  # Observed variable does not necessarily exist.
  if (!all(is.na(plot.data[["observed"]]))) {
    gg <- gg + geom_line(data=plot.data,
                         aes(x=date, y=observed, colour="Observed",
                             linetype="Observed"))
  }

  gg <- gg + scale_color_manual("",
                                values=c(Predicted="darkblue",
                                    Observed="darkred"))

  gg <- gg + scale_linetype_manual("",
                                   values=c(Predicted="solid",
                                       Observed="solid"))

  n.panels <- length(unique(plot.data[[kPanelLabel]]))

  if (n.panels >= 2) {
    gg <- gg + facet_grid(panel.label ~ ., scales="free_y")
  }

  # Vertical period separators.
  gg <- gg + geom_vline(data=plot.data,
                        aes(xintercept=as.numeric(pretest.start) - 0.5),
                        linetype="dotted")
  gg <- gg + geom_vline(data=plot.data,
                        aes(xintercept=as.numeric(intervention.start) - 0.5),
                        linetype="dashed")
  if (any(in.cooldown)) {
    gg <- gg + geom_vline(data=plot.data,
                          aes(xintercept=as.numeric(cooldown.start) - 0.5),
                          linetype="dashed")
  }
  gg <- gg + geom_vline(data=plot.data,
                        aes(xintercept=as.numeric(test.end) + 0.5),
                        linetype="dotted")

  # Format dates on the x-axis.
  date.breaks <- unique(c(pretest.start, intervention.start, cooldown.start,
                          test.end, posttest.end))
  gg <- gg + scale_x_date(date_labels=date.format, breaks=date.breaks)

  # Default theme.
  gg <- gg + theme_bw()

  # Rotate the dates on the x-axis.
  gg <- gg + theme(axis.text.x=element_text(angle=90, hjust=1))

  return(gg)
}

.GetWeeklyShadedBackgroundDataFrameForGgplot <- function(plot.data) {
  # [internal] Create a data.frame to plot a weekly shaded background to
  # highlight weekly patterns.
  #
  # Args:
  #   plot.data: the output of .GetTBRDataFrameForGgplot.
  #
  # Returns:
  #   A data.frame with the columns:
  #   \itemize{
  #      \item\code{xmin}.
  #      \item\code{panel.label}.
  #      \item\code{ymin}.
  #      \item\code{ymax}.
  #      \item\code{xmax}.}

  first.day <- min(plot.data[[kDate]])
  if (.GetWeekdays(first.day) == 1) {
    first.monday <- first.day - 0.5
  } else {
    first.monday <- first.day + (8 - .GetWeekdays(first.day)) - 0.5
  }
  rect <- expand.grid(xmin=seq(first.monday, max(plot.data[[kDate]]), by=14),
                      panel.label=unique(plot.data[["panel.label"]]))

  rect[["xmax"]] <- rect[["xmin"]] + 7
  for(i in seq.int(nrow(rect))) {
    rows <- plot.data[["panel.label"]] == rect[[i, "panel.label"]]
    cols <- !(names(plot.data) %in% c(kDate, "panel.label"))
    rect[["ymin"]][i] <- min(0, min(plot.data[rows, cols], na.rm=TRUE))
    rect[["ymax"]][i] <- max(0, max(plot.data[rows, cols], na.rm=TRUE))
  }
  return(rect)
}


plot.TBRAnalysisFitTbr1 <- function(x, y,
                                    panels=GetTBRPlotPanelNames(),
                                    periods=c("pretest", "prediction"),
                                    quantiles=c(0.1, 0.9),
                                    highlight.weeks=TRUE,
                                    date.format="%Y-%m-%d", ...) {

  # Creates a graph of the TBR analysis fit.
  #
  # Args:
  #   x: a TBRAnalysisFitTbr1 object.
  #   y: ignored.
  #   quantiles: (real vector of length 2) lower and upper quantiles of the
  #     credible interval to show.
  #   panels: (vector of strings) names of the panels to be plotted.
  #   periods: (vector of strings) names of the periods to show.
  #   highlight.weeks: (flag), alternate background shading to highlight weeks?
  #   date.format: (string) date format.
  #   ...: further arguments passed to methods 'as.TBRPlotData' and
  #     'plot.TBRPlotData'.
  #
  # Returns:
  #   A ggplot2 object.
  #
  # Documentation:
  #   method plot TBRAnalysisFitTbr1

  SetMessageContextString("plot.TBRPlotData")
  on.exit(SetMessageContextString())

  obj.plot <- as.TBRPlotData(x, panels=panels, periods=periods,
                             quantiles=quantiles, ...)
  gg <- plot(obj.plot,
             highlight.weeks=highlight.weeks, date.format=date.format, ...)
  return(gg)
}

plot.TBRROASAnalysisFit <- function(x, y,
                                    panels="cumulative",
                                    periods="prediction",
                                    quantiles=c(0.1, 0.9),
                                    highlight.weeks=TRUE,
                                    date.format="%Y-%m-%d", ...) {
  # Creates a graph of the TBR ROAS analysis fit.
  #
  # Args:
  #   x: a TBRROASAnalysisFit object.
  #   y: ignored.
  #   panels: (vector of strings) names of the panels to be plotted.
  #   periods: (vector of strings) names of the periods to show.
  #   quantiles: (real vector of length 2) lower and upper quantiles of the
  #     credible interval to show.
  #   highlight.weeks: (flag), alternate background shading to highlight weeks?
  #   date.format: (string) date format.
  #   ...: further arguments passed to methods 'as.TBRPlotData' and
  #     'plot.TBRPlotData'.
  #
  # Returns:
  #   A ggplot2 object.
  #
  # Documentation:
  #   method plot TBRROASAnalysisFit

  SetMessageContextString("plot.TBRPlotData")
  on.exit(SetMessageContextString())

  obj.plot <- as.TBRPlotData(x, panels=panels, periods=periods,
                             quantiles=quantiles, ...)
  gg <- plot(obj.plot,
             highlight.weeks=highlight.weeks, date.format=date.format, ...)
  return(gg)
}
