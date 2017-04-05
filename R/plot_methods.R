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

#' Generate a (gg)plot of a Geos object.
#'
#' @param x a Geos object.
#' @param y (string) name of the metric to plot. If omitted, plots the volume
#'   defined in the object by default.
#' @param geom (string) short name of the geom ggplot2 method to be used.
#' @param sort.by (string) name of the column to sort the plot by, defaults to
#'   y. If the column contains string, alphabetical order will be used
#'   unless every string can be coerced to an integer in which case
#'   numerical (ascending) order will be used. If the column contains
#'   numerical values, numerical (descending) order will be used.
#' @param log.scale (flag) plot y-axis on log scale?
#' @param ... ignored.
#'
#' @return A ggplot object.
#'
#' @method plot Geos

plot.Geos <- function(x, y=NULL, geom=c("bar", "point", "rect"),
                      sort.by=y, log.scale=FALSE, ...) {
  SetMessageContextString("plot.Geos")
  on.exit(SetMessageContextString())

  if (is.null(y)) {
    y <- kVolume
  }

  geom <- match.arg(geom)

  assert_that(is.string(y),
              y %in% names(x),
              msg=Message("'y' must be the name of a column in the data frame"))
  assert_that(is.numeric(x[[y]]),
              msg=Message("'y' must be the name of numeric column in",
                          " the data frame"))
  assert_that(is.string(sort.by))
  assert_that(is.flag(log.scale))

  x <- x[, c(kGeo, y)]
  # Order geos alphabetically, for colors.
  if (suppressWarnings(anyNA(as.numeric(x[[kGeo]])))) {
    n <- order(x[[kGeo]]) # At least one non-numeric, use alphabetical order.
  } else {
    n <- order(as.numeric(x[[kGeo]])) # All numeric, use numeric order.
  }
  if (sort.by == kGeo) {
    o <- n
  } else {
    o <- order(-x[[y]])
  }
  x[["col"]] <- factor(x[[kGeo]], levels = x[[kGeo]][n])
  x[[kGeo]] <- factor(x[[kGeo]], levels = x[[kGeo]][o])
  x[["ymax"]] <- cumsum(x[[y]]) / sum(x[[y]])
  x[["ymin"]] <- c(0, head(x[["ymax"]], n=-1))

  g <- ggplot(data=x)
  if (geom == "bar") {
      g <- g + geom_bar(aes_string(kGeo, weight=y, fill="col")) +
          guides(fill=guide_legend(title=kGeo))
  } else if (geom == "point") {
      g <- g + geom_point(aes_string(x=kGeo, y=y, col="col")) +
          guides(col=guide_legend(title=kGeo))
  } else if (geom == "rect") {
      g <- g + geom_rect(aes_string(ymax="ymax", ymin="ymin",
                                    xmax=4, xmin=3, fill="col")) +
          coord_polar(theta="y") + xlim(c(0, 4)) +
          theme(panel.grid=element_blank()) +
          theme(axis.text=element_blank()) +
          theme(axis.ticks=element_blank())
  }

  if (log.scale) {
    g <- g + scale_y_log10()
  }
  g <- g + theme_classic() + ylab(y)
  return(g)
}


#' Generate a (gg)plot of a GeoTimeseries object.
#'
#' @param x a GeoTimeseries object.
#' @param y (string) name of the metric to plot. If omitted, plots the first
#'   metric in the object by default.
#' @param by (string) group by which column? Default is 'geo'.
#' @param subset (vector) subset of the items in column 'by' to use.
#' @param aggregate (flag) aggregate over?
#' @param title (string) a title string. By default, the name of the metric to
#'   plot.
#' @param log.scale (flag) plot y-axis on log scale?
#' @param legend (flag) plot the legend?
#' @param ... ignored.
#'
#' @return A ggplot object.
#'
#' @note
#' Sets the lower y-axis limit to zero if all 'y' values are nonnegative
#' and log.scale=\code{FALSE}.
#'
#' @method plot GeoTimeseries

plot.GeoTimeseries <- function(x, y=NULL, by=NULL, subset=NULL, aggregate=FALSE,
                               title=y, log.scale=FALSE, legend=TRUE, ...) {
  SetMessageContextString("plot.GeoTimeseries")
  on.exit(SetMessageContextString())

  metrics <- GetInfo(x, "metrics")
  if (is.null(y)) {
    y <- metrics[1L]
  }
  if (is.null(by)) {
    by <- kGeo
  }
  assert_that(is.string(by),
              by %in% names(x),
              msg="'by' must be the name of a column in the data frame")
  assert_that(is.flag(aggregate))
  assert_that(is.null(title) || is.string(title))
  assert_that(is.flag(log.scale))
  assert_that(is.flag(legend))

  u <- unique(x[[by]])
  if (suppressWarnings(anyNA(as.numeric(u)))) {
    o <- order(u) # Alphabetical order is default.
  } else {
    o <- order(as.numeric(u)) # Numerical order when possible.
  }
  if (!is.null(subset)) {
    assert_that(is.vector(subset))
    x <- Subset(x, rows=x[[by]] %in% subset)
  }
  x[[by]] <- factor(x[[by]], levels = u[o])
  assert_that(is.vector.of.nonempty.strings(y),
              length(y) == 1)
  if (!(kGeo %in% by)) {
    if (aggregate) {
      x <- aggregate(x, by=kDate)
    } else {
      x <- aggregate(x, by=c(kDate, by))
    }
  } else if (aggregate) {
    x <- aggregate(x, by=kDate)
  }
  g <- ggplot(data=x) + aes_string(x=kDate, y=y)
  if (!aggregate) {
    g <- g + aes_string(group=by, col=by) + guides(col=guide_legend(by))
  }
  if (log.scale) {
    g <- g + scale_y_log10()
  } else {
    if (all(g[["data"]][y] >= 0)) {
      g <- g + expand_limits(y=0)
    }
  }
  if (length(title) == 0) {
    title <- ""
  }
  g <- g + geom_line() +
      geom_point() +
          theme_classic() +
              ggtitle(title)
  if (!legend) {
    g <- g + theme(legend.position="none")
  }
  return(g)
}
