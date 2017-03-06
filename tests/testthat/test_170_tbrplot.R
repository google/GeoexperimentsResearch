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

context("plot.TBRPlotData")

data(salesandcost)
data(geoassignment)
obj.gts <- GeoTimeseries(salesandcost, metrics=c("sales", "cost"))
# Data starts from 2016-01-05: define one week of pre-experiment period.
obj.per <- ExperimentPeriods(c("2015-01-12", "2015-02-16", "2015-03-15"))
obj.ga <- GeoAssignment(geoassignment)
obj.ged <- GeoExperimentData(obj.gts, periods=obj.per, geo.assignment=obj.ga,
                             treat.assignment=DefaultTreatmentAssignment())
tbr.fit <- DoTBRAnalysis(obj.ged, model=kTBRModel1, response="sales",
                         pretest.period=0, intervention.period=1,
                         cooldown.period=NULL,
                         control.group=1, treatment.group=2)
q.default <- c(0.1, 0.9)
in.pretest <- IsInPeriod(tbr.fit, periods="pretest")
tbrpd <- as.TBRPlotData(tbr.fit)

all.tbr.plot.panel.names <- GetTBRPlotPanelNames()

test_that("the object is a ggplot object", {
  x <- plot(tbrpd)
  expect_is(x, "ggplot")
})

test_that("highlight.weeks must not be NA or non-logical", {
  expect_error(plot(tbrpd, highlight.weeks=NA))
  expect_error(plot(tbrpd, highlight.weeks=NULL))
  expect_error(plot(tbrpd, highlight.weeks=1))
})

test_that("the associated data object equals the TBRPlotData object", {
  expect_error(x <- plot(tbrpd, highlight.weeks=FALSE),
               regexp=NA)
  for(i in 3:5) {
    expect_equivalent(x[["layers"]][[i]][["data"]], tbrpd)
  }

  expect_error(x <- plot(tbrpd, highlight.weeks=TRUE),
               regexp=NA)
  for(i in 2:4) {
    expect_equivalent(x[["layers"]][[i]][["data"]], tbrpd)
  }
})

test_that("highlighting weeks adds another layer with corresponding data", {
  x <- plot(tbrpd, highlight.weeks=TRUE)
  expect_error(data <-  .GetWeeklyShadedBackgroundDataFrameForGgplot(tbrpd),
               regexp=NA)
  expect_equivalent(x[["layers"]][[1]][["data"]], data)
})

test_that("date.format must be a nonempty string", {
  expect_error(plot(tbrpd, date.format=NA))
  expect_error(plot(tbrpd, date.format=""))
  expect_error(plot(tbrpd, date.format=NULL))
  expect_error(plot(tbrpd, date.format=1))
  expect_error(plot(tbrpd, date.format=as.Date("2016-11-23")))
})


test_that("default date.format must be %Y-%m-%d", {
  default.date.format <- "%Y-%m-%d"
  x <- plot(tbrpd)
  expect_identical(as.list(x[["plot_env"]])[["date.format"]],
                   default.date.format)
})

test_that("changing date.format changes the object", {
  new.date.format <- "%b-d"
  x <- plot(tbrpd, date.format=new.date.format)
  expect_identical(as.list(x[["plot_env"]])[["date.format"]], new.date.format)
})


context("plot.TBRAnalysisFitTbr1")

test_that("the object equals plot(as.TBRPlotData(object))", {
  x <- plot(as.TBRPlotData(tbr.fit))
  y <- plot(tbr.fit)
  # Due to a problem in base::all.equal in R-3.2.2, we can't compare the
  # environments within ggplot objects.  So omit them.
  x[["plot_env"]] <- NULL
  y[["plot_env"]] <- NULL
  expect_equal(x, y)
})

test_that("argument 'quantiles' is passed to as.TBRPlotData", {
  x <- plot(as.TBRPlotData(tbr.fit, quantiles=c(0.05, 0.5)))
  y <- plot(tbr.fit, quantiles=c(0.05, 0.5))
  # Due to a problem in base::all.equal in R-3.2.2, we can't compare the
  # environments within ggplot objects.  So omit them.
  x[["plot_env"]] <- NULL
  y[["plot_env"]] <- NULL
  expect_equal(x, y)
})

test_that("argument 'panels' is passed to as.TBRPlotData", {
  for (panel in all.tbr.plot.panel.names) {
    x <- plot(as.TBRPlotData(tbr.fit, panels=panel))
    y <- plot(tbr.fit, panels=panel)
    # Due to a problem in base::all.equal in R-3.2.2, we can't compare the
    # environments within ggplot objects.  So omit them.
    x[["plot_env"]] <- NULL
    y[["plot_env"]] <- NULL
    expect_equal(x, y)
  }
})

test_that("argument 'periods' is passed to as.TBRPlotData", {
  x <- plot(as.TBRPlotData(tbr.fit, periods="analysis"))
  y <- plot(tbr.fit, periods="analysis")
  # Due to a problem in base::all.equal in R-3.2.2, we can't compare the
  # environments within ggplot objects.  So omit them.
  x[["plot_env"]] <- NULL
  y[["plot_env"]] <- NULL
  expect_equal(x, y)
})

test_that("no warnings should be issued if a period is empty", {
  tbrpd <- as.TBRPlotData(tbr.fit)
  # Remove the posttest period.
  tbrpd <- tbrpd[!IsInPeriod(tbrpd, periods="posttest"), , drop=FALSE]
  expect_warning(plot(tbrpd),
                 regexp=NA)
})

context("GetTBRPlotPanelNames")

test_that("the function returns the TBR plot panel names", {
  expect_identical(GetTBRPlotPanelNames(), names(kTBRPlotPanels))
})


context("plot.TBRROASAnalysisFit")

tbr.roas <- DoTBRROASAnalysis(obj.ged, model=kTBRModel1, response="sales",
                              cost="cost",
                              pretest.period=0, intervention.period=1,
                              cooldown.period=NULL,
                              control.group=1, treatment.group=2)

all.tbr.roas.plot.panel.names <- names(kTBRROASPlotPanels)

test_that("the object equals plot(as.TBRPlotData(object))", {
  x <- plot(as.TBRPlotData(tbr.roas))
  y <- plot(tbr.roas)
  # Due to a problem in base::all.equal in R-3.2.2, we can't compare the
  # environments within ggplot objects.  So omit them.
  x[["plot_env"]] <- NULL
  y[["plot_env"]] <- NULL
  expect_equal(x, y)
})

test_that("argument 'quantiles' is passed to as.TBRPlotData", {
  x <- plot(as.TBRPlotData(tbr.roas, quantiles=c(0.05, 0.5)))
  y <- plot(tbr.roas, quantiles=c(0.05, 0.5))
  # Due to a problem in base::all.equal in R-3.2.2, we can't compare the
  # environments within ggplot objects.  So omit them.
  x[["plot_env"]] <- NULL
  y[["plot_env"]] <- NULL
  expect_equal(x, y)
})

for (name in all.tbr.roas.plot.panel.names) {
  test_that(sprintf("panel '%s' can be plotted individually", name), {
    expect_error(plot(tbr.roas, panels=name), regexp=NA)
  })
}

test_that("argument 'panels' is passed to as.TBRPlotData", {
  for (panel in all.tbr.roas.plot.panel.names) {
    x <- plot(as.TBRPlotData(tbr.roas, panels=panel))
    y <- plot(tbr.roas, panels=panel)
    # Due to a problem in base::all.equal in R-3.2.2, we can't compare the
    # environments within ggplot objects.  So omit them.
    x[["plot_env"]] <- NULL
    y[["plot_env"]] <- NULL
    expect_equal(x, y)
  }
})

test_that("argument 'periods' is passed to as.TBRPlotData", {
  x <- plot(as.TBRPlotData(tbr.roas, periods="analysis"))
  y <- plot(tbr.roas, periods="analysis")
  # Due to a problem in base::all.equal in R-3.2.2, we can't compare the
  # environments within ggplot objects.  So omit them.
  x[["plot_env"]] <- NULL
  y[["plot_env"]] <- NULL
  expect_equal(x, y)
})

test_that("no warnings should be issued if a period is empty", {
  tbrpd <- as.TBRPlotData(tbr.roas)
  # Remove the posttest period.
  tbrpd.new <- tbrpd[!IsInPeriod(tbrpd, periods="posttest"), , drop=FALSE]
  expect_warning(plot(tbrpd.new),
                 regexp=NA)
})
