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

# Tests for ROAS preanalysis related functions.

# Create a dummy data set.
kSales <- "sales"
kCost <- "cost"
period.lengths <- c(35, 14, 7)
experiment.length <- sum(period.lengths)
n.days <- (2 * experiment.length)
n.geos <- 20L
dfx <- expand.grid(1:n.days, 1:n.geos)
names(dfx) <- c(kDate, kGeo)
first.date <- as.Date("2016-01-01")
dfx[[kDate]] <- (first.date + dfx[[kDate]] - 1L)
set.seed(1) # Near constant data.
dfx[[kSales]] <- rnorm(nrow(dfx), mean=as.numeric(dfx[[kGeo]]) * 100, sd=1)
dfx[[kCost]] <- 0.0
obj.gts <- GeoTimeseries(dfx, metrics=c(kSales, kCost))
df.ga <- data.frame(as.character(1:n.geos), 1L + (1:n.geos %% 2L))
names(df.ga) <- c(kGeo, kGeoGroup)
obj.ga <- GeoAssignment(df.ga)
# Two groups.
obj.gs <- ExtractGeoStrata(obj.gts, n.groups=2)
obj.gpd.fixed <- GeoExperimentPreanalysisData(obj.gts,
                                              period.lengths=period.lengths,
                                              geos=obj.ga,
                                              recycle=FALSE)
obj.gpd <- GeoExperimentPreanalysisData(obj.gts,
                                        period.lengths=period.lengths,
                                        geos=obj.gs,
                                        recycle=FALSE)

context(".ROASPreanalysisFitRow")

test_that("returned object is a data frame with one row", {
  args <- list(sim=1, model="x", estimate=0.1, sd=0.1, df=10, i=1,
               geo.assignment=obj.ga)
  expect_is(obj <- do.call(.ROASPreanalysisFitRow, args), "data.frame")
  expect_equal(1, nrow(obj))
  # The complex object should be intact.
  args[["geo.assignment"]] <- list(obj.ga)
  expect_identical(args, as.list(obj))
})

test_that("a NULL geo.assignment is possible", {
  expect_is(obj <- .ROASPreanalysisFitRow(sim=1, model="x",
                                          estimate=0.1, sd=0.1,
                                          df=10, i=1,
                                          geo.assignment=NULL),
            "data.frame")
  expect_identical(obj[["geo.assignment"]], list(NULL))
})


context(".GetPreanalysisSummary")

context(".GetPreanalysisSummary.GBRROASAnalysisFitGbr1")

test_that("GBR summary has the correct estimates", {
  obj.ged <- SimulateGeoExperimentData(obj.gpd.fixed, i=2)
  SetSpendChange(obj.ged, prop.to="sales") <- 1
  fit.gbr <- DoGBRROASAnalysis(obj.ged, response="sales", cost=kSpendChange,
                               pretest.period=0, intervention.period=1,
                               cooldown.period=2, control.group=1,
                               treatment.group=2)
  lmfit <- fit.gbr[["lmfit"]]
  summ <- .GetPreanalysisSummary(fit.gbr, i=1, geo.assignment=NULL, sim=1)
  expect_equal(summ[["estimate"]], coef(lmfit)[["incr.cost"]])
  expect_equal(summ[["sd"]], coef(summary(lmfit))["incr.cost", "Std. Error"])
  expect_equal(summ[["df"]], summary(lmfit)[["df"]][2])
})


context(".GetPreanalysisSummary.TBRROASAnalysisFitTbr1")

test_that("the object is a ROASAnalysisFitRow object", {
  obj.ged <- SimulateGeoExperimentData(obj.gpd.fixed, i=2)
  SetSpendChange(obj.ged, prop.to="sales") <- 2.0
  fit.tbr.roas <- DoTBRROASAnalysis(obj.ged, response="sales",
                                    model=kTBRModel1,
                                    cost=kSpendChange, pretest.period=0,
                                    intervention.period=1, cooldown.period=2,
                                    n.sims=1)
  summ <- .GetPreanalysisSummary(fit.tbr.roas, i=1, geo.assignment=NULL, sim=1)
  expect_is(summ, "ROASAnalysisFitRow")
})

test_that("TBR summary has the correct estimates", {
  obj.ged <- SimulateGeoExperimentData(obj.gpd.fixed, i=2)
  SetSpendChange(obj.ged, prop.to="sales") <- 2.0
  fit.tbr.roas <- DoTBRROASAnalysis(obj.ged, response="sales",
                                    model=kTBRModel1,
                                    cost=kSpendChange, pretest.period=0,
                                    intervention.period=1, cooldown.period=2,
                                    n.sims=1)
  summ <- .GetPreanalysisSummary(fit.tbr.roas, i=1, geo.assignment=NULL, sim=1)
  fit.tbr.resp <- GetInfo(fit.tbr.roas, "tbr.resp")
  summ.tbr <- summary(fit.tbr.resp)
  expect_equal(summ[["estimate"]], summ.tbr[["estimate"]] / 2.0)
  expect_equal(summ[["sd"]], summ.tbr[["se"]] / 2.0)
  lmfit <- GetInfo(fit.tbr.resp, "fit")
  expect_equal(summ[["df"]], summary(lmfit)[["df"]][2])
})


context(".SimulateROASAnalysis")

test_that("the returned object is as expected for model gbr1", {
  summ1 <- .SimulateROASAnalysis(sim=2, randomize.i=FALSE, obj=obj.gpd.fixed,
                                 response="sales", prop.to="sales", cost=1,
                                 models=kGBRModel1, swap.and.redo=FALSE)
  # Reconstruct the summary and compare.
  obj.ged <- SimulateGeoExperimentData(obj.gpd.fixed, i=2)
  SetSpendChange(obj.ged, prop.to="sales") <- 1
  fit <- DoGBRROASAnalysis(obj.ged, response="sales", cost=kSpendChange,
                           pretest.period=0, intervention.period=1,
                           cooldown.period=2, control.group=1,
                           treatment.group=2)
  geo.assignment <- GetInfo(obj.ged, "geo.assignment")
  summ2 <- .GetPreanalysisSummary(fit, i=2,
                                  geo.assignment=geo.assignment, sim=2)
  class(summ2) <- c("ROASAnalysisFit", "data.frame")
  expect_equal(summ1, summ2)
})

test_that("the returned object is as expected for model tbr1", {
  summ1 <- .SimulateROASAnalysis(sim=2, randomize.i=FALSE, obj=obj.gpd.fixed,
                                 response="sales", prop.to="sales", cost=1,
                                 models=kTBRModel1, swap.and.redo=FALSE)
  # Reconstruct the summary and compare.
  obj.ged <- SimulateGeoExperimentData(obj.gpd.fixed, i=2)
  SetSpendChange(obj.ged, prop.to="sales") <- 1
  fit <- DoTBRROASAnalysis(obj.ged, model=kTBRModel1,
                           response="sales", cost=kSpendChange,
                           pretest.period=0, intervention.period=1,
                           cooldown.period=2, control.group=1,
                           treatment.group=2)
  geo.assignment <- GetInfo(obj.ged, "geo.assignment")
  summ2 <- .GetPreanalysisSummary(fit, i=2,
                                  geo.assignment=geo.assignment, sim=2)
  class(summ2) <- c("ROASAnalysisFit", "data.frame")
  expect_equal(summ1, summ2)
})

test_that("two models can be specified", {
  models <- c(kTBRModel1, kGBRModel1)
  summ1 <- .SimulateROASAnalysis(sim=2, randomize.i=FALSE, obj=obj.gpd.fixed,
                                 response="sales", prop.to="sales", cost=1,
                                 models=models, swap.and.redo=FALSE)
  # Reconstruct the summary and compare.
  obj.ged <- SimulateGeoExperimentData(obj.gpd.fixed, i=2)
  SetSpendChange(obj.ged, prop.to="sales") <- 1
  fit.tbr <- DoTBRROASAnalysis(obj.ged,  model=kTBRModel1,
                               response="sales",cost=kSpendChange,
                               pretest.period=0, intervention.period=1,
                               cooldown.period=2, control.group=1,
                               treatment.group=2, n.sims=1)
  fit.gbr <- DoGBRROASAnalysis(obj.ged, response="sales", cost=kSpendChange,
                               pretest.period=0, intervention.period=1,
                               cooldown.period=2, control.group=1,
                               treatment.group=2)
  geo.assignment <- GetInfo(obj.ged, "geo.assignment")
  summ2.tbr <- .GetPreanalysisSummary(fit.tbr, i=2,
                                      geo.assignment=geo.assignment, sim=2)
  summ2.gbr <- .GetPreanalysisSummary(fit.gbr, i=2,
                                      geo.assignment=geo.assignment, sim=2)
  summ2 <- rbind(summ2.tbr, summ2.gbr)
  class(summ2) <- c("ROASAnalysisFit", "data.frame")
  expect_equal(summ1, summ2)
})

test_that("swap-and-redo works", {
  set.seed(1)
  summ1 <- .SimulateROASAnalysis(sim=2, randomize.i=FALSE, obj=obj.gpd,
                                 response="sales", prop.to="sales", cost=1,
                                 models=kGBRModel1, swap.and.redo=TRUE)
  # Reconstruct the summary and compare.
  set.seed(1)
  obj.ged <- SimulateGeoExperimentData(obj.gpd, i=2)
  SetSpendChange(obj.ged, prop.to="sales") <- 1
  fit.gbr.2a <- DoGBRROASAnalysis(obj.ged, response="sales", cost=kSpendChange,
                                  pretest.period=0, intervention.period=1,
                                  cooldown.period=2, control.group=1,
                                  treatment.group=2)
  geo.assignment.2a <- GetInfo(obj.ged, "geo.assignment")
  MapGeoGroups(obj.ged) <- (2:1)
  SetSpendChange(obj.ged, prop.to="sales") <- 1
  fit.gbr.2b <- DoGBRROASAnalysis(obj.ged, response="sales", cost=kSpendChange,
                                  pretest.period=0, intervention.period=1,
                                  cooldown.period=2, control.group=1,
                                  treatment.group=2)
  geo.assignment.2b <- GetInfo(obj.ged, "geo.assignment")
  summ2a <- .GetPreanalysisSummary(fit.gbr.2a, i=2,
                                   geo.assignment=geo.assignment.2a, sim=3)
  summ2b <- .GetPreanalysisSummary(fit.gbr.2b, i=2,
                                   geo.assignment=geo.assignment.2b, sim=4)
  summ2 <- rbind(summ2a, summ2b)
  class(summ2) <- c("ROASAnalysisFit", "data.frame")
  expect_equal(summ1, summ2)
})

test_that("cooldown period is taken into account", {
  period.lengths.1 <- c(28, 14, 7)
  period.lengths.2 <- c(28, 14, 14)
  obj.gpd.1 <- GeoExperimentPreanalysisData(obj.gts,
                                            period.lengths=period.lengths.1,
                                            geos=obj.ga,
                                            recycle=FALSE)
  summ1a <- .SimulateROASAnalysis(sim=2, randomize.i=FALSE, obj=obj.gpd.1,
                                  response="sales", prop.to="sales", cost=1,
                                  models=kGBRModel1, swap.and.redo=FALSE)
  obj.gpd.2 <- GeoExperimentPreanalysisData(obj.gts,
                                            period.lengths=period.lengths.2,
                                            geos=obj.ga,
                                            recycle=FALSE)
  summ2a <- .SimulateROASAnalysis(sim=2, randomize.i=FALSE, obj=obj.gpd.2,
                                  response="sales", prop.to="sales", cost=1,
                                  models=kGBRModel1, swap.and.redo=FALSE)
  # The summaries must not be equal.
  expect_false(isTRUE(all.equal(summ1a, summ2a)))
  # Reconstruct the summary and compare.
  obj.ged.1 <- SimulateGeoExperimentData(obj.gpd.1, i=2)
  obj.ged.2 <- SimulateGeoExperimentData(obj.gpd.2, i=2)
  SetSpendChange(obj.ged.1, prop.to="sales") <- 1
  SetSpendChange(obj.ged.2, prop.to="sales") <- 1
  fit.1 <- DoGBRROASAnalysis(obj.ged.1, response="sales",
                             cost=kSpendChange,
                             pretest.period=0, intervention.period=1,
                             cooldown.period=2, control.group=1,
                             treatment.group=2)
  fit.2 <- DoGBRROASAnalysis(obj.ged.2, response="sales",
                             cost=kSpendChange,
                             pretest.period=0, intervention.period=1,
                             cooldown.period=2, control.group=1,
                             treatment.group=2)
  geo.assignment.1 <- GetInfo(obj.ged.1, "geo.assignment")
  geo.assignment.2 <- GetInfo(obj.ged.2, "geo.assignment")
  summ1b <- .GetPreanalysisSummary(fit.1, i=2,
                                   geo.assignment=geo.assignment.1, sim=2)
  summ2b <- .GetPreanalysisSummary(fit.2, i=2,
                                   geo.assignment=geo.assignment.2, sim=2)
  class(summ1b) <- c("ROASAnalysisFit", "data.frame")
  expect_equal(summ1a, summ1b)
  class(summ2b) <- c("ROASAnalysisFit", "data.frame")
  expect_equal(summ2a, summ2b)
})



context("DoROASPreanalysis")

context("DoROASPreanalysis.GeoExperimentAnalysisData")

test_that("returning object consists of calls to SimulateROASAnalysis", {
  # GeoExperimentPreanalysisData with a fixed GeoAssignment.
  models <- GetModelIds()
  n.sims.max <- 4
  expect_is(obj.pre <- DoROASPreanalysis(obj.gpd.fixed,
                                         response=kSales,
                                         prop.to=kSales,
                                         n.sims=n.sims.max,
                                         models=GetModelIds()),
            "ROASPreanalysisFit")
  summ <- NULL
  for (i in seq_len(n.sims.max)) {
    summ.model.1 <- .SimulateROASAnalysis(sim=i, randomize.i=FALSE,
                                          obj=obj.gpd.fixed, response="sales",
                                          prop.to="sales", cost=1,
                                          models=models[1],
                                          swap.and.redo=FALSE)
    summ.model.2 <- .SimulateROASAnalysis(sim=i , randomize.i=FALSE,
                                          obj=obj.gpd.fixed, response="sales",
                                          prop.to="sales", cost=1,
                                          models=models[2],
                                          swap.and.redo=FALSE)
    summ.1.2 <- rbind(summ.model.1, summ.model.2)
    if (is.null(summ)) {
      summ <- summ.1.2
    } else {
      summ <- rbind(summ, summ.1.2)
    }
  }
  summ$geo.assignment <- NULL
  # Testing 'equivalence' ignores attributes.
  summ <- SetInfo(summ, info=list())
  expect_equivalent(obj.pre, summ)
})


test_that("randomizable geo assignments do group-swapping by default", {
  # GeoExperimentPreanalysisData with a (randomizable) GeoStrata object.
  n.sims.max <- 4
  expect_is(obj.pre <- DoROASPreanalysis(obj.gpd,
                                         response=kSales,
                                         prop.to=kSales,
                                         n.sims=n.sims.max,
                                         models=kGBRModel1),
            "ROASPreanalysisFit")
  expect_equal(n.sims.max, nrow(obj.pre))
  geo.assignment <- GetInfo(obj.pre, "geo.assignment")
  geo.maps <- lapply(geo.assignment, FUN=function(x) {
    map <- GetGeoGroup(x); return(map[sort(names(map))])
  })
  expect_equal(geo.maps[[2]], 3L - geo.maps[[1]])
  expect_equal(geo.maps[[4]], 3L - geo.maps[[3]])
})


context("DoROASPreanalysis.GeoTimeseries")

test_that("GeoTimeseries or a preanalysis object => same result", {
  models <- GetModelIds()
  n.sims.max <- 4
  expect_is(obj.pre1 <- DoROASPreanalysis(obj.gts,
                                          period.lengths=period.lengths,
                                          geos=obj.ga,
                                          recycle=FALSE,
                                          response=kSales,
                                          prop.to=kSales,
                                          n.sims=n.sims.max,
                                          models=GetModelIds()),
            "ROASPreanalysisFit")
  expect_is(obj.pre2 <- DoROASPreanalysis(obj.gpd.fixed,
                                          response=kSales,
                                          prop.to=kSales,
                                          n.sims=n.sims.max,
                                          models=GetModelIds()),
            "ROASPreanalysisFit")
  expect_equal(obj.pre1, obj.pre2)
})


context("summary.ROASPreanalysisFit")

test_that("with 1 model specified, the object structure is ok", {
  obj.pre <- DoROASPreanalysis(obj.gpd.fixed,
                               response=kSales,
                               prop.to=kSales,
                               n.sims=4,
                               models=kGBRModel1)
  expect_is(obj.summ <- summary(obj.pre, level=0.90, interval.type="one-sided",
                                cost=1),
            "ROASPreanalysisResults")
  expect_is(obj.summ, "data.frame")
  expect_is(GetInfo(obj.summ, "precision.table"), "data.frame")
  prec.table <- GetInfo(obj.summ, "precision.table")
  expect_equal(names(prec.table), kGBRModel1)
  median.precision <- obj.summ[["precision"]]
  precisions <- (qt(0.9, df=obj.pre[["df"]]) * obj.pre[["sd"]])
  median.calculated <- median(precisions)
  expect_equal(median.calculated, median.precision)
  expect_equal(prec.table[[kGBRModel1]], precisions)
  # Check two-sided interval.
  expect_is(obj.summ <- summary(obj.pre, level=0.95, interval.type="two-sided",
                                cost=1),
            "ROASPreanalysisResults")
  expect_is(obj.summ, "data.frame")
  expect_is(GetInfo(obj.summ, "precision.table"), "data.frame")
  prec.table <- GetInfo(obj.summ, "precision.table")
  expect_equal(names(prec.table), kGBRModel1)
  median.precision <- obj.summ[["precision"]]
  precisions <- (qt(0.975, df=obj.pre[["df"]]) * obj.pre[["sd"]])
  median.calculated <- median(precisions)
  expect_equal(median.calculated, median.precision)
  expect_equal(prec.table[[kGBRModel1]], precisions)
})

test_that("with 2 models specified, the object structure is ok", {
  models <- c(kGBRModel1, kTBRModel1)
  obj.pre <- DoROASPreanalysis(obj.gpd.fixed,
                               response=kSales,
                               prop.to=kSales,
                               n.sims=4,
                               models=models)
  expect_is(obj.summ <- summary(obj.pre, level=0.90, interval.type="one-sided",
                                cost=1),
            "ROASPreanalysisResults")
  expect_is(obj.summ, "data.frame")
  expect_is(GetInfo(obj.summ, "precision.table"), "data.frame")
  prec.table <- GetInfo(obj.summ, "precision.table")
  expect_equal(names(prec.table), models)
  for (model in models) {
    obj.pre0 <- obj.pre[obj.pre[["model"]] %in% model, , drop=FALSE]
    precisions <- (qt(0.9, df=obj.pre0[["df"]]) * obj.pre0[["sd"]])
    median.precision <- obj.summ[obj.summ[["model"]] %in% model, "precision"]
    median.calculated <- median(precisions)
    expect_equal(median.calculated, median.precision)
    expect_equal(prec.table[[model]], precisions)
  }
})


test_that("summary yields the correct number of geos", {
  obj.pre <- DoROASPreanalysis(obj.gpd,
                               response=kSales,
                               prop.to=kSales,
                               n.sims=2,
                               models=kGBRModel1)
  expect_is(obj.summ <- summary(obj.pre), "ROASPreanalysisResults")
  expect_equal(obj.summ[["n.geos"]], n.geos)
})

test_that("[non-fixed geo assignment] n.geos is correct if 1 geo is omitted", {
  # Exclude one geo from the randomization process.
  obj.gs[["geo.group"]][1] <- kExcludeGeoGroup
  obj.gpd <- GeoExperimentPreanalysisData(obj.gts,
                                          period.lengths=period.lengths,
                                          geos=obj.gs,
                                          recycle=FALSE)
  obj.pre <- DoROASPreanalysis(obj.gpd,
                               response=kSales,
                               prop.to=kSales,
                               n.sims=2,
                               models=kGBRModel1)
  expect_is(obj.summ <- summary(obj.pre), "ROASPreanalysisResults")
  expect_equal(obj.summ[["n.geos"]], n.geos - 1)
})

test_that("[fixed geo assignment] n.geos is correct if 1 geo is omitted", {
  # Exclude one geo from the randomization process.
  obj.ga[["geo.group"]][1] <- kExcludeGeoGroup
  obj.gpd <- GeoExperimentPreanalysisData(obj.gts,
                                          period.lengths=period.lengths,
                                          geos=obj.ga,
                                          recycle=FALSE)
  obj.pre <- DoROASPreanalysis(obj.gpd,
                               response=kSales,
                               prop.to=kSales,
                               n.sims=2,
                               models=kGBRModel1)
  expect_is(obj.summ <- summary(obj.pre), "ROASPreanalysisResults")
  expect_equal(obj.summ[["n.geos"]], n.geos - 1)
})

test_that("Geo ratio 1:1 and cfrac 0.5 is given for 2 groups", {
  obj.pre2 <- DoROASPreanalysis(obj.gpd,
                                response=kSales,
                                prop.to=kSales,
                                n.sims=2,
                                models=kGBRModel1)
  expect_is(obj.summ <- summary(obj.pre2), "ROASPreanalysisResults")
  expect_equal(obj.summ[["gratio"]], "1:1")
  expect_equal(obj.summ[["cfrac"]], 0.5)
})

test_that("Geo ratio 1:1:1:1 and cfrac 0.25 is given for 4 groups", {
  obj.gs3 <- ExtractGeoStrata(obj.gts, n.groups=4)
  obj.gpd3 <- GeoExperimentPreanalysisData(obj.gts,
                                           period.lengths=period.lengths,
                                           geos=obj.gs3,
                                           recycle=FALSE)
  obj.pre <- DoROASPreanalysis(obj.gpd3,
                               response=kSales,
                               prop.to=kSales,
                               n.sims=2,
                               models=kGBRModel1)
  expect_is(obj.summ <- summary(obj.pre), "ROASPreanalysisResults")
  expect_equal(obj.summ[["gratio"]], "1:1:1:1")
  expect_equal(obj.summ[["cfrac"]], 0.25)
})


context("print.ROASPreanalysisFit")

test_that("the summary is printed and the summary object returned", {
  obj.pre <- DoROASPreanalysis(obj.gpd.fixed,
                               response=kSales,
                               prop.to=kSales,
                               n.sims=2,
                               models=kGBRModel1)
  summ <- summary(obj.pre)
  output <- utils::capture.output(print(summ))  # Two lines.
  expect_output(x <- print(obj.pre),
                regexp=paste0(output, collapse="\n"), fixed=TRUE)
  expect_identical(x, summ)
})
