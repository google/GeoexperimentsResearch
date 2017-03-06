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

context("as.GBRROASAnalysisData")

# Create a GeoTimeseries object that has 'sales' and 'cost.clicks' columns
# whose values sum up to a known number when aggregated.
period.weeks <- c(Pretest=4L, Test=3L, Cooldown=1L, Another=1L)
n.days <- ((7L * sum(period.weeks)) + 1L)
geos <- sprintf("%02d", 1:20)
dfgts <- expand.grid(date=seq_len(n.days) - 1L, geo=geos)
# Cost is equal to the number of the geo per each day.
dfgts[["cost.clicks"]] <- 1 * as.numeric(dfgts[[kGeo]])
# Sales is equal to 2 * cost per every day.
dfgts[["sales"]] <- 2 * dfgts[["cost.clicks"]]
first.date <- as.Date("2016-01-01")
dfgts[[kDate]] <- (first.date + dfgts[[kDate]])
dfgts[[kGeo]] <- sprintf("%02d", dfgts[[kGeo]])
obj.gts <- GeoTimeseries(dfgts,  metrics=c("sales", "cost.clicks"))
# 4 groups with 5 geos each.
geo.group <- (1L + (geos %in% geos[6:20]) + (geos %in% geos[11:20]) +
              (geos %in% geos[16:20]))
obj.ga <- GeoAssignment(data.frame(geo=geos, geo.group=geo.group))
# First date is not in the experiment.
period.dates <- (1L + first.date + 7L * cumsum(c(0L, period.weeks)))
period.dates[length(period.dates)] <- (period.dates[length(period.dates)] - 1L)
obj.periods <- ExperimentPeriods(period.dates=period.dates,
                                 period.names=names(period.weeks))
obj.ged <- GeoExperimentData(obj.gts,
                             periods=obj.periods,
                             geo.assignment=obj.ga,
                             treat.assignment=NULL)


context("as.GBRROASAnalysisData.GeoExperimentData")

test_that("GeoExperimentData object is coerced to GBRROASAnalysisData", {
  expect_is(as.GBRROASAnalysisData(obj.ged, response="sales",
                                   cost="cost.clicks"),
            "GBRROASAnalysisData")
})

test_that("defaults: control group 1, trt group 2, preperiod 0, test 1", {
  expect_error(x1 <- as.GBRROASAnalysisData(obj.ged, response="sales",
                                            cost="cost.clicks"),
               regexp=NA)
  expect_error(x2 <- as.GBRROASAnalysisData(obj.ged, response="sales",
                                            cost="cost.clicks",
                                            pretest.period=0,
                                            intervention.period=1,
                                            cooldown.period=NULL,
                                            control.group=1,
                                            treatment.group=2),
               regexp=NA)
  expect_identical(x1, x2)

})

test_that("switching ctrl and trt affects only column kControl", {
  expect_error(x1 <- as.GBRROASAnalysisData(obj.ged, response="sales",
                                            cost="cost.clicks",
                                            pretest.period=0,
                                            intervention.period=1,
                                            cooldown.period=2,
                                            control.group=1,
                                            treatment.group=2),
               regexp=NA)
  expect_error(x2 <- as.GBRROASAnalysisData(obj.ged, response="sales",
                                            cost="cost.clicks",
                                            pretest.period=0,
                                            intervention.period=1,
                                            cooldown.period=2,
                                            control.group=2,
                                            treatment.group=1),
               regexp=NA)
  expect_identical(x1[[kControl]], !x2[[kControl]])

})

test_that("pretest.period >= intervention.period gives an error", {
  expect_error(as.GBRROASAnalysisData(obj.ged, response="sales",
                                      cost="cost.clicks",
                                      pretest.period=1,
                                      intervention.period=1,
                                      cooldown.period=2),
               regexp=paste("Pretest period must occur before the intervention",
                            kPeriod))
  expect_error(as.GBRROASAnalysisData(obj.ged, response="sales",
                                      cost="cost.clicks",
                                      pretest.period=2,
                                      intervention.period=1,
                                      cooldown.period=2),
               regexp=paste("Pretest period must occur before the intervention",
                            kPeriod))
})

test_that("cooldown.period >= intervention.period gives an error", {
  expect_error(as.GBRROASAnalysisData(obj.ged, response="sales",
                                      cost="cost.clicks",
                                      pretest.period=0,
                                      intervention.period=1,
                                      cooldown.period=1),
               regexp=paste("Intervention period must occur before the",
                            "cooldown period"))
  expect_error(as.GBRROASAnalysisData(obj.ged, response="sales",
                                      cost="cost.clicks",
                                      pretest.period=0,
                                      intervention.period=2,
                                      cooldown.period=1),
               regexp=paste("Intervention period must occur before the",
                            "cooldown period"))
})

test_that(paste("an invalid pretest.period or intervention.period or",
                "cooldown.period gives an error"), {
  expect_error(as.GBRROASAnalysisData(obj.ged, response="sales",
                                  cost="cost.clicks",
                                  pretest.period=0.5,
                                  intervention.period=1,
                                  cooldown.period=2),
               regexp="'pretest.period' does not contain valid period numbers")
  expect_error(as.GBRROASAnalysisData(obj.ged, response="sales",
                                  cost="cost.clicks",
                                  pretest.period=0,
                                  intervention.period=1.5,
                                  cooldown.period=2),
               regexp=paste("'intervention.period' does not contain valid",
                            "period numbers"))
  expect_error(as.GBRROASAnalysisData(obj.ged, response="sales",
                                  cost="cost.clicks",
                                  pretest.period=0,
                                  intervention.period=1,
                                  cooldown.period=2.5),
               regexp=paste("'cooldown.period' does not contain valid",
                            "period numbers"))
})

test_that("multiple pretest periods are allowed", {
  expect_error(as.GBRROASAnalysisData(obj.ged, response="sales",
                                      cost="cost.clicks",
                                      pretest.period=0:1,
                                      intervention.period=2,
                                      cooldown.period=3),
               regexp=NA)
})

test_that("multiple intervention periods are allowed", {
  expect_error(as.GBRROASAnalysisData(obj.ged, response="sales",
                                      cost="cost.clicks",
                                      pretest.period=0,
                                      intervention.period=1:2,
                                      cooldown.period=3),
               regexp=NA)
})


test_that("multiple cooldown periods are allowed", {
  expect_error(as.GBRROASAnalysisData(obj.ged, response="sales",
                                      cost="cost.clicks",
                                      pretest.period=0,
                                      intervention.period=1,
                                      cooldown.period=2:3),
               regexp=NA)
})
test_that("multiple control and test groups are allowed", {
  expect_error(as.GBRROASAnalysisData(obj.ged, response="sales",
                                      cost="cost.clicks",
                                      pretest.period=0,
                                      intervention.period=1,
                                      cooldown.period=2,
                                      control.group=1:2,
                                      treatment.group=3:4),
               regexp=NA)
})

test_that("multiple control and test groups are correctly combined", {
  expect_error(gbr <- as.GBRROASAnalysisData(obj.ged, response="sales",
                                             cost="cost.clicks",
                                             pretest.period=0,
                                             intervention.period=1,
                                             cooldown.period=2,
                                             control.group=1:2,
                                             treatment.group=3:4),
               regexp=NA)
  expect_identical(gbr[[kControl]], geo.group %in% 1:2)
  expect_identical(!gbr[[kControl]], geo.group %in% 3:4)
  expect_error(gbr <- as.GBRROASAnalysisData(obj.ged, response="sales",
                                             cost="cost.clicks",
                                             pretest.period=0,
                                             intervention.period=1,
                                             cooldown.period=2,
                                             control.group=1,
                                             treatment.group=2:4),
               regexp=NA)
  expect_identical(gbr[[kControl]], geo.group %in% 1)
  expect_identical(!gbr[[kControl]], geo.group %in% 2:4)
})

test_that("control and test groups must not overlap", {
  expect_error(gbr <- as.GBRROASAnalysisData(obj.ged, response="sales",
                                             cost="cost.clicks",
                                             pretest.period=0,
                                             intervention.period=1,
                                             cooldown.period=2,
                                             control.group=1:2,
                                             treatment.group=1:2),
               regexp="Control and treatment groups must not overlap")
})

test_that("overlapping pretest/intervention periods are disallowed", {
  expect_error(as.GBRROASAnalysisData(obj.ged, response="sales",
                                      cost="cost.clicks",
                                      pretest.period=0:1,
                                      intervention.period=1:2,
                                      cooldown.period=3),
               regexp=paste("Pretest period must occur before the intervention",
                            kPeriod))
})

test_that("overlapping intervention/cooldown periods are disallowed", {
  expect_error(as.GBRROASAnalysisData(obj.ged, response="sales",
                                      cost="cost.clicks",
                                      pretest.period=0,
                                      intervention.period=1:2,
                                      cooldown.period=2:3),
               regexp=paste("Intervention period must occur before the",
                            "cooldown period"))
})

test_that(paste("an unknown pretest.period or intervention.period or",
                "cooldown.period throws an error"), {
  expect_error(as.GBRROASAnalysisData(obj.ged, response="sales",
                                      cost="cost.clicks",
                                      pretest.period=4,
                                      intervention.period=5,
                                      cooldown.period=6),
               regexp="'pretest.period' 4 is not present in the data",
               fixed=TRUE)
  expect_error(as.GBRROASAnalysisData(obj.ged, response="sales",
                                      cost="cost.clicks",
                                      pretest.period=1,
                                      intervention.period=4,
                                      cooldown.period=5),
               regexp="'intervention.period' 4 is not present in the data",
               fixed=TRUE)
  expect_error(as.GBRROASAnalysisData(obj.ged, response="sales",
                                      cost="cost.clicks",
                                      pretest.period=0,
                                      intervention.period=1,
                                      cooldown.period=4),
               regexp="'cooldown.period' 4 is not present in the data",
               fixed=TRUE)
})

test_that("invalid geo group number gives an error", {
  max.geo.group <- max(obj.ged[[kGeoGroup]], na.rm=TRUE)
  bad.group <- (max.geo.group + 1L)
  expect_error(as.TBRAnalysisData(obj.ged, response="sales",
                                  control.group=bad.group,
                                  treatment.group=2),
               regexp=paste("Specified 'control.group'", bad.group,
                   "is not present in the data"))
  expect_error(as.TBRAnalysisData(obj.ged, response="sales",
                                  control.group=1L,
                                  treatment.group=bad.group),
               regexp=paste("Specified 'treatment.group'", bad.group,
                   "is not present in the data"))
  expect_error(as.TBRAnalysisData(obj.ged, response="sales",
                                  control.group="Control",
                                  treatment.group=2),
               regexp=paste0("'control.group' does not contain ",
                   "valid geo group numbers"))
  expect_error(as.TBRAnalysisData(obj.ged, response="sales",
                                  control.group=1L,
                                  treatment.group="Treatment"),
               regexp=paste0("'treatment.group' does not contain ",
                   "valid geo group numbers"))
  expect_error(as.TBRAnalysisData(obj.ged, response="sales",
                                  control.group=1.01,
                                  treatment.group=2L),
               regexp=paste0("'control.group' does not contain ",
                   "valid geo group numbers"))
  expect_error(as.TBRAnalysisData(obj.ged, response="sales",
                                  control.group=1L,
                                  treatment.group=2.01),
               regexp=paste0("'treatment.group' does not contain ",
                   "valid geo group numbers"))
})

test_that("error is thrown if period is all NAs", {
  obj.ged[[kPeriod]] <- NA_integer_
  expect_error(as.GBRROASAnalysisData(obj.ged, response="sales",
                                  cost="cost.clicks"),
               regexp="There are no period numbers in the data",
               fixed=TRUE)
})

test_that("error is thrown if geo.group is all NAs", {
  obj.ged[[kGeoGroup]] <- NA_integer_
  expect_error(as.GBRROASAnalysisData(obj.ged, response="sales",
                                  cost="cost.clicks"),
               regexp="There are no geo group numbers in the data")
})

test_that("error is thrown if either response or cost are nonexistent", {
  expect_error(as.GBRROASAnalysisData(obj.ged, response="sales2",
                                  cost="cost.clicks"),
               regexp=paste0("The specified column 'sales2' is not ",
                   "in the data frame"))
  expect_error(as.GBRROASAnalysisData(obj.ged, response="sales",
                                  cost="cost.clicks2"),
               regexp=paste0("The specified column 'cost.clicks2' is not ",
                   "in the data frame"))
  expect_error(as.GBRROASAnalysisData(obj.ged, response="sales2",
                                  cost="cost.clicks2"),
               regexp=paste0("The specified columns 'sales2', 'cost.clicks2' ",
                   "are not in the data frame"))
})

test_that("error is thrown if response or cost have NA during the experiment", {
  bad.sales <- obj.ged
  bad.sales[["sales"]][!is.na(bad.sales[[kPeriod]])][1] <- NA_real_
  expect_error(as.GBRROASAnalysisData(bad.sales, response="sales",
                                  cost="cost.clicks"),
               regexp="Column 'sales' has one missing value in row 21",
               fixed=TRUE)
  bad.cost <- obj.ged
  bad.cost[["cost.clicks"]][!is.na(bad.cost[[kPeriod]])][1] <- NA_real_
  expect_error(as.GBRROASAnalysisData(bad.cost, response="sales",
                                  cost="cost.clicks"),
               regexp="Column 'cost.clicks' has one missing value in row 21",
               fixed=TRUE)
})

test_that("no errors if response or cost have NA outside of the experiment", {
  obj.ged[["sales"]][is.na(obj.ged[[kPeriod]])][1] <- NA_real_
  obj.ged[["cost.clicks"]][is.na(obj.ged[[kPeriod]])][1] <- NA_real_
  expect_error(as.GBRROASAnalysisData(obj.ged, response="sales",
                                      cost="cost.clicks"),
               regexp=NA)
})

test_that("geos mapped to kExcludeGeoGroup are indeed excluded", {
  ga <- ExtractGeoAssignment(obj.ged)
  drop.geo <- ga[[kGeo]][1]
  ga[[kGeoGroup]][ga[[kGeo]] %in% drop.geo] <- kExcludeGeoGroup
  SetGeoAssignment(obj.ged) <- ga
  expect_error(gbr <- as.GBRROASAnalysisData(obj.ged, response="sales",
                                             cost="cost.clicks"),
               regexp=NA)
  expect_false(drop.geo %in% gbr[[kGeo]])
})

test_that("columns are correctly calculated for single periods", {
  # 'cost.clicks' = number of days in the period * number of the geo.
  # 'sales' == 2 times cost of clicks.
  expect_error(gbr <- as.GBRROASAnalysisData(obj.ged, response="sales",
                                             cost="cost.clicks",
                                             intervention.period=1,
                                             cooldown.period=NULL,
                                             control.group=1:2,
                                             treatment.group=3:4),
               regexp=NA)
  pretest.length <- period.weeks["Pretest"] * 7
  test.length <- period.weeks["Test"] * 7
  geos <- as.numeric(geos)
  expect_equal(gbr[[kCostPre]], pretest.length * geos)
  expect_equal(gbr[[kCostTest]], test.length * geos)
  expect_equal(gbr[[kRespPre]], 2 * pretest.length * geos)
  expect_equal(gbr[[kRespTest]], 2 * test.length * geos)
})

test_that("columns are correctly calculated for combined periods", {
  # 'cost.clicks' = number of days in the period * number of the geo.
  # 'sales' == 2 times cost of clicks.
  expect_error(gbr <- as.GBRROASAnalysisData(obj.ged, response="sales",
                                             cost="cost.clicks",
                                             pretest.period=0,
                                             intervention.period=1:2,
                                             cooldown.period=NULL,
                                             control.group=1:2,
                                             treatment.group=3:4),
               regexp=NA)
  pretest.length <- period.weeks["Pretest"] * 7
  test.length <- sum(period.weeks[c("Test", "Cooldown")]) * 7
  geos <- as.numeric(geos)
  expect_equal(gbr[[kCostPre]], pretest.length * geos)
  expect_equal(gbr[[kCostTest]], test.length * geos)
  expect_equal(gbr[[kRespPre]], 2 * pretest.length * geos)
  expect_equal(gbr[[kRespTest]], 2 * test.length * geos)
})

test_that("NA allowed in response and cost for excluded geos", {
  ga <- ExtractGeoAssignment(obj.ged)
  drop.geo <- ga[[kGeo]][1]
  ga[[kGeoGroup]][ga[[kGeo]] %in% drop.geo] <- kExcludeGeoGroup
  bad.sales <- obj.ged
  SetGeoAssignment(bad.sales) <- ga
  bad.sales[["sales"]][(bad.sales[[kGeoGroup]] == kExcludeGeoGroup) &
                       !is.na(bad.sales[[kPeriod]])][1] <- NA_real_
  expect_error(gbr <- as.GBRROASAnalysisData(bad.sales, response="sales",
                                             cost="cost.clicks"),
               regexp=NA)

  bad.cost <- obj.ged
  SetGeoAssignment(bad.cost) <- ga
  bad.cost[["cost.clicks"]][(bad.cost[[kGeoGroup]] == kExcludeGeoGroup) &
                            !is.na(bad.cost[[kPeriod]])][1] <- NA_real_
  expect_error(gbr <- as.GBRROASAnalysisData(bad.cost, response="sales",
                                             cost="cost.clicks"),
               regexp=NA)
})
