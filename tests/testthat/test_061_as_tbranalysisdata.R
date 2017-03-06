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

context("as.TBRAnalysisData")

# Create a GeoTimeseries object that has 'sales' column whose
# whose values sum up to a known number when aggregated.
period.weeks <- c(Pretest=4L, Test=3L, Cooldown=1L, Another=1L)
n.days <- ((7L * sum(period.weeks)) + 1L)
geos <- sprintf("%02d", 1:20)
dfgts <- expand.grid(date=seq_len(n.days) - 1L, geo=geos)
# Sales is equal to the number of the geo per each day.
dfgts[["sales"]] <- as.numeric(dfgts[[kGeo]])
first.date <- as.Date("2016-01-01")
dfgts[[kDate]] <- (first.date + dfgts[[kDate]])
dfgts[[kGeo]] <- sprintf("%02d", dfgts[[kGeo]])
obj.gts <- GeoTimeseries(dfgts,  metrics="sales")
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


context("as.TBRAnalysisData.GeoExperimentData")

test_that("GeoExperimentData object is coerced to TBRAnalysisData", {
  expect_is(as.TBRAnalysisData(obj.ged, response="sales"),
            "TBRAnalysisData")
})

test_that("defaults: control group 1, trt group 2, preperiod 0, test 1", {
  expect_error(x1 <- as.TBRAnalysisData(obj.ged, response="sales"),
               regexp=NA)
  expect_error(x2 <- as.TBRAnalysisData(obj.ged, response="sales",
                                            pretest.period=0,
                                            intervention.period=1,
                                            cooldown.period=NULL,
                                            control.group=1,
                                            treatment.group=2),
               regexp=NA)
  expect_identical(x1, x2)

})

test_that("switching ctrl and trt swaps x and y", {
  expect_error(x1 <- as.TBRAnalysisData(obj.ged, response="sales",
                                            pretest.period=0,
                                            intervention.period=1,
                                            cooldown.period=2,
                                            control.group=1,
                                            treatment.group=2),
               regexp=NA)
  expect_error(x2 <- as.TBRAnalysisData(obj.ged, response="sales",
                                            pretest.period=0,
                                            intervention.period=1,
                                            cooldown.period=2,
                                            control.group=2,
                                            treatment.group=1),
               regexp=NA)
  expect_identical(x1[[kX]], x2[[kY]])
  expect_identical(x1[[kY]], x2[[kX]])

})

test_that("pretest.period >= intervention.period gives an error", {
  expect_error(as.TBRAnalysisData(obj.ged, response="sales",
                                      pretest.period=1,
                                      intervention.period=1,
                                      cooldown.period=2),
               regexp=paste("Pretest period must occur before the intervention",
                            kPeriod))
  expect_error(as.TBRAnalysisData(obj.ged, response="sales",
                                      pretest.period=2,
                                      intervention.period=1,
                                      cooldown.period=2),
               regexp=paste("Pretest period must occur before the intervention",
                            kPeriod))
})

test_that("intervention.period >= cooldown.period gives an error", {
  expect_error(as.TBRAnalysisData(obj.ged, response="sales",
                                      pretest.period=0,
                                      intervention.period=1,
                                      cooldown.period=1),
               regexp=paste("Intervention period must occur before the",
                            "cooldown period"))
  expect_error(as.TBRAnalysisData(obj.ged, response="sales",
                                      pretest.period=0,
                                      intervention.period=2,
                                      cooldown.period=1),
               regexp=paste("Intervention period must occur before the",
                            "cooldown period"))
})
test_that(paste("an invalid pretest.period or intervention.period or",
                "cooldown.period gives an error"), {
  expect_error(as.TBRAnalysisData(obj.ged, response="sales",
                                  pretest.period=0.5,
                                  intervention.period=1,
                                  cooldown.period=2),
               regexp="'pretest.period' does not contain valid period numbers")
  expect_error(as.TBRAnalysisData(obj.ged, response="sales",
                                  pretest.period=0,
                                  intervention.period=1.5,
                                  cooldown.period=2),
               regexp=paste("'intervention.period' does not contain valid",
                            "period numbers"))
  expect_error(as.TBRAnalysisData(obj.ged, response="sales",
                                  pretest.period=0,
                                  intervention.period=1,
                                  cooldown.period=2.5),
               regexp=paste("'cooldown.period' does not contain valid",
                            "period numbers"))
})

test_that("multiple pretest periods are allowed", {
  expect_error(as.TBRAnalysisData(obj.ged, response="sales",
                                      pretest.period=0:1,
                                      intervention.period=2,
                                      cooldown.period=3),
               regexp=NA)
})

test_that("multiple intervention periods are allowed", {
  expect_error(as.TBRAnalysisData(obj.ged, response="sales",
                                      pretest.period=0,
                                      intervention.period=1:2,
                                      cooldown.period=3),
               regexp=NA)
})

test_that("multiple cooldown periods are allowed", {
  expect_error(as.TBRAnalysisData(obj.ged, response="sales",
                                      pretest.period=0,
                                      intervention.period=1,
                                      cooldown.period=2:3),
               regexp=NA)
})

test_that("multiple control and test groups are allowed", {
  expect_error(as.TBRAnalysisData(obj.ged, response="sales",
                                      pretest.period=0,
                                      intervention.period=1,
                                      cooldown.period=2,
                                      control.group=1:2,
                                      treatment.group=3:4),
               regexp=NA)
})

test_that("multiple control and test groups are correctly combined", {
  expect_error(tbr <- as.TBRAnalysisData(obj.ged, response="sales",
                                             pretest.period=0,
                                             intervention.period=1,
                                             cooldown.period=2,
                                             control.group=1:2,
                                             treatment.group=3:4),
               regexp=NA)
  geos <- as.numeric(geos)
  ctl <- sum(geos[geo.group %in% 1:2])
  trt <- sum(geos[geo.group %in% 3:4])
  expect_identical(tbr[[kY]], rep(trt, nrow(tbr)))
  expect_identical(tbr[[kX]], rep(ctl, nrow(tbr)))
  expect_error(tbr <- as.TBRAnalysisData(obj.ged, response="sales",
                                             pretest.period=0,
                                             intervention.period=1,
                                             cooldown.period=2,
                                             control.group=1,
                                             treatment.group=2:3),
               regexp=NA)
  ctl <- sum(geos[geo.group %in% 1])
  trt <- sum(geos[geo.group %in% 2:3])
  expect_identical(tbr[[kY]], rep(trt, nrow(tbr)))
  expect_identical(tbr[[kX]], rep(ctl, nrow(tbr)))
})

test_that("control and test groups must not overlap", {
  expect_error(gbr <- as.TBRAnalysisData(obj.ged, response="sales",
                                             pretest.period=0,
                                             intervention.period=1,
                                             cooldown.period=2,
                                             control.group=1:2,
                                             treatment.group=1:2),
               regexp="Control and treatment groups must not overlap")
})

test_that("overlapping pretest/intervention periods are disallowed", {
  expect_error(as.TBRAnalysisData(obj.ged, response="sales",
                                      pretest.period=0:1,
                                      intervention.period=1:2,
                                      cooldown.period=3),
               regexp=paste("Pretest period must occur before the intervention",
                            kPeriod))
})

test_that("overlapping intervention/cooldown periods are disallowed", {
  expect_error(as.TBRAnalysisData(obj.ged, response="sales",
                                      pretest.period=0,
                                      intervention.period=1:2,
                                      cooldown.period=2:3),
               regexp=paste("Intervention period must occur before the",
                            "cooldown period"))
})

test_that(paste("an unknown pretest.period or intervention.period or",
                "cooldown.period throws an error"), {
  expect_error(as.TBRAnalysisData(obj.ged, response="sales",
                                      pretest.period=4,
                                      intervention.period=5,
                                      cooldown.period=6),
               regexp="'pretest.period' 4 is not present in the data",
               fixed=TRUE)
  expect_error(as.TBRAnalysisData(obj.ged, response="sales",
                                      pretest.period=1,
                                      intervention.period=4,
                                      cooldown.period=5),
               regexp="'intervention.period' 4 is not present in the data",
               fixed=TRUE)
  expect_error(as.TBRAnalysisData(obj.ged, response="sales",
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
  expect_error(as.TBRAnalysisData(obj.ged, response="sales"),
               regexp="There are no period numbers in the data",
               fixed=TRUE)
})

test_that("error is thrown if geo.group is all NAs", {
  obj.ged[[kGeoGroup]] <- NA_integer_
  expect_error(as.TBRAnalysisData(obj.ged, response="sales"),
               regexp="There are no geo group numbers in the data")
})

test_that("error is thrown if response is nonexistent", {
  expect_error(as.TBRAnalysisData(obj.ged, response="sales2"),
               regexp=paste0("The specified column 'sales2' is not ",
                   "in the data frame"))
})

test_that("error is thrown if response has NA during the experiment", {
  bad.sales <- obj.ged
  bad.sales[["sales"]][!is.na(bad.sales[[kPeriod]])][1] <- NA_real_
  expect_error(as.TBRAnalysisData(bad.sales, response="sales"),
               regexp="Column 'sales' has one missing value in row 21",
               fixed=TRUE)
})

test_that("no errors if response has NA outside of the experiment", {
  obj.ged[["sales"]][is.na(obj.ged[[kPeriod]])][1] <- NA_real_
  expect_error(as.TBRAnalysisData(obj.ged, response="sales"),
               regexp=NA)
})

test_that("geos mapped to kExcludeGeoGroup are indeed excluded", {
  ga <- ExtractGeoAssignment(obj.ged)
  drop.geo <- ga[[kGeo]][1]
  ga[[kGeoGroup]][ga[[kGeo]] %in% drop.geo] <- kExcludeGeoGroup
  SetGeoAssignment(obj.ged) <- ga
  expect_error(gbr <- as.TBRAnalysisData(obj.ged, response="sales"),
               regexp=NA)
  expect_false(drop.geo %in% gbr[[kGeo]])
})

test_that("columns are correctly calculated for single periods", {
  # 'sales' == number of days
  expect_error(tbr <- as.TBRAnalysisData(obj.ged, response="sales",
                                         pretest.period=0,
                                         intervention.period=1,
                                         cooldown.period=NULL),
               regexp=NA)
  period <- rep(NA, length.out=nrow(tbr))
  in.preexp <- (tbr[[kDate]] < obj.periods[1, "Start"])
  in.pretest <- (tbr[[kDate]] >= obj.periods[1, "Start"] &
                    tbr[[kDate]] <= obj.periods[1, "End"])
  in.intervention <- (tbr[[kDate]] >= obj.periods[2, "Start"] &
                      tbr[[kDate]] <= obj.periods[2, "End"])
  in.posttest <- (tbr[[kDate]] > obj.periods[2, "End"])
  period[in.preexp] <- kStandardPeriods[["preexperiment"]]
  period[in.pretest] <- kStandardPeriods[["pretest"]]
  period[in.intervention] <- kStandardPeriods[["intervention"]]
  period[in.posttest] <- kStandardPeriods[["posttest"]]
  expect_identical(tbr[[kPeriod]], period)
})

test_that("columns are correctly calculated for combined periods", {
  # 'cost.clicks' = number of days in the period * number of the geo.
  # 'sales' == 2 times cost of clicks.
  expect_error(tbr <- as.TBRAnalysisData(obj.ged, response="sales",
                                             cost="cost.clicks",
                                             pretest.period=0,
                                             intervention.period=1:2,
                                             cooldown.period=NULL),
               regexp=NA)
  period <- rep(NA, length.out=nrow(tbr))
  in.preexp <- (tbr[[kDate]] < obj.periods[1, "Start"])
  in.pretest <- (tbr[[kDate]] >= obj.periods[1, "Start"] &
                    tbr[[kDate]] <= obj.periods[1, "End"])
  in.intervention <- (tbr[[kDate]] >= obj.periods[2, "Start"] &
                      tbr[[kDate]] <= obj.periods[3, "End"])
  in.posttest <- (tbr[[kDate]] >= obj.periods[4, "Start"])
  period[in.preexp] <- kStandardPeriods[["preexperiment"]]
  period[in.pretest] <- kStandardPeriods[["pretest"]]
  period[in.intervention] <- kStandardPeriods[["intervention"]]
  period[in.posttest] <- kStandardPeriods[["posttest"]]
  expect_identical(tbr[[kPeriod]], period)
})

test_that("geo.group 0 is excluded and does not generate a dummy NA column.", {
  expect_error(tbr1 <- as.TBRAnalysisData(obj.ged, response="sales",
                                          pretest.period=0,
                                          intervention.period=1,
                                          cooldown.period=2,
                                          control.group=1,
                                          treatment.group=2),
               regexp=NA)
  # 07 is group number 2 (treatment)
  obj.ga[[kGeoGroup]][obj.ga[[kGeo]] %in% "07"] <- kExcludeGeoGroup
  SetGeoAssignment(obj.ged) <- obj.ga
  expect_error(tbr2 <- as.TBRAnalysisData(obj.ged, response="sales",
                                          pretest.period=0,
                                          intervention.period=1,
                                          cooldown.period=2,
                                          control.group=1,
                                          treatment.group=2),
               regexp=NA)
  # Omitting geo 07 diminished the sum of all geos in the treatment group by 7.
  expect_equal(tbr1[[kY]], 7 + tbr2[[kY]])
  # 02 is group number 1 (control)
  obj.ga[[kGeoGroup]][obj.ga[[kGeo]] %in% "02"] <- kExcludeGeoGroup
  SetGeoAssignment(obj.ged) <- obj.ga
  expect_error(tbr2 <- as.TBRAnalysisData(obj.ged, response="sales",
                                          pretest.period=0,
                                          intervention.period=1,
                                          cooldown.period=2,
                                          control.group=1,
                                          treatment.group=2),
               regexp=NA)
  # Omitting geo 02 diminished the sum of all geos in the control group by 2.
  expect_equal(tbr1[[kX]], 2 + tbr2[[kX]])
})
