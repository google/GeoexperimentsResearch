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

context("method aggregate.GeoTimeseries / GeoExperimentData")

obj.gts <- GeoTimeseries(dfged2, metrics=metrics.df2)
obj.ged <- GeoExperimentData(obj.gts)
obj.list <- list(obj.gts, obj.ged)

# Test for both GeoTimeseries and GeoExperimentData.
for (i in seq_along(obj.list)) {

  obj <- obj.list[[i]]
  switch(paste(i),
         "1"=context("method aggregate.GeoTimeseries"),
         "2"=context("method aggregate.GeoExperimentData"),
         stop("cannot happen"))

  test_that("by default, all metrics are aggregated as expected, by geo", {
    obj.sub <- aggregate(obj)
    expect_false(inherits(obj.sub, "GeoTimeseries"))
    dfx <- as.data.frame(obj)
    metrics <- GetInfo(obj, "metrics")
    default.by <- kGeo
    dfa <- aggregate(dfx[metrics], by=dfx[default.by], FUN=base::sum)
    expect_identical(dfa, obj.sub)
  })

  test_that("FUN is a function", {
    expect_error(aggregate(obj, FUN="sum"),
                 regexp="FUN is not a function")
  })

  test_that("nonexisting 'metrics' and 'by' are detected", {
    expect_error(aggregate(obj, metrics=paste0(metrics.df2, collapse="")),
                 regexp=paste0("The specified column 'salesconversions' ",
                     "is not in the data frame"))
    expect_error(aggregate(obj, by=paste0(metrics.df2, collapse="")),
                 regexp=paste0("The specified column 'salesconversions' ",
                     "is not in the data frame"))
  })

  test_that("'metrics' and 'by' cannot intersect", {
    expect_error(aggregate(obj, metrics=metrics.df2, by=metrics.df2[1L]),
                 regexp="'metrics' and 'by' cannot intersect")
  })
}
