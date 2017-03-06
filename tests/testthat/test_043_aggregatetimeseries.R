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

context("AggregateTimeseries.GeoTimeseries")

n.days2 <- (7L * 52L)
dfgt <- .SimulateGeoTimeseries(n.geos=n.geos, n.days=n.days2, date.col=kDate,
                               date.from="2015-01-05",  # Monday.
                               geo.col=kGeo, metric.col=metric.df1, seed=SEED)
sales <- metric.df1
daily <- 2L
dfgt[[sales]] <- daily
obj2 <- GeoTimeseries(dfgt, metrics=sales)

test_that("Argument 'freq' is mandatory", {
  expect_error(AggregateTimeseries(obj2))
})

test_that("a daily is aggregated to weekly data properly", {
  expect_is(obj.agg <- AggregateTimeseries(obj2, freq="weekly"),
            "GeoTimeseries")
  expect_equal(nrow(obj.agg), nrow(obj2) %/% 7L)
  expect_true(all(obj.agg[[sales]] %in% (7L * daily)))
  expect_equal(length(unique(obj.agg[[kDate]])), n.days2 / 7)
})

test_that("a daily data set is aggregated to monthly data properly", {
  expect_is(obj.agg <- AggregateTimeseries(obj2, freq="monthly"),
            "GeoTimeseries")
  expect_equal(sum(obj.agg[[sales]]), sum(obj2[[sales]]))
  expect_equal(length(unique(obj.agg[[kDate]])), n.days2 / 28)
})

test_that("a daily data set is aggregated to weekly data properly", {
  expect_is(obj.agg <- AggregateTimeseries(obj2, freq="weekly"),
            "GeoTimeseries")
  expect_equal(nrow(obj.agg), nrow(obj2) %/% 7L)
  expect_true(all(obj.agg[[sales]] %in% (7L * daily)))
  expect_equal(length(unique(obj.agg[[kDate]])), n.days2 / 7)
})
