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

context("method Subset for GeoTimeseries and GeoExperimentData")

obj.gts <- GeoTimeseries(dfged2, metrics=metrics.df2)
obj.ged <- GeoExperimentData(obj.gts)
obj.list <- list(obj.gts, obj.ged)

# Both methods should pass the tests.

for (i in seq_along(obj.list)) {
  obj <- obj.list[[i]]
  switch(paste(i),
         "1"=context("method Subset.GeoTimeseries"),
         "2"=context("method Subset.GeoExperimentData"),
         stop("cannot happen"))

  test_that("argument 'rows' selects complex subsets", {
    max.date <- max(obj[[kDate]])
    try(obj.sub <- Subset(obj, rows=obj$date < max.date & obj$geo %in% "1" &
                          obj$.weekday == 1 & obj$conversions > 100))
    expect_is(obj.sub, "GeoTimeseries")
    choose <- with(obj, date < max.date & geo %in% "1" &
                   .weekday == 1 & conversions > 100)
    cols <- names(obj.sub)
    expect_equal(as.list(obj.sub)[cols], as.list(obj[choose, ])[cols])
  })

  test_that("argument 'rows' is rejected if NAs exist", {
    s <- obj[["sales"]]
    s[1L] <- NA  # Introduce a missing value in 'rows'.
    expect_error(Subset(obj, rows=s > 0),
                 paste0("Argument 'rows' has one missing value \\(of total ",
                        nrow(obj), "\\) in row 1"))
  })

  test_that("argument 'columns' selects columns correctly", {
    choose.metric <- metrics.df2[2L]
    obj.sub <- Subset(obj, columns=choose.metric)
    expect_true(choose.metric %in% names(obj.sub))
    expect_true(!metrics.df2[1L] %in% names(obj.sub))
  })

  test_that("specifying nonexisting columns triggers an error", {
    expect_error(Subset(obj, columns="foo"),
                 regexp="The specified column 'foo' is not in the data frame")
    expect_error(Subset(obj, columns=c("foo", "bar")),
                 regexp=paste0("The specified columns 'foo', 'bar' are not ",
                     "in the data frame"))
  })

  test_that("date and geo columns are not removed", {
    choose.metric <- metrics.df2[1L]
    obj.sub <- Subset(obj, columns=choose.metric)
  })

  test_that("must specify at least one column, including one metric", {
    expect_error(Subset(obj, columns=NULL),
                 regexp=paste0(
                     "Argument 'columns' ",
                     "is not a vector of nonempty strings"))
    expect_error(Subset(obj, columns=character(0)),
                 regexp=paste0(
                     "Argument 'columns' ",
                     "is not a vector of nonempty strings"))
    expect_error(Subset(obj, columns=kGeo),
                 regexp="Must include at least one metric in 'columns'")
  })
}  # End of for loop.
