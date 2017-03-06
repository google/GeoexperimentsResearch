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

context("IsInPeriod")

context("IsInPeriod.TBRAnalysisData")

tbr.key.columns <- c(kDate, kPeriod)
tbr.num.columns <- c(kY, kX)
tbr.required.columns <- c(tbr.key.columns, tbr.num.columns)

dftbr1 <- data.frame(as.Date("2016-05-01") + 0:20,
                     as.integer(c(-1, -1, rep(0, 8), NA, rep(1, 8), 2, 3)),
                     abs(rnorm(21)),
                     abs(rnorm(21)))

names(dftbr1) <- c(kDate, kPeriod, kY, kX)
obj.tbr <- TBRAnalysisData(dftbr1)
n <- nrow(obj.tbr)

test_that("periods are correctly identified", {
  for (name in names(kStandardPeriods)) {
    expect_warning(x <- IsInPeriod(obj.tbr, periods=name),
                   regexp=NA)
    in.period <- (obj.tbr[[kPeriod]] %in% kStandardPeriods[[name]])
    expect_identical(x, in.period)
  }
})

test_that("multiple periods can be specified", {
  periods <- c("pretest", "intervention")
  expect_identical(x <- IsInPeriod(obj.tbr,
                                   periods=periods),
                   obj.tbr[[kPeriod]] %in% unlist(kStandardPeriods[periods]))
})

test_that("error is thrown when an unknown period is given", {
  expect_error(x <- IsInPeriod(obj.tbr, periods="unknown"),
               regexp="Unknown periods: 'unknown'")
})

test_that("error is thrown if 'periods' is not a nonempty string", {
  expect_error(x <- IsInPeriod(obj.tbr, periods=1))
  expect_error(x <- IsInPeriod(obj.tbr, periods=""))
  expect_error(x <- IsInPeriod(obj.tbr, periods=NA_character_))
})


context("IsInPeriod.TBRAnalysisFitTbr1")

obj.tbrfit <- DoTBRAnalysis(obj.tbr, model=kTBRModel1)

test_that("the method works also for TBRAnalysisFitTbr1 objects", {
  for (name in names(kStandardPeriods)) {
    expect_identical(x <- IsInPeriod(obj.tbrfit, periods=name),
                     obj.tbrfit[[kPeriod]] %in% kStandardPeriods[[name]])
  }
})


context("IsInPeriod.TBRROASAnalysisFit")

test_that("the method works also for TBRROASAnalysisFit objects", {
  # Construct such an object.
  in.prediction <- IsInPeriod(obj.tbrfit, periods="prediction")
  n.prediction <- sum(in.prediction)
  obj.tbrroas <- as.matrix(rep(0, n.prediction), nrow=n.prediction, ncol=1)
  class(obj.tbrroas) <- c("TBRROASAnalysisFit", class(obj.tbrroas))
  obj.tbrroas <- SetInfo(obj.tbrroas,
                         tbr.resp=obj.tbrfit)
  in.prediction <- IsInPeriod(obj.tbrfit, periods="prediction")
  for (name in names(kStandardPeriods)) {
    expect_warning(x <- IsInPeriod(obj.tbrroas, periods=name),
                   regexp=NA)
    expect_identical(x, IsInPeriod(obj.tbrfit, periods=name)[in.prediction])
  }
})

context("IsInPeriod.TBRQuantiles")

test_that("the method works also for TBRQuantiles objects", {
  # Construct such an object.
  obj.tbrq <- obj.tbrfit[IsInPeriod(obj.tbrfit, periods="prediction"), ]
  class(obj.tbrq) <- c("TBRQuantiles", "data.frame")
  in.prediction <- IsInPeriod(obj.tbrfit, periods="prediction")
  for (name in names(kStandardPeriods)) {
    expect_warning(x <- IsInPeriod(obj.tbrq, periods=name),
                   regexp=NA)
    expect_identical(x, IsInPeriod(obj.tbrfit, periods=name)[in.prediction])
  }
})
