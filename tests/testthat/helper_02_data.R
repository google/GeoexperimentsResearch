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

# Construct data sets used in unit tests.
#
# Data sets representing GeoTimeseries:
#
#   df1: one metric only ('sales').
#   df2: two metrics ('sales', 'conversions') and two additional columns.
#        ('anything', 'another').
#   df3: as df1, except for nonstandard names for 'date' and 'geo'
#        ('date2' and 'geo2').
#
# Data sets representing GeoExperimentData:
#
#   dfged1: as df1, except has also 'period' and 'geo.group'.
#   dfged2: as df2, except has also 'period' and 'geo.group' and 'assignment'.
#
# Data frames representing GBRROASAnalysisData:
#
#   dfgbr2: data frame constructed from dfged2.
#   dfgbr3: as dfgbr2, but has also 'cost.clicks'
#

# Constants used in this file:
n.geos <- (11L)
n.geos.many <- (210L)
n.days <- (7L * 12L)
# Column names of data sets df1, df2:
metric.df1 <- "sales"
metrics.df2 <- c(metric.df1, "conversions")
other.df2 <- c("other.column", "another")
# Test period length.
n.test.days <- 14L

# Data sets representing GeoTimeseries.

# df1: A data frame with standard names for date and geo, 1 metric,
#      pre-ordered by date and geo.
df1 <- .SimulateGeoTimeseries(n.geos=n.geos, n.days=n.days, date.col=kDate,
                              geo.col=kGeo, metric.col=metric.df1, seed=SEED)
# df2: df1 + additional metrics and other columns
df2.conv <- data.frame(x=rpois(nrow(df1), lambda=100))
names(df2.conv) <- metrics.df2[2L]
df2.other <- data.frame(x="anything", y=rep(NA, nrow(df1)))
names(df2.other) <- other.df2
df2 <- data.frame(df1, df2.conv, df2.other)
# df3: A data frame with nonstandard names for date and geo.
df3 <- .SimulateGeoTimeseries(n.geos=n.geos, n.days=n.days, date.col="date2",
                              geo.col="geo2", metric.col=metric.df1, seed=SEED)

# Data frames representing GeoExperimentData.

n.ctrl.group <- floor(n.geos / 2)
group.1 <- sample.int(n.geos, size=n.ctrl.group)
# - dfged1: no 'assignment' column.
geo.groups <- list(group.1, setdiff(seq_len(n.geos), group.1))
experiment.dates <- list(as.Date(kDateFrom) + 14L, 35L, n.test.days, 28L)
dfged1 <- .MakeGeoExperimentData(df1, geo.groups=geo.groups,
                                 experiment.dates=experiment.dates)
# - dfged2: has an 'assignment' column.
dfged2 <- .MakeGeoExperimentData(df2, geo.groups=geo.groups,
                                 experiment.dates=experiment.dates)
dfged2 <- data.frame(dfged2, assignment=
                     ifelse(dfged2$geo.group %in% 2 & dfged2$period %in% 1,
                            yes=kTreatmentAssignment["change"],
                            no=kTreatmentAssignment["none"]),
                     stringsAsFactors=FALSE)
dfged2[[kAssignment]][is.na(dfged2[[kPeriod]])] <- NA_integer_
# - dfged3: much more geos.
n.ctrl.group.many <- floor(n.geos.many / 2)
group.1 <- sample.int(n.geos.many, size=n.ctrl.group.many)
geo.groups <- list(group.1, setdiff(seq_len(n.geos.many), group.1))
dfmany <- .SimulateGeoTimeseries(n.geos=n.geos.many, n.days=n.days,
                                 date.col=kDate, geo.col=kGeo,
                                 metric.col=metric.df1, seed=SEED)
dfged3 <- .MakeGeoExperimentData(dfmany, geo.groups=geo.groups,
                                 experiment.dates=experiment.dates)
dfged3 <- data.frame(dfged3, assignment=
                     ifelse(dfged3$geo.group %in% 2 & dfged3$period %in% 1,
                            yes=kTreatmentAssignment["change"],
                            no=kTreatmentAssignment["none"]),
                     stringsAsFactors=FALSE)
dfged3[[kAssignment]][is.na(dfged3[[kPeriod]])] <- NA_integer_
# Data sets for testing GBRROASAnalysisData:

dfgbr1.key <- kGeo
metric.resp.dfgbr1 <- metrics.df2[1L]
metric.cost.dfgbr1 <- "cost.clicks"
metrics.dfgbr1 <- c(metric.resp.dfgbr1, metric.cost.dfgbr1)

# Construct a data set representing GBRROASAnalysisData.
# Aggregate by geo.
dfgbr1 <- aggregate(as.data.frame(dfged2)[metric.resp.dfgbr1],
                    by=dfged2[c(kGeo, kPeriod)], FUN=base::sum)
dfgbr1[[metric.cost.dfgbr1]] <- ceiling(dfgbr1[[metric.resp.dfgbr1]] / 10)
#
dfgbr1.0 <- subset(dfgbr1, period %in% 1L, select=c(dfgbr1.key, metrics.dfgbr1))
names(dfgbr1.0) <- c(kGeo, kRespPre, kCostPre)
dfgbr1.1 <- subset(dfgbr1, period %in% 2L, select=c(dfgbr1.key, metrics.dfgbr1))
names(dfgbr1.1) <- c(kGeo, kRespTest, kCostTest)

dfgbr1 <- merge(dfgbr1.0, dfgbr1.1, by=dfgbr1.key)
.dfgbr1.control.geos <- unique(with(dfged2, geo[geo.group %in% 1]))
dfgbr1[[kControl]] <- with(dfgbr1, geo %in% .dfgbr1.control.geos)
dfgbr1 <- cbind(dfgbr1, other.column=NA, another="anything")
rownames(dfgbr1) <- NULL
