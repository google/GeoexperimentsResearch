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

#' Constructs a TBRAnalysisData object.
#'
#' @param x a _plain_ \code{data.frame} or an object that has to be coercible
#' to a _plain_ \code{data.frame} with the required columns \code{data}, which
#' _must_ be a \code{Date} vector; integer-valued column \code{period}
#' indicating the pre-experiment, pretest, intervention, cooldown, and posttest
#' periods; numeric columns \code{y} (response), \code{x}
#' (covariate). Optionally the data frame can have other columns, which are
#' ignored but whose names cannot start with a dot ('.'). No missing values are
#' allowed in the pretest and test periods (but allowed outside of either
#' period).#'
#' @return A \code{TBRAnalysisData} object, which is similarly a
#' \code{data.frame} with the columns:
#'
#' \code{TBRAnalysisData} is a \code{data.frame}, with the required columns,
#' \itemize{
#'   \item{\code{date}}: a 'Date'-valued vector.
#'   \item{\code{period}}: numeric indicator of various periods.
#'     \code{0} indicates the pretest period. \code{NA} indicates a date
#'     that has been excluded from the analyses.
#'   \item{\code{y}}: numeric values of the metric of the Treatment group.
#'   \item{\code{x}}: numeric values of the metric in the Control group.
#'     In addition, optionally any other user-definable columns.
#' }
#'
#' @note The column \code{date} is the primary key; it is guaranteed that no
#' rows with duplicate geos exist and that the rows are in temporal order.

TBRAnalysisData <- function(x) {
  kClassName <- "TBRAnalysisData"
  SetMessageContextString(kClassName)
  on.exit(SetMessageContextString())

  # Attempt to coerce into a plain data.frame.
  x <- try(as.data.frame(x), silent=TRUE)
  assert_that(is.data.frame(x),
              msg=Message("'x' must be a data.frame"))

  # Reject any 'x' with column names starting with dots.
  CheckForAllTrue(structure(!grepl("^\\.", names(x)), names=names(x)),
                  "Columns with names starting with a dot ",
                  "are not allowed: $X")
  # Specify type of each column.
  checklist <- list(is.date, is.numeric, is.numeric.vector,
                    is.numeric.vector)
  names(checklist) <- c(kDate, kPeriod, kY, kX)
  # Check that all required columns are there.
  required.columns <- names(checklist)
  CheckForMissingColumns(required.columns, dataframe=x)
  # Check types of each required column.
  CheckForTypes(x, checklist=checklist)
  # Check for missing values in date.
  CheckForBadValues(x, columns=kDate, CHECK=is.na, good=FALSE, what="missing")
  in.analysis <- (x[[kPeriod]] %in% kStandardPeriods[["experiment"]])
  .IsMissing <- function(z) {
    # Allow missing values in metrics outside the experiment periods.
    return(in.analysis & is.na(z))
  }
  CheckForBadValues(x, columns=c(kY, kX), CHECK=.IsMissing, good=FALSE,
                    what="missing")
  # Check for duplicate dates.
  CheckForDuplicateRows(x, columns=kDate)
  # Check for integrity of column 'period'.
  assert_that(all(is.period.number(x[[kPeriod]]) %in% c(TRUE, NA)),
              msg=Message("There are invalid period numbers"))
  assert_that(all(diff(x[[kPeriod]]) >= 0, na.rm=TRUE),
              msg=Message("Periods must be consecutive"))
  assert_that(all(unlist(kStandardPeriods[c("pretest", "intervention")])
                  %in% x[[kPeriod]]),
              msg="Both pretest and intervention periods must be present")
  # Sort the rows by date.
  x <- x[order(x[[kDate]]), ]
  rownames(x) <- NULL
  # Set order of columns to: date, y, x, others.
  new.col.order <- union(required.columns, names(x))
  obj <- x[new.col.order]
  class(obj) <- c(kClassName, oldClass(obj))
  return(obj)
}
