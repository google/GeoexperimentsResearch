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

#' Constructs a GBRROASAnalysisData object.
#'
#' @param x a _plain_ 'data.frame' or an object that has to be coercible to a
#'   _plain_ 'data.frame' with the required columns 'geo', which is a
#'   character vector; numeric columns 'resp.pre', 'resp.test',
#'   'cost.pre', 'cost.test', and a logical (TRUE/FALSE) column 'control'.
#'   Optionally the data frame can have other columns, whose names can not
#'   start with a dot ('.'). No missing values are allowed.
#' @return A GBRROASAnalysisData object, which is similarly a 'data.frame'
#'   with the same columns as described above.
#'
#' @note
#' 'GBRROASAnalysisData' is a 'data.frame', with the required columns,
#' \itemize{
#' \item\code{geo} a 'character'-valued vector of Geo IDs.
#' \item\code{resp.pre} numeric values of the response metric in the
#' pre-test period.
#' \item\code{resp.test} numeric values of the response metric in the test
#' period.
#' \item\code{cost.pre} numeric values of the cost metric in the pre-test
#' period.
#' \item\code{cost.test} numeric values of the response metric in the test
#' period.
#' \item\code{control} : indicator ('TRUE'/'FALSE') of which columns are
#' in the Control group.} In addition, optionally any other user-definable
#' columns. The column 'geo' is the primary key; it is guaranteed that no
#' rows with duplicate geos exist. The object includes the following
#' fields stored in the attribute 'info':
#' \itemize{
#'   \item\code{keys}: names of the primary key columns ('geo').
#'     \item\code{required}: names of the required columns.
#'     \item\code{metrics}: names of the columns representing metric data.
#' }
#'
#' @rdname gbrroasanalysisdata
GBRROASAnalysisData <- function(x) {
  kClassName <- "GBRROASAnalysisData"
  SetMessageContextString(kClassName)
  on.exit(SetMessageContextString())
  # Attempt to coerce to a plain data.frame.
  x <- try(as.data.frame(x), silent=TRUE)
  assert_that(is.data.frame(x),
              msg=Message("'x' must be a data.frame"))
  # Reject any 'x' with column names starting with dots.
  CheckForAllTrue(structure(!grepl("^\\.", names(x)), names=names(x)),
                  "Columns with names starting with a dot ",
                  "are not allowed: $X")
  # Names for each column.
  kGeo <- "geo"
  kControl <- "control"
  kPrePostMetrics <- c("resp.test", "resp.pre", "cost.test", "cost.pre")
  # Specify type of each column. Use only functions that work well
  # with assert_that for more informative messages.
  checklist <- list(is.character.vector, is.logical.vector, is.numeric.vector,
                    is.numeric.vector, is.numeric.vector, is.numeric.vector)
  names(checklist) <- c(kGeo, kControl, kPrePostMetrics)
  # Check that all required columns are there.
  required.columns <- names(checklist)
  CheckForMissingColumns(required.columns, dataframe=x)
  # Check types of each required column.
  CheckForTypes(x, checklist=checklist)
  # Check for missing values in all columns.
  CheckForBadValues(x, columns=c(required.columns, kPrePostMetrics),
                    CHECK=is.na, good=FALSE, what="missing")
  # Check for duplicate keys.
  CheckForDuplicateRows(x, columns=kGeo)
  # Re-order the rows.
  sort.by <- c(kGeo, kControl)
  x <- x[do.call(order, as.list(x[sort.by])), ]
  rownames(x) <- NULL
  # Set order of columns to keys, metrics, others.
  other.cols <- setdiff(names(x), required.columns)
  new.col.order <- c(required.columns, other.cols)
  obj <- x[new.col.order]
  # Add related information.
  obj <- SetInfo(obj,
                 keys=kGeo,
                 attr=kControl,
                 metrics=kPrePostMetrics)
  class(obj) <- c(kClassName, oldClass(obj))
  return(obj)
}
