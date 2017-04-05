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

#' Constructs a \code{GeoTimeseries} object.
#'
#' @param x a \emph{plain} \code{data.frame} or an object that has to be
#' coercible to a plain \code{data.frame} with columns \code{date},
#' \code{geo}. \code{date} must be a \code{Date} object or a character vector
#' or factor coercible to Date, and 'geo' must be a character vector, factor,
#' or integer-valued. All columns that start with a dot (\code{.}) are
#' removed. If \code{geo} is an integer-valued numeric or factor, it is coerced
#' to character silently without error. If \code{date} is a character or
#' factor, it is silently coerced to \code{Date}. An error is output if the
#' date conversion fails.
#' @param metrics (character) column names that point to numeric columns. At
#' least one metric must be specified. All metrics have to be not all
#' \code{NA}.
#' @param date.format (string, optional) format of the column \code{date} in
#' the form understood by \code{as.Date()}. Used only if the \code{date} column
#' is of character type.
#'
#' @return A \code{GeoTimeseries} object, which is a \code{data.frame},
#' with the required columns,
#' \itemize{
#'   \item{\code{date}}: a vector of class \code{Date}.
#'   \item{\code{geo}}: a \code{character}-valued vector of Geo IDs.
#'   \item{\code{metrics}} : one or more numeric metrics. In addition, optionally
#'     any other user-definable columns. (These are ignored by the specified
#'     methods but may be convenient for the user). Three columns are added,
#'     for convenience:
#'     \itemize{
#'       \item{\code{.weekday}}: day number (1=Monday, 2=Tuesday, ..., 7=Sunday)
#'       \item{\code{.weeknum}}: week number in the year from 0 to 53.
#'       \item{\code{.weekindex}}: absolute index of week, year + weeknum
#'         (e.g., \code{201542})
#'     }
#' }
#' These should be convenient for generating totals and averages, using the
#' \code{aggregate} method. The columns 'date' and 'geo' form the primary
#' keys: it is guaranteed that no duplicate ('date', 'geo') pairs exist.
#'
#' The object includes fields stored in the attribute 'info':
#' \itemize{
#'   \item\code{metrics}: names of the metric columns.
#'   \item\code{other}: names of the other user-supplied columns.
#' }
#'

GeoTimeseries <- function(x, metrics=character(0), date.format="%Y-%m-%d") {
  kClassName <- "GeoTimeseries"
  SetMessageContextString(kClassName)
  on.exit(SetMessageContextString())

  # Attempt to coerce 'x' to a plain data frame.
  x <- try(as.data.frame(x), silent=TRUE)
  assert_that(is.data.frame(x),
              msg=Message("'x' must be a data.frame"))
  # Required columns.
  keys <- c(date=kDate, geo=kGeo)
  required <- keys
  # Silently remove any columns with a name starting with dots.
  if (any(dot.column <- grepl("^\\.", names(x)))) {
    x <- x[!dot.column]
  }
  # Ensure that all required columns are in the given data frame.
  CheckForMissingColumns(required, dataframe=x, what="required")
  # Ensure that at least one metric is specified.
  assert_that(length(metrics) > 0,
              msg=Message("At least one metric must be specified"))
  # Ensure that specified metrics are in the given data frame.
  CheckForMissingColumns(metrics, dataframe=x, what="specified")
  # Ensure that none of the specified metrics is one of the 'required' columns.
  CheckForAllTrue(structure(!(metrics %in% required), names=metrics),
                  msg="The specified column{|s} $X cannot be used as metrics.")
  # Coerce 'geo' to character.
  if (is.factor(x[[kGeo]]) || is.integer.valued(x[[kGeo]])) {
    x[[kGeo]] <- as.character(x[[kGeo]])
  }
  # Coerce 'date' to character if it is factor.
  if (is.factor(x[[kDate]])) {
    x[[kDate]] <- as.character(x[[kDate]])
  }
  # Coerce 'date' to Date if it is character.
  if (is.character(x[[kDate]])) {
    x[[kDate]] <- as.Date(x[[kDate]], format=date.format)
    CheckForBadValues(x, columns=kDate, CHECK=is.na, good=FALSE,
                      what="invalid date")
  }
  # Ensure that the required columns are of the specified type.
  required.type <- list(date=is.date,
                        geo=is.character.vector)
  names(required.type) <- keys[names(required.type)]
  CheckForTypes(x, checklist=required.type)
  # Ensure that all metrics are not all NA.
  is.not.all.NA <- sapply(x, function(z) {
                            !all(is.na(z))
                        })
  CheckForAllTrue(is.not.all.NA[metrics],
                  "Specified metric{|s} $X {is|are} all NA")
  # Ensure that all metrics are numeric.
  is.numeric.column <- sapply(x, is.numeric)
  CheckForAllTrue(is.numeric.column[metrics],
                  "Specified metric{|s} $X {is|are} not numeric")

  # Do not allow missing values in the required columns.
  CheckForBadValues(x, columns=required, CHECK=is.na, good=FALSE,
                    what="missing")
  # Check for empty values.
  CheckForBadValues(x, columns=kGeo, CHECK=is.empty.string,
                    good=FALSE, what="empty")
  # Ensure that the combinations are not duplicated.
  CheckForDuplicateRows(x, columns=keys)
  # Order by keys.
  x <- x[do.call(order, as.list(x[keys])), ]
  rownames(x) <- NULL
  # Set order of columns to keys, metrics, others.
  other.cols <- setdiff(names(x), c(keys, metrics))
  new.col.order <- c(keys, metrics, other.cols)
  # Add .weekday, .weeknum, .weekindex.
  date <- x[[keys["date"]]]
  df.addl <- data.frame(
      .GetWeekdays(date),
      .GetWeekNumbers(date),
      .GetWeekIndex(date))
  names(df.addl) <- c(kWeekday, kWeeknum, kWeekindex)
  obj <- data.frame(x[new.col.order], df.addl)
  # Add related information.
  obj <- SetInfo(obj,
                 metrics=metrics,
                 other=other.cols)
  if (!inherits(obj, kClassName)) {
    class(obj) <- c(kClassName, oldClass(obj))
  }
  return(obj)
}
