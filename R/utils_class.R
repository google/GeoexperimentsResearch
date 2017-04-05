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

#' Get a value of the 'info' attribute of an object.
#'
#' @param obj any R object.
#' @param field (string) name of the field the value of which to retrieve.
#' @return The requested value (object).
GetInfo <- function (obj, field) {
  info <- attr(obj, "info")
  if (missing(field)) {
    return(info)
  } else if (!(field %in% names(info))) {
    stop(sprintf("Field '%s' does not exist in this object of class '%s'",
                 field, class(obj)[1]))
  }
  return(info[[field]])
}

#' Set the values of the 'info' attribute of an object.
#'
#' @param obj some object.
#' @param ... name-value pairs to set in the list-valued attribute 'info'.
#' @param info the initial value of the 'info' attribute; if specified, will
#'   re-set the value before changing the individual name-value pairs.
#'
#' @note
#' Used only internally in this package. Do not use for data tables.
#' @return The object ('obj') that was changed.
SetInfo <- function (obj, ..., info=NULL) {
  assert_that(is.object(obj))
  if (is.null(info)) {
    info <- GetInfo(obj)
    if (is.null(info)) {
      info <- list()
    }
  }
  assert_that(is.plain.list(info) &&
              (length(info) == 0L || is.named.list(info)),
              msg="'info' is not a named list or an empty list")
  dots <- list(...)
  for (i in seq_along(dots)) {
    name <- names(dots)[i]
    info[name] <- dots[i]
  }
  attr(obj, "info") <- info
  return(obj)
}
