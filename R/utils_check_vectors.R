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

#' Tests each component of the vector 'x' for valid values.
#'
#' @param x object to test.
#'
#' @return A logical vector of the same length as 'x', with 'TRUE' if and only
#' if the string is a valid value, NA if the string is NA, otherwise FALSE. If
#' 'x' is not character, throws an error.
#'
#' @note
#' NA in a component returns NA. These are supposed to be tested
#' separately.
#'
#' \itemize{
#'   \item{\code{is.treatment.assignment}} The valid treatment assignment
#'     symbols can be found in the constant \code{kTreatmentAssignment}.
#'     Note however that the treatment assignment symbols are not used
#'     in the current version of the package (included for possible future
#'     use).
#'   \item{\code{is.geo.group.number}} The value '0' has a special meaning:
#'    it indicates a geo that has been omitted from the experiment.
#'   \item{\code{is.period.number}} Negative period numbers are allowed.
#' }
#'
#' @rdname utils_check_vectors
is.treatment.assignment <- function(x) {
  assert_that((is.logical(x) && all(is.na(x))) || is.integer.valued(x))
  pass <- ifelse(is.na(x), yes=NA, no=x %in% kTreatmentAssignment)
  return(pass)
}

#' @rdname utils_check_vectors
is.geo.group.number <- function(x) {
  assert_that((is.logical(x) && all(is.na(x))) || is.integer.valued(x))
  pass <- ifelse(is.na(x), yes=NA, no=(x >= 0L))
  return(pass)
}

#' @rdname utils_check_vectors
is.period.number <- function(x) {
  assert_that((is.logical(x) && all(is.na(x))) || is.integer.valued(x))
  pass <- ifelse(is.na(x), yes=NA, no=TRUE)
  return(pass)
}
