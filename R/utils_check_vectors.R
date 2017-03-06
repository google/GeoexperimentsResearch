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

is.treatment.assignment <- function(x) {
  # Tests each component of the vector 'x' for valid treatment assignment
  # symbols.
  #
  # Args:
  #   x : object to test.
  #
  # Returns:
  #   A logical vector of the same length as 'x', with 'TRUE' if and
  #   only if the string is a valid treatment assignment symbol, NA if
  #   the string is NA, otherwise FALSE.  If 'x' is not character,
  #   throws an error.
  #
  # Notes:
  #   NA in a component returns NA. These are supposed to be tested
  #   separately.

  assert_that((is.logical(x) && all(is.na(x))) || is.integer.valued(x))
  pass <- ifelse(is.na(x), yes=NA, no=x %in% kTreatmentAssignment)
  return(pass)
}

is.geo.group.number <- function(x) {
  # Tests each component of the vector 'x' for valid geo group number.
  #
  # Args:
  #   x : object to test.
  #
  # Returns:
  #   A logical vector of the same length as 'x', with 'TRUE' if and only if
  #   the component is nonnegative integer-valued numeric value, NA if the
  #   value is NA, otherwise FALSE.  If 'x' is not integer-valued, throws an
  #   error.
  #
  # Notes:
  #   NA in a component returns NA. These are supposed to be tested
  #   separately.
  #
  #   The value '0' has a special meaning: it indicates a geo that has been
  #   omitted from the experiment.

  assert_that((is.logical(x) && all(is.na(x))) || is.integer.valued(x))
  pass <- ifelse(is.na(x), yes=NA, no=(x >= 0L))
  return(pass)
}

is.period.number <- function(x) {
  # Tests each component of the vector 'x' for valid experiment period number.
  #
  # Args:
  #   x : object to test.
  #
  # Returns:
  #   A logical vector of the same length as 'x', with 'TRUE' if and only if
  #   the component is an integer-valued numeric value, NA if the value is NA,
  #   otherwise FALSE.  If 'x' is not integer-valued, throws an error.
  #
  # Notes:
  #   NA in a component returns NA. These are supposed to be tested separately.
  #   Negative period numbers are allowed.

  assert_that((is.logical(x) && all(is.na(x))) || is.integer.valued(x))
  pass <- ifelse(is.na(x), yes=NA, no=TRUE)
  return(pass)
}
