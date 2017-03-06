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

# General utilities.

RenameColumns <- function(x, map=character(0)) {
  # Renames columns of a data frame.
  #
  # Args:
  #   x : a data frame.
  #   map: a named character vector, mapping old column names to new
  #     ones such that the new ones are in 'names' and the values are
  #     the old ones. For example, \code{c(geo='GMA', date='Week
  #     Ending')}. If 'map' has length 0, the original data frame is
  #     returned.
  #
  # Returns:
  #   The data frame, with the column names renamed.

  SetMessageContextString("RenameColumns")
  on.exit(SetMessageContextString())

  assert_that(is.data.frame(x))
  assert_that(is.character(map) || is.null(map))
  if (length(map) == 0) {
    return(x)
  }
  assert_that(length(map) > 0,
              length(names(map)) == length(map),
              is.vector.of.nonempty.strings(names(map)),
              msg="'map' must be a named character vector")
  x.names <- names(x)
  there <- structure(map %in% x.names, names=map)
  assert_that(all(there),
              msg=Message(FormatText(!there,
                  "The following specified columns are not ",
                  "in the data frame: $X")))
  i.names <- which(x.names %in% map)
  inv.map <- structure(names(map), names=map)
  x.names[i.names] <- inv.map[x.names[i.names]]
  dups <- structure(duplicated(x.names), names=x.names)
  assert_that(!any(dups),
              msg=Message(FormatText(dups,
                  "Duplicated column names: $X")))
  names(x) <- x.names
  return(x)
}
