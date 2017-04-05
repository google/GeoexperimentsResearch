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

#' Constructor for Geos objects.
#'
#' @param x a data frame with column 'geo' and optionally other columns. Each
#'   'geo' must be unique.
#' @param volume name of the column that represents the 'volume' of the geo.
#'   The proportion of volume is computed. If omitted, the assumed volumes
#'   will be 1 for each geo. The volumes have to be non-negative and sum
#'   up to a positive value.
#'
#' @return An object of class 'Geos', which is a data frame with the column
#'   'geo', the column 'proportion' and 'volume', and other optional
#'   columns. The rows are sorted by the descending order of 'volume'.

Geos <- function(x, volume=NULL) {
  kClassName <- "Geos"
  SetMessageContextString(kClassName)
  on.exit(SetMessageContextString())

  CheckForMissingColumns(kGeo, dataframe=x, what="required")

  if (is.factor(x[[kGeo]])) {
    x[[kGeo]] <- as.character(x[[kGeo]])
  }
  checklist <- structure(list(is.character.vector), names=kGeo)
  CheckForTypes(x, checklist=checklist)

  # Ensure all geos are unique.
  CheckForDuplicateRows(x, columns=kGeo)

  # Disallow any missing values.
  CheckForBadValues(x, columns=names(x),
                    CHECK=is.na, good=FALSE, what="missing")

  if (length(volume) > 0) {
    assert_that(is.nonempty.string(volume))
    CheckForMissingColumns(volume, dataframe=x, what="specified")
    CheckForBadValues(x, columns=volume,
                      CHECK=function(z) { z < 0 }, good=FALSE, what="negative")
    vol <- x[[volume]]
    assert_that(sum(vol) > 0,
                msg=sprintf(paste("To be defined as a volume, column '%s'",
                                  "needs to sum up to a positive value"),
                            volume))
  } else {
    vol <- rep(1, length.out=nrow(x))
  }

  x[[kVolume]] <- vol
  proportion <- (vol / sum(vol))
  x[[kProportion]] <- proportion
  first.cols <- c(kGeo, kProportion, kVolume)
  x <- x[union(first.cols, names(x))]
  obj <- x[rev(order(vol)), , drop=FALSE]
  rownames(obj) <- NULL

  class(obj) <- c(kClassName, class(obj))
  return(obj)
}
