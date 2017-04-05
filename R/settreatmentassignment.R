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

#' Associates the object with an TreatmentAssignment object.
#'
#' @param obj the object to change.
#' @param ... further arguments passed to methods.
#' @param value a TreatmentAssignment object.
#'
#' @return The object (that has been modified in place).
#'
#' @rdname SetTreatmentAssignment

"SetTreatmentAssignment<-" <- function(obj, ..., value) {
  UseMethod("SetTreatmentAssignment<-")
}

#' @param strict (flag) insist that data has all treatment combinations?
#'
#' @note The value pairs ('period', 'geo.group') are mapped to a (treatment)
#' 'assignment' column.
#'
#' If 'strict' is TRUE, throws an error if some treatment assignments
#' are not found in the data. If 'strict' is FALSE, no warnings or errors
#' are thrown.
#'
#' @rdname SetTreatmentAssignment
"SetTreatmentAssignment<-.GeoExperimentData" <- function(obj, strict=TRUE, ...,
                                                         value) {
  SetMessageContextString("SetTreatmentAssignment<-.GeoExperimentData")
  on.exit(SetMessageContextString())

  assert_that(is.flag(strict))
  assert_that(inherits(value, "TreatmentAssignment") || is.null(value),
              msg=Message(
                  "A TreatmentAssignment object or 'NULL' was expected"))

  obj.treat.assignment <- value
  info <- GetInfo(obj)
  if (is.null(obj.treat.assignment) ||
      is.null(GetInfo(obj, "periods")) ||
      is.null(GetInfo(obj, "geo.assignment"))) {
    # Not enough information: assignment will remain undefined.
    assignment <- NA_integer_
  } else {
    key <- c(kPeriod, kGeoGroup)
    df.period.group <- as.data.frame(obj[key])
    df.period.group[[kTmpi]] <- seq_len(nrow(obj))
    df.trt <- as.data.frame(obj.treat.assignment)
    df.trt[[kTmpj]] <- seq_len(nrow(df.trt))
    df.m <- merge(df.period.group, df.trt, by=key, all.x=TRUE, all.y=FALSE)
    # Restore original order.
    df.m <- df.m[order(df.m[[kTmpi]]), , drop=FALSE]
    assignment <- df.m[[kAssignment]]
    # Find out which ('period', 'geo.group') combinations were not
    # mapped.
    specified <- (!is.na(df.m))  # A matrix.
    key.specified <- (specified[, kPeriod] & specified[, kGeoGroup])
    assigned <- specified[, kAssignment]
    needs.default <- (key.specified & !assigned)
    if (any(needs.default)) {
      assignment[needs.default] <- kTreatmentAssignment["none"]
    }
    excluded <- (obj[[kGeoGroup]] %in% kExcludeGeoGroup)
    assignment[excluded] <- kTreatmentAssignment["exclude"]
  }
  obj[[kAssignment]] <- assignment
  obj <- SetInfo(obj, treat.assignment=obj.treat.assignment)
  return(obj)
}
