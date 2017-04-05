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

#' Randomize geos to groups.
#'
#' @param obj an object.
#' @param ... arguments passed to other methods.
#'
#' @return A \code{GeoAssignment} object.
#'
#' @rdname Randomize
Randomize <- function(obj, ...) {
  UseMethod("Randomize")
}

#' Randomize geos, assigning geo groups by strata.
#'
#' @param n.groups (integer) number of groups.
#' @param group.ratios (integer vector of length n.groups) vector of ratios of
#'   the sizes of each group. By default each group is assumed to have
#'   equal ratios.
#'
#' @return A \code{GeoAssignment} object.
#'
#' @rdname Randomize
Randomize.Geos <- function(obj, n.groups=2,
                           group.ratios=rep(1, length.out=n.groups), ...) {
  SetMessageContextString("Randomize.Geos")
  on.exit(SetMessageContextString())

  geo.strata <- GeoStrata(obj, n.groups=n.groups, group.ratios=group.ratios)
  obj.result <- Randomize(geo.strata)
  return(obj.result)
}

#' @rdname Randomize
Randomize.GeoStrata <- function(obj, ...) {
  SetMessageContextString("Randomize.GeoStrata")
  on.exit(SetMessageContextString())

  n.groups <- GetInfo(obj, "n.groups")
  n.geos <- nrow(obj)
  strata <- obj[[kStratum]]
  geo.group <- obj[[kGeoGroup]]
  pre.assigned <- (!is.na(geo.group))

  group.ratios <- GetInfo(obj, "group.ratios")
  group.numbers <- rep(seq_len(n.groups), times=group.ratios)

  if (!any(pre.assigned)) {
    # None of the geos have been pre-assigned to any groups. Assign all geos at
    # once.
    for (i in unique(strata)) {
      j <- which(strata %in% i)  # Length <= n.groups.
      groups <- sample(group.numbers)  # Randomize.
      geo.group[j] <- groups[seq_along(j)]
    }
  } else {
    # Some of the geos have been pre-assigned. Work through the strata
    # separately to ensure that the remaining geos within each stratum are
    # correctly assigned to the remaining group ids.
    for (i in unique(strata)) {
      j <- which(strata %in% i)  # Length <= n.groups.
      groups <- sample(group.numbers)  # Randomize.
      assignment <- geo.group[j]
      not.assigned <- is.na(assignment)
      if (all(not.assigned)) {
        # None of the assignments are pre-assigned.
        # The strata are each of length n.groups but as an exception,
        # the last strata might be shorter, hence use seq_along(j)
        # instead of seq_len(n.groups).
        geo.group[j] <- groups[seq_along(j)]
      } else if (any(not.assigned)) {
        # Some geos are not yet assigned.
        assigned.groups <- geo.group[j[!not.assigned]]
        groups <- groups[!(groups %in% assigned.groups)]
        k <- j[not.assigned]  # Index of those that are not assigned.
        geo.group[k] <- groups[seq_along(k)]
      }  # or else: All geos in a stratum are fixed. Nothing to do.
    }
  }
  df.geo.assign <- as.data.frame(obj)
  df.geo.assign[[kGeoGroup]] <- geo.group
  obj.result <- GeoAssignment(df.geo.assign)
  return(obj.result)
}
