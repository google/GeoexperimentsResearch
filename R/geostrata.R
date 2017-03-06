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

GeoStrata <- function(geos, n.groups=2,
                      group.ratios=rep(1, length.out=n.groups)) {
  # Constructor for GeoStrata objects.
  #
  # Args:
  #   geos: a 'Geos' object.
  #   n.groups: number of groups. At least 2 and at most the number of geos.
  #   group.ratios: (integer vector of length n.groups) vector of ratios of the
  #     sizes of each group. By default each group is assumed to have equal
  #     ratios. The sum of these numbers also imply the size of a stratum. For
  #     example, c(2, 1) implies that group 1 should be 2 times larger than
  #     group 2, and the stratum size is 2 + 1 = 3. Note: the ratios do not
  #     have to be normalized to have greatest common divisor 1. For example,
  #     c(4, 2) implies that the ratio of group sizes is 2:1 but the stratum
  #     size is 4 + 2 = 6.
  #
  # Returns:
  #   A 'GeoStrata' object that inherits from 'Geos'. There is an extra column
  #   'stratum' that indicates the stratum number to be used in randomization,
  #   and column 'geo.group' for fixing the geo-to-group mapping.
  #
  # Notes:
  #   GeoStrata objects are used for (stratified) randomization of geos into
  #   groups. The geos are sorted by their 'volume' (definable by the user) and
  #   then divided into strata of size n.groups (column 'stratum'). This object
  #   has also a column 'geo.group', which offers the possibility to fix
  #   certain geos to certain groups. By default, this column is filled with
  #   NAs, indicating that none of the geos are mapped to any groups. The
  #   randomization itself is done by the method 'Randomize'.\cr
  #
  #   Any individual geo -> geo.group mappings should be fixed by using the
  #   'SetGeoGroup<-' method on a GeoStrata object.\cr
  #
  #   A stratum number '0' indicates a geo that is excluded from the scheme
  #   stratification. Any geo that is mapped to group '0' will have stratum
  #   number '0'; for example if geo 2 was omitted (geo.groups were NA, 0, NA,
  #   NA, NA, ...)  with group.ratios c(2, 1), the strata would be assigned as
  #   1, 0, 1, 1, 2, 2, 2, 3, 3, 3, 4, ...\cr
  #
  #   Setting 'group.ratios' to some other value than the default
  #   1,1,... enables creating groups that have different sizes. The stratum
  #   size is then determined by the sum of the number in 'group.ratios'. For
  #   example, group.ratios=c(1, 2) implies a ratio of 1:2. Each stratum has
  #   size 3; the 3 geos in this stratum are assigned a random sample with
  #   replacement from the set 1,2,2. Similarly, c(3, 1) implies that group 1
  #   will be on average 3 times as large as group 2.\cr
  #
  #   Note: the ratios do not have to be normalized to have greatest common
  #   divisor 1. For example, c(4, 2) implies that the ratio of group sizes is
  #   2:1 but the stratum size is 4 + 2 = 6.
  #
  # Documentation:
  #   seealso: 'Randomize', 'SetGeoGroup<-'.

  kClassName <- "GeoStrata"
  SetMessageContextString(kClassName)
  on.exit(SetMessageContextString())

  assert_that(inherits(geos, "Geos"))
  n.geos <- nrow(geos)
  assert_that(is.integer.valued(n.groups),
              n.groups >= 2,
              n.groups <= n.geos)
  assert_that(is.integer.valued(group.ratios),
              all(group.ratios >= 1),
              length(group.ratios) == n.groups,
              sum(group.ratios) <= n.geos)

  df.geos <- as.data.frame(geos)
  df.geos[[kGeoGroup]] <- NA_integer_
  df.geos[[kStratum]] <- .GenerateStrata(df.geos[[kGeoGroup]],
                                         group.ratios=group.ratios)

  first.cols <- c(kGeo, kStratum, kGeoGroup, kProportion, kVolume)
  obj <- df.geos[union(first.cols, names(geos))]

  obj <- SetInfo(obj, n.groups=n.groups, group.ratios=group.ratios)
  class(obj) <- c(kClassName, class(geos))
  return(obj)
}

.GenerateStrata <- function(geo.group, group.ratios) {
  # [internal] Generate stratum numbers along a given vector.
  #
  # Args:
  #   geo.group: (integer vector of length equal to number of geos) a
  #     vector of geo group numbers (may be NAs).
  #   group.ratios: (integer vector of length equal to number of geo groups)
  #     vector of ratios of the sizes of each group.
  #
  # Returns:
  #   An integer vector of the same length as 'geo.group', with the
  #   corresponding stratum numbers. '0' signifies a geo that is excluded from
  #   the set. Otherwise the numbers identify the strata. There are no NAs.
  #
  # Notes:
  #   For internal use; no checking of arguments is done.

  n.geos <- length(geo.group)
  stratum.size <- sum(group.ratios)

  strata <- rep(0L, length.out=n.geos)
  assign.to.strata <- which(!(geo.group %in% 0))
  n.assignable.geos <- length(assign.to.strata)
  if (n.assignable.geos >= 1) {
    n.strata <- ceiling(n.assignable.geos / stratum.size)
    strata[assign.to.strata] <- rep(seq_len(n.strata), each=stratum.size,
                                    length.out=n.assignable.geos)
  }
  return(strata)
}

IsFixedRandomization <- function(geostrata) {
  # Returns TRUE iff randomizing the geostrata can lead to multiple outcomes.
  #
  # Args:
  #   geostrata: a GeoStrata object.
  #
  # Returns:
  #   TRUE if randomizing the geostrata can only lead to a single GeoAssignment,
  #   FALSE otherwise.

  assert_that(inherits(geostrata, "GeoStrata"))

  group.ratios <- GetInfo(geostrata, "group.ratios")
  geos <- geostrata[geostrata[[kStratum]] != 0, , drop=FALSE]
  for(idx in geos[[kStratum]]) {
    stratum <- geos[[kGeoGroup]][geos[[kStratum]] == idx]
    count <- CountRandomizationsInAStratum(stratum,
                                           group.ratios=group.ratios,
                                           log.scale=TRUE)
    if (count != 0) {
      return(FALSE)
    }
  }
  return(TRUE)
}

CountRandomizations <- function(geostrata,
                                show.warnings=TRUE, log.scale=FALSE) {
  # Counts the total numbers of randomizations in a GeoStrata object.
  #
  # Args:
  #   geostrata: a GeoStrata object.
  #   show.warnings: (flag) if TRUE, shows a warning when a stratum is not
  #     compatible with the group.ratios.
  #   log.scale: (flag) if TRUE, returns the result on the log.scale.
  #
  # Returns:
  #   An integer vector of the same length as the number of strata in geostrata
  #   (excluding stratum 0, if it exists). The i-th coordinate corresponds to
  #   the total number of possible randomizations for the i-th stratum.
  #
  # Note:
  #   A warning is issued if one or more of the stratas are not compatible with
  #   the group.ratios.

  SetMessageContextString("CountRandomizations")
  on.exit(SetMessageContextString())

  assert_that(inherits(geostrata, "GeoStrata"))
  assert_that(is.flag(log.scale))

  group.ratios <- GetInfo(geostrata, "group.ratios")
  geos <- geostrata[geostrata[[kStratum]] != 0, , drop=FALSE]
  compatible <- tapply(X=geos[[kGeoGroup]],
                       INDEX=as.factor(geos[[kStratum]]),
                       FUN=IsStratumCompatibleWithRatios,
                       group.ratios=group.ratios,
                       simplify=FALSE)
  compatible <- as.vector(unlist(compatible))
  if (show.warnings && any(!compatible)) {
    warning(Message(FormatText(!compatible, "{Stratum|Strata} $w",
                               " {is|are} not compatible with group.ratios")))
  }
  count <- tapply(X=geos[[kGeoGroup]],
                  INDEX=as.factor(geos[[kStratum]]),
                  FUN=CountRandomizationsInAStratum,
                  group.ratios=group.ratios,
                  log.scale=log.scale,
                  simplify=FALSE)
  count <- as.vector(unlist(count))
  return(count)
}

IsStratumCompatibleWithRatios <- function(geo.group, group.ratios) {
  # Checks whether strata are compatible with group.ratios.
  #
  # Args:
  #   geo.group: (integer vector of length equal to number of geos) a
  #     vector of geo group numbers (may be NAs, but zeros are not allowed).
  #   group.ratios: (integer vector of length equal to number of geo groups)
  #     vector of ratios of the sizes of each group.
  #
  # Returns:
  #   TRUE if geo.group is compatible with group.ratios, i.e. no group has been
  #   assigned more geos than what group.ratios allows. FALSE otherwise.

  SetMessageContextString("IsStratumCompatibleWithRatios")
  on.exit(SetMessageContextString())

  n.groups <- length(group.ratios)
  unique.group <- unique(geo.group)
  groups.exist.or.na <- structure(
      ((unique.group) <= n.groups) | is.na(unique.group),
      names=paste0(unique.group))
  assert_that(all(groups.exist.or.na), msg=Message(
      FormatText(!groups.exist.or.na,
                 "Group{|s} $x {is|are} not compatible with",
                 " the specified group.ratios")))

  counts <- tabulate(geo.group, nbins=n.groups)
  unassigned.gr <- group.ratios - counts
  return(all(unassigned.gr >= 0))
}

CountRandomizationsInAStratum <- function(geo.group, group.ratios,
                                          log.scale=TRUE) {
  # Counts the total numbers of randomization in a single stratum.
  #
  # Args:
  #   geo.group: (integer vector of length equal to number of geos) a
  #     vector of geo group numbers (may be NAs, but zeros are not allowed).
  #   group.ratios: (integer vector of length equal to number of geo groups)
  #     vector of ratios of the sizes of each group.
  #   log.scale: (flag) if TRUE, returns the result on the log.scale.
  #
  # Returns:
  #   An integer value that corresponds to the total number of possible
  #   randomizations for the stratum represented by geo.group.

  SetMessageContextString("CountRandomizationsInAStratum")
  on.exit(SetMessageContextString())

  assert_that(all(geo.group != 0, na.rm=TRUE))  # Ignored geos were removed.
  assert_that(length(geo.group) <= sum(group.ratios))

  n.groups <- length(group.ratios)
  unique.group <- unique(geo.group)
  groups.exist.or.na <- structure(
      ((unique.group) <= n.groups) | is.na(unique.group),
      names=paste0(unique.group))
  assert_that(all(groups.exist.or.na), msg=Message(
      FormatText(!groups.exist.or.na,
                 "Group{|s} $x {is|are} not compatible with",
                 " the specified group.ratios")))

  counts <- tabulate(geo.group, nbins=n.groups)
  unassigned.gr <- group.ratios - counts
  .Choose <- function(x, amongst) {
    # Assign 'x' geos to groups whose ratios are specified via 'amongst'.
    available <- (amongst > 0)
    if (x == 1) {
      # Only one geo to assign, how many unique groups are there
      # still available?
      lresult <- log(sum(available))
    } else if (x == sum(amongst[available])) {
      # Multinomial formula.
      z <- amongst[available]
      lresult <- lfactorial(sum(z)) - sum(lfactorial(z))
    } else {
      # Need to assign fewer geos than there are of (non-unique) groups
      # available. For example, we need to assign 3 geos, with the
      # group.ratios being 4,2,1,1 (i.e. 1,1,1,1,2,2,3,4).
      # We count recursively, by assigning the first geo to a group, and
      # count how many randomizations there are, based on what group was
      # chosen.
      lresult <- 0
      for(i in seq_along(amongst)) {
        if (available[i] > 0) {
          lresult <- lresult + Recall(x - 1,
                                      amongst - (seq_along(amongst) == i))
        }
      }
    }
    return(lresult)
  }
  lresult <- .Choose(x=sum(is.na(geo.group)), amongst=unassigned.gr)
  if (log.scale) {
    return(lresult)
  }
  return(exp(lresult))
}
