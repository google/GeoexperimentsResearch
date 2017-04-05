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

#' Save a string representing the current context of the program flow.
#'
#' @param context (a string) context of the message; to be displayed in (error)
#'   messages.
#' @return The previous message context strings (a character vector).

SetMessageContextString <- function(context) {
  opt <- getOption("geoexperiments", default=list())
  context.old <- opt[["context"]]
  if (is.null(context.old)) {
    opt.context <- character(0)
  } else {
    opt.context <- context.old
  }
  if (missing(context)) {
    opt.context <- opt.context[-1L]
  } else {
    assert_that(is.nonempty.string(context),
                msg="'context' must be a nonempty string")
    opt.context <- c(context, opt.context)
  }
  opt["context"] <- list(opt.context)
  options(geoexperiments=opt)
  invisible(context.old)
}

#' Get the current message context string.
#'
#' @return The current message context string. If not available, returns an
#'   empty string ("").
#'
#' @note
#' Pastes the strings together with '>'

GetMessageContextString <- function() {
  kSeparator <- ">"
  context.strings <- getOption("geoexperiments", default=list())[["context"]]
  if (length(context.strings) == 0L) {
    context.string <- ""
  } else {
    context.string <- paste(rev(context.strings), collapse=kSeparator)
  }
  return(context.string)
}

#' Form a message, prefixed with the message context string.
#'
#' @param ... R vectors, usually character, to be collapsed using 'paste0'.
#' @return A string.

Message <- function(...) {
  prefix <- GetMessageContextString()
  if (nchar(prefix) > 0L) {
    prefix <- paste0(prefix, ": ")
  }
  paste0(prefix, ..., collapse="")
}

#' Form a message, using sprintf.
#'
#' @param fmt a character vector of format strings (see 'sprintf' for details).
#' @param ... values to be passed to 'fmt'.
#' @return A string.

Messagef <- function(fmt, ...) {
  assert_that(is.nonempty.string(fmt))
  Message(sprintf(fmt, ...))
}
