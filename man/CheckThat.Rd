% Copyright (C) 2017 Google, Inc.

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_assert.R
\name{CheckThat}
\alias{CheckThat}
\title{An alternative version of assert_that, outputting a modified error
message.}
\usage{
CheckThat(..., env = parent.frame(), name = NULL)
}
\arguments{
\item{...}{see 'assert_that'.}

\item{env}{see 'assert_that'.}

\item{name}{(string) name of the object to output in the error message.}
}
\value{
'TRUE' if tests pass; otherwise throws an error.
}
\description{
An alternative version of assert_that, outputting a modified error
message.
}
\note{
Replaces anything up to ' is not ' in the error message with the
value of 'name'.
}
\examples{
# Outputs "Column 'foo' is not an integer-valued numeric vector"
# if 'x' fails the test.
\dontrun{
CheckThat(is.integer.valued(x), name="Column 'foo'")}
}

