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

context("Set/GetMessageContextString")

test_that("by default, message context string is ''", {
  options(geoexperiments=NULL)
  expect_identical(GetMessageContextString(), "")
  options(geoexperiments=list())
  expect_identical(GetMessageContextString(), "")
})

test_that("setting the message context strings works like a fifo stack", {
  options(geoexperiments=list())
  string <- c("Foo", "Bar")
  SetMessageContextString(string[1L])
  expect_identical(GetMessageContextString(), string[1L])
  SetMessageContextString(string[2L])
  string2 <- paste(string, collapse=">")
  expect_identical(GetMessageContextString(), string2)
  SetMessageContextString()
  expect_identical(GetMessageContextString(), string[1L])
  SetMessageContextString()
  expect_identical(GetMessageContextString(), "")
})

test_that("subsequent resetting of message context strings have no effect", {
  options(geoexperiments=list())
  string <- "Foo"
  SetMessageContextString(string)
  expect_identical(GetMessageContextString(), string)
  SetMessageContextString()
  expect_identical(GetMessageContextString(), "")
  SetMessageContextString()
  expect_identical(GetMessageContextString(), "")
})

test_that("Message() shows the context string", {
  options(geoexperiments=list())
  expect_identical(Message("msg"), "msg")
  string <- "Foo"
  SetMessageContextString(string)
  expect_identical(Message("msg"),
                   paste0(string, ": msg"))
  SetMessageContextString()
  expect_identical(Message("msg"), "msg")
})

test_that("Messagef() works like sprintf", {
  options(geoexperiments=list())
  expect_identical(Messagef("n=%d.", 314L), "n=314.")
  string <- "Foo"
  SetMessageContextString(string)
  expect_identical(Messagef("n=%d.", 314L),
                   paste0(string, ": n=314."))
  SetMessageContextString()
  expect_identical(Messagef("n=%d.", 314L), "n=314.")
})
