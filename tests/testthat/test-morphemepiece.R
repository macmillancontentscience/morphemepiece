# Copyright 2021 Bedford Freeman & Worth Pub Grp LLC DBA Macmillan Learning.
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

test_that("lookup words tokenize as expected.", {
  # I don't want to have to deal with file paths, so we generate and then kill
  # the vocab and lookup. Ideally this should happen overall for the test suite.
  letter_pairs <- paste0("##", as.vector(outer(letters, letters, FUN = paste0)))
  # remove most pairs that end in "s"
  letter_pairs <- letter_pairs[!grepl("##[^sc]s", letter_pairs)]
  vocab_text <- c(
    "[UNK]",
    ".",
    "##",
    "un##",
    "affable",
    "able",
    "like",
    "fox",
    "run",
    "##ing",
    letters,
    paste0("##", letters),
    letter_pairs
  )
  temp_vocab_path <- tempfile(
    pattern = "vocab",
    fileext = ".txt"
  )
  writeLines(vocab_text, temp_vocab_path)
  on.exit(unlink(temp_vocab_path))
  vocab <- load_vocab(temp_vocab_path)

  lookup_text <- c(
    "foxes fox ##s",
    "running run ##ing"
  )
  temp_lookup_path <- tempfile(
    pattern = "lookup",
    fileext = ".txt"
  )
  writeLines(lookup_text, temp_lookup_path)
  on.exit(unlink(temp_lookup_path), add = TRUE)
  lookup <- load_lookup(temp_lookup_path)

  test_result <- morphemepiece_tokenize(
    text = c("unaffable unable foxes running.", "affable able fox runs"),
    vocab = vocab,
    lookup = lookup
  )
  expected_result <- list(
    c(
      `un##` = 3L, affable = 4L, `un##` = 3L, able = 5L, fox = 7L, `##s` = 54L,
      run = 8L, `##ing` = 9L, `.` = 1L
    ),
    c(affable = 4L, able = 5L, fox = 7L, run = 8L, `##s` = 54L)
  )
  testthat::expect_identical(test_result, expected_result)

  # Can also test using the imported morphemepiece.data vocab + lookup
  test_result <- morphemepiece_tokenize("chairball")
  # Manually construct the expected result from the vocab.
  expected_result <- list(
    c(
      morphemepiece_vocab()["chair"],
      morphemepiece_vocab()["##"],
      morphemepiece_vocab()["ball"]
    )
  )
  testthat::expect_identical(test_result, expected_result)

  # test max_chars
  test_result <- morphemepiece_tokenize("longfakewordxzz", max_chars = 7L)
  expected_result <- list(c(`[UNK]` = 1L))
  testthat::expect_identical(test_result, expected_result)

  # Find a word that tests the backwards run of the fall-through...
  test_result <- morphemepiece_tokenize("unarcher")
  # Manually construct the expected result from the vocab.
  expected_result <- list(
    c(
      morphemepiece_vocab()["un##"],
      morphemepiece_vocab()["archer"]
    )
  )
  testthat::expect_identical(test_result, expected_result)
})

test_that("Corner cases are handled properly.", {
  vocab <- morphemepiece_vocab()

  test_result <- morphemepiece_tokenize(text = "")
  expected_result <- list(structure(integer(0), .Names = character(0)))
  expect_identical(test_result, expected_result)

  test_result <- morphemepiece_tokenize(text = c("two word", "", "that"))
  expected_result <- list(
    vocab[c("two", "word")],
    structure(integer(0), .Names = character(0)),
    vocab["that"]
  )
  expect_identical(test_result, expected_result)
})
