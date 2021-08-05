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
  expect_identical(test_result, expected_result)
})
