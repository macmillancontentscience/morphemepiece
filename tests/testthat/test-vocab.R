test_that("the vocab handler works", {
  definitely_not_a_vocab <- 1.5
  testthat::expect_error(.process_mp_vocab(definitely_not_a_vocab),
                         "Unsupported vocabulary")
})
