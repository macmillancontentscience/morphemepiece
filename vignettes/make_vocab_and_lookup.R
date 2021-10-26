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

# Creates a morphemepiece vocabulary and lookup table, given a processed
# wiktionary dump and an existing wordpiece vocabulary.
# Optionally, a word frequency table from some corpus can be given.
#

# Throughout this code, I've followed the "begin internal functions with '.'"
# convention to indicate thoughts about eventual internal/exported functions.
# For now these will remain just in the vignette, but we should slowly migrate
# them into the package (with refinement wherever we made some very specific
# opinionated decisions).

make_vocab_and_lookup <- function(full_lookup,
                                  full_vocabulary = wikimorphemes::wiktionary_word_list(),
                                  wordpiece_vocab = wordpiece.data::wordpiece_vocab(),
                                  target_vocab_size = 30000L,
                                  word_frequency_table = NULL) {
  full_lookup <- .add_words_to_lookup(full_lookup, full_vocabulary)

  unnested_lookup <- .unnest_lookup(full_lookup, clean = TRUE)
  token_counts <- count_tokens(
    unnested_lookup,
    word_frequency_table
  )

  # punctuation, numbers, [CLS], etc. from wordpiece vocab
  fancy_wp_tokens <- .get_fancy_wp_tokens(wordpiece_vocab)

  actual_wp_words <- .get_actual_wp_words(full_lookup, wordpiece_vocab)
  # short actual words from wp will get a VIP pass directly to vocab.
  # Though most of them wouldn't be considered words by most people. :)
  short_words <- actual_wp_words[nchar(actual_wp_words) <= 3]
  rest_of_words <- actual_wp_words[nchar(actual_wp_words) > 3]
  # make sure we keep enough tokens to cover the rest of the wp words
  processed_tokens <- unnested_lookup %>%
    dplyr::filter(.data$word %in% rest_of_words) %>%
    dplyr::distinct(.data$token) %>%
    dplyr::pull(.data$token)

  # we *didn't* clean the processed tokens before this, so clean them now.
  # This mainly filters out a few capitalized proper nouns that sneaked back in,
  # but we'll recover the lowercase version of those later.
  processed_tokens <- processed_tokens[stringr::str_detect(processed_tokens,
                                                           pattern = "[^a-z#]",
                                                           negate = TRUE)]

  wp_proper_nouns <- .get_wp_proper_nouns(full_lookup, wordpiece_vocab)
  # There's a lot of overlap with "actual" words (e.g. apple). This
  # *shouldn't* make a difference, but just to be sure, filter those out:
  wp_proper_nouns <- setdiff(wp_proper_nouns, actual_wp_words)

  # still need to add in letters, letter pairs for completeness.
  letter_pairs <- paste0("##", as.vector(outer(letters, letters, FUN = paste0)))
  # remove most pairs that end in "s"
  letter_pairs <- letter_pairs[!grepl("##[^sc]s", letter_pairs)]

  supplemental_tokens <- c(
    paste0("##", letters),
    letters,
    letter_pairs
  )

  vocab <- c(
    "[PAD]", # make sure this one is first.
    fancy_wp_tokens,
    short_words,
    processed_tokens,
    wp_proper_nouns,
    supplemental_tokens
  ) %>%
    piecemaker::validate_utf8() %>%
    unique()

  if (length(vocab) < target_vocab_size) {
    # Fill up the difference with top tokens, as judged by rank in token_counts.
    # Maybe later be more sophisticated about proper nouns.
    n_tokens_to_add <- target_vocab_size - length(vocab)
    additional_tokens <- token_counts %>%
      dplyr::filter(!(.data$token %in% vocab)) %>%
      dplyr::arrange(.data$rank) %>%
      utils::head(n_tokens_to_add) %>%
      dplyr::pull(.data$token)
    vocab <- unique(c(vocab, additional_tokens))
  }
  lookup <- .make_lookup(unnested_lookup, vocab)
  return(list(vocab = vocab, lookup = lookup))
}

.add_words_to_lookup <- function(full_lookup, full_vocabulary) {
  missing_words <- dplyr::tibble(word = full_vocabulary) %>%
    dplyr::anti_join(
      dplyr::distinct(full_lookup, .data$word),
      by = "word"
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      morphemes = list(c(base_word = .data$word)),
      n_morphemes = 1L
    ) %>%
    dplyr::ungroup()

  return(
    dplyr::distinct(
      dplyr::bind_rows(
        full_lookup,
        missing_words
      )
    )
  )
}

.unnest_lookup <- function(lookup, clean = TRUE) {
  if (clean) { # Clean out characters that aren't lowercase latin.
    lookup <- lookup %>%
      # Replace -- with -. Somehow some made it through. Also get rid of
      # diacritics.
      dplyr::mutate(
        word = stringr::str_replace_all(.data$word, stringr::fixed("--"), "-"),
        word = piecemaker::remove_diacritics(.data$word)
      ) %>%
      dplyr::filter(
        # Get rid of any remaining words that contain anything other than letters
        # stringr::str_starts(.data$word, "[A-Za-z]")
        !stringr::str_detect(.data$word, "[^a-z]"),
        !(.data$word == "")
      ) %>%
      dplyr::distinct()

    # Do a similar process in the morphemes. This can be rough because we don't
    # want to lose the names.
    lookup <- lookup %>%
      dplyr::mutate(
        morphemes = purrr::map(
          .data$morphemes,
          function(these_morphemes) {
            to_return <- piecemaker::remove_diacritics(tolower(these_morphemes))
            names(to_return) <- names(these_morphemes)
            return(to_return)
          }
        ),
        has_special = purrr::map_lgl(
          .data$morphemes,
          function(these_morphemes) {
            any(stringr::str_detect(these_morphemes, "[^a-z-]"))
          }
        )
      ) %>%
      dplyr::filter(!.data$has_special) %>%
      dplyr::select(-.data$has_special) %>%
      dplyr::distinct()

    # Keep the version of the word with the most morphemes. In case of a tie,
    # just keep the first one.
    lookup <- lookup %>%
      dplyr::group_by(.data$word) %>%
      dplyr::filter(.data$n_morphemes == max(.data$n_morphemes)) %>%
      dplyr::ungroup() %>%
      dplyr::distinct(.data$word, .keep_all = TRUE)
  }
  if (!nrow(lookup)) {
    return(
      dplyr::tibble(
        word = character(0),
        token = character(0),
        morpheme_type = character(0)
      )
    )
  }

  unnested_lookup <- tidyr::unnest_longer(
    data = lookup,
    col = .data$morphemes,
    indices_to = "morpheme_type"
  ) %>%
    dplyr::filter(.data$morphemes != "") %>%
    dplyr::mutate(
      before = ifelse(
        test = .data$morpheme_type %in% c("suffix", "inflection", "interfix"),
        yes = "##",
        no = ""
      ),
      after = ifelse(
        test = .data$morpheme_type == "prefix",
        yes = "##",
        no = ""
      ),
      morphemes = stringr::str_remove_all(.data$morphemes, "-"),
      token = paste0(.data$before, .data$morphemes, .data$after)
    ) %>%
    dplyr::select(.data$word, .data$token, .data$morpheme_type)

  return(unnested_lookup)
}

.unnest_lookup <- memoise::memoise(.unnest_lookup)

# Eventually we'll want to clean this one up and export it, I think.
count_tokens <- function(unnested_lookup,
                         word_frequency_table = NULL) {
  # if word_frequency_table is supplied, weight by word freq
  if (!is.null(word_frequency_table)) {
    unnested_lookup <- dplyr::left_join(
      unnested_lookup,
      word_frequency_table,
      by = "word"
    ) %>%
      dplyr::mutate(
        word_count = as.integer(.data$word_count),
        word_count = ifelse(
          test = is.na(.data$word_count),
          1L,
          .data$word_count
        )
      )
  } else {
    unnested_lookup <- unnested_lookup %>%
      dplyr::mutate(word_count = 1L)
  }
  token_counts <- unnested_lookup %>%
    dplyr::group_by(.data$token) %>%
    dplyr::summarize(token_score = sum(.data$word_count)) %>%
    dplyr::arrange(dplyr::desc(.data$token_score)) %>%
    dplyr::mutate(rank = dplyr::row_number()) %>%
    dplyr::filter(!stringr::str_detect(.data$token, "[^a-z#]"))

  return(token_counts)
}

# token_counts <- get_token_counts(unnested_processed_wiktionary)
# token_counts <- get_token_counts(unnested_processed_wiktionary, word_freq_tab)

.get_fancy_wp_tokens <- function(wp_vocab) {
  # exclude [unused]
  is_fancy <- stringr::str_detect(names(wp_vocab), pattern = "[^a-z#]")
  is_unused <- stringr::str_detect(names(wp_vocab), pattern = "\\[unused")
  # note that this excludes tokens "#", "##"... add those back in
  return(c(names(wp_vocab[is_fancy & !is_unused]), "#", "##"))
}

.get_actual_wp_words <- function(full_lookup, wp_vocab) {
  # wordpieces that have a wiktionary entry
  clean_wik <- full_lookup %>%
    dplyr::filter(!stringr::str_detect(.data$word, "[^a-z]")) %>%
    dplyr::pull(.data$word)
  in_wik <- names(wp_vocab) %in% clean_wik
  return(names(wp_vocab[in_wik]))
}

.get_wp_proper_nouns <- function(full_lookup, wp_vocab) {
  # Proper nouns are capitalized in Wiktionary. This gets a *lot* of words,
  # but most of the weird ones won't be in WP vocab anyway.
  cap_words <-  full_lookup %>%
    # still exclude words with non-alpha chars:
    dplyr::filter(
      !stringr::str_detect(.data$word, "[^[:alpha:]]"),
      stringr::str_starts(.data$word, "[A-Z][a-z]+")
    ) %>%
    dplyr::pull(.data$word) %>%
    tolower()

  in_wik <- names(wp_vocab) %in% cap_words
  return(names(wp_vocab[in_wik]))
}

.make_lookup <- function(unnested_lookup, vocab) {
  covered_words <- unnested_lookup %>%
    dplyr::mutate(token_in_vocab = .data$token %in% vocab) %>%
    dplyr::group_by(.data$word) %>%
    dplyr::summarize(covered = all(.data$token_in_vocab)) %>%
    dplyr::filter(.data$covered) %>%
    dplyr::select(.data$word)

  lookup_df <- dplyr::semi_join(
    unnested_lookup, covered_words,
    by = "word"
  ) %>%
    dplyr::group_by(.data$word) %>%
    dplyr::summarize(tokenization = paste(.data$token, collapse = " ")) %>%
    # insert the ## between base words
    dplyr::mutate(
      tokenization = stringr::str_replace_all(
        .data$tokenization,
        "([a-z]) ([a-z])",
        "\\1 ## \\2")
    )
  return(lookup_df)
}

# we save the lookup as text in a particular (simple) format, for general
# distribution. We can make future performance faster by putting common words
# near the top.
.make_text_lookup <- function(voc, lu, word_freq_tab = NULL) {
  # first let's take words with no breakdown out of the lookup list (they are
  # already in the vocab, along with other no-breakdown words that aren't in
  # lookup, such as proper nouns).
  lu <- lu %>%
    dplyr::filter(!(.data$word %in% voc))

  if (!is.null(word_freq_tab)) {
    # arrange lookup so that common words are first.
    lu <- lu %>%
      dplyr::left_join(word_freq_tab, by = "word") %>%
      dplyr::arrange(dplyr::desc(.data$word_count))
  }

  # the text lookup is basically the first two columns...
  text_lookup <- paste(lu$word, lu$tokenization)

  return(text_lookup)
}
