# Creates a morphemepiece vocabulary and lookup table, given a processed
# wiktionary dump and an existing wordpiece vocabulary.
# Optionally, a word frequency table from some corpus can be given.
#

make_vocab_and_lookup <- function(processed_wiktionary,
                                  wp_vocab,
                                  target_vocab_size = 25000,
                                  word_freq_tab = NULL) {
  unnested_processed_wiktionary <- unnest_pw(processed_wiktionary, clean = TRUE)
  token_counts <- get_token_counts(unnested_processed_wiktionary, word_freq_tab)

  # punctuation, numbers, [CLS], etc. from wordpiece vocab
  fancy_wp_tokens <- get_fancy_wp_tokens(wp_vocab)

  actual_wp_words <- get_actual_wp_words(processed_wiktionary, wp_vocab)
  # short actual words from wp will get a VIP pass directly to vocab.
  # Though most of them wouldn't be considered words by most people. :)
  short_words <- actual_wp_words[nchar(actual_wp_words) <= 3]
  rest_of_words <- actual_wp_words[nchar(actual_wp_words) > 3]
  # make sure we keep enough tokens to cover the rest of the wp words
  processed_tokens <- unnested_processed_wiktionary %>%
    dplyr::filter(word %in% rest_of_words) %>%
    dplyr::distinct(token) %>%
    dplyr::pull(token)

  # we *didn't* clean the processed tokens before this, so clean them now.
  # This mainly filters out a few capitalized proper nouns that sneaked back in,
  # but we'll recover the lowercase version of those later.
  processed_tokens <- processed_tokens[stringr::str_detect(processed_tokens,
                                                           pattern = "[^a-z#]",
                                                           negate = TRUE)]

  wp_proper_nouns <- get_wp_proper_nouns(processed_wiktionary, wp_vocab)
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
    unique()

  if (length(vocab) < target_vocab_size) {
    # Fill up the difference with top tokens, as judged by rank in token_counts.
    # Maybe later be more sophisticated about proper nouns.
    n_tokens_to_add <- target_vocab_size - length(vocab)
    additional_tokens <- token_counts %>%
      dplyr::filter(! token %in% vocab) %>%
      dplyr::arrange(rank) %>%
      utils::head(n_tokens_to_add) %>%
      dplyr::pull(token)
    vocab <- unique(c(vocab, additional_tokens))
  }
  lookup <- make_lookup(unnested_processed_wiktionary, vocab)
  return(list(vocab = vocab, lookup = lookup))
}

unnest_pw <- function(processed_wiktionary, clean = TRUE) {
  if (clean) { # keep only words in all lowercase latin alphabet
    processed_wiktionary <- processed_wiktionary %>%
      dplyr::filter(!stringr::str_detect(word, "[^a-z]"))
  }
  upw <- processed_wiktionary %>%
    tidyr::unnest_longer(morphemes) %>%
    dplyr::filter(morphemes != "") %>%
    dplyr::mutate(before = ifelse(morphemes_id %in% c("suffix",
                                                      "inflection",
                                                      "interfix"),
                                  "##", ""),
                  after = ifelse(morphemes_id == "prefix", "##", ""),
                  token = paste0(before, morphemes, after)) %>%
    dplyr::rename(morpheme_type = morphemes_id) %>%
    dplyr::select(word, token, morpheme_type)

  if (clean) {
    # Don't remove tokens here! Otherwise we'll get incomplete words.
    # BUT... let's cast all tokens to lowercase at this point.
    upw <- upw %>%
      dplyr::mutate(token = tolower(token))
  }

  return(upw)
}
unnest_pw <- memoise::memoise(unnest_pw)

get_token_counts <- function(unnested_processed_wiktionary,
                             word_freq_tab = NULL) {
  # if word_freq_tab is supplied, weight by word freq
  if (!is.null(word_freq_tab)) {
    unnested_processed_wiktionary <- dplyr::left_join(
      unnested_processed_wiktionary,
      word_freq_tab,
      by = c("word" = "word")) %>%
      dplyr::mutate(word_count = ifelse(is.na(word_count), 1, word_count))
  } else {
    unnested_processed_wiktionary <- unnested_processed_wiktionary %>%
      dplyr::mutate(word_count = 1)
  }
  token_counts <- unnested_processed_wiktionary %>%
    dplyr::group_by(token) %>%
    dplyr::summarize(token_score = sum(word_count)) %>%
    dplyr::arrange(dplyr::desc(token_score)) %>%
    dplyr::mutate(rank = dplyr::row_number()) %>%
    dplyr::filter(!stringr::str_detect(token, "[^a-z#]"))

  return(token_counts)
}

# token_counts <- get_token_counts(unnested_processed_wiktionary)
# token_counts <- get_token_counts(unnested_processed_wiktionary, word_freq_tab)

get_fancy_wp_tokens <- function(wp_vocab) {
  # exclude [unused]
  is_fancy <- stringr::str_detect(names(wp_vocab), pattern = "[^a-z#]")
  is_unused <- stringr::str_detect(names(wp_vocab), pattern = "\\[unused")
  # note that this excludes tokens "#", "##"... add those back in
  return(c(names(wp_vocab[is_fancy&!is_unused]), "#", "##"))
}

get_actual_wp_words <- function(processed_wiktionary, wp_vocab) {
  # wordpieces that have a wiktionary entry
  clean_wik <- processed_wiktionary %>%
    dplyr::filter(!stringr::str_detect(word, "[^a-z]")) %>%
    dplyr::pull(word)
  in_wik <- names(wp_vocab) %in% clean_wik
  return(names(wp_vocab[in_wik]))
}


get_wp_proper_nouns <- function(processed_wiktionary, wp_vocab) {
  # Proper nouns are capitalized in Wiktionary. This gets a *lot* of words,
  # but most of the weird ones won't be in WP vocab anyway.
  cap_words <-  processed_wiktionary %>%
    # still exclude words with non-alpha chars:
    dplyr::filter(!stringr::str_detect(word, "[^[:alpha:]]"),
           stringr::str_starts(word, "[A-Z][a-z]+")) %>%
    dplyr::pull(word) %>%
    tolower()

  in_wik <- names(wp_vocab) %in% cap_words
  return(names(wp_vocab[in_wik]))
}

make_lookup <- function(unnested_processed_wiktionary, vocab) {
  covered_words <- unnested_processed_wiktionary %>%
    dplyr::mutate(token_in_vocab = token %in% vocab) %>%
    dplyr::group_by(word) %>%
    dplyr::summarize(covered = all(token_in_vocab)) %>%
    dplyr::filter(covered) %>%
    dplyr::select(word)

  lookup_df <- dplyr::semi_join(unnested_processed_wiktionary, covered_words,
                   by = c("word" = "word")) %>%
    dplyr::group_by(word) %>%
    dplyr::summarize(tokenization = paste(token, collapse = " ")) %>%
    # insert the ## between base words
    dplyr::mutate(tokenization = stringr::str_replace_all(tokenization,
                                                          "([a-z]) ([a-z])",
                                                          "\\1 ## \\2"))
  return(lookup_df)
}

# we save the lookup as text in a particular (simple) format, for general
# distribution. We can make future performance faster by putting common words
# near the top.
make_text_lookup <- function(voc, lu, word_freq_tab = NULL) {
  # first let's take words with no breakdown out of the lookup list (they are
  # already in the vocab, along with other no-breakdown words that aren't in
  # lookup, such as proper nouns).
  lu <- lu %>%
    dplyr::filter(!word %in% voc)

  if (!is.null(word_freq_tab)) {
    # arrange lookup so that common words are first.
    lu <- lu %>%
      dplyr::left_join(word_freq_tab, by = "word") %>%
      dplyr::arrange(dplyr::desc(word_count))
  }

  # the text lookup is basically the first two columns...
  text_lookup <- paste(lu$word, lu$tokenization)

  return(text_lookup)
}
