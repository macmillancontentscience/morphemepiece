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

# .mp_tokenize_word -----------------------------------------------------------

#' Tokenize a Word
#'
#' Tokenize a single "word" (no whitespace). The word can technically contain
#' punctuation, but typically punctuation has been split off by this point.
#'
#' This is an adaptation of wordpiece:::.tokenize_word. The main differences are
#' that it was designed to work with a morphemepiece vocabulary, which can
#' include prefixes (denoted like "pre##"). As in wordpiece, the algorithm uses
#' a repeated greedy search for the largest piece from the vocabulary found
#' within the word, but starting from either the beginning or the end of the
#' word (controlled by the `dir` parameter). The input vocabulary must be split
#' into prefixes, suffixes, and "words".
#'
#' @param word Word to tokenize.
#' @param vocab_split List of character vector containing vocabulary words.
#'   Should have components named "prefixes", "words", "suffixes".
#' @param dir Integer; if 1 (the default), look for tokens starting at the
#'   beginning of the word. Otherwise, start at the end.
#' @param allow_compounds Logical; whether to allow multiple whole words in the
#'   breakdown.
#' @param unk_token Token to represent unknown words.
#' @param max_chars Maximum length of word recognized.
#'
#' @return Input word as a list of tokens.
#' @keywords internal
.mp_tokenize_word <- function(word,
                              vocab_split,
                              dir = 1, # -1 for backwards
                              allow_compounds = TRUE,
                              unk_token = "[UNK]",
                              max_chars = 100) {
  if (nchar(word) > max_chars) {
    return(unk_token)
  }
  frag_pat <- "##"

  prefixes <- vocab_split$prefixes
  words <- vocab_split$words
  suffixes <- vocab_split$suffixes

  is_bad <- FALSE
  start <- 1
  sub_tokens <- character(0)

  wordlen <- nchar(word)
  end <- wordlen

  word_allowed <- "XXX"
  if (allow_compounds) {
    # If "#" is allowed next, that counts as a word, but with a flag to
    # insert the "##" token
    word_allowed <- "#"
  }

  if (dir == 1) {
    # rules for what kind of token can follow what (prefix, word, suffix)
    allowed_next_rules <- list(
      "p" = c("p", "w", "s"),
      "w" = c("s", word_allowed),
      "s" = "s"
    )
    allowed_next <- c("p", "w")
  } else {
    # for backwards run
    allowed_next_rules <- list(
      "s" = c("p", "w", "s"),
      "w" = c("p", word_allowed),
      "p" = "p"
    )

    allowed_next <- c("s", "w")
  }

  keepgoing <- TRUE
  while (keepgoing) {
    if (dir == 1) {
      end <- wordlen
    } else {
      start <- 1
    }

    cur_substr <- NA_character_
    while (start <= end) {
      sub_str <- substr(word, start, end) # inclusive on both ends

      # first look for prefixes, if allowed
      if ("p" %fin% allowed_next & end < wordlen & sub_str %fin% prefixes) {
        cur_substr <- paste0(sub_str, frag_pat)
        allowed_next <- allowed_next_rules[["p"]]
        break
      }
      # next, look for suffix-like pieces, if we're not at start of word
      if ("s" %fin% allowed_next & start > 1 & sub_str %fin% suffixes) {
        cur_substr <- paste0(frag_pat, sub_str)
        allowed_next <- allowed_next_rules[["s"]]
        break
      }
      # finally, look for complete words, if allowed
      if (any(c("w", "#") %fin% allowed_next) & sub_str %fin% words) {
        cur_substr <- sub_str
        # insert the frag_pat token in, if we're between complete words
        if ("#" %fin% allowed_next) {
          if (dir == 1) {
            cur_substr <- append(frag_pat, cur_substr)
          } else {
            cur_substr <- append(cur_substr, frag_pat)
          }
        }
        allowed_next <- allowed_next_rules[["w"]]
        break
      }

      if (dir == 1) {
        end <- end - 1 # forward; hold start fixed
      } else {
        start <- start + 1 # backward; hold end fixed
      }
    }

    if (is.na(cur_substr[[1]])) {
      is_bad <- TRUE # nocov
      break # nocov
    }

    if (dir == 1) {
      sub_tokens <- append(sub_tokens, cur_substr) # append to end
      start <- end + 1 # rest of word, after taking piece off the front
      keepgoing <- start <= wordlen
    } else {
      sub_tokens <- append(cur_substr, sub_tokens) # append to beginning
      end <- start - 1 # rest of word, after taking piece off the end
      keepgoing <- end >= 1
    }
  }

  if (is_bad) {
    return(unk_token) # nocov
  }
  return(sub_tokens)
}


# .mp_tokenize_word_bidir -------------------------------------------------

#' Tokenize a Word Bidirectionally
#'
#' Apply .mp_tokenize_word from both directions and pick the result with fewer
#' pieces.
#'
#' @param word Character scalar; word to tokenize.
#' @param vocab_split List of character vector containing vocabulary words.
#'   Should have components named "prefixes", "words", "suffixes".
#' @param unk_token Token to represent unknown words.
#' @param max_chars Maximum length of word recognized.
#' @param allow_compounds Logical; whether to allow multiple whole words in the
#'   breakdown. Default is TRUE. This option will not be exposed to end users;
#'   it is kept here for documentation + development purposes.
#'
#' @return Input word as a list of tokens.
#' @keywords internal
.mp_tokenize_word_bidir <- function(word,
                                    vocab_split,
                                    unk_token,
                                    max_chars,
                                    allow_compounds = TRUE) {
  # vocab_split <- attr(vocab, "vocab_split")
  t1 <- .mp_tokenize_word(word, vocab_split,
                          dir = 1,
                          allow_compounds = allow_compounds,
                          unk_token = unk_token,
                          max_chars = max_chars
  )
  t2 <- .mp_tokenize_word(word, vocab_split,
                          dir = -1,
                          allow_compounds = allow_compounds,
                          unk_token = unk_token,
                          max_chars = max_chars
  )
  # Let's *not* count the ## token for purposes of deciding which breakdown
  # to take. But we may want to come back to this, since it seemed to help.
  t1_0 <- t1[t1 != "##"]
  t2_0 <- t2[t2 != "##"]
  if (length(t2_0) < length(t1_0) & length(t2) > 1) {
    return(t2)
  } else {
    return(t1)
  }
}

# .mp_tokenize_single_string -------------------------------------------------

#' Tokenize an Input Word-by-word
#'
#' @inheritParams .mp_tokenize_word_lookup
#' @param words Character; a vector of words (generated by space-tokenizing a
#'   single input).
#'
#' @return A named integer vector of tokenized words.
#' @keywords internal
.mp_tokenize_single_string <- function(words,
                                       vocab,
                                       lookup,
                                       unk_token,
                                       max_chars) {
  return(
    unlist(
      purrr::map(
        words,
        .f = .mp_tokenize_word_lookup,
        vocab = vocab,
        lookup = lookup,
        unk_token = unk_token,
        max_chars = max_chars
      )
    )
  )
}

# .mp_tokenize_word_lookup -------------------------------------------------

#' Tokenize a Word Including Lookup
#'
#' Look up a word in the table; go to fall-back otherwise.
#'
#' @inheritParams .mp_tokenize_word_bidir
#' @param vocab A morphemepiece vocabulary.
#' @param lookup A morphemepiece lookup table.
#'
#' @return Input word, broken into tokens.
#' @keywords internal
.mp_tokenize_word_lookup <- function(word,
                                     vocab,
                                     lookup,
                                     unk_token,
                                     max_chars) {
  # first get the split_vocab from the attributes...
  vocab_split <- attr(vocab, "vocab_split")
  # Now we just need the vocab itself, with no other attributes.
  # Think about moving this higher up in the chain. But then we'd need to keep
  # the split vocab around till now.
  vocab <- .process_mp_vocab(vocab) 

  if (word == "") {
    return(integer(0))
  }
  if (word %fin% vocab) { # includes punctuation, etc.
    # Get ID by position.
    id <- fastmatch::fmatch(word, vocab)
    names(id) <- word
    return(id - 1L) # default to 0-based index, for historical consistency
  }

  # for profiling purposes
  nl <- names(lookup)
  if (word %fin% nl) {
    breakdown <- lookup[[word]]
    token_list <- stringr::str_split(breakdown, pattern = " ")[[1]]
  } else {
    token_list <- .mp_tokenize_word_bidir(word,
                                          vocab_split,
                                          unk_token,
                                          max_chars)
  }

  # Get IDs by position.
  ids <- fastmatch::fmatch(token_list, vocab)
  names(ids) <- token_list
  return(ids - 1L) # default to 0-based index, for historical consistency
}


# morphemepiece_tokenize --------------------------------------------------

#' Tokenize Sequence with Morpheme Pieces
#'
#' Given a single sequence of text and a morphemepiece vocabulary, tokenizes the
#' text.
#'
#' @inheritParams .mp_tokenize_single_string
#' @param text Character scalar; text to tokenize.
#'
#' @return A character vector of tokenized text (later, this should be a named
#' integer vector, as in the wordpiece package.)
#' @export
morphemepiece_tokenize <- function(text,
                                   vocab = morphemepiece_vocab(),
                                   lookup = morphemepiece_lookup(),
                                   unk_token = "[UNK]",
                                   max_chars = 100) {
  is_cased <- attr(vocab, "is_cased")
  if (!is_cased) {
    text <- tolower(text)
  }

  word_lists <- piecemaker::prepare_and_tokenize(
    text = text,
    prepare = TRUE,
    remove_terminal_hyphens = FALSE
  )

  tokens <- purrr::map(
    word_lists,
    .f = .mp_tokenize_single_string,
    vocab = vocab,
    lookup = lookup,
    unk_token = unk_token,
    max_chars = max_chars
  )
  # Length-0 token vectors need to still be named.
  empty_int <- integer(0)
  names(empty_int) <- character(0)
  tokens[lengths(tokens) == 0] <- list(empty_int)
  return(tokens)
}
