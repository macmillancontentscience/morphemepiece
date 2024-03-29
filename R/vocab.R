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

# load_vocab --------------------------------------------------------------

#' Load a vocabulary file
#'
#' Usually you will want to use the included vocabulary that can be accessed via
#' `morphemepiece_vocab()`. This function can be used to load a different
#' vocabulary from a file.
#'
#' @param vocab_file path to vocabulary file. File is assumed to be a text file,
#'   with one token per line, with the line number (starting at zero)
#'   corresponding to the index of that token in the vocabulary.
#'
#' @return The vocab as a character vector of tokens. The casedness of the
#'   vocabulary is inferred and attached as the "is_cased" attribute. The
#'   vocabulary indices are taken to be the positions of the tokens, 
#'   *starting at zero* for historical consistency.
#'
#'   Note that from the perspective of a neural net, the numeric indices *are*
#'   the tokens, and the mapping from token to index is fixed. If we changed the
#'   indexing, it would break any pre-trained models using that vocabulary. 
#'
#' @export
load_vocab <- function(vocab_file) {
  token_list <- readr::read_lines(
    vocab_file,
    lazy = FALSE
  )

  return(prepare_vocab(token_list))
}

# prepare_vocab --------------------------------------------------------------

#' Format a Token List as a Vocabulary
#'
#' We use a character vector with class morphemepiece_vocabulary to provide
#' information about tokens used in
#' \code{\link{morphemepiece_tokenize}}. This function takes a character vector
#' of tokens and puts it into that format.
#'
#' @param token_list A character vector of tokens.
#'
#' @return The vocab as a character vector of tokens. The casedness of the
#'   vocabulary is inferred and attached as the "is_cased" attribute. The
#'   vocabulary indices are taken to be the positions of the tokens, 
#'   *starting at zero* for historical consistency.
#'
#'   Note that from the perspective of a neural net, the numeric indices *are*
#'   the tokens, and the mapping from token to index is fixed. If we changed the
#'   indexing, it would break any pre-trained models using that vocabulary. 
#' @export
#'
#' @examples
#' my_vocab <- prepare_vocab(c("some", "example", "tokens"))
#' class(my_vocab)
#' attr(my_vocab, "is_cased")
prepare_vocab <- function(token_list) {
  token_list <- piecemaker::validate_utf8(trimws(token_list))

  # attach processed form of vocab as attribute to speed up computations.
  vocab_split <- .split_vocab(token_list)
  is_cased <- .infer_case_from_vocab(token_list) # sure, why not.
  vocab_all <- .new_morphemepiece_vocabulary(
    token_list,
    vocab_split,
    is_cased
  )
  return(.validate_morphemepiece_vocabulary(vocab_all))
}


# load_or_retrieve_vocab ------------------------------------------------------


#' Load a vocabulary file, or retrieve from cache
#'
#' Usually you will want to use the included vocabulary that can be accessed via
#' `morphemepiece_vocab()`. This function can be used to load (and cache) a
#' different vocabulary from a file.
#'
#' @inheritParams load_vocab
#'
#' @return The vocab as a character vector of tokens. The casedness of the
#'   vocabulary is inferred and attached as the "is_cased" attribute. The
#'   vocabulary indices are taken to be the positions of the tokens, 
#'   *starting at zero* for historical consistency.
#'
#'   Note that from the perspective of a neural net, the numeric indices *are*
#'   the tokens, and the mapping from token to index is fixed. If we changed the
#'   indexing, it would break any pre-trained models using that vocabulary. 
#'
#' @export
load_or_retrieve_vocab <- function(vocab_file) {
  return( # nocov start
    dlr::read_or_cache(
      source_path = vocab_file,
      appname = "morphemepiece",
      process_f = load_vocab
    )
  ) # nocov end
}


# load_lookup --------------------------------------------------------------


#' Load a morphemepiece lookup file
#'
#' Usually you will want to use the included lookup that can be accessed via
#' `morphemepiece_lookup()`. This function can be used to load a different
#' lookup from a file.
#'
#' @param lookup_file path to lookup file. File is assumed to be a text
#'   file, with one word per line. The lookup value, if different from the word,
#'   follows the word on the same line, after a space.
#'
#' @return The lookup as a named list. Names are words in lookup.
#'
#' @export
load_lookup <- function(lookup_file) {
  lookup_lines <- readr::read_lines(lookup_file, lazy = FALSE)
  # patch for now; fix in wikimorphemes (see "blithely" "fidget" "cyber")
  lookup_lines <- piecemaker::validate_utf8(
    stringr::str_remove_all(lookup_lines, "[^a-z]*$")
  )
  split_lup <- stringr::str_split_fixed(lookup_lines, pattern = " ", n = 2)

  words <- split_lup[, 1]
  breakdowns <- split_lup[, 2]
  no_breakdown <- breakdowns == ""
  breakdowns[no_breakdown] <- words[no_breakdown]
  names(breakdowns) <- words
  return(breakdowns) # maybe later make class?
}


# load_or_retrieve_lookup ------------------------------------------------------

#' Load a lookup file, or retrieve from cache
#'
#' Usually you will want to use the included lookup that can be accessed via
#' `morphemepiece_lookup()`. This function can be used to load (and cache) a
#' different lookup from a file.
#'
#' @inheritParams load_lookup
#'
#' @return The lookup table as a named character vector.
#'
#' @export
load_or_retrieve_lookup <- function(lookup_file) {
  return( # nocov start
    dlr::read_or_cache(
      source_path = lookup_file,
      appname = "morphemepiece",
      process_f = load_lookup
    )
  ) # nocov end
}


# .split_vocab ------------------------------------------------------------

# turn a flat mp vocab into a list of prefixes, words, suffixes
.split_vocab <- function(vocab) {
  frag_pat <- "##"
  is_prefix <- stringr::str_ends(vocab, paste0(".", frag_pat))
  prefixes <- stringr::str_sub(vocab[is_prefix], end = -3)

  is_suffix <- stringr::str_starts(vocab, paste0(frag_pat, "."))
  suffixes <- stringr::str_sub(vocab[is_suffix], start = 3)

  is_word <- stringr::str_ends(vocab, frag_pat, negate = TRUE) &
    stringr::str_starts(vocab, frag_pat, negate = TRUE)
  words <- vocab[is_word]
  return(list(
    prefixes = prefixes,
    words = words,
    suffixes = suffixes
  ))
}

# .infer_case_from_vocab --------------------------------------------------

#' Determine Vocabulary Casedness
#'
#' Determine whether or not a wordpiece vocabulary is case-sensitive.
#'
#' If none of the tokens in the vocabulary start with a capital letter, it will
#' be assumed to be uncased. Note that tokens like "\\[CLS\\]" contain uppercase
#' letters, but don't start with uppercase letters.
#'
#' @param vocab The vocabulary as a character vector.
#' @return TRUE if the vocabulary is cased, FALSE if uncased.
#'
#' @keywords internal
.infer_case_from_vocab <- function(vocab) {
  is_cased <- any(grepl(pattern = "^[A-Z]", vocab))
  return(is_cased)
}


# .new_morphemepiece_vocabulary ------------------------------------------------

#' Constructor for Class morphemepiece_vocabulary
#'
#' @param vocab Character vector; the "actual" vocabulary.
#' @param vocab_split List of character vectors; the split vocabulary.
#' @param is_cased Logical; whether the vocabulary is cased.
#' @return The vocabulary with `is_cased` attached as an attribute, and the
#'   class `morphemepiece_vocabulary` applied. The split vocabulary is also
#'   attached as an attribute.
#'
#' @keywords internal
.new_morphemepiece_vocabulary <- function(vocab,
                                          vocab_split,
                                          is_cased) {
  return(
    structure(
      vocab,
      "vocab_split" = vocab_split,
      "is_cased" = is_cased,
      class = c("morphemepiece_vocabulary", "character")
    )
  )
}



# .validate_morphemepiece_vocabulary ------------------------------------------

#' Validator for Objects of Class morphemepiece_vocabulary
#'
#' @param vocab morphemepiece_vocabulary object to validate
#' @return \code{vocab} if the object passes the checks. Otherwise, abort with
#'   message.
#'
#' @keywords internal
.validate_morphemepiece_vocabulary <- function(vocab) {
  if (length(vocab) == 0) {
    stop("Empty vocabulary.") # nocov
  }
  tokens <- names(vocab)
  if (anyDuplicated(tokens) > 0) {
    stop("Duplicate tokens found in vocabulary.") # nocov
  }
  if (any(grepl("\\s", tokens))) {
    stop("Whitespace found in vocabulary tokens.") # nocov
  }
  return(vocab)
}


# .process_vocab -----------------------------------------------------------


#' Process a Morphemepiece Vocabulary for Tokenization
#'
#' @param v An object of class `morphemepiece_vocabulary`.
#'
#' @return A character vector of tokens for tokenization.
#' @keywords internal
.process_mp_vocab <- function(v) {
  UseMethod(".process_mp_vocab", v)
}

#' @rdname dot-process_mp_vocab
#' @keywords internal
#' @export
.process_mp_vocab.default <- function(v) {
  stop("Unsupported vocabulary type. ",
       "The vocabulary should be an object of type `morphemepiece_vocabulary`.",
       " To use the default morphemepiece vocabulary, see ",
       "`morphemepiece_vocabulary()`.")
}

#' @rdname dot-process_mp_vocab
#' @keywords internal
#' @export
.process_mp_vocab.morphemepiece_vocabulary <- function(v) {
  NextMethod()
}

#' @rdname dot-process_mp_vocab
#' @keywords internal
#' @export
.process_mp_vocab.integer <- function(v) {
  # I don't know why `order`ing the vocab, even with memoisation, is so much
  # slower than it was with wordpiece. But for now, assume that an integer
  # vocab is in order.
  # return(names(v)[order(v)])
  return(names(v)) # nocov
}

#' @rdname dot-process_mp_vocab
#' @keywords internal
#' @export
.process_mp_vocab.character <- function(v) {
  return(v)
}


