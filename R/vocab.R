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
#' @param vocab_file path to vocabulary file. File is assumed to be a text file,
#'   with one token per line, with the line number (starting at zero)
#'   corresponding to the index of that token in the vocabulary.
#'
#' @return The vocab as a named integer vector. Names are tokens in vocabulary,
#'   values are integer indices. The casedness of the vocabulary is inferred
#'   and attached as the "is_cased" attribute.
#'
#'   Note that from the perspective of a neural net, the numeric indices *are*
#'   the tokens, and the mapping from token to index is fixed. If we changed the
#'   indexing, it would break any pre-trained models using that vocabulary. This
#'   is why the vocabulary is stored as a named integer vector, and why it
#'   starts with index zero.
#'
#' @export
#'
#' @examples
#' # todo, after I make tiny sample vocab to include
load_vocab <- function(vocab_file) {
  token_list <- readLines(vocab_file)

  return(prepare_vocab(token_list))
}

#' Format a Token List as a Vocabulary
#'
#' We use a special named integer vector with class morphemepiece_vocabulary to
#' provide information about tokens used in
#' \code{\link{morphemepiece_tokenize}}. This function takes a character vector
#' of tokens and puts it into that format.
#'
#' @param token_list A character vector of tokens.
#'
#' @return The vocab as a named integer vector. Names are tokens in vocabulary,
#'   values are integer indices. The casedness of the vocabulary is inferred
#'   and attached as the "is_cased" attribute.
#'
#'   Note that from the perspective of a neural net, the numeric indices *are*
#'   the tokens, and the mapping from token to index is fixed. If we changed the
#'   indexing, it would break any pre-trained models using that vocabulary. This
#'   is why the vocabulary is stored as a named integer vector, and why it
#'   starts with index zero.
#' @export
#'
#' @examples
#' my_vocab <- prepare_vocab(c("some", "example", "tokens"))
#' class(my_vocab)
#' attr(my_vocab, "is_cased")
prepare_vocab <- function(token_list) {
  token_list <- piecemaker::validate_utf8(trimws(token_list))

  # The vocab is zero-indexed.
  named_vocab <- seq_along(token_list) - 1L
  names(named_vocab) <- token_list

  # attach processed form of vocab as attribute to speed up computations.
  vocab_split <- .split_vocab(token_list)
  is_cased <- .infer_case_from_vocab(token_list) # sure, why not.
  vocab_all <- .new_morphemepiece_vocabulary(
    named_vocab,
    vocab_split,
    is_cased
  )
  return(.validate_morphemepiece_vocabulary(vocab_all))
}


# load_or_retrieve_vocab  ------------------------------------------------------


#' Load a vocabulary file, or retrieve from cache
#'
#' @inheritParams load_vocab
#' @param use_cache Logical; if TRUE, will attempt to retrieve the vocabulary
#'   from the specified cache location, or, if not found there, will ask to save
#'   the vocabulary as an .rds file.
#' @param cache_dir Character; the path to a cache directory (defaults to
#'   location returned by `morphemepiece_cache_dir()`).
#'
#' @return The vocab as a list of named integer vectors. Names are tokens in
#'   vocabulary, values are integer indices. The casedness of the vocabulary is
#'   inferred and attached as the "is_cased" attribute.
#'
#'   Note that from the perspective of a neural net, the numeric indices *are*
#'   the tokens, and the mapping from token to index is fixed. If we changed the
#'   indexing, it would break any pre-trained models. This is why the vocabulary
#'   is stored as a named integer vector, and why it starts with index zero.
#'
#' @export
#'
#' @examples
#' # todo, after I make tiny sample vocab to include
load_or_retrieve_vocab <- function(vocab_file,
                                   use_cache = TRUE,
                                   cache_dir = morphemepiece_cache_dir()) {
  return(
    .load_or_retrieve_file(
      vocab_file,
      load_vocab,
      use_cache,
      cache_dir
    )
  )
}


# load_lookup --------------------------------------------------------------


#' Load a morphemepiece lookup file
#'
#' @param lookup_file path to vocabulary file. File is assumed to be a text
#'   file, with one word per line. The lookup value, if different from the word,
#'   follows the word on the same line, after a space.
#'
#' @return The lookup as a named list. Names are words in lookup.
#'
#' @export
#'
#' @examples
#' # todo, after I make tiny sample vocab to include
load_lookup <- function(lookup_file) {
  lookup_lines <- readLines(lookup_file)
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
# generalize vocab function to do both

#' Load a lookup file, or retrieve from cache
#'
#' @inheritParams load_lookup
#' @param use_cache Logical; if TRUE, will attempt to retrieve the lookup
#'   from the specified cache location, or, if not found there, will ask to save
#'   the lookup as an .rds file.
#' @param cache_dir Character; the path to a cache directory (defaults to
#'   location returned by `morphemepiece_cache_dir()`).
#'
#' @return The lookup table as a named character vector.
#'
#' @export
#'
#' @examples
#' # todo, after I make tiny sample vocab to include
load_or_retrieve_lookup <- function(lookup_file,
                                    use_cache = TRUE,
                                    cache_dir = morphemepiece_cache_dir()) {
  return(
    .load_or_retrieve_file(
      lookup_file,
      load_lookup,
      use_cache,
      cache_dir
    )
  )
}


# .load_or_retrieve_file ------------------------------------------------------

#' Load a vocab or lookup file, or retrieve from cache
#'
#' @param file Character; path to file to load.
#' @param load_function Function to call to load vocabulary or lookup from txt.
#' @param use_cache Logical; if TRUE, will attempt to retrieve the vocab or
#'   lookup table from the specified cache location, or, if not found there,
#'   will ask to save the vocabulary or lookup as an .rds file.
#' @param cache_dir Character; the path to a cache directory (defaults to
#'   location returned by `morphemepiece_cache_dir()`).
#'
#' @return The lookup table or vocabulary as returned by `load_function`.
#'
#' @keywords internal
.load_or_retrieve_file <- function(file,
                                   load_function,
                                   use_cache = TRUE,
                                   cache_dir = morphemepiece_cache_dir()) {
  if (use_cache) {
    cache_filepath <- file.path(
      cache_dir,
      .make_cache_filename(file)
    )
    if (file.exists(cache_filepath)) {
      return(readRDS(cache_filepath)) # nocov
    }
  }
  # Guess we have to load the vocab or lookup from text file.
  contents <- load_function(file)

  if (use_cache) { # nocov start
    # ask for permission to write to cache
    if (interactive()) {
      if (isTRUE(utils::askYesNo(paste0(
        "Cache contents at ",
        cache_filepath, "?"
      )))) {
        # make sure that the directory exists
        if (!dir.exists(cache_dir)) {
          # probably should invalidate the cache across package versions
          dir.create(path = cache_dir, recursive = TRUE)
        }
        saveRDS(contents, cache_filepath)
      }
    }
  } # nocov end
  return(contents)
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
#' @param vocab The vocabulary as a named integer vector.
#' @return TRUE if the vocabulary is cased, FALSE if uncased.
#'
#' @keywords internal
.infer_case_from_vocab <- function(vocab) {
  is_cased <- any(grepl(pattern = "^[A-Z]", names(vocab)))
  return(is_cased)
}


# .new_morphemepiece_vocabulary ------------------------------------------------

#' Constructor for Class morphemepiece_vocabulary
#'
#' @param vocab Named integer vector; the "actual" vocabulary.
#' @param vocab_split List of named integer vectors; the split vocabulary.
#' @param is_cased Logical; whether the vocabulary is cased.
#' @return The vocabulary with `is_cased` attached as an attribute, and the
#'   class `morphemepiece_vocabulary` applied. The split and reversed
#'   vocabularies are also attached as attributes.
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
      class = c("morphemepiece_vocabulary", "integer")
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
    stop("Empty vocabulary.")
  }
  tokens <- names(vocab)
  if (anyDuplicated(tokens) > 0) {
    stop("Duplicate tokens found in vocabulary.")
  }
  if (any(grepl("\\s", tokens))) {
    stop("Whitespace found in vocabulary tokens.")
  }
  return(vocab)
}