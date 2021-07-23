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


# pipe for internal use ----------------------------------------

#' @importFrom magrittr %>%
magrittr::`%>%`


# .pull_out_longest_piece -----------------------------------------------------


#' Pull the longest piece out of the given word
#'
#' Valid pieces meet the following restrictions: all internal "breaks" have at
#' least one "##" between, and no external edges have a "##".
#'
#' @param word Character; word to break into pieces.
#' @param mp_vocab_nohash A morphemepiece vocabulary with ## in names only.
#' @param original_start Logical; is the beginning of this word the beginning of
#'   the original word?
#' @param original_end Logical; is the end of this word the end of the original
#'   word?
#' @param prefixes List of prefixes from vocab. (I still need to clean this up.)
#'
#' @return A list of three "character" elements. The middle piece is the piece
#' "pulled out" by this function, the first and last are the left and right
#' remainders, respectively. They may be empty (""). (If an empty word is input,
#' the output will be simply "". This is an inconsistency which we may want to
#' deal with.)
#'
#' @keywords internal
.pull_out_longest_piece <- function(word, 
                                    mp_vocab_nohash,
                                    original_start = TRUE,
                                    original_end = TRUE,
                                    prefixes) {
    if (word == "") {
        return(word)
    }
    
    # deduce whether this is a valid_start from the ##
    valid_start <- stringr::str_starts(word, pattern = "##", negate = TRUE)
    
    word <- stringr::str_remove_all(word, pattern = "#")
    
    match <- stringr::str_locate(string = word, pattern = mp_vocab_nohash) %>%
        tibble::as_tibble() %>%
        # need an index...
        dplyr::mutate(which_wp = dplyr::row_number()) %>%
        # take out any pieces *not* found in word
        dplyr::filter(!is.na(start)) %>%
        # full_token includes the ##
        dplyr::mutate(full_token = names(mp_vocab_nohash)[which_wp]) %>%
        dplyr::mutate(left_piece = stringr::str_sub(word, start = 1, 
                                                    end = start-1)) %>%
        dplyr::mutate(right_piece = stringr::str_sub(word, start = end+1, 
                                                     end = -1))
    
    # take out matches that don't qualify.
    # Distinguish between original start and valid start:
    # a valid start may, but doesn't have to, begin with ##.
    # original start may not.
    if (valid_start) {
        if (original_start) {
            # If this *is also* the original_start:
            match <- match %>%
                # ...if start == 1, the wordpiece must *not* start with ##.
                dplyr::filter(start > 1 |
                                  !stringr::str_starts(full_token, 
                                                       pattern = "##"))
        }
        # ...if start != 1, The piece *must* start with ## OR the entire left
        # piece must be a valid prefix. Add the ## on that piece if so.
        match <- match %>%
            dplyr::filter(start == 1 |
                              stringr::str_starts(full_token, pattern = "##") |
                              left_piece %in% names(prefixes))
    } else {
        # If this is *not* a valid_start, the wordpiece must start with ##
        match <- match %>%
            dplyr::filter(stringr::str_starts(full_token, pattern = "##"))
    }
    if (original_end) {
        # if this is the end of the word, don't end in ##!
        match <- match %>%
            dplyr::filter(right_piece != "" |
                              !stringr::str_ends(full_token, pattern = "##"))
    }
    
    match <- match %>%
        dplyr::mutate(len = (end - start) + 1) %>%
        # Want ... longest matching piece, with ties broken by starting position
        # (earliest is better). This can still leave ties between base_words
        # and prefixes. I think we should prefer prefixes.
        dplyr::arrange(-len, start, -nchar(full_token)) %>%
        head(n = 1)
    if (nrow(match) != 1) {
        stop("hmm, this shouldn't happen in a complete vocabulary.")
    }
    # return the word, and the remaining pieces
    found_piece <- match$full_token
    left_piece <- match$left_piece
    
    #add the ## back to the left piece if we removed it.
    if (left_piece != "" & !valid_start) {
        left_piece <- paste0("##", left_piece)
    }
    
    right_piece <- match$right_piece
    return(list(left_piece, found_piece, right_piece))
}


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
#' @param vocab_split List of named integer vector containing vocabulary words.
#'   Should have components named "prefixes", "words", "suffixes"
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
  
  is_bad  <- FALSE
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
    allowed_next_rules <- list("p" = c("p", "w", "s"),
                               "w" = c("s", word_allowed),
                               "s" = "s")
    allowed_next <- c("p", "w")
  } else {
    # for backwards run
    allowed_next_rules <- list("s" = c("p", "w", "s"),
                               "w" = c("p", word_allowed),
                               "p" = "p")
    
    allowed_next <- c("s", "w")  
  }
  
  keepgoing <- TRUE
  while (keepgoing) {
    if (dir == 1) {
      end <- wordlen
    } else {
      start <- 1
    }
    
    cur_substr  <- NA_character_
    while (start <= end) {
      sub_str <- substr(word, start, end)   # inclusive on both ends
      
      # first look for prefixes, if allowed
      if ("p" %in% allowed_next & end < wordlen & sub_str %in% prefixes) {
        cur_substr <- paste0(sub_str, frag_pat)
        allowed_next <- allowed_next_rules[["p"]]
        break
      }
      # next, look for suffix-like pieces, if we're not at start of word
      if ("s" %in% allowed_next & start > 1 & sub_str %in% suffixes) {
        cur_substr <- paste0(frag_pat, sub_str)
        allowed_next <- allowed_next_rules[["s"]]
        break
      }
      # finally, look for complete words, if allowed
      if (any(c("w", "#") %in% allowed_next) & sub_str %in% words) {
        cur_substr <- sub_str
        # insert the frag_pat token in, if we're between complete words
        if ("#" %in% allowed_next) {
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
    
    if (is.na(cur_substr[[1]]) ) {
      is_bad <-  TRUE #nocov
      break           #nocov
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
    return(unk_token) #nocov
  }
  return(sub_tokens)
}


# .mp_tokenize_word_bidir -------------------------------------------------

#' Tokenize a Word Bidirectionally
#'
#' Apply .mp_tokenize_word from both directions and pick the result with fewer
#' pieces.
#'
#' @param word Word to tokenize.
#' @param vocab Named integer vector containing vocabulary words. Should have
#'   "vocab_split" attribute, with components named "prefixes", "words",
#'   "suffixes".
#' @param allow_compounds Logical; whether to allow multiple whole words in the
#'   breakdown.
#'   
#' @return Input word as a list of tokens.
#' @keywords internal
.mp_tokenize_word_bidir <- function(word, vocab, allow_compounds = TRUE) {
    vocab_split <- attr(vocab, "vocab_split")
    t1 <- .mp_tokenize_word(word, vocab_split, dir = 1,
                            allow_compounds = allow_compounds)
    t2 <- .mp_tokenize_word(word, vocab_split, dir = -1,
                            allow_compounds = allow_compounds)
    if (length(t2) < length(t1) & length(t2) > 1) {
        return(t2)
    } else {
        return(t1)
    }
}

# .mp_tokenize_word_lookup -------------------------------------------------

#' Tokenize a Word Including Lookup
#'
#' Look up a word in the table; go to fall-back otherwise.
#'
#' @inheritParams .mp_tokenize_word_bidir
#' @param lookup A morphemepiece lookup table.
#'
#' @return Input word, broken into tokens.
#' @keywords internal
.mp_tokenize_word_lookup <- function(word, vocab, lookup) {
    if (word %in% names(vocab)) { # punctuation, etc.
        return(vocab[word])
    }
    # may as well remove unbroken words from lookup, even though it's relatively
    # small component?
    # mean(purrr::map_int(lookup, grepl, pattern="##"))
    if (word %in% names(lookup)) { 
        breakdown <- lookup[[word]]
        token_list <- stringr::str_split(breakdown, pattern = " ")[[1]]
    } else {
        token_list <- .mp_tokenize_word_bidir(word, vocab)
    }
    return(vocab[token_list])
}


# morphemepiece_tokenize --------------------------------------------------


#' Tokenize Sequence with Morpheme Pieces
#'
#' Given a single sequence of text and a morphemepiece vocabulary, tokenizes the
#' text.  
#'
#' @inheritParams .mp_tokenize_word_lookup
#' @param text Character scalar; text to tokenize.
#'
#' @return A character vector of tokenized text (later, this should be a named
#' integer vector, as in the wordpiece package.)
#' @export
morphemepiece_tokenize <- function(text,
                                   vocab,
                                   lookup,
                                   unk_token = "[UNK]",
                                   max_chars = 100) {
  is_cased <- attr(vocab, "is_cased")
  if (!is_cased) {
    text <- tolower(text)
  }
  
  text <- .convert_to_unicode(text)
  text <- .clean_text(text)
  text <- .tokenize_chinese_chars(text)
  text <- .strip_accents(text)
  text <- .split_on_punc(text)
  text <- purrr::map(wordpiece:::.whitespace_tokenize(text),
                     .f = .mp_tokenize_word_lookup, 
                     vocab = vocab, 
                     lookup = lookup)
  text <- unlist(text)
  return(text)
  #  For testing on datascience:
  # mp_vocab <- load_or_retrieve_vocab("/shared/morphemepiece_vocabs/mp_vocab_big.txt")
  # mp_lookup <- load_or_retrieve_lookup("/shared/morphemepiece_vocabs/mp_lookup_big.txt")
  # morphemepiece_tokenize("I love tacos! And prexxxxationings", mp_vocab, mp_lookup)
}


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
    token_list <- purrr::map_chr(token_list, function(token) {
        .convert_to_unicode(trimws(token))})
    # The vocab is zero-indexed.
    named_vocab <- seq_along(token_list) -1
    names(named_vocab) <- token_list
    
    # attach processed form of vocab as attribute to speed up computations.
    vocab_split <- .split_vocab(token_list)
    is_cased <- .infer_case_from_vocab(token_list) # sure, why not.
    vocab_all <- .new_morphemepiece_vocabulary(named_vocab, 
                                               vocab_split, 
                                               is_cased)
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
#'   location returned by `get_cache_dir()`).
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
                                   cache_dir = get_cache_dir()) {
  return(
    .load_or_retrieve_file(vocab_file,
                           load_vocab,
                           use_cache,
                           cache_dir)
  )
}

# load_lookup --------------------------------------------------------------


#' Load a morphemepiece lookup file
#'
#' @param lookup_file path to vocabulary file. File is assumed to be a text file,
#'   with one word per line. The lookup value, if different from the word,
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
    lookup_lines <- purrr::map_chr(lookup_lines, function(l) {
        .convert_to_unicode(stringr::str_remove_all(l, "[^a-z]*$"))
        # patch for now; fix in wikimorphemes (see "blithely" "fidget" "cyber")
        })
    split_lup <- stringr::str_split_fixed(lookup_lines, pattern = " ", n = 2)

    words <- split_lup[,1]
    breakdowns <- split_lup[,2]
    no_breakdown <- breakdowns == ""
    breakdowns[no_breakdown] <- words[no_breakdown]
    names(breakdowns) <- words
    return(breakdowns) # maybe later make class?
}


# load_or_retrieve_lookup ------------------------------------------------------
#generalize vocab function to do both

#' Load a lookup file, or retrieve from cache
#'
#' @inheritParams load_lookup
#' @param use_cache Logical; if TRUE, will attempt to retrieve the lookup
#'   from the specified cache location, or, if not found there, will ask to save
#'   the lookup as an .rds file.
#' @param cache_dir Character; the path to a cache directory (defaults to
#'   location returned by `get_cache_dir()`).
#'
#' @return The lookup table as a named character vector.
#'
#' @export
#'
#' @examples
#' # todo, after I make tiny sample vocab to include
load_or_retrieve_lookup <- function(lookup_file,
                                    use_cache = TRUE,
                                    cache_dir = get_cache_dir()) {
  return(
    .load_or_retrieve_file(lookup_file,
                           load_lookup,
                           use_cache,
                           cache_dir)
  )
}


# .load_or_retrieve_file ------------------------------------------------------

#' Load a vocab or lookup file, or retrieve from cache
#'
#' @param file Character; path to file to load.
#' @param load_function Function to call to load vocabuary or lookup from txt.
#' @param use_cache Logical; if TRUE, will attempt to retrieve the vocab or
#'   lookup table from the specified cache location, or, if not found there,
#'   will ask to save the vocabulary or lookup as an .rds file.
#' @param cache_dir Character; the path to a cache directory (defaults to
#'   location returned by `get_cache_dir()`).
#'
#' @return The lookup table or vocabulary as returned by `load_function`.
#'
#' @keywords internal
.load_or_retrieve_file <- function(file,
                                   load_function,
                                   use_cache = TRUE,
                                   cache_dir = get_cache_dir()) {
  if (use_cache) {
    cache_filepath <- file.path(cache_dir, 
                                .make_cache_filename(file))
    if (file.exists(cache_filepath)) {
      return(readRDS(cache_filepath)) # nocov
    }
  }
  # Guess we have to load the vocab or lookup from text file.
  contents <- load_function(file)
  
  if (use_cache) { # nocov start
    # ask for permission to write to cache
    if (interactive()) {
      if (isTRUE(utils::askYesNo(paste0("Cache contents at ",
                                        cache_filepath, "?")))) {
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
    return(list(prefixes = prefixes,
                words = words,
                suffixes = suffixes))
}
