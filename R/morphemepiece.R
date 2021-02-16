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

#' Pipe operator
#'
#' The pipe function, \code{\%>\%}, allows you to turn function composition into
#' a series of imperative statements.
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @param lhs,rhs A vector of fields or a tibble of fields and values, and a
#'   function to apply to them.
NULL


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

#' Apply Morphemepiece tokenization to word
#'
#' Repeatedly pull out the longest valid vocabulary piece from the word until
#' the whole word is represented in terms of vocabulary pieces. This algorithm
#' is intended to be used as a "fall-back" for words that are not found in the
#' morphemepiece lookup table.
#'
#' @param word Character; word to tokenize.
#' @param vocab A morphemepiece vocabulary.
#'
#' @return A character vector corresponding to the original word, broken into 
#' pieces.
#'
#' @keywords internal
.mp_tokenize_word <- function(word, vocab) {
    # handle prefixes a little differently...
    prefixes <- vocab[stringr::str_ends(vocab, "##")]
    names(prefixes) <- stringr::str_sub(prefixes, end = -3)
    
    # here, the names have the ##, because that's what's unique.
    mp_vocab_nohash <- stringr::str_remove_all(vocab, "#")
    names(mp_vocab_nohash) <- vocab
    
    current_breakdown <- word
    keepgoing <- TRUE
    while(keepgoing) {
        current_breakdown <- current_breakdown[current_breakdown != ""]
        piece_found <- current_breakdown %in% vocab
        if (all(piece_found)) {
            break
        } else {
            piece_to_work_on <- match(FALSE, piece_found)
            current_word <- current_breakdown[piece_to_work_on]
        }
        original_start <- FALSE
        if (piece_to_work_on == 1) {
            original_start <- TRUE
        }
        original_end <- FALSE
        if (piece_to_work_on == length(current_breakdown)) {
            original_end <- TRUE
        }

        this_breakdown <- unlist(
            .pull_out_longest_piece(current_word, mp_vocab_nohash,
                                    original_start = original_start,
                                    original_end = original_end,
                                    prefixes = prefixes)
        )
        if (!stringr::str_ends(this_breakdown[[2]], "##") & 
            this_breakdown[[3]] != "") {
            this_breakdown[[3]] <- paste0("##", this_breakdown[[3]])
        }
        if (!stringr::str_starts(this_breakdown[[2]], "##") & 
            this_breakdown[[1]] != "") {
            this_breakdown[[1]] <- paste0(this_breakdown[[1]], "##")
        }
        current_breakdown[piece_to_work_on] <- list(this_breakdown)
        current_breakdown <- unlist(current_breakdown)
    }
    return(current_breakdown)
}


# morphemepiece_tokenize --------------------------------------------------


#' Tokenize Sequence with Morpheme Pieces
#'
#' Given a single sequence of text and a morphemepiece vocabulary, tokenizes the
#' text. (This is *not* fully functional yet. It's mainly a placeholder so that
#' the package exports something.)
#' 
#'
#' @inheritParams .mp_tokenize_word
#' @param text Character scalar; text to tokenize.
#'
#' @return A character vector of tokenized text (later, this should be a named
#' integer vector, as in the wordpiece package.)
#' @export
morphemepiece_tokenize <- function(text,
                                   vocab) {

    text <- tolower(text)

    # collapse whitespace
    text <- stringr::str_replace_all(text, pattern = "\\s+", " ")
    # for placeholder, strip out all non-latin characters
    text <- stringr::str_remove_all(text, pattern = "[^a-z ]")

    # split on whitespace
    text <- unlist(stringi::stri_split_regex(text, "\\s", omit_empty = TRUE))
    # tokenize each piece
    text <- purrr::map(text, .f = .mp_tokenize_word, vocab = vocab)
    text <- unlist(text)
    # eventually want to do something like:
    # ids <- vocab[text]
    # names(ids) <- text
    # return(ids)
    return(text)
    #  For testing on datascience:
    #  mp_vocab <- readRDS("/shared/morphemepiece_vocabs/mp_vocab.rds")
    #  morphemepiece_tokenize("I love tacos! And prexxxxationings", mp_vocab)
}

