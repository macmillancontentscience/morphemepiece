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

# %||% ---------------------------------------------------------------------

#' Default value for `NULL`
#'
#' Mostly copied from rlang package.
#'
#' @param x,y If `x` is NULL, will return `y`; otherwise returns `x`.
#' @return Returns `x` if `x` is not NULL; otherwise returns `y`.
#' @keywords internal
#' @name op-null-default
`%||%` <- function(x, y) {
  if (is.null(x)) y else x # nocov
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
  return(structure(vocab,
    "vocab_split" = vocab_split,
    "is_cased" = is_cased,
    class = c("morphemepiece_vocabulary", "integer")
  ))
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

# .make_cache_filename --------------------------------------------------

#' Construct Cache File Name
#'
#' Given the path to a vocabulary file, construct a unique filename using the
#' hash of the path.
#'
#' @inheritParams load_vocab
#' @return A unique filename to use for cacheing the vocabulary.
#'
#' @keywords internal
.make_cache_filename <- function(vocab_file) {
  just_name <- basename(vocab_file)
  dirpath <- normalizePath(dirname(vocab_file))
  path_hash <- digest::digest(dirpath, algo = "xxhash32")
  return(paste(just_name, path_hash, "rds", sep = "."))
}


# get_cache_dir --------------------------------------------------

#' Retrieve Directory for vocabulary Cache
#'
#' @return A unique filename to use for cacheing the vocabulary.
#' @export
get_cache_dir <- function() {
  return(
    getOption("morphemepiece.dir") %||%
      rappdirs::user_cache_dir(appname = "morphemepiece")
  )
}
