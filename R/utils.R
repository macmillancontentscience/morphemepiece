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


# morphemepiece_cache_dir --------------------------------------------------

#' Retrieve Directory for Morphemepiece Cache
#'
#' The morphemepiece cache directory is a platform- and user-specific path where
#' morphemepiece saves caches (such as a downloaded lookup). You can override
#' the default location in a few ways:
#' \itemize{
#'   \item{Option: \code{morphemepiece.dir}}{Use
#'   \code{\link{set_morphemepiece_cache_dir}} to set a specific cache directory
#'   for this session}
#'   \item{Environment: \code{MORPHEMEPIECE_CACHE_DIR}}{Set this environment
#'   variable to specify a morphemepiece cache directory for all sessions.}
#'   \item{Environment: \code{R_USER_CACHE_DIR}}{Set this environment variable
#'   to specify a cache directory root for all packages that use the caching
#'   system.}
#' }
#'
#' @return A character vector with the normalized path to the cache.
#' @export
morphemepiece_cache_dir <- function() {
  return(dlr::app_cache_dir("morphemepiece"))
}

#' Set a Cache Directory for Morphemepiece
#'
#' Use this function to override the cache path used by morphemepiece for the
#' current session. Set the \code{MORPHEMEPIECE_CACHE_DIR} environment variable
#' for a more permanent change.
#'
#' @param cache_dir Character scalar; a path to a cache directory.
#'
#' @return A normalized path to a cache directory. The directory is created if
#'   the user has write access and the directory does not exist.
#' @export
set_morphemepiece_cache_dir <- function(cache_dir = NULL) {
  return(dlr::set_app_cache_dir("morphemepiece", cache_dir))
}
