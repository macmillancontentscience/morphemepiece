% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{set_morphemepiece_cache_dir}
\alias{set_morphemepiece_cache_dir}
\title{Set a Cache Directory for Morphemepiece}
\usage{
set_morphemepiece_cache_dir(cache_dir = NULL)
}
\arguments{
\item{cache_dir}{Character scalar; a path to a cache directory.}
}
\value{
A normalized path to a cache directory. The directory is created if
the user has write access and the directory does not exist.
}
\description{
Use this function to override the cache path used by morphemepiece for the
current session. Set the \code{MORPHEMEPIECE_CACHE_DIR} environment variable
for a more permanent change.
}
