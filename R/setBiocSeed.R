holding <- new.env()
holding$enabled <- TRUE

#' Smart seed setter
#'
#' Set the seed in a \dQuote{deterministically variable} manner,
#' typically for use within functions that have some stochastic component.
#' This aims to provide a compromise between convenient reproducibility and statistical rigor.
#'
#' @param x An integer or character vector, or a list containing any number of such vectors.
#' @param digits Integer scalar specifying the number of significant digits to retain for double or complex \code{x}.
#' @param seed An optional integer scalar specifying the actual seed to use.
#' @param previous An integer vector corresponding to the previous seed or \code{NA}, typically returned by \code{setBiocSeed}.
#'
#' @return 
#' \code{setBiocSeed} will set the global seed to some deterministically chosen value based on \code{x}.
#' It invisibly returns the old value of \code{\link{.Random.seed}}.
#'
#' \code{unsetBiocSeed} will restore the global seed to what it was before calling \code{setBiocSeed}.
#' It will also return \code{NULL} invisibly.
#'
#' \code{disableBiocSeed} will cause all \code{setBiocSeed} and \code{unsetBiocSeed} to be no-ops.
#' This is reversed by \code{enableBiocSeed}.
#' Both functions return \code{NULL} invisibly.
#'
#' @details
#' The choice of seed is based on \code{x}, somehow derived from the dataset of interest. 
#' Specifically, we take the integer/character vectors in \code{x} and hash them to obtain an integer to use as the seed.
#' This ensures that results for any given dataset are reproducible while avoiding the use of a constant seed for all datasets.
#' 
#' The original seed in \code{\link{.Random.seed}} is restored upon calling \code{\link{unsetBiocSeed}}.
#' This ensures that we do not clobber the seed in the user's session by consistently resetting it to the same value.
#' The typical pattern is to call \code{unsetBiocSeed} in \code{\link{on.exit}} with the return value of \code{setBiocSeed}.
#' 
#' Alternatively, the user can explicitly pass in a non-\code{NULL} value for \code{seed}, which is used directly rather than deriving the seed from \code{x}.
#' This may be useful if the seed derived from \code{x} is not appropriate and/or the user wants to test out the effects of different seeds.
#' Note that this, again, has no effect in nested calls beyond the first.
#'
#' Finally, the entire system can be turned on or with \code{disableBiocSeed} and \code{enableBiocSeed}.
#' This is useful for backcompatibility where users can control the process via the global \code{\link{set.seed}}.
#'
#' @author Aaron Lun 
#'
#' @examples
#' info <- setBiocSeed(letters)
#' runif(5)
#' unsetBiocSeed(info)
#' 
#' # Same as above.
#' info <- setBiocSeed(letters)
#' runif(5)
#' unsetBiocSeed(info)
#'
#' # Different, but deterministically so.
#' info <- setBiocSeed(LETTERS)
#' runif(5)
#' unsetBiocSeed(info)
#'
#' # Does not affect the RNG outside of the set/unset pair.
#' info <- setBiocSeed(LETTERS)
#' X <- runif(5)
#' unsetBiocSeed(info)
#' runif(1)
#'
#' info <- setBiocSeed(LETTERS)
#' X <- runif(5)
#' unsetBiocSeed(info)
#' runif(1)
#'
#' # Typical usage in a function:
#' FUN <- function(x) {
#'     info <- setBiocSeed(head(x))
#'     on.exit(unsetBiocSeed(info))
#'     sample(x)
#' }
#' 
#' FUN(1:10)
#' FUN(1:10)
#'
#' @export
setBiocSeed <- function(x, seed=NULL) {
    old.seed <- NULL
    if (!is.list(x)) {
        x <- list(x)
    }

    if (holding$enabled) {
        holding$active <- TRUE

        if (exists(".Random.seed", envir=.GlobalEnv)) {  
            old.seed <- get(".Random.seed", envir=.GlobalEnv)
        } else {
            old.seed <- NA
        }

        if (is.null(seed)) {
            seed <- .hash_list(x)
            if (is.na(seed)) {
                stop("'x' can only contain integer or string vectors")
            }
        }

        set.seed(seed)
    }

    invisible(old.seed)
}

#' @export
#' @rdname setBiocSeed
unsetBiocSeed <- function(previous) {
    if (holding$enabled) {
        if (!identical(previous, NA)) {
            assign(".Random.seed", value=previous, envir=.GlobalEnv)
        } else {
            rm(".Random.seed", envir=.GlobalEnv)
        }
    }
    invisible(NULL)
}

#' @useDynLib BiocSeed, .registration=TRUE
.hash_list <- function(x) .Call("djb2_list", x, PACKAGE="BiocSeed")

#' @export
#' @rdname setBiocSeed
enableBiocSeed <- function() {
    holding$enabled <- TRUE
    invisible(NULL)
}

#' @export
#' @rdname setBiocSeed
disableBiocSeed <- function() {
    holding$enabled <- FALSE
    invisible(NULL)
}
