holding <- new.env()
holding$active <- FALSE
holding$enabled <- TRUE

#' Smart seed setter
#'
#' Set the seed in a \dQuote{deterministically variable} manner,
#' typically for use within functions that have some stochastic component.
#' This reduces the pressure on the user to ensure that the seed is properly set.
#'
#' @param x Any R object that can be coerced into a character vector.
#' @param info Output of \code{setBiocSeed}.
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
#' The choice of seed is based on \code{x}, usually a chunk of the dataset of interest. 
#' Specifically, we convert \code{x} to a character vector and hash it to obtain an integer to use as the seed. 
#' This process yields a deterministic seed for reproducible results, which is also variable with different \code{x}.
#' 
#' Secondly, the original seed in \code{\link{.Random.seed}} is restored upon calling \code{\link{unsetBiocSeed}}.
#' This ensures that we do not clobber the seed in the user's session by consistently resetting it to the same value.
#' 
#' Thirdly, repeated invocations of \code{setBiocSeed} have no effect beyond the first.
#' If we have a function that calls other functions that call \code{setBiocSeed}, it can be a good idea to call \code{setBiocSeed} explicitly at the start of the outer function.
#' This means that the PRNG has an opportunity to progress further through the random number stream.
#'
#' Finally, the entire system can be turned on or with \code{disableBiocSeed} and \code{enableBiocSeed}.
#' This may be useful if the seed derived from \code{x} is not appropriate and/or the user wants to control the process via the global \code{\link{set.seed}}.
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
setBiocSeed <- function(x) {
    old.seed <- NULL

    if (holding$enabled && !holding$active) {
        holding$active <- TRUE

        if (exists(".Random.seed", envir=.GlobalEnv)) {  
            old.seed <- get(".Random.seed", envir=.GlobalEnv)
        } else {
            old.seed <- NA
        }

        x <- as.character(x)
        alt <- .hash(x)
        set.seed(alt)
    }

    invisible(old.seed)
}

#' @useDynLib BiocSeed, .registration=TRUE
.hash <- function(x) .Call("djb2", x, PACKAGE="BiocSeed")

#' @export
#' @rdname setBiocSeed
unsetBiocSeed <- function(info) {
    if (holding$enabled && !is.null(info)) {
        holding$active <- FALSE

        if (!identical(info, NA)) {
            assign(".Random.seed", value=info, envir=.GlobalEnv)
        } else {
            rm(".Random.seed", envir=.GlobalEnv)
        }
    }
    invisible(NULL)
}

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
