holding <- new.env()
holding$seed <- NULL
holding$active <- TRUE 

#' Smart seed setter
#'
#' Set the seed in a \dQuote{deterministically variable} manner,
#' typically for use within functions that have some stochastic component.
#' This reduces the pressure on the user to ensure that the seed is properly set.
#'
#' @param x Any R object, typically a slice of the dataset being operated on.
#'
#' @return 
#' \code{setBiocSeed} will set the global seed to some deterministically chosen value based on \code{x}.
#'
#' \code{unsetBiocSeed} will restore the global seed to what it was before calling \code{setBiocSeed}.
#'
#' \code{disableBiocSeed} will cause all \code{setBiocSeed} and \code{unsetBiocSeed} to be no-ops.
#' This is reversed by \code{enableBiocSeed}.
#'
#' All functions return \code{NULL} invisibly.
#'
#' @details
#' In some situations, an algorithm is theoretically deterministic but the implementation has a stochastic component, e.g., for initialization.
#' It would be helpful if we could abstract away the stochastic component so that users do not have to manually call \code{\link{set.seed}} to obtain reproducible results.
#' This is the purpose of \code{setBiocSeed}, which implements a number of tricks to play nice with the global seed.
#'
#' Firstly, the choice of seed is based on \code{x}, usually a chunk of the dataset of interest. 
#' Specifically, we serialize \code{x} and hash the resulting raw vector to obtain an integer to use as the seed. 
#' This process yields a deterministic choice that is also variable with different \code{x}.
#' It ensures that we do not always pick the same seed in all situations,
#' which could lead to problems, e.g., due to biases or from a poor choice of initializations.
#' 
#' Secondly, the original seed in \code{\link{.Random.seed}} is restored upon calling \code{\link{unsetBiocSeed}}.
#' This ensures that we do not clobber the seed in the user's session by consistently resetting it to the same value.
#' For example, let's say we have a function calling \code{setBiocSeed}, and that function was also used in a simulation with multiple replicate iterations.
#' Failing to restore the global seed would cause all iterations to be identical if the seed is always reset to some constant value.
#' 
#' Thirdly, repeated invocations of \code{setBiocSeed} have no effect beyond the first.
#' If we have a function that calls other functions that call \code{setBiocSeed}, it can be a good idea to call \code{setBiocSeed} explicitly at the start of the outer function.
#' This means that the PRNG has opportunity to progress further through the random number stream -
#' away from the \eqn{2^{32}-1} possible seed choices, which should further reduce the risk of biases.
#'
#' Finally, the entire system can be turned on or with \code{disableBiocSeed} and \code{enableBiocSeed}.
#' This may be useful if the seed derived from \code{x} is not appropriate and/or the user wants to control the process via the global \code{\link{set.seed}}.
#'
#' @author Aaron Lun 
#'
#' @examples
#' setBiocSeed(letters)
#' runif(5)
#' unsetBiocSeed()
#' 
#' # Same as above.
#' setBiocSeed(letters)
#' runif(5)
#' unsetBiocSeed()
#'
#' # Different, but deterministically so.
#' setBiocSeed(LETTERS)
#' runif(5)
#' unsetBiocSeed()
#'
#' # Does not affect the RNG outside of the set/unset pair.
#' setBiocSeed(LETTERS)
#' X <- runif(5)
#' unsetBiocSeed()
#' runif(1)
#'
#' setBiocSeed(LETTERS)
#' X <- runif(5)
#' unsetBiocSeed()
#' runif(1)
#' @export
setBiocSeed <- function(x) {
    if (is.null(holding$seed) && holding$active) {
        if (exists(".Random.seed", envir=.GlobalEnv)) {  
            holding$seed <- get(".Random.seed", envir=.GlobalEnv)
        }

        vals <- serialize(x, NULL)
        alt <- .hash(vals)
        set.seed(alt)
    }
    invisible(NULL)
}

#' @useDynLib BiocSeed, .registration=TRUE
.hash <- function(x) .Call("djb2", x, PACKAGE="BiocSeed")

#' @export
#' @rdname setBiocSeed
unsetBiocSeed <- function() {
    if (holding$active) {
        if (!is.null(holding$seed)) {
            assign(".Random.seed", value=holding$seed, envir=.GlobalEnv)
            holding$seed <- NULL
        } else {
            rm(".Random.seed", envir=.GlobalEnv)
        }
    }
    invisible(NULL)
}

#' @export
#' @rdname setBiocSeed
enableBiocSeed <- function() {
    holding$active <- TRUE
    invisible(NULL)
}

#' @export
#' @rdname setBiocSeed
disableBiocSeed <- function() {
    holding$active <- FALSE
    invisible(NULL)
}