holding <- new.env()
holding$active <- FALSE
holding$enabled <- TRUE

#' Smart seed setter
#'
#' Set the seed in a \dQuote{deterministically variable} manner,
#' typically for use within functions that have some stochastic component.
#' This reduces the pressure on the user to ensure that the seed is properly set.
#'
#' @param x An R object of an atomic type, e.g., an integer or character vector.
#' @param digits Integer scalar specifying the number of significant digits to retain for double or complex \code{x}.
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
#' Specifically, we convert \code{x} to an integer or character vector, and hash it to obtain an integer to use as the seed. 
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
#' @section Dealing with floating-point:
#' When \code{x} is double-precision, some care is required as different machines may not perform computations with the same precision.
#' While \code{x} might be \emph{theoretically} identical across machines, it may differ in practice in some of the less significant bits. 
#' These differences would result in \code{setBiocSeed} generating a machine-dependent seed, which is not desirable.
#'
#' To overcome this, we round all doubles to the first \code{digits} significant figures, 
#' thus ignoring any machine-dependent differences in the least significant bits.
#' We then convert each double into two integers for the base-10 exponent and mantissa in preparation for hashing.
#' For complex numbers, the same treatment is applied to the real and imaginary parts separately.
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
setBiocSeed <- function(x, digits=6) {
    old.seed <- NULL

    if (holding$enabled && !holding$active) {
        holding$active <- TRUE

        if (exists(".Random.seed", envir=.GlobalEnv)) {  
            old.seed <- get(".Random.seed", envir=.GlobalEnv)
        } else {
            old.seed <- NA
        }

        if (is.character(x)) {
            alt <- .hash_char(x)
        } else {
            if (is.double(x)) {
                x <- .real2int(x, digits=digits)
            } else if (is.complex(x)) {
                x <- cbind(
                    .real2int(Re(x), digits=digits),
                    .real2int(Im(x), digits=digits)
                )
            } else if (!is.atomic(x)) {
                stop("'x' must be an atomic type")
            }
            alt <- .hash_int(x)
        }

        set.seed(alt)
    }

    invisible(old.seed)
}

.real2int <- function(x, digits) 
# We use this rather convoluted way of obtaining integers from a double as I
# don't want to rely on the format() function. Changes in the defaults could
# conceivably change the output string and cause headaches. 
{
    expo <- floor(log10(abs(x)))
    expo[!is.finite(expo)] <- 0
    mant <- x / 10^(expo - digits + 1)
    cbind(as.integer(mant), as.integer(expo))
}

#' @useDynLib BiocSeed, .registration=TRUE
.hash_char <- function(x) .Call("djb2_char", x, PACKAGE="BiocSeed")

.hash_int <- function(x) .Call("djb2_int", x, PACKAGE="BiocSeed")

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
