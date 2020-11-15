# This tests the internal hashing functions.
# library(testthat); library(BiocSeed); source("test-internal.R")

test_that("char and int hashing methods yield the same results", {
    value <- "luna"
    target <- BiocSeed:::.hash_char(value)
    expect_identical(target, 2090485941L)

    converter <- function(x) {
        X <- vapply(x, function(y) sum(as.integer(charToRaw(y)) * (256L ^ (0:3))), 0)
        as.integer(X)
    }
    target2 <- BiocSeed:::.hash_int(converter(value))
    expect_identical(target, target2)

    # Behaves properly with vectors.
    self <- c("luna", "alun")
    expect_identical(
        BiocSeed:::.hash_char(self),
        BiocSeed:::.hash_int(converter(self))
    )
})

test_that("unsigned to signed conversion works as expected", {
    converter <- function(x) .Call("check_conversion", x, PACKAGE="BiocSeed")

    expect_identical(converter(0), 0L)
    expect_identical(converter(1*.Machine$integer.max), .Machine$integer.max)

    expect_identical(converter(1*.Machine$integer.max+1), 0L) # NA failsafe.

    expect_identical(converter(1*.Machine$integer.max+2), -.Machine$integer.max) 
    expect_identical(converter(2*.Machine$integer.max+1), -1L)
})

set.seed(1000)
test_that("double splitting to integers works as expected", {
    out <- runif(11, -1, 1) * 10^(-5:5)
    X <- BiocSeed:::.real2int(out, digits=5)

    expect_true(all(abs(X[,1]) < 1e5))
    recon <- X[,1] / 1e4 * 10^X[,2]
    expect_true(all(abs(1 - recon/out) < 1e-4))

    # More digits. 
    out <- runif(11, -1, 1) * 10^(5:-5)
    X <- BiocSeed:::.real2int(out, digits=8)

    expect_true(all(abs(X[,1]) < 1e8))
    recon <- X[,1] / 1e7 * 10^X[,2]
    expect_true(all(abs(1 - recon/out) < 1e-7))
})
