# This tests the internal hashing functions.
# library(testthat); library(BiocSeed); source("test-internal.R")

test_that("char and int hashing methods yield the same results", {
    value <- "luna"
    target <- BiocSeed:::.hash_list(list(value))
    expect_identical(target, 2090485941L)

    converter <- function(x) {
        X <- vapply(x, function(y) sum(as.integer(charToRaw(y)) * (256L ^ (0:3))), 0)
        as.integer(X)
    }
    target2 <- BiocSeed:::.hash_list(list(converter(value)))
    expect_identical(target, target2)

    # Behaves properly with vectors.
    self <- c("luna", "alun")
    converted <- converter(self)

    ref <- BiocSeed:::.hash_list(list(self))
    expect_identical(ref, BiocSeed:::.hash_list(list(converted)))
    expect_identical(ref, BiocSeed:::.hash_list(list(self[1], converted[2])))
    expect_identical(ref, BiocSeed:::.hash_list(list(converted[1], self[2])))
})

test_that("unsigned to signed conversion works as expected", {
    converter <- function(x) .Call("check_conversion", x, PACKAGE="BiocSeed")

    expect_identical(converter(0), 0L)
    expect_identical(converter(1*.Machine$integer.max), .Machine$integer.max)

    expect_identical(converter(1*.Machine$integer.max+1), 0L) # NA failsafe.

    expect_identical(converter(1*.Machine$integer.max+2), -.Machine$integer.max) 
    expect_identical(converter(2*.Machine$integer.max+1), -1L)
})
