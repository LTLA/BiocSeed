# This tests the basic BiocSeed functionality.
# library(testthat); library(BiocSeed); source("test-seed.R")

test_that("setBiocSeed works correctly", {
    info <- setBiocSeed(letters)
    X <- runif(5)
    unsetBiocSeed(info)

    # Same as above.
    info <- setBiocSeed(letters)
    Y <- runif(5)
    unsetBiocSeed(info)
    expect_identical(X, Y)
                                           
    # Different, but deterministically so.
    info <- setBiocSeed(LETTERS)
    Z <- runif(5)
    unsetBiocSeed(info)
    expect_false(identical(X, Z))

    info <- setBiocSeed(LETTERS)
    Z2 <- runif(5)
    unsetBiocSeed(info)
    expect_identical(Z, Z2)
})

test_that("setBiocSeed does not affect the global RNG", {
    info <- setBiocSeed(LETTERS)
    runif(5)
    unsetBiocSeed(info)
    X <- runif(1)
                                                             
    info <- setBiocSeed(LETTERS)
    runif(5)
    unsetBiocSeed(info)
    Y <- runif(1)

    expect_false(identical(X, Y))

    # Trying again in another situation.
    set.seed(1000)
    X <- runif(10)

    set.seed(1000)
    info <- setBiocSeed(LETTERS)
    runif(5)
    unsetBiocSeed(info)

    Y <- runif(10)
    expect_identical(X, Y)
})

test_that("setBiocSeed responds to disabling", {
    set.seed(100)
    X <- runif(10)
    
    set.seed(100)
    info <- setBiocSeed(1:100)
    Y <- runif(10)
    unsetBiocSeed(info)
    expect_false(identical(X, Y))

    disableBiocSeed()
    set.seed(100)
    info <- setBiocSeed(1:100)
    Z <- runif(10)
    unsetBiocSeed(info)
    expect_identical(X, Z)

    enableBiocSeed()
    set.seed(100)
    info <- setBiocSeed(1:100)
    Z2 <- runif(10)
    unsetBiocSeed(info)
    expect_identical(Y, Z2)
})

test_that("setBiocSeed responds to seed= specification", {
    set.seed(100)
    X <- runif(10)

    info <- setBiocSeed(1:100, seed=100)
    Y <- runif(10)
    unsetBiocSeed(info)

    expect_identical(X, Y)
})
