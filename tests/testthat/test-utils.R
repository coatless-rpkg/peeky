# Test validate_app_json() ----

test_that("validate_app_json(): checks structure correctly", {
    # Test case 1: Valid app.json structure
    # Contains all required fields: name, content, and type
    valid_data <- list(
        list(
            name = "app.R",
            content = "library(shiny)\n...",
            type = "text"
        )
    )
    # Should return TRUE for valid structure
    testthat::expect_true(validate_app_json(valid_data))

    # Test case 2: Empty list
    # app.json must contain at least one file
    testthat::expect_error(
        validate_app_json(list()),
        "File list is empty"
    )

    # Test case 3: Missing required fields
    # Each file entry must have name, content, and type
    testthat::expect_error(
        validate_app_json(list(list(name = "app.R"))),
        "Missing required fields"
    )

    # Test case 4: Invalid data type
    # app.json must be a list/array
    testthat::expect_error(
        validate_app_json("not a list"),
        "Expected a list"
    )
})

# Test padding_width() ----

test_that("padding_width(): calculates correct width", {
    # Test single digit numbers (1-9)
    # Should return padding width of 1
    testthat::expect_equal(padding_width(1), 1)
    testthat::expect_equal(padding_width(9), 1)

    # Test two digit numbers (10-99)
    # Should return padding width of 2
    testthat::expect_equal(padding_width(10), 2)
    testthat::expect_equal(padding_width(99), 2)

    # Test three digit numbers (100+)
    # Should return padding width of 3
    testthat::expect_equal(padding_width(100), 3)

    # Test edge cases
    # Zero and negative numbers should return minimum width of 1
    testthat::expect_equal(padding_width(0), 1)   # Zero case
    testthat::expect_equal(padding_width(-1), 1)  # Negative number case
})
