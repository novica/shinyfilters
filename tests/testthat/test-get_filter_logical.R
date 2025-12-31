# length-0 `val` orivuded ####
test_that("get_filter_logical() returns all TRUE when val is empty", {
	x <- 1:10
	result <- get_filter_logical(x, val = numeric(0))
	expect_equal(result, rep(TRUE, 10))

	df <- data.frame(a = 1:5)
	result <- get_filter_logical(df, val = character(0), column = "a")
	expect_equal(result, rep(TRUE, 5))
})

# NULL `x` provided ####
test_that("get_filter_logical() returns NULL for NULL input", {
	result <- get_filter_logical(NULL, val = 1:3)
	expect_null(result)
})

# data.frame methods ####
test_that("get_filter_logical() filters data.frame columns correctly", {
	df <- data.frame(
		chr_col = c("a", "b", "c", "a", "b"),
		num_col = 1:5
	)

	# Character column
	result <- get_filter_logical(df, val = c("a", "b"), column = "chr_col")
	expect_equal(result, c(TRUE, TRUE, FALSE, TRUE, TRUE))

	# Numeric column
	result <- get_filter_logical(
		df,
		val = 3,
		column = "num_col",
		comparison = `<=`
	)
	expect_equal(result, c(TRUE, TRUE, TRUE, FALSE, FALSE))
})

test_that("get_filter_logical() throws error for missing column", {
	df <- data.frame(a = 1:5, b = letters[1:5])
	expect_error(
		get_filter_logical(df, val = "test", column = "nonexistent"),
		"Column `nonexistent` not found in `x`"
	)
})

# character `x` provided ####
## no NA provided ####
test_that("get_filter_logical() filters character vectors", {
	x <- c("a", "b", "c", "a", "d")

	# Single value
	result <- get_filter_logical(x, val = "a")
	expect_equal(result, c(TRUE, FALSE, FALSE, TRUE, FALSE))

	# Multiple values
	result <- get_filter_logical(x, val = c("a", "b"))
	expect_equal(result, c(TRUE, TRUE, FALSE, TRUE, FALSE))

	# With NA in val
	result <- get_filter_logical(x, val = c("a", NA))
	expect_equal(result, c(TRUE, FALSE, FALSE, TRUE, FALSE))
})

## NA handling ####
test_that("get_filter_logical() handles NA values in character data", {
	x <- c("a", "b", NA, "a", "c")

	# NA in val includes NA in x
	result <- get_filter_logical(x, val = c("a", NA))
	expect_equal(result, c(TRUE, FALSE, TRUE, TRUE, FALSE))

	# Only NA in val
	result <- get_filter_logical(x, val = NA_character_)
	expect_equal(result, c(FALSE, FALSE, TRUE, FALSE, FALSE))
})

# factor `x` provided ####
test_that("get_filter_logical() filters factor vectors", {
	x <- factor(c("a", "b", "c", "a", "d"))

	# Single value
	result <- get_filter_logical(x, val = "a")
	expect_equal(result, c(TRUE, FALSE, FALSE, TRUE, FALSE))

	# Multiple values
	result <- get_filter_logical(x, val = c("a", "b"))
	expect_equal(result, c(TRUE, TRUE, FALSE, TRUE, FALSE))
})

# logical `x` provided ####
test_that("get_filter_logical() filters logical vectors", {
	x <- c(TRUE, FALSE, TRUE, NA, FALSE)

	# Single value - NA in x remains NA when not explicitly matched
	result <- get_filter_logical(x, val = TRUE)
	expect_equal(result, c(TRUE, FALSE, TRUE, NA, FALSE))

	# With NA - explicitly including NA in val matches NA in x
	result <- get_filter_logical(x, val = c(TRUE, NA))
	expect_equal(result, c(TRUE, FALSE, TRUE, TRUE, FALSE))
})

# numeric `x` provided ####
## single value ####
test_that("get_filter_logical() filters numeric vectors with single value", {
	x <- 1:10

	# Default comparison (<=)
	result <- get_filter_logical(x, val = 5)
	expect_equal(result, x <= 5)

	# Custom comparison
	result <- get_filter_logical(x, val = 5, comparison = `>`)
	expect_equal(result, x > 5)

	result <- get_filter_logical(x, val = 5, comparison = `==`)
	expect_equal(result, x == 5)
})

## range ####
test_that("get_filter_logical() filters numeric vectors with range", {
	x <- 1:10

	# Default (gte = TRUE, lte = TRUE)
	result <- get_filter_logical(x, val = c(3, 7))
	expect_equal(result, x >= 3 & x <= 7)

	# gte = FALSE
	result <- get_filter_logical(x, val = c(3, 7), gte = FALSE)
	expect_equal(result, x > 3 & x <= 7)

	# lte = FALSE
	result <- get_filter_logical(x, val = c(3, 7), lte = FALSE)
	expect_equal(result, x >= 3 & x < 7)

	# Both FALSE
	result <- get_filter_logical(x, val = c(3, 7), gte = FALSE, lte = FALSE)
	expect_equal(result, x > 3 & x < 7)
})

## %in% ####
test_that("get_filter_logical() filters numeric vectors with multiple values", {
	x <- 1:10

	# More than 2 values uses %in%
	result <- get_filter_logical(x, val = c(2, 5, 8))
	expect_equal(result, x %in% c(2, 5, 8))
})

## NA handling ####
test_that("get_filter_logical() handles NA values in numeric data", {
	x <- c(1, 2, NA, 4, 5)

	# NA in val only
	result <- get_filter_logical(x, val = NA_real_)
	expect_equal(result, c(FALSE, FALSE, TRUE, FALSE, FALSE))

	# NA in val with single value
	result <- get_filter_logical(x, val = c(2, NA))
	expect_equal(result, c(TRUE, TRUE, TRUE, FALSE, FALSE))

	# NA in val with two values
	result <- get_filter_logical(x, val = c(2, 5, NA))
	expect_equal(result, c(FALSE, TRUE, TRUE, TRUE, TRUE))

	# NA in val with multiple values
	result <- get_filter_logical(x, val = c(1, 2, 5, NA))
	expect_equal(result, c(TRUE, TRUE, TRUE, FALSE, TRUE))
})

# Date ####
## no NA provided ####
test_that("get_filter_logical() filters Date vectors", {
	x <- Sys.Date() + 0:9

	# Single value
	result <- get_filter_logical(x, val = x[5])
	expect_equal(result, x <= x[5])

	# Range
	result <- get_filter_logical(x, val = c(x[3], x[7]))
	expect_equal(result, x >= x[3] & x <= x[7])

	# Custom comparison
	result <- get_filter_logical(x, val = x[5], comparison = `!=`)
	expect_equal(result, x != x[5])
})

## NA handling ####
test_that("get_filter_logical() handles NA values in Date data", {
	x <- Sys.Date() + c(1, 2, NA, 4, 5)

	# NA in val includes NA in x - with single value Date, uses comparison operator
	# which will propagate NA
	val_with_na <- c(x[2], NA)
	result <- get_filter_logical(x, val = val_with_na)
	# When val has 2 elements and one is NA, after removing NA it becomes single value
	# So it uses comparison operator (<=) with x[2]
	expect_equal(result, c(TRUE, TRUE, TRUE, FALSE, FALSE))
})

# POSIXct `x` provided ####
test_that("get_filter_logical() filters POSIXct vectors", {
	x <- Sys.time() + as.difftime(0:9, units = "days")

	# Single value
	result <- get_filter_logical(x, val = x[5])
	expect_equal(result, as.Date(x) <= as.Date(x[5]))

	# Range
	result <- get_filter_logical(x, val = c(x[3], x[7]))
	expect_equal(
		result,
		as.Date(x) >= as.Date(x[3]) & as.Date(x) <= as.Date(x[7])
	)
})

# POSIXlt `x` provided ####
test_that("get_filter_logical() filters POSIXlt vectors", {
	x <- as.POSIXlt(Sys.time() + as.difftime(0:9, units = "days"))

	# Single value
	result <- get_filter_logical(x, val = x[5])
	expect_equal(result, as.Date(x) <= as.Date(x[5]))

	# Range
	result <- get_filter_logical(x, val = c(x[3], x[7]))
	expect_equal(
		result,
		as.Date(x) >= as.Date(x[3]) & as.Date(x) <= as.Date(x[7])
	)
})

# Type mismatches ####
test_that("get_filter_logical() handles type mismatches appropriately", {
	# This should trigger an S7 dispatch error
	x <- 1:5
	val <- "not_a_number"
	expect_error(
		get_filter_logical(x, val),
		class = "error"
	)

	# Character x with numeric val
	x <- c("a", "b", "c")
	val <- 1:2
	expect_error(
		get_filter_logical(x, val),
		class = "error"
	)
})

# Empty inputs ####
test_that("get_filter_logical() handles empty inputs", {
	# Empty x
	x <- character(0)
	result <- get_filter_logical(x, val = "a")
	expect_equal(result, logical(0))

	# Empty numeric
	x <- numeric(0)
	result <- get_filter_logical(x, val = 5)
	expect_equal(result, logical(0))
})

# Vector length preserved ####
test_that("get_filter_logical() preserves vector length", {
	# Character
	x <- letters[1:10]
	result <- get_filter_logical(x, val = c("a", "z"))
	expect_length(result, 10)

	# Numeric
	x <- 1:20
	result <- get_filter_logical(x, val = c(5, 15))
	expect_length(result, 20)

	# Date
	x <- Sys.Date() + 1:15
	result <- get_filter_logical(x, val = x[7])
	expect_length(result, 15)
})
