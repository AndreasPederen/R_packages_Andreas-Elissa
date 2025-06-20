# inst/tinytest/test_cointoss.R

# Test class
expect_true(inherits(toss_coin(10), "cointoss"))

# Test error on invalid input
expect_error(toss_coin(-5))
expect_error(toss_coin("ten"))

# Test correct number of outcomes
result <- toss_coin(20)
expect_equal(length(result$outcomes), 20)

# Test that outcomes are only "Heads" or "Tails"
expect_true(all(result$outcomes %in% c("Heads", "Tails")))

# Test summary output is a list with correct names
s <- summary(result)
expect_true(is.list(s))
expect_true(all(names(s) %in% c("Heads", "Tails")))
