#' Simulate coin tosses
#'
#' @param n Number of tosses to simulate
#'
#' @return An object of class `cointoss`
#' @export
#'
#' @examples
#' toss_coin(10)
toss_coin <- function(n = 10) {
  if (!is.numeric(n) || n <= 0) stop("n must be a positive number")

  outcomes <- sample(c("Heads", "Tails"), size = n, replace = TRUE)
  structure(
    list(
      n = n,
      outcomes = outcomes
    ),
    class = "cointoss"
  )
}
