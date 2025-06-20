#' Print method for cointoss object
#' @param x A cointoss object
#' @param ... Ignored
#' @export
print.cointoss <- function(x, ...) {
  cat("Coin toss simulation:\n")
  cat("Number of tosses:", x$n, "\n")
  head_count <- sum(x$outcomes == "Heads")
  tail_count <- sum(x$outcomes == "Tails")
  cat("Heads:", head_count, "\nTails:", tail_count, "\n")
}

#' Summary method for cointoss object
#' @param object A cointoss object
#' @param ... Ignored
#' @return A named list of proportions
#' @export
summary.cointoss <- function(object, ...) {
  tab <- table(object$outcomes)
  props <- prop.table(tab)
  as.list(props)
}

#' Plot method for cointoss object
#'
#' @param x A cointoss object
#' @param type Type of plot: "bar" (default) or "path"
#' @param ... Additional arguments passed to plotting functions
#' @export
plot.cointoss <- function(x, type = c("bar", "path"), ...) {
  type <- match.arg(type)

  if (type == "bar") {
    # Barplot of total outcomes
    tab <- table(x$outcomes)
    barplot(as.numeric(tab),
            names.arg = names(tab),
            col = c("skyblue", "tomato"),
            main = "Coin Toss Results",
            ylab = "Frequency")
  }

  if (type == "path") {
    # Summation path plot
    numeric_outcomes <- ifelse(x$outcomes == "Heads", 1, -1)
    cum_path <- cumsum(numeric_outcomes)
    plot(cum_path,
         type = "l",
         col = "darkgreen",
         lwd = 2,
         main = "Cumulative Coin Toss Path",
         xlab = "Toss Number",
         ylab = "Net Heads (Heads - Tails)")
    abline(h = 0, col = "gray", lty = 2)
  }
}


