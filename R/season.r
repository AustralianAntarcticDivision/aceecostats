#' Season classifying
#'
#' @param x date
#'
#' @return text Spring, Summer, Autumn, or Winter
#' @export
#'
#'
#' @examples
#' aes_season("Autumn")
aes_season <-
  function(x) {
    x <- (as.POSIXlt(x)$mon) + 1L
    c("Summer", "Summer", "Autumn", "Autumn", "Autumn", "Winter", "Winter", "Winter", "Spring", "Spring", "Spring", "Summer")[x]
  }