#' Season classifying
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
aes_season <-
  function(x) {
    x <- (as.POSIXlt(x)$mon) + 1L
    c("Summer", "Summer", "Autumn", "Autumn", "Autumn", "Winter", "Winter", "Winter", "Spring", "Spring", "Spring", "Summer")[x]
  }