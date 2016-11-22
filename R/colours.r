#' Habitat region colours
#'
#' @return tibble with name and col-our
#' @export
#' @importFrom grDevices rgb
#' @examples
#' aes_zone_cols()
aes_zone_cols <- function() {
  tibble::tibble(name = c("Atlantic", "Indian", "WestPacific", "EastPacific"), col = rgb(c(124, 0, 248, 199), c(174, 191, 118, 124), c(0, 196, 109, 255), maxColorValue = 255))
}
