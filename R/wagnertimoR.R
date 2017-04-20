


#' @title formatGermanNumber
#'
#' @param x - a 'german' number in the format like e.g. 123.456.789,09
#'
#' @return the converted input number with only a point as decimal delimiter. E.g. 123546789.09
#'
#' @examples
#' singlePointNumber <- formatGermanNumber("123.456.789,09")
#'
#' @export
#'
formatGermanNumber <- function(x){
  z <- gsub("[^0-9,.]", "", x)
  z <- gsub("\\.", "", z)
  gsub(",", ".", z)
}





