#' imports
#' @importFrom R6 R6Class
#' @import hellno
#' @import stringb
#' @import rtext
#' @useDynLib diffrprojects
dummyimport <- function(){
  R6::R6Class()
  Rcpp::sizeof(1)
  1 %>% magrittr::add(1)
}

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`
