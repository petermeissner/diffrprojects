#' imports
#' @importFrom R6 R6Class
#' @import hellno
#' @importFrom magrittr %>%
#' @import stringb
#' @import rtext
dummyimport <- function(){
  R6::R6Class()
  1 %>% magrittr::add(1)
}

# #' @useDynLib diffrprojects
# #' @importFrom Rcpp sourceCpp
# NULL
