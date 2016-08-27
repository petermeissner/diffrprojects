#' class for diffrproject
#'
#' @docType class
#'
#' @name diffrproject
#'
#' @export
#'
#' @keywords data
#'
#' @return Object of \code{\link{diffrproject}}
#'
#' @format \code{\link{R6Class}} object.
#'
#'
diffrproject <-
  R6::R6Class(

    #### class name ============================================================
    classname    = "diffrproject",

    #### misc ====================================================================
    active       = NULL,
    inherit      = dp_align,
    lock_objects = TRUE,
    class        = TRUE,
    portable     = TRUE,
    lock_class   = FALSE,
    cloneable    = TRUE,
    parent_env   = asNamespace('diffrprojects')

  )# closes R6Class

