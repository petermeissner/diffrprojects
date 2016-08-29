
#' class for dp_align
#'
#' @docType class
#'
#' @name diffrproject
#'
#' @export
#'
#' @keywords data
#'
#' @return Object of \code{\link{dp_align}}
#'
#' @format \code{\link{R6Class}} object.
#'
#'
dp_align <-
  R6::R6Class(

    #### class name ============================================================
    classname    = "dp_align",

    #### misc ====================================================================
    active       = NULL,
    inherit      = dp_base,
    lock_objects = TRUE,
    class        = TRUE,
    portable     = TRUE,
    lock_class   = FALSE,
    cloneable    = TRUE,
    parent_env   = asNamespace('diffrprojects'),

    #### public ================================================================
    public = list(

      #### data ================================================================

      #### methods =============================================================

      text_align = function(t1=NULL, t2=NULL, ...){
        if( is.null(t1) & is.null(t2) ){
          # check again
          # if(interactive() & self$options$ask){
          #   y <- readline("Alignment for ALL files? \nyes / no : ")
          #   if( !any(grepl("y", y)) ){
          #     return(FALSE)
          #   }
          # }
        for(i in seq_along(self$link) ){
          self$text_align( self$link[[i]]$from, self$link[[i]]$to )
        }
        }else{
          self$message("- doing alignment")
          tt1 <- self$text[[t1]]$text_get()
          tt2 <- self$text[[t2]]$text_get()
          self$alignment <-
            subset(
              diff_align(tt1, tt2, ...),
              select=-c("token_1", "token_2")
            )
        }
      }
    ) # closes public
  )# closes R6Class






































