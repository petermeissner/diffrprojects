
#' class for dp_inherit
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
dp_inherit <-
  R6::R6Class(

    #### class name ============================================================
    classname    = "dp_inherit",

    #### misc ====================================================================
    active       = NULL,
    inherit      = dp_align,
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

      #### [ inherit ] #### ................................................
      text_inherit = function(link=NULL){

        # checking inputs and setting defaults
        if( is.null(link) ){
          link <- names(self$link)
        }

        # fetching link name if necessary
        if( !is.character(link) ){
          link <- names(self$link)[link]
        }

        links <- self$link[link]

        current_link <- names(links)[1]

        text1_subset <-
          self$alignment[[current_link]] %>%
            subset(subset=distance==0) %>%
            subset(select=c(from_1, to_1))

        text2_susbet <-
          self$alignment[[current_link]] %>%
          subset(subset=distance==0) %>%
          subset(select=c(from_1, to_1))

        self$text[[self$link[[current_link]]$from]]$get("char_data")
        self$text[[self$link[[current_link]]$to]]$get("char_data")



      }


    ) # closes public
  )# closes R6Class






































