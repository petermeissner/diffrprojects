#' class for dp_base
#'
#' @docType class
#'
#' @name diffrproject
#'
#' @export
#'
#' @keywords data
#'
#' @return Object of \code{\link{dp_base}}
#'
#' @format \code{\link{R6Class}} object.
#'
#'
dp_base <-
  R6::R6Class(

    #### class name ============================================================
    classname    = "dp_base",

    #### misc ====================================================================
    active       = NULL,
    inherit      = rtext::R6_rtext_extended,
    lock_objects = TRUE,
    class        = TRUE,
    portable     = TRUE,
    lock_class   = FALSE,
    cloneable    = TRUE,
    parent_env   = asNamespace('diffrprojects'),


    #### private ===============================================================
    private = list(),



    #### public ================================================================
    public = list(


      #### data ================================================================
      meta           = list(),
      alignment      = structure(list(), class=c("alignment_list","list")),
      alignment_data = list(),
      text           = list(),
      link           = structure(list(), class=c("alignment_list","list")),


      #### methods =============================================================


      #### [ initialize() ] ====================================================

      initialize = function(ask=TRUE){
        self$options$ask <- ask
      },


      #### [ add text() ] ======================================================

      text_add = function(rtext=NULL, text=NULL, text_file=NULL,  name=NULL, ...){

        text_add_worker = function( rtext=NULL, name = NULL ){
          # input check
          stopifnot( "rtext"  %in% class(rtext) )
          # working variable creation
          names <- names(self$text)
          ids   <- vapply(self$text, `[[`, "", "id")
          id    <- rtext$id
          # doing-duty-to-do
          if( is.null(name) ){
            name <-
              tryCatch(
                basename(rtext$text_file), error=function(e){NA}
              )
            if( is.na(name) ){
              next_num <- max(c(as.numeric(text_extract(names, "\\d+")),0))+1
              name     <- text_c( "noname_", next_num)
            }
          }
          self$text[[name]]    <- rtext
          i <- 0
          while( rtext$id %in% ids ){
            rtext$id <- text_c(id, "_", i)
            i <- i+1
          }
        }

        # doing-duty-to-do
        if( !is.null(rtext) ){                                 # - for rtext
          text_add_worker(
            rtext,
            name = name
          )
        }else if( !is.null(text) ){                            # - for text
          stopifnot(class(text) %in% c("character", "list"))
          for(i in seq_along(text) ){
            text_add_worker(
              rtext=rtext::rtext$new(text = text[[i]], ...),
              name = name[i]
            )
          }
        }else if( !is.null(text_file) ){                       # - for text_file
          for(i in seq_along(text_file) ){
            text_add_worker(
              rtext::rtext$new(text_file = text_file[i], ...),
              name = ifelse(is.null(name), basename(text_file[i]), name[i])
            )
          }
        }else{
          warning("no file added")
        }

        # return
        return(invisible(self))
      },


      #### [ text_delete() ] ===================================================

      text_delete = function(name=NULL, id=NULL){
        if( is.null(name) & is.null(id) ){
          name <- length(self$text)
          self$text[[name]] <- NULL
        }else if( !is.null(id) & is.null(name) ){
            name <- vapply(self$text, `[[`, "", "id")==id
            self$text[name] <- NULL
        }else{
          self$text[[name]] <- NULL
        }
        # return self for piping
        return(invisible(self))
      },


      #### [ text_data() ] =====================================================

      text_data = function(){
        dp_text_base_data(self)
      },


      #### [ text_link ] =======================================================

      text_link = function(from=NULL, to=NULL, delete=FALSE){
        if( is.null(from) & is.null(to) ){
          from <- shift(names(self$text), 1, NULL)
          to   <- shift(names(self$text), -1, NULL)
        }
        from <- names(self$text[from])
        to   <- names(self$text[to])
        linker <- function(from, to, delete){
          name <- text_c(from, "_", to)
          if(delete){
            self$link[name] <- NULL
          }else{
            self$link[[name]] <- list(from=from, to=to)
          }
        }
        mapply(linker, from, to, delete=delete)
        invisible(self)
      }

    )# closes public
  )# closes R6Class

