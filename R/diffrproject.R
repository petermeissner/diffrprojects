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
#' @return Object of \code{\link{R6Class}}
#'
#' @format \code{\link{R6Class}} object.
#'
#'
diffrproject <-
  R6::R6Class(

    #### class name ============================================================
    "diffrproject",



    #### private ===============================================================
    private = list(

    ),



    #### public ================================================================
    public = list(


      #### data ================================================================
      meta     = list(),
      options  = list(),
      tracks   = list(),
      links    = list(),
      linkage  = list(),
      distance = list(),
      texts    = list(),


      #### methods =============================================================
      # add text
      text_add = function( rtext, name = NULL ){

        # input check
        stopifnot("rtext"  %in% class(rtext) )

        # working variable creation
        names <- names(self$texts)
        ids   <- vapply(self$texts, `[[`, "", "id")
        id    <- rtext$id

        # doing-duty-to-do
        if( is.null(name) ){
          next_num <- max(c(as.numeric(text_extract(names, "\\d+")),0))+1
          name <- text_c( "noname_", next_num)
        }
        self$texts[[name]]    <- rtext
        i <- 0
        while( rtext$id %in% ids ){
          rtext$id <- text_c(id, "_", i)
          i <- i+1
        }

        # return self for piping
        return(invisible(self))
      },

      # delete text
      text_delete = function(name=NULL, id=NULL){
        if( is.null(name) & is.null(id) ){
          name <- length(self$texts)
          self$texts[[name]] <- NULL
        }else if( !is.null(id) & is.null(name) ){
            name <- vapply(self$texts, `[[`, "", "id")==id
            self$texts[name] <- NULL
        }else{
          self$texts[[name]] <- NULL
        }
        # return self for piping
        return(invisible(self))
      },

      # basic info on texts
      text_data = function(){
        dp_text_base_data(self)
      },

      texts_link = function(from=NULL, to=NULL, delete=FALSE){
        from <- names(self$texts[from])
        to   <- names(self$texts[to])
        linker <- function(from, to, delete){
          name <- text_c(from, "_", to)
          if(delete){
            self$links[name] <- NULL
          }else{
            self$links[[name]] <- list(from=from, to=to)
          }
        }
        mapply(linker, from, to, delete=delete)
        invisible(self)
      },

      # universal getter
      get = function(name){
        if(name=="private"){
          return(private)
        }
        if( name %in% names(self) ){
          return(get(name, envir=self))
        }else if( name %in% names(private) ){
          return(get(name, envir=private))
        }else{
          return(NULL)
        }
      }

    )
  )
















