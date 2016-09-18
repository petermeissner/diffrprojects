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
#' @return Object of \code{\link{dp_loadsave}}
#'
#' @format \code{\link{R6Class}} object.
#'
#'
dp_loadsave <-
  R6::R6Class(

    #### misc ====================================================================
    classname    = "rtext_loadsave",
    active       = NULL,
    inherit      = dp_base,
    lock_objects = TRUE,
    class        = TRUE,
    portable     = TRUE,
    lock_class   = FALSE,
    cloneable    = TRUE,
    parent_env   = asNamespace('diffrprojects'),

    #### private =================================================================
    private = list(

      #### [ prepare_save ] #### ...............................................
      prepare_save = function(){

        # texts
        text <- lapply(self$text, function(x){x$text_get()})
        text <-
          data.frame(
            text_name = names(self$text),
            length    = nchar(text),
            text_id   = unlist(lapply(self$text, function(x){x$id})),
            text
          )

        # text data
        text_data <- lapply( self$text, rtext_char_data_to_data_frame )
        for( i in seq_along(text_data) ){
          text_data[[i]]$text_name <- text$text_name[i]
          text_data[[i]]$text_id   <- text$text_id[i]
        }
        text_data <- rbind_list(text_data)


        # alignment
        alignment <- self$alignment
        for(i in seq_along(alignment) ){
          alignment[[i]]$alignment_name <- names(alignment)[i]
          alignment[[i]]$text_name_1 <- self$link[[names(alignment)[i]]]$from
          alignment[[i]]$text_name_2 <- self$link[[names(alignment)[i]]]$to
        }
        alignment <- rbind_list(alignment)


        # alignment_data
        alignment_data <- as.data.frame(self$alignment_data)
        names(alignment_data)[3] <- "variable"
        names(alignment_data)[5] <- "value"
        alignment_data$value <- as.character(alignment_data$value)

        # link
        link <- as.data.frame(self$link)
        names(link)[3] <- "alignment_name"

        # put together information
        tb_saved <-
          list(
            meta = meta ,
            hashes       =
              data.frame(
                name = names(private$hash()),
                hash = unlist(private$hash()),
                row.names=NULL
              ),
            text           = text,
            text_data      = text_data,
            alignment      = alignment ,
            alignment_data = alignment_data,
            link           = link
          )
        class(tb_saved) <- c("dp_save","list")
        # return
        return(tb_saved)
      },


      #### [ execute_load ] #### ...............................................
      execute_load = function(tmp){

        warning("TBD")

        # return for piping
        invisible(self)
      }
    ),

    #### public ==================================================================
    public = list(


      #### [ save ] #### .........................................................
      save = function(file=NULL, id=NULL){
        dp_save <- as.environment(private$prepare_save(id=id))
        # handle file option
        if( is.null(dp_save$meta$db_path) & is.null(file) ){
          stop("rtext$save() : Neither file nor db_path given, do not know where to store file.")
        }else if( !is.null(file) ){
          file <- file
        }else if( !is.null(dp_save$meta$db_path) ){
          file <- dp_save$meta$db_path
        }
        # save to file
        base::save(
          list = ls(dp_save),
          file = file,
          envir = dp_save
        )
        # return for piping
        return(invisible(self))
      },

      #### [ load ] ..............................................................
      load = function(file=NULL){
        # handle file option
        if( is.null(file) ){
          stop("dp$load() : file is not given, do not know where to load file from.")
        }else{
          file <- file
        }
        # loading info
        tmp <- rtext:::load_into(file)
        # applying loaded info to self
        private$execute_load(tmp)
        # return self for piping
        return(invisible(self))
      }
    )
  )

