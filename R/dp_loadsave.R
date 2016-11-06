#' class for dp_base
#'
#' @docType class
#' @name dp_loadsave
#' @export
#' @keywords data
#' @return Object of \code{\link{dp_loadsave}}
#' @format \code{\link{R6Class}} object.
#' @seealso \code{\link{diffrproject}}
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

        # meta
        meta <-
          list(
            project_id   = self$meta$project_id,
            db_path      = self$meta$db_path,
            file_path    = self$meta$file_path,
            ts_created   = self$meta$ts_created,
            n_texts      = length(self$text),
            nchar_text   = sum(unlist(lapply(self$text, function(x){x$char_length()}))),
            dp_version   = as.character(packageVersion("diffrprojects")),
            rtext_version= as.character(packageVersion("rtext")),
            save_format_version = 1
          )

        # texts
        text <- lapply( self$text, function(x){ get_private(x)$prepare_save() })

        # alignment
        alignment <- self$alignment

        # alignment_data
        alignment_data <- self$alignment_data

        # link
        link <- self$link

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
            alignment      = alignment,
            alignment_data = alignment_data,
            link           = link
          )
        class(tb_saved) <- c("dp_save","list")
        # return
        return(tb_saved)
      },


      #### [ execute_load ] #### ...............................................
      execute_load = function(tmp){

        #  meta
        self$meta$db_path      <- tmp$meta$db_path
        self$meta$file_path    <- tmp$meta$file_path
        self$meta$project_id   <- tmp$meta$project_id
        if( "numeric" %in% class(tmp$meta$ts_created)  ){
          self$meta$ts_created   <- as.POSIXct(tmp$meta$ts_created, origin = "1970-01-01", tz="UTC")
        }else if( "character" %in% class(tmp$meta$ts_created) ){
          self$meta$ts_created   <- as.POSIXct(tmp$meta$ts_created, tz="UTC")
        }else{
          self$meta$ts_created   <- tmp$meta$ts_created
        }

        # alignment
        self$alignment_data <- tmp$alignment_data

        # alignment data
        self$alignment      <- tmp$alignment

        # link
        self$link           <- tmp$link

        # texts
        self$text <- list()
        text_names <- names(tmp$text)

        for(i in seq_along(text_names)){
          self$text[[text_names[i]]] <- rtext$new()
          self$text[[text_names[i]]]$get("private")$execute_load(tmp$text[[i]])
        }

        # hash update
        private$hashes <- private$hash()

        # return for piping
        invisible(self)
      }
    ),

    #### public ==================================================================
    public = list(


      #### [ save ] #### .........................................................
      save = function(file=NULL, id=NULL){
        dp_save <- as.environment(private$prepare_save())
        # handle file option
        if( is.null(dp_save$meta$save_path) & is.null(file) ){
          if( self$options$warning ){
            warning("dp$save() : Neither file nor save_path given: storing in default location: ")
          }
          file <- "./diffrproject.RData"
        }else if( !is.null(file) ){
          file <- file
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

