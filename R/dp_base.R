#' class for dp_base
#'
#' @docType class
#'
#' @name dp_base
#'
#' @export
#'
#' @keywords data
#'
#' @return Object of \code{\link{dp_base}}
#'
#' @format \code{\link{R6Class}} object.
#'
#' @seealso \code{\link{diffrproject}}
#'
dp_base <-
  R6::R6Class(

    #### class name ============================================================
    classname    = "dp_base",

    #### misc ==================================================================
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
      meta           =
        list(
          ts_created   = "",
          db_path      = "",
          file_path    = "",
          project_id   = ""
        ),
      alignment      = structure(list(), class=c("alignment_list","list")),
      alignment_data = structure(list(), class=c("alignment_data_list","list")),
      text           = list(),
      link           = structure(list(), class=c("alignment_list","list")),


      #### methods =============================================================


      #### [ initialize() ] ====================================================

      initialize =
        function(
          project_id = digest::digest( list(sessionInfo(), Sys.time()) ) ,
          ask          = TRUE,
          ts_created   = force(as.POSIXct(as.numeric(Sys.time()), origin = "1970-01-01", tz="UTC")),
          db_path      = "./diffrproject.db"
        ){
        self$options$ask       <- ask
        self$meta$project_id   <- project_id
        self$meta$ts_created   <- ts_created
        self$meta$db_path      <- db_path
      },


      #### [ add text() ] ======================================================

      text_add = function(text=NULL, text_file=NULL, rtext=NULL, name=NULL, ...){

        # case: < rtext >
        if( !is.null(rtext) ){
          text_add_worker(
            self,
            rtext,
            name = name
          )
          # return
          return(invisible(self))
        }

        # case: < text >
        if( !is.null(text) ){
          stopifnot(class(text) %in% c("character", "list"))
          if( is.null(text_file) ){
            for(i in seq_along(text) ){
              text_add_worker(
                self,
                rtext=rtext::rtext$new(text = text[[i]], ..., verbos=self$options$verbose),
                name = name[i]
              )
            }
          }else{
            for(i in seq_along(text) ){
              text_add_worker(
                self,
                rtext=rtext::rtext$new(text = text[[i]], text_file = text_file[i], ..., verbos=self$options$verbose),
                name = name[i]
              )
            }
          }
          # return
          return(invisible(self))
        }

        # case: < text_file >
        if( !is.null(text_file) ){
          for(i in seq_along(text_file) ){
            text_add_worker(
              self,
              rtext::rtext$new(text_file = text_file[i], ...),
              name = ifelse(is.null(name), basename(text_file[i]), name[i])
            )
          }
          # return
          return(invisible(self))
        }

        # case: < nothing added >
        warning("no file added")
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


      #### [ text_meta_data() ] =====================================================

      text_meta_data = function(){
        dp_text_base_data(self)
      },


      #### [ text_data() ] =====================================================
      text_data = function(text=NULL){
        tmp <- list()
        if( is.null(text) ){
          is <- seq_along(self$text)
        }else{
          is <- text
        }
        for(i in is){
          tmp[[i]] <- self$text[[i]]$char_data_get()
          tmp[[i]]$name <- rep(names(self$text)[i], nrow(tmp[[i]]))
        }
        tmp <- do.call(rbind_fill, tmp)
        return(tmp)
      },

      #### [ tokenize_text_data_lines() ] ======================================
      tokenize_text_data_lines = function(
        text               = NULL,
        join               = c("full", "left", "right", ""),
        aggregate_function = NULL
      ){
        tmp <- list()
        if( is.null(text) ){
          is <- seq_along(self$text)
        }else{
          is <- text
        }
        for(i in is){
          tmp[[i]] <-
            self$text[[i]]$tokenize_data_lines(
              aggregate_function = aggregate_function,
              join = join
            )
          tmp[[i]]$name <- names(self$text)[i]
        }
        tmp <- do.call(rbind_fill, tmp)
        return(tmp)
      },

      #### [ tokenize_text_data_words() ] ======================================
      tokenize_text_data_words = function(
        text        = NULL,
        join        = c("full", "left", "right", ""),
        aggregate_function = NULL
      ){
        tmp <- list()
        if( is.null(text) ){
          is <- seq_along(self$text)
        }else{
          is <- text
        }
        for(i in is){
          tmp[[i]] <-
            self$text[[i]]$
              tokenize_data_words(
                join               = join,
                aggregate_function = aggregate_function
              )
          tmp[[i]]$name <- names(self$text)[i]
        }
        tmp <- do.call(rbind_fill, tmp)
        return(tmp)
      },

      #### [ tokenize_text_data_regex() ] ======================================
      tokenize_text_data_regex = function(
        split       = NULL,
        ignore.case = FALSE,
        fixed       = FALSE,
        perl        = FALSE,
        useBytes    = FALSE,
        non_token   = FALSE,
        join        = c("full", "left", "right", ""),
        aggregate_function = NULL
      ){
        tmp <- list()
        if( is.null(text) ){
          is <- seq_along(self$text)
        }else{
          is <- text
        }
        for(i in is){
          tmp[[i]] <-
            self$text[[i]]$
            tokenize_data_regex(
              split       = NULL,
              ignore.case = FALSE,
              fixed       = FALSE,
              perl        = FALSE,
              useBytes    = FALSE,
              non_token   = FALSE,
              join        = join,
              aggregate_function = aggregate_function
            )
          tmp[[i]]$name <- names(self$text)[i]
        }
        tmp <- do.call(rbind_fill, tmp)
        return(tmp)
      },


      #### [ text_code() ] =====================================================

      text_code = function(text=NULL, x=NULL, i=NULL, val=NA, hl = 0){
        if( is.null(text) ){
          warning("no text selected, so I will code nothing")
        }else{
          text <- self$text[[text]]
          text$char_data_set( x=x, i=i, val=val, hl=0)
        }
        return(invisible(self))
      },


      #### [ text_code_regex() ] ===============================================

      text_code_regex = function(text=NULL, x=NULL, pattern=NULL, val=NA, hl=0, ...){
        if( is.null(text) ){
          warning("no text selected, so I will code nothing")
        }else{
          text <- self$text[[text]]
          text$char_data_set_regex(x=x, pattern=pattern, val=val, hl=hl, ...)
        }
        return(invisible(self))
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
          name <- stringb::text_c(from, "~", to)
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

