#' R6 class - linking text and data
#'
#' @docType class
#' @name dp_export
#' @export
#' @keywords data
#' @return Object of \code{\link{R6Class}}
#' @format \code{\link{R6Class}} object.
#'
dp_export <-
  R6::R6Class(

    #### misc ====================================================================
    classname    = "dp_export",
    active       = NULL,
    inherit      = dp_loadsave,
    lock_objects = TRUE,
    class        = TRUE,
    portable     = TRUE,
    lock_class   = FALSE,
    cloneable    = TRUE,
    parent_env   = asNamespace('diffrprojects'),



    #### private =================================================================
    private = list(),



    #### public ==================================================================
    public = list(

      #### [ export_csv ] #### .......................................................
      export_csv = function(folder_name = ""){
        stopifnot(file.info(folder_name)$isdir)
        "TBD"
      },

      #### [ import_csv ] #### .......................................................
      import_csv = function(folder_name = ""){
        stopifnot(file.info(folder_name)$isdir)
        "TBD"
      },

      #### [ export_sqlite ] #### .......................................................
      export_sqlite = function(db_name = ""){
        # establish connection
        if( is.character(db_name) ){
          con <- RSQLite::dbConnect( RSQLite::SQLite(), db_name)
          on.exit({
            RSQLite::dbDisconnect(con)
          })
        }else{
          con <- db_name
        }

        # preapare data to be exportd
        tb_exported <- private$prepare_save()

        # export data
        RSQLite::dbBegin(con)

          # meta
          RSQLite::dbWriteTable(con, "meta",      as.data.frame(tb_exported$meta), overwrite=TRUE)

          # hashes
          RSQLite::dbWriteTable(con, "hashes",    tb_exported$hashes, overwrite=TRUE)

          # text_char
          char <- lapply(tb_exported$text, function(x){ data.frame(char=x$char, i=seq_along(x$char) )} )
          write_numerous_parts_to_table(
            x          = char ,
            con        = con,
            table_name = "text_char",
            meta =
              data.frame(
                text_name = names(tb_exported$text),
                text_id = vapply(tb_exported$text, function(x){x$meta$id}, "")
              )
          )

          # text char_data
          char_data <- lapply( tb_exported$text, function(x){x$char_data})
          text_names <- names(char_data)

          for( i in seq_along(char_data) ){
            for( k in seq_along(char_data[[i]]) ){
              char_data[[i]][[k]]$variable <- names(char_data[[i]][[k]])[3]
              names(char_data[[i]][[k]])[3] <- "value"
              char_data[[i]][[k]]$text_name <- text_names[i]
              char_data[[i]][[k]]$text_id <- tb_exported$text[[i]]$meta$id
            }
          }
          char_data <- unlist(char_data, recursive = FALSE)
          write_numerous_parts_to_table(
            x = char_data,
            con = con,
            table_name = "text_char_data"
          )

          # text_meta
          text_meta <- do.call(rbind,lapply(tb_exported$text, function(x){x$meta}))
          RSQLite::dbWriteTable(con, "text_meta", text_meta, overwrite=TRUE)

          # link
          RSQLite::dbWriteTable(con, "link", as.data.frame(tb_exported$link), overwrite=TRUE)

          # alignment
          RSQLite::dbWriteTable(con, "alignment", as.data.frame(tb_exported$alignment), overwrite=TRUE)

          # alignment data
          RSQLite::dbWriteTable(con, "alignment_data", as.data.frame(tb_exported$alignment_data), overwrite=TRUE)

          # meta
          RSQLite::dbWriteTable(con, "meta", as.data.frame(tb_exported$meta), overwrite=TRUE)

        RSQLite::dbCommit(con)

        # return
        return(invisible(self))
      },

      #### [ import_sqlite ] #### .......................................................
      import_sqlite = function(db_path = ""){
        # establish connection
        if( is.character(db_path) ){
          if( db_path == "" ){
            if( self$options$warning ){
              warn("no db_path specified, using default value")
            }
            db_path <- "./diffrproject.db"
          }
          con <- RSQLite::dbConnect(RSQLite::SQLite(), db_path)
          on.exit({
            RSQLite::dbDisconnect(con)
          })
        }else{
          con <- db_path
        }
        # import data
        imported <- list()

        imported$alignment      <- RSQLite::dbReadTable(con, "alignment")
        imported$alignment      <- split(imported$alignment, f=imported$alignment$link)
        imported$alignment      <- lapply(imported$alignment, subset, select=-link)

        imported$alignment_data <- RSQLite::dbReadTable(con, "alignment_data")
        imported$alignment_data <- split(imported$alignment_data, imported$alignment_data$link)
        imported$alignment_data <- lapply(imported$alignment_data, subset, select=-link)
        imported$alignment_data <- lapply(imported$alignment_data, function(x){split(x, x[[3]])})
        for( i in seq_along(imported$alignment_data) ) {
          for( k in seq_along(imported$alignment_data[[i]]) ){
            names(imported$alignment_data[[i]][[k]])[4] <- imported$alignment_data[[i]][[k]]$name[1]
            imported$alignment_data[[i]][[k]][[3]] <- NULL
          }
        }


        imported$text_char      <- RSQLite::dbReadTable(con, "text_char")
        imported$text_char      <- split(imported$text_char, f=imported$text_char$text_name)
        imported$text_char      <- lapply(imported$text_char, subset, select=char, drop=TRUE)

        imported$text_meta      <- RSQLite::dbReadTable(con, "text_meta")

        # import char_data
        if( RSQLite::dbExistsTable(con, "text_char_data") ){
          imported$text_char_data <- RSQLite::dbReadTable(con, "text_char_data")
          imported$text_char_data <- split(imported$text_char_data, f=imported$text_char_data$text_name)
          imported$text_char_data <- lapply( imported$text_char_data, function(x){ split(x, f=x$variable) })

          for( i in seq_along(imported$text_char_data) ) {
            for( k in seq_along(imported$text_char_data[[i]]) ){
              nam <- imported$text_char_data[[i]][[k]]$variable[1]
              imported$text_char_data[[i]][[k]][["text_name"]] <- NULL
              imported$text_char_data[[i]][[k]][["text_id"]] <- NULL
              names(imported$text_char_data[[i]][[k]])[3] <- nam
              imported$text_char_data[[i]][[k]][["variable"]] <- NULL
            }
          }
        }else{
          imported$text_char_data <- list()
        }


        # incorporate data
        private$execute_load(imported)

        # return self for piping
        return(invisible(self))
      }
    )
  )












