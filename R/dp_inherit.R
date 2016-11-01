
#' class for dp_inherit
#'
#' @docType class
#'
#' @name dp_inherit
#'
#' @export
#'
#' @keywords data
#'
#' @return Object of \code{\link{dp_align}}
#'
#' @format \code{\link{R6Class}} object.
#'
#' @seealso \code{\link{diffrproject}}
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


      #### [ text_data_inherit ] #### ..........................................

      text_data_inherit = function(
        link=NULL,
        direction = c("both", "forward", "backward")
      ){

        # checking inputs and setting defaults
        direction <- direction[1]
        if( is.null(link) ){
          link <- names(self$link)
        }

        # fetching link name if necessary
        if( !is.character(link) ){
          link <- names(self$link)[link]
        }


        # cycling through links
        links      <- self$link[link]

        # getting directions right
        if( direction == "forward" ){
          directions <- rep(direction, length(links))
        }

        if( direction == "backward" ){
          links      <- rev(links)
          directions <- rep(direction, length(links))
        }

        if( direction == "both" ){
          directions <- c(rep("forward", length(links)), rep("backward", length(links)))
          links <- c(links, rev(links))
        }

        # cycling through link list
        for( link_i in seq_along(links) ){
          # current link
          current_link_name <- names(links)[link_i]
          current_link      <- links[[link_i]]

          # current direction
          direction <- directions[link_i]

          # gathering zero distance alignments
          text1_tokens <-
            self$alignment[[current_link_name]] %>%
              subset(subset=distance==0) %>%
              subset(select=c(from_1, to_1))

          text2_tokens <-
            self$alignment[[current_link_name]] %>%
            subset(subset=distance==0) %>%
            subset(select=c(from_2, to_2))

          # getting direction right
          if( direction == "backward" ){
            text1 <- self$text[[current_link$to]]
            text2 <- self$text[[current_link$from]]
            tmp <- text1_tokens
            text1_tokens <- text2_tokens
            text1_tokens <- tmp
          }else{
            text1 <- self$text[[current_link$from]]
            text2 <- self$text[[current_link$to]]
          }

          # pushing data from one text to the other
          for(i in seq_len(nrow(text1_tokens)) ){
            push_text_char_data(
              from_text  = text1,
              to_text    = text2,
              from_token = text1_tokens[i, ],
              to_token   = text2_tokens[i, ],
              warn       = self$options$warning
            )
          }
        }
        return(invisible(self))
      } # end of text_data_inherit()
    ) # closes public
  )# closes R6Class






































