#' Wrapper around bq_table_download
#'
#' @description
#' This function downloads a bq_table and reduces the automatically chosen page
#' size if the download fails.
#'
#' @param .bq_table A bq_table object as provided by bq_project_query
#' @param .page_size Integer: Page size argument for downloading data
#' @param .quiet Flag if function should send messages. FALSE necessary for
#' reducing page_size
#' @param .tries Integer: Amount of times the tryCatch should reduce page_size
#'
#' @return
#' A `tibble` object containing the requested data from BigQuery

bq_download_retry <- function(.bq_table,
                              .page_size,
                              .quiet = FALSE,
                              .tries = 4) {
        results <- NULL
        try <- 1
        
        while(is.null(results) & try < .tries) {
                tryCatch(
                        {
                                logs <- ""
                                # WithCallingHandlers can manage Errors/Messages displayed by code. Is used
                                # here to capturing the messages displayed by the function
                                withCallingHandlers(
                                        {
                                                try <- try + 1
                                                results <- bigrquery::bq_table_download(
                                                        x         = .bq_table,
                                                        page_size = .page_size,
                                                        quiet     = .quiet
                                                )
                                        },
                                        message = function(msg) {
                                                # <<- (Super assignment operator) used as logs need to be used in different
                                                # environment
                                                logs <<- paste(logs, msg)
                                        }
                                )
                        },
                        error = function(err) {
                                .page_size <<- stringr::str_extract(
                                        stringr::str_flatten(logs),
                                        "(?<=\\(up\\sto\\)\\s)\\d+[:punct:]?\\d+"
                                ) %>%
                                        stringr::str_replace("[:punct:]", "") %>%
                                        as.integer() %>%
                                        "*"(0.5) %>%
                                        round()
                                
                                message("Download failed: Page size will be reduced! \n", err)
                                message("Start try ", try, ". Page size set to ", .page_size, ".")
                        }
                )
        }
        
        return(results)
}
