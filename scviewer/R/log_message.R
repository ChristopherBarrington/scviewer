
#' Do not print log messages
#' 
#' @param ... All arguments are ignored
#' 
#' @describeIn show_log_message Hide log messages
#' @family log messages
#' 
skip_log_message <- function(...)
  invisible()

#' Print log messages
#' 
#' @param text Message to output
#' @param prepend Maker to print in front of message
#' 
#' @describeIn show_log_message Print log messages
#' @family log messages
#' 
show_log_message <- function(text, prepend='///')
  str_c(prepend, {Sys.time() %>% format('%H:%M:%S')}, text, sep=' ') %>%
    message()
