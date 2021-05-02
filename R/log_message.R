
skip_log_message <- function(...)
  invisible()

show_log_message <- function(text, prepend='///')
  str_c(prepend, {Sys.time() %>% format('%H:%M:%S')}, text, sep=' ') %>%
    message()
