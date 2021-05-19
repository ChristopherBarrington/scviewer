
#' Write cell filter parameters to a h5 file
#' 
#' @param h5_file Path to `h5` file
#' @param cell_filter_parameters A `list` of filter parameters (see details)
#' 
#' @details The cell filter parameters list is named by the UI element label and contains:
#' * `var` the variable from `metadata` (which should be a factor) that is to be filtered
#' * `selected` the levels that are selected by default
#' 
#' @import rhdf5
#' 
#' @export
#' 
write_cell_filter_parameters <- function(h5_file, cell_filter_parameters) {
  if(missing(cell_filter_parameters))
    stop('!!! the cell_filter_parameters argument is required!')

  if(h5ls(h5_file, recursive=1) %>% pluck('name') %>% is_in(x='cell_filter_parameters')) {
    message('- deleting cell_filter_parameters')
    h5delete(file=h5_file, name='cell_filter_parameters')
  }

  message('+ writing cell filter parameters')
  cell_filter_parameters %T>%
    h5write(file=h5_file, name='cell_filter_parameters') %>%
    invisible()
}
