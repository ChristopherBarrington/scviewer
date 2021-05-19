
#' Write metadata to a h5 file
#' 
#' @param h5_file Path to `h5` file
#' @param metadata A `data.frame` of meta data
#' @param ... Arguments passed to `guess_metadata`
#' 
#' @import rhdf5
#' 
#' @describeIn write_metadata Write meta data to `h5` file
#' 
#' @export
#' 
write_metadata <- function(h5_file, metadata, ...) {
  if(h5ls(h5_file, recursive=1) %>% pluck('name') %>% is_in(x='metadata')) {
    message('- deleting metadata')
    h5delete(file=h5_file, name='metadata')
  }

  sprintf(fmt='+ adding metadata group to %s', h5_file) %>% message()
  h5createGroup(file=h5_file, group='metadata')
  h5createGroup(file=h5_file, group='metadata/factor_levels')

  if(missing(metadata))
    metadata <- guess_metadata(...)

  message('+ writing metadata')
  h5write(obj=metadata, file=h5_file, name='metadata/data')
  for(i in {metadata %>% select_if(is.factor) %>% colnames()})
    h5write(obj=levels(metadata[[i]]), file=h5_file, name=sprintf(fmt='metadata/factor_levels/%s', i))

  invisible(metadata)
}

#' Get dataset meta data
#' 
#' @param seurat Seurat object
#' 
#' @details Extracts the factor- or character-type variables from the `seurat` meta data, but it uses `!is.numeric`!
#' 
#' @describeIn write_metadata Select non-numeric variables from the meta data slot
#' 
guess_metadata <- function(seurat) {
  message('+ collecting meta data')
  seurat@meta.data %>%
    as.data.frame() %>%
    rownames_to_column('cell_id') %>%
    select_if(negate(is.numeric)) %>%
    mutate_all(as.factor) %>%
    mutate_at(vars(contains('_snn_res.')), function(x) x %>% fct_relevel({levels(.) %>% mixedsort()}))
}
