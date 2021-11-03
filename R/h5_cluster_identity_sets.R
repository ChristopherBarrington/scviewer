
#' Write cell cluster sets to a h5 file
#' 
#' @param h5_file Path to `h5` file
#' @param cluster_identity_sets A `data.frame` of meta data
#' @param ... Arguments passed to `guess_cluster_identity_sets`
#' 
#' @import rhdf5
#' 
#' @describeIn collect_cluster_identity_sets Write cluster identities list to `h5` file
#' 
#' @export
#' 
write_cluster_identity_sets <- function(h5_file, cluster_identity_sets, ...) {
  if(h5ls(h5_file, recursive=1) %>% pluck('name') %>% is_in(x='cluster_identity_sets')) {
    message('- deleting cluster_identity_sets')
    h5delete(file=h5_file, name='cluster_identity_sets')
  }

  if(missing(cluster_identity_sets))
    cluster_identity_sets <- guess_cluster_identity_sets(...)

  message('+ writing cluster identity sets')
  cluster_identity_sets %T>%
    h5write(file=h5_file, name='cluster_identity_sets') %>%
    invisible()
}

#' Get cluster identity sets
#' 
#' @param seurat Seurat object
#' 
#' @details Selects `_snn_res` variables as cluster identity sets and selects all identities by default.
#' 
#' @describeIn collect_cluster_identity_sets Select Seurat cluster identities
#' 
guess_cluster_identity_sets <- function(seurat) {
  message('+ collecting cluster identity sets')
  seurat@meta.data %>%
    select(any_of('seurat_clusters'), contains('_snn_res.')) %>%
    colnames() %>%
    set_names() %>%
    lapply(function(x) list(var=x, name={str_replace(x, '^.*_snn_res.', 'res._') %>% str_replace_all('_', ' ') %>% str_to_title()}, selected=levels(seurat@meta.data[[x]])))
}
