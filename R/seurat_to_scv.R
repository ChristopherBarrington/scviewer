#' Convert a Seurat object to a scviewer-formatted `h5` file
#' 
#' @param h5_file Path to `h5` file
#' @param seurat Seurat object or file path from which an `rds` can be read
#' @param recalculate_reductions Should 2D and 3D tSNE and UMAP reductions be calculated?
#' @param ... Arguments passed to `add_all_projections`
#' 
#' @export
#' 
seurat_to_scv <- function(h5_file, seurat, recalculate_reductions=FALSE, ...) {
  create_h5_scv(h5_file=h5_file)

  seurat %<>%
    when(is.character(.)~readRDS(.),
         is_in(x='Seurat', table=class(.))~.,
         TRUE~stop('!!! seurat is not a character or Seurat')) %>%
    when(recalculate_reductions~add_all_projections(seurat=., ...),
         TRUE~.)

  write_reductions(h5_file=h5_file, seurat=seurat)
  write_features(h5_file=h5_file, seurat=seurat, ...)
  write_metadata(h5_file=h5_file, seurat=seurat)
  write_cluster_identity_sets(h5_file=h5_file, seurat=seurat)

  invisible(NULL)
}

#' Create an empty `h5` file
#' 
#' @param h5_file Path to `h5` file
#' @param remove Should `h5_file` be removed if it exists?
#' 
#' @importFrom rhdf5 h5createFile
#' 
#' @export
#' 
create_h5_scv <- function(h5_file, delete=TRUE) {
  if(file.exists(h5_file)) {
    sprintf(fmt='- the h5_file (%s) exists!', h5_file) %>% message()
  
    if(delete) {
      message('- and has been removed!')
      file.remove(h5_file)
    }
  }

  h5createFile(file=h5_file) %>% invisible()
}
