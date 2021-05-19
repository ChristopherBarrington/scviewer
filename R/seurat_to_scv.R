#' Convert a Seurat object to a scviewer-formatted `h5` file
#' 
#' @param h5_file Path to `h5` file
#' @param seurat Seurat object
#' @param recalculate_reductions Should 2D and 3D tSNE and UMAP reductions be calculated?
#' @param ... Arguments passed to `add_all_projections`
#' 
#' @importFrom rhdf5 h5createFile
#' 
#' @export
#' 
seurat_to_scv <- function(h5_file, seurat, recalculate_reductions=FALSE, ...) {
  if(recalculate_reductions)
    seurat %<>% add_all_projections(...)

  if(file.exists(h5_file)) {
    sprintf(fmt='- the h5_file (%s) exists and will not be removed!', h5_file) %>% message()
  } else {
    h5createFile(file=h5_file) %>% invisible()
  }

  write_reductions(h5_file=h5_file, seurat=seurat)
  write_features(h5_file=h5_file, seurat=seurat, ...)
  write_metadata(h5_file=h5_file, seurat=seurat)
  write_cluster_identity_sets(h5_file=h5_file, seurat=seurat)

  invisible(NULL)
}
