
#' Write feature values to a h5 file
#' 
#' @param h5_file Path to `h5` file
#' @param features_matrix Matrix of cells (rows) and features (columns)
#' @param ... Arguments passed to `guess_features_matrix`
#' 
#' @import rhdf5
#' 
#' @describeIn write_features Write feature matrix to `h5_file`
#' 
#' @export
#' 
write_features <- function(h5_file, features_matrix, ..., dry_run=FALSE) {
  if(h5ls(h5_file, recursive=1) %>% pluck('name') %>% is_in(x='features')) {
    message('- deleting features')
    h5delete(file=h5_file, name='features')
  }

  message('+ adding features group')
  h5createGroup(file=h5_file, group='features') %>% invisible()
  h5createGroup(file=h5_file, group='features/values') %>% invisible()

  if(missing(features_matrix))
    features_matrix <- guess_features_matrix(...)

  if(dry_run)
    features_matrix <- features_matrix[,1:100]

  message('+ writing features')
  h5write(obj=colnames(features_matrix), file=h5_file, name='features/names')
  h5write(obj=rownames(features_matrix), file=h5_file, name='features/cell_ids')

  for(i in colnames(features_matrix))
    str_to_lower(i) %>%
      sprintf(fmt='features/values/%s') %>%
      h5write(obj=features_matrix[,i], file=h5_file)

  invisible(features_matrix)
}

#' Get a feature values matrix
#' 
#' @param seurat Seurat object
#' 
#' @details Combines the `RNA@data` slot and any numeric meta data variables into a `feature_matrix`.
#' 
#' @describeIn write_features Create a feature values matrix using default features
#' 
#' @importFrom Matrix as.matrix
#' @importFrom Matrix t
#' @importFrom Seurat Assays
#' 
guess_features_matrix <- function(seurat) {
  message('+ collecting features matrix')
  cbind({Assays(seurat, slot='RNA') %>% slot(name='data') %>% Matrix::t()},
        {select_if(seurat@meta.data, is.numeric) %>% Matrix::as.matrix()}) %>%
    Matrix::as.matrix()
}
