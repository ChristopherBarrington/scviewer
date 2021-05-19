
#' Write reduction information to a h5 file
#' 
#' @param h5_file Path to `h5` file
#' @param reductions A `list` of cell coordinates
#' @param ... Arguments passed to `guess_reductions`
#' 
#' @import rhdf5
#' 
#' @describeIn write_reductions Write coordinates for reductions to `h5` file
#' 
#' @export
#' 
write_reductions <- function(h5_file, reductions, ...) {
  if(h5ls(h5_file, recursive=1) %>% pluck('name') %>% is_in(x='reductions')) {
    message('- deleting reductions')
    h5delete(file=h5_file, name='reductions')
  }

  message('+ adding reductions group')
  h5createGroup(file=h5_file, group='reductions') %>% invisible()

  if(missing(reductions))
    reductions <- guess_reductions(...)

  message('+ writing reductions')
  reductions %T>%
    Map(data=., name=names(.), function(data, name) h5write(obj=data, file=h5_file, name=sprintf(fmt='reductions/%s', name))) %>%
    invisible()
}

#' Get a reductions list
#' 
#' @param seurat Seurat object
#' 
#' @details Creates a list of 2D and 3D coordinates for reductions in `seurat`.
#' 
#' @describeIn write_reductions Create a list of coordinates for reductions
#' 
guess_reductions <- function(seurat) {
  message('+ collecting reductions')
  lapply(seurat@reductions, function(reduction)
      slot(object=reduction, name='cell.embeddings') %>% as.data.frame()) %>%
    append(list(pca_3d=.$pca[,1:3])) %>% # get 3d pca embedding coordinates
    (function(x) {x$pca %<>% select(1:2); x}) %>% # select first 2 pca embedding coordinates
    lapply(function(df) df %>% set_names(function(n) head(x=c('x','y','z'), n=length(n)))) %>% # select x/y for 2d or x/y/z for 3d reductions
    lapply(rownames_to_column, var='cell_id')
}
