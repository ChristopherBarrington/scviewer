
#' Calculate tSNE and UMAP projections in 2- and 3-dimensions
#' 
#' @param seurat Seurat object
#' @param input_reduction The reduction from which the projection is calculated
#' @param n_dimensions Number of dimensions from which the projection is calculated
#'
#' @describeIn add_all_projections Add 3D tSNE reduction to Seurat object
#' 
#' @importFrom Seurat RunTSNE
#' @importFrom Seurat RunUMAP
#' 
#' @export
#' 
add_all_projections <- function(seurat, n_dimensions=40, input_reduction='pca')
  seurat %>%
    DietSeurat(counts=TRUE, data=TRUE, scale.data=TRUE, assays=Assays(seurat), dimreducs='pca') %>%
    add_2d_tsne(n_dimensions=n_dimensions, input_reduction=input_reduction) %>%
    add_3d_tsne(n_dimensions=n_dimensions, input_reduction=input_reduction) %>%
    add_2d_umap(n_dimensions=n_dimensions, input_reduction=input_reduction) %>%
    add_3d_umap(n_dimensions=n_dimensions, input_reduction=input_reduction)

#' Calculate tSNE in 2-dimensions
#' 
#' @describeIn add_all_projections Add 2D tSNE reduction to Seurat object
#' 
#' @importFrom Seurat RunTSNE
#' 
#' @export
#' 
add_2d_tsne <- function(seurat, n_dimensions=40, input_reduction='pca') {
  sprintf(fmt='/// Getting 2D tSNE over %s dimensions', n_dimensions) %>% message()
  RunTSNE(object=seurat, reduction=input_reduction, dims=seq(n_dimensions),
          reduction.name='tsne', reduction.key='tSNE_',
          dim.embed=2, seed.use=1, verbose=FALSE, check_duplicates=FALSE)
}

#' Calculate UMAP in 2-dimensions
#' 
#' @describeIn add_all_projections Add 3D UMAP reduction to Seurat object
#' 
#' @importFrom Seurat RunUMAP
#' 
#' @export
#' 
add_2d_umap <- function(seurat, n_dimensions=40, input_reduction='pca') {
  sprintf(fmt='/// Getting 2D UMAP over %s dimensions', n_dimensions) %>% message()
  RunUMAP(object=seurat, reduction=input_reduction, dims=seq(n_dimensions),
          reduction.name='umap', reduction.key='UMAP_',
          n.components=2, seed.use=1, verbose=FALSE)
}

#' Calculate tSNE in 3-dimensions
#' 
#' @describeIn add_all_projections Add 3D tSNE reduction to Seurat object
#' 
#' @importFrom Seurat RunTSNE
#' 
#' @export
#' 
add_3d_tsne <- function(seurat, n_dimensions=40, input_reduction='pca') {
  sprintf(fmt='/// Getting 3D tSNE over %s dimensions', n_dimensions) %>% message()
  RunTSNE(object=seurat, reduction=input_reduction, dims=seq(n_dimensions),
          reduction.name='tsne_3d', reduction.key='tSNE3D_',
          dim.embed=3, seed.use=1, verbose=FALSE, check_duplicates=FALSE)
}

#' Calculate UMAP in 3-dimensions
#' 
#' @describeIn add_all_projections Add 3D UMAP reduction to Seurat object
#' 
#' @importFrom Seurat RunUMAP
#' 
#' @export
#' 
add_3d_umap <- function(seurat, n_dimensions=40, input_reduction='pca') {
  sprintf(fmt='/// Getting 3D UMAP over %s dimensions', n_dimensions) %>% message()
  RunUMAP(object=seurat, reduction=input_reduction, dims=seq(n_dimensions),
          reduction.name='umap_3d', reduction.key='UMAP3D_',
          n.components=3, seed.use=1, verbose=FALSE)
}
