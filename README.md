# scviewer

a lightweight single cell visualisation tool

# Convert Seurat to scviewer

The objects required for plotting should be available in a `list` saved as an `rds` object. A `Seurat` object can be exported using the following function.

Assumptions:

* features in `scviewer` are the normalised RNA and gene module scores. The `cbind` can be removed if you only want `RNA$data`, for example.
* clusters are defined in the `seurat_clusters` variable of the `seurat@meta.data` slot. These can be names, not just numbers!
* `seurat@reductions` must contain a `pca` entry and describe at least 3 components. Both '2D' and '3D' PCA coordinates are taken from the `pca` slot.

```r
seurat_to_scviewer <- function(seurat, idents='seurat_clusters')
  list(feature_values=Matrix::cBind({seurat@assays$RNA@data %>% Matrix::t()},
                                    {select(seurat@meta.data, starts_with('GeneModule')) %>% set_names(str_remove, 'GeneModule-') %>% Matrix::as.matrix()}),
       metadata=seurat@meta.data %>% as.data.frame() %>% rownames_to_column('cell_id') %>% select(cell_id, seurat_clusters),
       reductions=lapply(seurat@reductions, function(reduction)
                      slot(object=reduction, name='cell.embeddings') %>%
                        as.data.frame()) %>%
                    append(list(pca_3d=.$pca[,1:3])) %>%
                    (function(x) {x$pca %<>% select(1:2); x}) %>%
                    lapply(function(df) df %>% set_names(function(n) head(x=c('x','y','z'), n=length(n)))) %>%
                    lapply(rownames_to_column, var='cell_id'))
```

# Data format

The `rds` file must be a `list` and contain:

* `feature_values`: a `matrix` (likely sparse) of values that can be plotted. Rows are cells and columns are features. Column (feature) names are used in the feature entry UI element.
* `metadata`: a `data.frame` with at least a `seurat_clusters` variable that will be used ... as cluster identities.
* `reductions`: a list of coordinates from each projection method. A 2D and 3D PCA projection are defined using the `pca` slot and the first two (or three) components.

# Configuration file

The `yaml` configuration file contains parameters for the session. The `datasets` section of the configuration file is used to populate the dataset selection dropdown. It is a two-level list (hardcoded at the moment) and must contain an `rds_file` key which can be loaded into the session and be properly formatted.

