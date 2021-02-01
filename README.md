# scviewer

a lightweight single cell visualisation tool

# Convert Seurat to scviewer

The objects required for plotting should be available in a `list` saved as an `rds` object. A `Seurat` object can be exported using the following function.

Assumptions:

* features in `scviewer` are the normalised RNA and gene module scores.
* `seurat@reductions` must contain a `pca` entry and describe at least 3 components. Both '2D' and '3D' PCA coordinates are taken from the `pca` slot.
* `cluster_id` and `group_id` are correctly-ordered `factor`s


```r
library(Matrix)
library(Seurat)
library(magrittr)
library(tidyverse)

lapply(seurat@reductions, function(reduction)
    slot(object=reduction, name='cell.embeddings') %>%
      as.data.frame()) %>%
  append(list(pca_3d=.$pca[,1:3])) %>%
  (function(x) {x$pca %<>% select(1:2); x}) %>%
  lapply(function(df) df %>% set_names(function(n) c('x','y','z')[seq_along(n)])) %>% # untested!
  lapply(rownames_to_column, var='cell_id') -> reductions

cbind({seurat@assays$RNA@data %>% Matrix::t()},
      {select(seurat@meta.data, starts_with('GeneModule')) %>% set_names(str_remove, 'GeneModule-') %>% Matrix::as.matrix()}) %>%
  as.matrix() -> feature_values

seurat@meta.data %>%
  as.data.frame() %>%
  rownames_to_column('cell_id') %>%
  select(cell_id, group_id=seurat_clusters, cluster_id=seurat_clusters) -> metadata
```

# Data format

The `h5` file must contain:

* `features/values`: contains the normalised count data, added feature-by-feature
* `reductions`: contains the coordinates for each cell in each reduction method or paramter set
* `metadata`: a `data.frame` with _at least_ `cell_id`, `group_id` and `cluster_id` variables
  * `group_id` will be used to plot distibution of selected feature values
  * `cluster_id` can be used to highlight clusters on the reduced dimension plot

**Feature names are converted to lower case here to avoid capitalisation problems with user input.**

```r
library(rhdf5)

#+ prep_h5
sprintf('/// prepping h5 %s', Project(seurat)) %>% message()
h5_file <- file.path(save_path, str_c(Project(seurat), '.h5'))
if(file.exists(h5_file))
  sprintf('!!! %s already exists!', h5_file) %>% stop()

h5createFile(file=h5_file)
h5createGroup(file=h5_file, group='features')
h5createGroup(file=h5_file, group='features/values')
h5createGroup(file=h5_file, group='reductions')

#+ write_h5
sprintf('/// writing h5 features %s', Project(seurat)) %>% message()
h5write(obj=colnames(feature_values), file=h5_file, name='features/names')
h5write(obj=rownames(feature_values), file=h5_file, name='features/cell_ids')
for(i in colnames(feature_values))
  str_to_lower(i) %>%
    sprintf(fmt='features/values/%s') %>%
    h5write(obj=feature_values[,i], file=h5_file)

sprintf('/// writing h5 reductions %s', Project(seurat)) %>% message()
for(i in names(reductions))
  h5write(obj=reductions[[i]], file=h5_file, name=sprintf(fmt='reductions/%s', i))

sprintf('/// writing h5 metadata %s', Project(seurat)) %>% message()
h5write(obj=metadata, file=h5_file, name='metadata')
```

# Configuration file

The `yaml` configuration file contains parameters for the session. The `datasets` section of the configuration file is used to populate the dataset selection dropdown. It is a two-level list: the first level denotes groups within the dropdown (eg. species) and the second denotes datasets (eg. samples). Each dataset must contain a `file` key which is the path to a properly -formatted `h5` file (as described above). Other keys may be included, though only `file` is currently used.

# Running scviewer

The `scviewer.R` (renamed to `app.R`) and `config.yaml` files should be saved in the same directory and visited in a web browser. It's a `shiny` app after all.
