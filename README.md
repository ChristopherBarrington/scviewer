# scviewer

A lightweight single cell visualisation tool



# Create an `h5` file

`scviewer` expects all datasets to be saved into a properly-formatted `h5` file with keys for the following:

* `metadata/data` is a `data.frame` that contains ... metadata ... and cluster identities
* `metadata/factor_levels` is a group that contains the `factor` levels for each factor in the metadata `data.frame`
* `features` is a group that contains three elements:
  * `values` is a group that contains the values of features that are plotted (rows of a Seurat expression matrix)
  * `names` is a `vector` of feature names (`rownames` of a Seurat matrix)
  * `cell_ids` is a vector of barcodes (`colnames` of a Seurat matrix)
* `reductions` is a group of `data.frame` objects with coordinates for each cell in 2D and 3D space
* `cell_filter_parameters` is a list of metadata colums on which a filter should be permitted
* `cluster_identity_sets` is a list of metadata variables and default identifiers that can be used to display cell clusters with different resolutions/methods etc



# Convert Seurat to scviewer

The following examples can be used to create a new scviewer-compatible `h5` file from a Seurat object. Some assumptions are that:

* features in `scviewer` are the normalised RNA and gene module scores.
* `seurat@reductions` must contain a `pca` entry and describe at least 3 components. Both '2D' and '3D' PCA coordinates are taken from the `pca` slot.

First, load any handy libraries:

```r
library(gtools) # to sort vectors
library(scales) # to format numbers with commas
library(Seurat) # ...
library(rhdf5) # to create and write h5 files
library(magrittr)
library(tidyverse)
```

Load the seurat object, here I use `readRDS` but any equivalent method to get an object should be fine. I also define a path into which the `h5` files should be written.

```R
seurat <- Sys.getenv('INPUT_SEURAT_RDS') %>% readRDS()
save_path <- Sys.getenv('OUTPUT_PATH') %T>% dir.create(recursive=TRUE, showWarnings=FALSE)
```

## Formatting data from Seurat object

In the following, the data from the Seurat object is parsed/formatted/wrangled and mangled into structures that can be saved in the `h5` file.

### Reductions

The following extracts all of the reductions in the Seurat object into a list of `data.frame` objects.

An assumption here is that for each reduction method (eg UMAP) there are two reduction objects: `umap` and `umap_3d` for example. If you don't have those embeddings, you can use `RunUMAP` to add them to your Seurat object.

For the PCA reduction, the first three components are selected to make the `pca_3d` data from the `pca` reduction then the first two are selected to make the 2D version.

```R
lapply(seurat@reductions, function(reduction)
    slot(object=reduction, name='cell.embeddings') %>%
      as.data.frame()) %>%
  append(list(pca_3d=.$pca[,1:3])) %>% # get 3d pca embedding coordinates
  (function(x) {x$pca %<>% select(1:2); x}) %>% # select first 2 pca embedding coordinates
  lapply(function(df) df %>% set_names(function(n) head(x=c('x','y','z'), n=length(n)))) %>% # select x/y for 2d or x/y/z for 3d redcutions
  lapply(rownames_to_column, var='cell_id') -> reductions
```

### Feature values

The values of features are extracted into a matrix and can be appended with any other numeric data. Here I append any metadata variables that start with 'GeneModule', but you can add `nCount_RNA`  or `percent.mt` or cell cycle scores etc so that they can be plotted in the viewer as a feature.

```R
cbind({seurat@assays$RNA@data %>% Matrix::t()},
      {select(seurat@meta.data, starts_with('GeneModule')) %>% set_names(str_replace, 'GeneModule-', 'GeneModule ') %>% Matrix::as.matrix()}) %>%
  as.matrix() -> feature_values
```

The metadata is extracted and subset. Any cell filters need to be defined here - these are one or more variables that can be used to determine if a cell should be displayed. The logic uses `%in%` to identify cells whose filter value is selected. In this example, I create a `dataset_filter` which reformats the `orig.ident` and adds in the number of cells in the filter. Cluster identities are defined here too, keeping any variable with the '_snn_res' string in this case. But these variables are completely flexible, any names and any content. 

Keep `cell_id` in, it might be important.

### Dataset metadata

The filter and cluster variables are converted to factors and their levels ordered; the order of levels here is the order of the levels in the app.

```R
seurat@meta.data %>%
  as.data.frame() %>%
  rownames_to_column('cell_id') %>%
  mutate(datasets_filter=str_remove(orig.ident, '_all') %>% str_replace('_', ' ')) %>% 
  select(datasets_filter, cell_id, contains('_snn_res.')) %>%
  group_by(datasets_filter) %>%
  mutate(N=n()) %>%
  ungroup() %>%
  mutate(datasets_filter={sprintf(fmt='%s (n=%s)', datasets_filter, comma(N)) %>% factor() %>% fct_relevel({levels(.) %>% mixedsort()})}) %>%
  mutate_at(vars(contains('_snn_res.')), function(x) x %>% fct_relevel({levels(.) %>% mixedsort()})) %>%
  select(-N) -> metadata

```

### Cell clusters

A list is created that determines which cluster sets to include in the dropdown selector and which cluster identities should be shown by default. Here, I take all of the cluster sets in the metadata table and show all cluster identities by default. The final output of this chunk is a list of lists. Each index of the first-level list is named according to the metadata variable. The second-level list contains:

* `var` the metadata variable
* `name` a name for the cluster set to be shown in the dropdown - here the clustering resolution is appended to 'Res. '
* `selected` is a vector of cluster identities (whcih should be levels of the `var`) to show by default

```R
metadata %>%
  colnames() %>%
  str_subset('_snn_res.') %>%
  set_names() %>%
  lapply(function(x) list(var=x, name={str_remove(x, '^\\D+') %>% sprintf(fmt='Res. %s')}, selected=levels(metadata[[x]]))) -> cluster_identity_sets
```

## Writing the `h5` file

All of the above structures are now written into a new `h5` file (the functions here will not overwrite an existing file). I use the `rhdf5` library here but this should not be absolutely required.

The `h5createGroup` function initialises an element in the `h5` file onto which we can add levels of data. `h5write` is used to write the data from the above structures into the `h5` file; the `obj` argument is the `R` data object (or part thereof), `file` is the path to the `h5` file and `name` is the location of the data in the `h5` file. `h5ls` can be used to see the structure of an `h5` file (though this will be a lot of output once `features` have been written!)

```R
h5_file <- file.path(save_path, str_c(Project(seurat), '.h5'))
h5createFile(file=h5_file)
```

### Reductions

A new group called `reductions` is created (this *is* a required name!) into which each reduction is written.

```R
h5createGroup(file=h5_file, group='reductions')
for(i in names(reductions))
  h5write(obj=reductions[[i]], file=h5_file, name=sprintf(fmt='reductions/%s', i))
```

### Feature values

This is the slow bit. For every feature, a new element in the `h5` file is written; each feature is a key in the `feature/values` location. Feature names are converted here to lower case, since the retrieval from the app is case-sensitive.

```R
h5createGroup(file=h5_file, group='features')
h5createGroup(file=h5_file, group='features/values')
h5write(obj=colnames(feature_values), file=h5_file, name='features/names')
h5write(obj=rownames(feature_values), file=h5_file, name='features/cell_ids')

for(i in colnames(feature_values))
  str_to_lower(i) %>%
    sprintf(fmt='features/values/%s') %>%
    h5write(obj=feature_values[,i], file=h5_file)
```

### Metadata

The metadata table and the factor levels required to recapitulate it are written here.

```R
h5createGroup(file=h5_file, group='metadata')
h5createGroup(file=h5_file, group='metadata/factor_levels')
h5write(obj=metadata, file=h5_file, name='metadata/data')
for(i in {metadata %>% select_if(is.factor) %>% colnames()})
  h5write(obj=levels(metadata[[i]]), file=h5_file, name=sprintf(fmt='metadata/factor_levels/%s', i))
```

### Cell filters

The list of cell filteres here is used to create the dropdown UI elements and filter the cells. The list is named according to the label that should be displayed next to the UI element and the `var` element is the variable in the metadata that should be filtered. In this example, I define filters only for the 'E105' or 'opossum' datasets, with an empty list otherwise to indictate that there are no filtering options.

```R
cell_filter_parameters <- list()
if(Project(seurat) %>% is_in(c('E105', 'opossum')))
  cell_filter_parameters %<>% append(list(`Constituent datasets`=list(var='datasets_filter')))
h5write(obj=cell_filter_parameters, file=h5_file, name='cell_filter_parameters')
```

### Cell clusters

The list of cell cluster information that was defined above is written to the `h5` file as a list.

```R
h5write(obj=cluster_identity_sets, file=h5_file, name='cluster_identity_sets')
```



# Configuration file

The `yaml` configuration file contains parameters for the session. The `datasets` section of the configuration file is used to populate the dataset selection dropdown. It is a two-level list: the first level denotes groups within the dropdown (eg. species) and the second denotes datasets (eg. samples). Each dataset must contain a `file` key which is the path to a properly -formatted `h5` file (as described above).

The top-level `initial_feature` key can be used to define an initial feature that is displayed for all datasets in the instance. This can be overridden by specifying the `initial_feature` at any level of the configuration.

Ignore the `tracker`.



# Running scviewer

The `scviewer.R` (renamed to `app.R`) and `config.yaml` files should be saved in the same directory and visited in a web browser. It's a `shiny` app after all.
