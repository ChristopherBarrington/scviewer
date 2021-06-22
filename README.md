# scviewer

A lightweight single cell visualisation tool



# Running scviewer

This package provides the functions to create `h5` files and run an `scviewer` shiny app, see [scviewer-app](https://github.com/ChristopherBarrington/scviewer-app) for information on configuring the app.

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

* features in `scviewer` are the normalised RNA and numeric meta data.
* `seurat@reductions` must contain a `pca` entry and describe at least 3 components. Both '2D' and '3D' PCA coordinates are taken from the `pca` slot.

Once this repository has been cloned, the `scviewer` package can be loaded without installation:

```R
devtools::load_all('scviewer', export_all=FALSE)
```

or with installation:

```R
remotes::install_github(repo='ChristopherBarrington/scviewer')
library(scviewer)
```

To use the package to create files only, any shiny-related missing library warnings can be ignored.

Load the seurat object, here I use `readRDS` but any equivalent method to get an object should be fine. I also define a path into which the `h5` files should be written. Any method to define these variable would work.

```R
seurat <- Sys.getenv('INPUT_SEURAT_RDS') %>% readRDS()
save_path <- Sys.getenv('OUTPUT_PATH') %T>% dir.create(recursive=TRUE, showWarnings=FALSE)
```

**There is a tl;dr in the [Bundling the whole process](#bundling-the-whole-process) section**

## Required reductions

To make the 2- and 3-dimension tSNE and UMAP panels, the reductions must be available in the `reductions` slot of the input Seurat object. Assuming there is a PCA `Reduction` in the object, the following will remove any other `Reductions` than `pca` and calculate tSNE and UMAP `Reductions`, where `n_dimensions` is specified.

```R
seurat %<>% add_all_projections(n_dimensions=30)
```

## Formatting data from Seurat object and writing the h5

In the following, the data from the Seurat object is parsed/formatted/wrangled and mangled into structures that are written into the `h5` file.

### Create the `h5` file

An empty `h5` formatted file is created here. If the file exists, it should be deleted beforehand otherwise this command will fail. If an existing `h5` file is used as `h5_file` the groups written will be deleted from the file before being re-written by the following functions.

```R
h5_file <- file.path(save_path, str_c(Project(seurat), '.scv'))
create_h5_scv(h5_file=h5_file)
```
### Reductions

The following extracts all of the reductions in the Seurat object into a list of `data.frame` objects.

An assumption here is that for each reduction method (eg UMAP) there are two reduction objects: `umap` and `umap_3d` for example. If you don't have those embeddings, you can use `RunUMAP` to add them to your Seurat object or try using `scviewer::add_all_reductions()`.

For the PCA reduction, the first three components are selected to make the `pca_3d` data from the `pca` reduction then the first two are selected to make the 2D version.

```R
> reductions <- scviewer:::guess_reductions(seurat)
> lapply(reductions, head, n=3)
$pca
             cell_id         x           y
1 AAACCCACACCTCTGT-1  1.934101  3.70549631
2 AAACCCAGTAAGAACT-1 -6.095149  2.35777169
3 AAACGAAAGACTGAGC-1 11.628598 -0.09594123

$tsne
             cell_id          x         y
1 AAACCCACACCTCTGT-1   7.718922 15.008067
2 AAACCCAGTAAGAACT-1   1.156841 -5.977375
3 AAACGAAAGACTGAGC-1 -24.378159 25.883702

$tsne_3d
             cell_id         x         y           z
1 AAACCCACACCTCTGT-1 -20.08531 -6.058905   0.7906353
2 AAACCCAGTAAGAACT-1  -1.68918  5.855843 -29.6971474
3 AAACGAAAGACTGAGC-1 -13.84951  4.869889  43.4326906

$umap
             cell_id         x         y
1 AAACCCACACCTCTGT-1 -1.345953  1.311305
2 AAACCCAGTAAGAACT-1  4.432537  2.619599
3 AAACGAAAGACTGAGC-1 -6.841505 -1.717847

$umap_3d
             cell_id          x         y         z
1 AAACCCACACCTCTGT-1  1.1464871  1.226989 -1.242662
2 AAACCCAGTAAGAACT-1 -0.9679188 -2.856427 -1.369519
3 AAACGAAAGACTGAGC-1  5.5932413  2.799470  1.500001

$pca_3d
             cell_id         x           y         z
1 AAACCCACACCTCTGT-1  1.934101  3.70549631 -6.434000
2 AAACCCAGTAAGAACT-1 -6.095149  2.35777169 -4.475791
3 AAACGAAAGACTGAGC-1 11.628598 -0.09594123  1.120231

>
```

```R
write_reductions(h5_file=h5_file, seurat=seurat) # uses `guess_reductions` to collect coordinates
```

```R
write_reductions(h5_file=h5_file, reductions=reductions) # user-defined coordinates list
```

### Feature values

By default, the values of RNA features are extracted into a matrix and can be appended with any other numeric meta data. Any numeric data can be added here so that they can be plotted in the viewer as a feature. This matrix can be automatically created by `write_features` using `guess_features_matrix` when the `features_matrix` argument is omitted.

```R
> features_matrix <- scviewer:::guess_features_matrix(seurat)
> features_matrix[1:10, c('COX3', 'RPS12', 'percent_mt', 'nFeature_RNA', 'nCount_RNA')]
                       COX3    RPS12 percent_mt nFeature_RNA nCount_RNA
AAACCCACACCTCTGT-1 4.811714 4.817744   4.610008         2225      13449
AAACCCAGTAAGAACT-1 4.468762 4.881488   3.730982         1831      10319
AAACGAAAGACTGAGC-1 4.244721 4.617961   3.177644         3048      17749
AAACGAAAGGCTCTAT-1 4.787714 4.974337   5.194957         2634      17055
AAACGAAGTGAGCCAA-1 4.458634 4.937042   3.297027         1456       6794
AAACGCTAGACCAGAC-1 4.841800 4.738045   6.368101         2452      12013
AAACGCTCAAGGCTTT-1 4.744864 4.509862   4.752348         2387      12457
AAACGCTCAGACGCTC-1 4.959195 4.717896   6.495177         2369      12440
AAACGCTCATCAGTGT-1 5.197880 4.598387   6.815642         1984       8950
AAACGCTGTAGATTAG-1 4.911110 4.777138   5.093644         2539      14096
>
```

(This is the slow bit). For every feature, a new element in the `h5` file is written; each feature is a key in the `feature/values` location. Feature names are converted here to lower case, since the retrieval from the app is case-sensitive.

```R
write_features(h5_file=h5_file, seurat=seurat) # uses `guess_features_matrix` to collect normalised RNA and numeric meta data
```

```R
write_features(h5_file=h5_file, features_matrix=features_matrix) # user-defined feature values matrix
```

#### Module scores

If the `Seurat` object contains numeric variables in the `meta.data` slot, these will be added to the feature matrix. To tell `scviewer` what type of feature these are, the `feature_types` argument can be defined, if not everything is assumed to be a 'count'.

The following list adds all of the 'normal' features from the matrix into 'count' and selects the `seurat@meta.data` variables (which will become features) that start with 'ModuleScore'.

```R
feature_types <- list(count=colnames(seurat), module_score={colnames(seurat@meta.data) %>% str_subset('^ModuleScore:')})
write_features(h5_file=h5_file, seurat=seurat, feature_types=feature_types) # uses `guess_features_matrix` to collect normalised RNA and numeric meta data
```

### Dataset metadata

The metadata is extracted and subset. Any cell filters need to be defined here - these are one or more variables that can be used to determine if a cell should be displayed. The logic uses `%in%` to identify cells whose filter value is selected. In this example, I create a `dataset_filter` which reformats the `orig.ident` and adds in the number of cells in the filter. Cluster identities are defined here too, keeping any variable with the '\_snn_res' string in this case. But these variables are completely flexible, any names and any content. 

The filter and cluster variables are converted to factors and their levels ordered; the order of levels here is the order of the levels in the app.

The code below shows how the meta data table can be customised and provided. If omitted, any non-numeric variables of `seurat@meta.data` will be exported and converted to factors by `guess_metadata`. 

```R
> seurat@meta.data %>%
+   as.data.frame() %>%
+   rownames_to_column('cell_id') %>%
+   mutate(datasets_filter=str_replace(orig.ident, '_', ' ')) %>%
+   select(datasets_filter, cell_id, contains('_snn_res.')) %>%
+   group_by(datasets_filter) %>%
+   mutate(N=n()) %>%
+   ungroup() %>%
+   mutate(datasets_filter={sprintf(fmt='%s (n=%s)', datasets_filter, comma(N)) %>% factor() %>% fct_relevel({levels(.) %>% mixedsort()})}) %>%
+   mutate_at(vars(contains('_snn_res.')), function(x) x %>% fct_relevel({levels(.) %>% mixedsort()})) %>%
+   select(-N) -> metadata
>
> metadata[1:5, 1:7] %>% as.data.frame()
  datasets_filter            cell_id RNA_snn_res.0.2 RNA_snn_res.0.4 RNA_snn_res.0.6 RNA_snn_res.0.8 RNA_snn_res.1
1     E85 (n=477) AAACCCAAGTTAACGA-1               0               2               1               1             1
2     E85 (n=477) AAAGGATAGTAGACCG-1               1               0               2               2             2
3     E85 (n=477) AAATGGATCGAACACT-1               0               1               0               0             0
4     E85 (n=477) AACAAAGTCCGACATA-1               0               1               0               0             0
5     E85 (n=477) AACAACCAGATCCTAC-1               0               2               1               1             1
>
```

Now the `metadata` is now written to the `h5_file`.

```R
write_metadata(h5_file=h5_file, seurat=seurat) # uses `guess_metadata` to collect factor meta data
```

```R
write_metadata(h5_file=h5_file, metadata=metadata) # user-defined metadata
```

### Cell clusters

A list is created that determines which cluster sets to include in the drop down selector and which cluster identities should be shown by default. Here, I take all of the cluster sets in the meta data table and show all cluster identities by default. The final output of this chunk is a list of lists. Each index of the first-level list is named according to the meta data variable. The second-level list contains:

* `var` the meta data variable
* `name` a name for the cluster set to be shown in the drop down - here the clustering resolution is appended to 'Res. '
* `selected` is a vector of cluster identities (which should be levels of the `var`) to show by default

```R
> cluster_identity_sets <- guess_cluster_identity_sets(seurat)
> head(cluster_identity_sets, n=3)
$RNA_snn_res.0.2
$RNA_snn_res.0.2$var
[1] "RNA_snn_res.0.2"

$RNA_snn_res.0.2$name
[1] "Res. 0.2"

$RNA_snn_res.0.2$selected
[1] "0" "1" "2" "3" "4"


$RNA_snn_res.0.4
$RNA_snn_res.0.4$var
[1] "RNA_snn_res.0.4"

$RNA_snn_res.0.4$name
[1] "Res. 0.4"

$RNA_snn_res.0.4$selected
[1] "0" "1" "2" "3" "4" "5" "6"


$RNA_snn_res.0.6
$RNA_snn_res.0.6$var
[1] "RNA_snn_res.0.6"

$RNA_snn_res.0.6$name
[1] "Res. 0.6"

$RNA_snn_res.0.6$selected
[1] "0" "1" "2" "3" "4" "5" "6" "7" "8"


>
```

The list of cell cluster information that was defined above is written to the `h5` file as a list. The default behaviour (above) can be automatically applied by omitting the `cluster_identity_sets` argument.

```R
write_cluster_identity_sets(h5_file=h5_file, seurat=seurat) # uses `guess_cluster_identity_sets` to collect cluster sets
```

```R
write_cluster_identity_sets(h5_file=h5_file, cluster_identity_sets=cluster_identity_sets) # user-defined cluster definitions
```

### Cell filters

The list of cell filters here is used to create the drop down UI elements and filter the cells. The list is named according to the label that should be displayed next to the UI element and the `var` element is the variable in the meta data that should be filtered. Filters are not required and can be omitted, if there are none.

In this example, I define filters only for the 'datasets_filter' variable and specify that one dataset should be selected by default. (is functionality even working?!)

```R
cell_filter_parameters <- list(`Constituent datasets`=list(var='datasets_filter', selected=c('E85 (n=477)'))) %>%
write_cell_filter_parameters(h5_file=h5_file, cell_filter_parameters=cell_filter_parameters)
```

## Bundling the whole process

The following wrapper function _could_ work. No cell filters are applied!

It will :

* use `readRDS` to read `seurat` when `seurat` is a character
* recalculate the tSNE and UMAP projections in 2D and 3D (if `recalculate_reductions=TRUE`)
* run`write_reductions`
* run`write_feature_values`
* run`write_metadata`
* run`write_cluster_identity_sets`

Once the `h5` file is written, each component can be modified using the examples above.

```R
seurat_to_scv(h5_file=h5_file, seurat=seurat, recalculate_reductions=TRUE, n_dimensions=40)
```

# Configuration file

The `yaml` configuration file contains parameters for the session. The `datasets` section of the configuration file is used to populate the dataset selection dropdown. It is a two-level list: the first level denotes groups within the dropdown (eg. species) and the second denotes datasets (eg. samples). Each dataset must contain a `file` key which is the path to a properly -formatted `h5` file (as described above).

The top-level `initial_feature` key can be used to define an initial feature that is displayed for all datasets in the instance. This can be overridden by specifying the `initial_feature` at any level of the configuration.

Ignore the `tracker`.



