
#' Read the configuration yaml
#' 
#' @param file Path to YAML
#' 
load_app_config <- function(file='config.yaml')
  yaml::read_yaml(file=file)

#' Parse configuration object to get available datasets
#' 
#' @param config The parsed configuration file
#' @param datasets The nested list of datasets, a subset of `config`
#' 
get_dataset_choices <- function(config, datasets=config$datasets)
  map_depth(.x=datasets, .depth=2, .f=pluck, 'file') %>%
    map_depth(.depth=1, function(x) x[names(x)!='config']) %>%
    plyr::ldply(function(x) data.frame(name=names(x))) %>%
    unite('key', .id, name, sep='$', remove=FALSE) %>%
    mutate_if(is.factor, as.character) %>%
    plyr::dlply(~.id, plyr::dlply, ~name, pluck, 'key')

#' Parse configuration object to get parameter values
#' 
#' @param config The parsed configuration file
#' @param key The key to extract
#' 
#' @details Searches in `config` to find the most-dataset-specific value for `key`.
#' 
get_config_values <- function(config, key)
  reshape2::melt(config) %>%
    filter_at(vars(-value), any_vars(.==key)) %>%
    select_if(~all(.!=key | is.na(.))) %>%
    unite(index, L2, L3, sep='$') %>%
    spread(key=index, value=value) %>%
    rename_at(vars(matches('^NA\\$NA$')), function(x) 'default') %>%
    set_names(str_remove, pattern='\\$config') %>%
    mutate_all(as.character)

#' Pluck nested values from config in order of priority
#' 
#' @param values A named vector from which a values is chosen
#' @param priority A ranked set of keys
#' 
get_prioritised_value <- function(values, priority)
  priority %>%
    sapply(pluck, .x=values, .default=NA, simplify=TRUE) %>%
    na.omit() %>%
    head(n=1) %>%
    unname()
