
#' Define the server
#' 
#' @import plotly
#' 
#' @importFrom digest digest
#' @importFrom dqshiny hidden update_autocomplete_input
#' @importFrom grid grid.draw
#' @importFrom gtable is.gtable
#' @importFrom magrittr equals extract
#' @importFrom plotly layout
#' @importFrom RColorBrewer brewer.pal 
#' @importFrom rhdf5 h5read
#' @importFrom scales comma rescale squish
#' @importFrom shinyWidgets pickerInput sendSweetAlert
#' @importFrom stringi stri_rand_strings
#' @importFrom tibble tibble
#' @importFrom tidyr unnest
#' 
server <- function(input, output, session) {

  log_message <- skip_log_message
  if(getOption('scviewer.verbose'))
    log_message <- show_log_message

  app_config <- load_app_config('config.yaml')


  ## get UI inputs
  ### load the dataset file
  reactive(x={
    req(input$filename)

    input$filename %T>%
      (. %>% sprintf(fmt='(app_data) initialising from: %s') %>% log_message()) -> input_dataset_key

    #### parse dropdown key to give levels 1 and 2 from the datasets key of the yaml config
    input_dataset_key %>%
      str_split('\\$') %>%
      pluck(1) %>%
      set_names('L1','L2') %>%
      as.list() -> dataset_selection

    #### get the h5 file from the config file
    app_config %>%
      pluck('datasets', dataset_selection$L1, dataset_selection$L2, 'file') %T>%
      (. %>% sprintf(fmt='(app_data) h5_file: %s') %>% log_message(prepend='+++')) -> h5_file

    #### render error message if h5_file is not found, don't carry on
    if(h5_file %>% file.exists() %>% not()) {
      sprintf(fmt='(add_data) h5_file does not exist! Error!') %>% log_message(prepend='!!!')
      sendSweetAlert(session=session,
                     title='Error reading dataset!',
                     text=tags$span('Please check the', tags$code('file'), 'attribute(s) in ', tags$code('config.yaml'), tags$br(), tags$br(),
                                    'The following file could not be found:', tags$br(), tags$code(h5_file)),
                     html=TRUE,
                     type='error', closeOnClickOutside=FALSE, btn_colors='#F27474')

      return(NULL)
    }

    #### get the initial values from the config file
    get_config_values(app_config, 'initial_feature') %>%
      get_prioritised_value(priority=c(input_dataset_key, dataset_selection$L1, 'default')) %T>%
      (. %>% sprintf(fmt='(app_data) initial_feature: %s') %>% log_message(prepend='+++')) -> initial_feature

    get_config_values(app_config, 'initial_reduction') %>%
      get_prioritised_value(priority=c(input_dataset_key, dataset_selection$L1, 'default'), missing='unspecified') %T>%
      (. %>% sprintf(fmt='(app_data) initial_reduction: %s') %>% log_message(prepend='+++')) -> initial_reduction

    get_config_values(app_config, 'initial_cluster_identity_set') %>%
      get_prioritised_value(priority=c(input_dataset_key, dataset_selection$L1, 'default'), missing=1) %T>%
      (. %>% sprintf(fmt='(app_data) initial_cluster_identity_set: %s') %>% log_message(prepend='+++')) -> initial_cluster_identity_set

    #### load metadata table
    sprintf(fmt='(add_data) collecting metadata') %>% log_message(prepend='+++')
    metadata_list <- h5read(file=h5_file, name='metadata')

    ##### for each would-be-factor variable in metadata_list$data, update the factor levels using metadata_list$factor_levels
    sprintf(fmt='(add_data) adding factor levels') %>% log_message(prepend='+++')
    for(i in names(metadata_list$factor_levels))
      metadata_list$data %<>%
        mutate(across(.cols=i, function(x) factor(metadata_list$factor_levels[[i]][x], levels=metadata_list$factor_levels[[i]])))

    #### pull out the list of reductions
    sprintf(fmt='(add_data) collecting reductions') %>% log_message(prepend='+++')
    reductions <- h5read(file=h5_file, name='reductions')

    #### update UI elements
    sprintf(fmt='(add_data) updating ui elements') %>% log_message(prepend='+++')

    ##### features dropdown
    sprintf(fmt='(add_data) updating feature name options') %>% log_message(prepend='+++')
    h5read(file=h5_file, name='features/types') %>%
      tibble(type=names(.), name=.) %>%
      unnest(cols=name) %T>%
      {update_autocomplete_input(session=session, id='feature',
                                 options=.$name,
                                 value=initial_feature)} %>% # put the features in the text input ui element
      select(name, type) %>%
      deframe() %>%
      as.list() -> feature_names_2_types

    ##### dimension reduction methods
    sprintf(fmt='(add_data) updating reduction method selector') %>% log_message(prepend='+++')
    reduction_names <- c(umap='UMAP', tsne='tSNE', pca='PCA', lsi='LSI')
    
    names(reductions) %>%
      str_remove('(_|\\.)(2|3)d$') %>%
      unique() %>%
      set_names() -> available_reductions
    
    available_reductions %>%
      as.list() %>%
      set_names(function(name)
        str_split(name, '\\.') %>%
          lapply(function(x) {x[1] <- reduction_names[x[1]]; x}) %>%
          sapply(str_c, collapse='.')) -> reduction_choices
    
    initial_reduction %>%
      when(.=='unspecified' ~ preferred_choice(x=available_reductions, preferences=names(reduction_names), default=1),
           TRUE ~ initial_reduction) -> selected_reduction

    updateSelectInput(session=session, inputId='reduction_method',
                      choices=reduction_choices, selected=selected_reduction) # update the reduction methods ui element

    ##### cell cluster idents
    cluster_identity_sets <- tryCatch(h5read(file=h5_file, name='cluster_identity_sets'), error=function(...) NULL)

    if(is.null(cluster_identity_sets)) { # if there are no cluster sets, make a dummy one
      cluster_identity_sets <- list(monocluster=list(var='monocluster', name='monocluster', selected='All cells'))
      metadata_list$data %<>% mutate(monocluster=factor('All cells'))
    }

    cluster_identity_sets %>% pluck(initial_cluster_identity_set, 'name') -> default_cluster_identity_name 
    cluster_identity_sets %>% length() -> n_cluster_identity_sets
    cluster_identity_sets %>% lapply(pluck, 'name') %>% {set_names(names(.), unlist(.))} -> cluster_identity_sets_choices

    ###### if there is more than one cluster set, add a picker for them
    removeUI(selector='#cluster_set_filter', immediate=TRUE) # clear UI elements that are already drawn
    removeUI(selector='#cluster_identitites_filter', immediate=TRUE) # clear UI elements that are already drawn
    pickerInput(inputId='cluster_identity_set_index', label='Cluster identity sets',
                choices=cluster_identity_sets_choices, selected=initial_cluster_identity_set,
                options=list(`actions-box`=TRUE, size=9),
                multiple=FALSE) %>%
      (function(p) if(n_cluster_identity_sets==1) hidden(p) else p) %>%
      tags$div(id='cluster_set_filter') %>%
      insertUI(selector='#cluster_sets', where='afterBegin', immediate=TRUE, session=session)

    ##### create the filter UI elements
    removeUI(selector='#all_filters', immediate=TRUE) # clear any filter UI elements that are already drawn
    tryCatch(h5read(file=h5_file, name='cell_filter_parameters'), error=function(...) NULL) %>%
      lapply(function(x) modifyList(x=x, val=list(inputId=str_c(x$var, stri_rand_strings(n=1, length=5), sep='.')))) -> cell_filter_parameters # provide an inputId for each filtering element

    cell_filter_parameters %>%
      lapply(pluck, 'inputId') %>%
      unlist() %>%
      str_c(collapse=', ') %>%
      sprintf(fmt='(app_data) creating filter UI elements for: %s') %>%
      log_message(prepend='+++') # send a log message

    cell_filter_parameters %>%
      Map(params=., label=names(.), function(params, label)
        metadata_list$data %>%
          pluck(params$var) %>%
          levels() %>%
          pickerInput(inputId=params$inputId, label=label,
                      choices=., selected=.,
                      options=list(`actions-box`=TRUE, size=9, `selected-text-format`='count>1'),
                      multiple=TRUE)) %>%
      tags$div(id='all_filters') %>%
      insertUI(selector='#filters', where='afterBegin', immediate=TRUE, session=session) # make a pickerInput for each filtering element and put it after the filters div

    #### add to reactive values list
    list(dataset_key=input_dataset_key,
         h5_file=h5_file,
         initial_feature=initial_feature,
         initial_reduction=initial_reduction,
         reductions=reductions,
         metadata=metadata_list$data,
         cell_filter_parameters=cell_filter_parameters,
         cluster_identity_sets=cluster_identity_sets,
         feature_names_2_types=feature_names_2_types)}) -> app_data



  ### collect the user-specified feature
  reactive(x={
    app_data <- app_data()

    input_feature <- input$feature
    initial_feature <- app_data$initial_feature
    h5_file <- app_data$h5_file

    input_feature <- if_else(input_feature=='', initial_feature, input_feature)

    sprintf('(selected_feature) reading feature [%s] from: %s', input_feature, h5_file) %>% log_message()

    h5_name <- input_feature %>% str_to_lower() %>% sprintf(fmt='features/values/%s')
    feature_values <- h5read(file=h5_file, name=h5_name)
     
    feature_values %>% min() %>% subtract(0.05) %>% round(digits=1) -> slider_min
    feature_values %>% max() %>% add(0.05) %>% round(digits=1) -> slider_max

    sprintf('(selected_feature) setting value limits for %s to [%s]', input_feature, str_c(slider_min, slider_max, sep=',')) %>% log_message(prepend='+++')
 
    updateSliderInput(session=session, inputId='feature_value_limits',
                      min=slider_min, max=slider_max,
                      value=c(slider_min, slider_max))

    list(name=input_feature,
         values=feature_values)}) -> selected_feature



  ### collect the colour scale limits
  reactive(x={
    req(input$feature_value_limits)
 
    if(all(input$feature_value_limits==0))
      return(NULL)
 
    selected_feature <- isolate(selected_feature())
    feature_value_limits <- input$feature_value_limits
 
    sprintf('(input_feature_value_limits) getting value limits for %s as [%s]', selected_feature$name, str_c(feature_value_limits, collapse=',')) %>% log_message()
 
    list(min=feature_value_limits[1],
         max=feature_value_limits[2],
         limits=feature_value_limits)}) -> input_feature_value_limits



  ### collect the reduction method
  reactive(x={
    req(input$reduction_method)

    app_data <- app_data()

    h5_file <- app_data$h5_file
    reductions <- app_data$reductions
    reduction_method <- input$reduction_method

    sprintf('(reduction_coords) reading reduction [%s] from: %s', reduction_method, h5_file) %>% log_message()

    name_2d <- str_c(reduction_method, c('', '_2d', '.2d')) %>% subset(is_in(., names(reductions)))
    name_3d <- str_c(reduction_method, c('_3d', '.3d')) %>% subset(is_in(., names(reductions)))

    reductions[c(name_2d, name_3d)] %>%
      when(length(.)!=2 ~ stop('Exactly 2 reductions needed!', call.=FALSE),
           TRUE ~ .) %>%
      purrr::set_names(c('d2','d3'))}) -> reduction_coords



  ### collect the point size and reduce reactivity
  reactive(x={
    input$point_size %T>%
      (. %>% sprintf(fmt='(input_point_size) set point_size [%s]') %>% log_message())}) %>%
    debounce(500) -> input_point_size



  ### collect the input whether to tie scenes of 3d plots together
  reactive(x={
    input$match_scenes_3d_plotly %T>%
      (. %>% sprintf(fmt='(input_match_scenes_3d_plotly) set match_scenes_3d_plotly [%s]') %>% log_message())}) %>%
    debounce(500) -> input_match_scenes_3d_plotly



  ### collect input for cluster set to display
  reactive(x={
    req(input$cluster_identity_set_index)

    # collect input reactives
    app_data <- app_data()
    cluster_identity_set_index <- input$cluster_identity_set_index

    # define variables for function
    app_data$metadata %>% pluck(cluster_identity_set_index) %>% levels() -> cluster_identities
    cluster_identities %>% length() -> n_cluster_identities
    app_data$cluster_identity_sets %>% pluck(cluster_identity_set_index, 'selected') -> selected_cluster_identities

    # create a picker for the cluster idents for the selected cluster set
    removeUI(selector='#cluster_identitites_filter', immediate=TRUE) # clear UI elements that are already drawn
    pickerInput(inputId='cluster_identities', label='Cluster identities',
                choices=cluster_identities, selected=selected_cluster_identities,
                options=list(`actions-box`=TRUE, size=9, `selected-text-format`='count>1'),
                multiple=TRUE) %>%
      (function(p) if(n_cluster_identities==1) hidden(p) else p) %>%
      tags$div(id='cluster_identitites_filter') %>%
      insertUI(selector='#cluster_identities', where='afterBegin', immediate=TRUE, session=session)

    # return the index of the selected cluster set
    input$cluster_identity_set_index %T>%
      (. %>% sprintf(fmt='(cluster_identity_set_index) set cluster_identity_set_index [%s]') %>% log_message())}) %>%
    debounce(500) -> input_cluster_identity_set_index



  ### collect the cluster identities to filter
  reactive(x={
    req(input$cluster_identities)
    input$cluster_identities %T>%
      (. %>% length() %>% sprintf(fmt='(cluster_identities) set selected %s clusters') %>% log_message())}) %>%
    debounce(500) -> input_cluster_identities



  ### collect the selected colour palette
  reactive(x={
    req(input$predefined_palette)
 
    #### split the palette source and name from the string
    input$predefined_palette %T>%
      (. %>% sprintf(fmt='(selected_palette) selected palette [%s]') %>% log_message()) %>%
      str_split(pattern=':') %>%
      pluck(1) %>%
      as.list() %>%
      purrr::set_names(c('package', 'name', 'type')) %>%
      append(list(direction={.$type %>% switch(f=1, r=-1)}))}) -> selected_palette



  ### collect the cell filtering values
  reactive(x={
    #### collect input reactives
    app_data <- app_data()
    cluster_identity_set_index <- input_cluster_identity_set_index()

    sprintf(fmt='(formatted_cell_filter) making formatted filter expression') %>% log_message()

    #### define variables for function
    app_data$cluster_identity_sets %>%
      pluck(cluster_identity_set_index) -> cluster_identity_set -> cluster_filter_parameters

    if(!is.null(cluster_identity_set))
      cluster_filter_parameters <- list(`cluster identities`=list(var=cluster_identity_set$var, inputId='cluster_identities'))

    #### make a selection expression
    append(app_data$cell_filter_parameters, cluster_filter_parameters) -> all_filtering_parameters

    if(all_filtering_parameters %>% length() %>% equals(0))
      'TRUE'
    else
      all_filtering_parameters %>% ### TODO: clusters are filtered only when cells are filtered!
        lapply(function(x) 
          sprintf('input$%s', x$inputId) %>%
            parse(text=.) %>%
            eval() %>%
            str_c(collapse='","') %>%
            sprintf(fmt='%s %%in%% c("%s")', x$var, .)) %>%
        str_c(collapse=' & ') %T>%
        log_message(prepend='+++')}) -> formatted_cell_filter



  ## on startup, show reminder to load a dataset
  sendSweetAlert(session=session,
                 title='Getting started',
                 text='Select a dataset to view using the "Datasets" dropdown',
                 type='info')



  ## common elements
  ### background colour of plot panles
  panel_background_rgb <- rgb(red=1, green=1, blue=1, alpha=0)



  ### plotly configurations
  plotly_config <- list(modebar=list())
  list(activecolor='#66965a',
       color='lightgrey',
       bgcolor='transparent') -> plotly_config$modebar



  ## make selected feature scatterplots
  ### 2D ggplot
  output$feature_scatterplot <- renderPlot({
    log_message('(output$feature_scatterplot) making 2d feature scatterplot')

    app_data <- app_data()
    formatted_cell_filter <- formatted_cell_filter()
    input_feature_value_limits <- input_feature_value_limits()
    input_point_size <- input_point_size()
    reduction_coords <- reduction_coords()
    selected_feature <- isolate(selected_feature())
    selected_palette <- selected_palette()
    cluster_identity_set_index <- input_cluster_identity_set_index()

    app_data$cluster_identity_sets %>% pluck(cluster_identity_set_index, 'var') -> cluster_identity_set_var

    metadata <- app_data$metadata
    reduction_coords %<>% pluck('d2')
    feature_values <- selected_feature$values
    feature_name <- selected_feature$name
    limits <- input_feature_value_limits$limits
    palette_package <- selected_palette$package
    picked_palette <- selected_palette$name
    palette_direction <- selected_palette$direction

    feature_values %>% digest() %>% sprintf(fmt='(output$feature_scatterplot) feature_values: %s') %>% log_message()

    get_labels <- function(x) {
      x %>% is.na() %>% not() %>% which() %>% range() -> idx
      x[-idx] <- ''
      x
    }

    if(palette_package=='brewer') {
      colour_gradient <- scale_colour_distiller(palette=picked_palette, direction=palette_direction, labels=get_labels, limits=limits, oob=squish)
    } else if(palette_package=='viridis') {
      colour_gradient <- scale_colour_viridis_c(option=picked_palette, direction=1, labels=get_labels, limits=limits, oob=squish)
    } else {
      colour_gradient <- scale_colour_gradient()
    }

    # check that variables are where they should be
    if(is.null(cluster_identity_set_var) || !is.element(el=cluster_identity_set_var, set=colnames(metadata)))
      return(NULL)

    # make the data.frame and plot
    data.frame(reduction_coords, feature_value=feature_values, metadata) %>%
      mutate_(cluster_id=cluster_identity_set_var) %>%
      mutate_(is_selected=formatted_cell_filter) %>%
      arrange(is_selected, feature_value) %>%
      {ggplot(data=.) +
       aes(x=x, y=y, colour=feature_value, alpha=is_selected) +
       labs(title=sprintf(fmt='%s in cells', feature_name), subtitle=sprintf(fmt='n=%s, N=%s', {sum(.$is_selected) %>% comma()}, {nrow(.) %>% comma()})) +
       geom_point(size=input_point_size) +
       colour_gradient +
       scale_alpha_manual(values=c(`TRUE`=1, `FALSE`=0.05)) +
       guides(color=guide_colourbar(label.position='bottom', frame.colour='black', frame.linewidth=2, ticks.colour='black', ticks.linewidth=2),
              alpha=FALSE) +
       theme_void() +
       theme(aspect.ratio=1,
             legend.box.margin=margin(r=10, b=10, t=0, l=0),
             legend.direction='horizontal',
             legend.justification=c(1,0),
             legend.position=c(1,0),
             legend.title=element_blank(),
             panel.border=element_rect(fill=NA, colour=NA),
             panel.background=element_rect(fill=panel_background_rgb, colour=NA),
             text=element_text(size=14))}}, bg='transparent')



  ### 3D plotly
  output$feature_scatterplot_3d <- renderPlotly({
    log_message('(output$feature_scatterplot_3d) making 3d feature scatterplot')

    app_data <- app_data()
    formatted_cell_filter <- formatted_cell_filter()
    input_feature_value_limits <- input_feature_value_limits()
    reduction_coords <- reduction_coords()
    selected_feature <- isolate(selected_feature())
    selected_palette <- selected_palette()
    input_point_size <- input_point_size()
    cluster_identity_set_index <- input_cluster_identity_set_index()

    app_data$cluster_identity_sets %>% pluck(cluster_identity_set_index, 'var') -> cluster_identity_set_var

    metadata <- app_data$metadata
    reduction_coords %<>% pluck('d3')
    feature_values <- selected_feature$values
    feature_name <- selected_feature$name
    limits <- input_feature_value_limits$limits
    palette_package <- selected_palette$package
    picked_palette <- selected_palette$name
    palette_direction <- selected_palette$direction

    feature_values %>% digest() %>% sprintf(fmt='(output$feature_scatterplot_3d) feature_values: %s') %>% log_message()

    if(palette_package=='brewer') {
      colour_gradient <- brewer_pal(palette=picked_palette, direction=palette_direction)(8)
    } else if(palette_package=='viridis') {
      colour_gradient <- viridis_pal(option=picked_palette, direction=1)(32)
    }
 
    # check that variables are where they should be
    if(is.null(cluster_identity_set_var) || !is.element(el=cluster_identity_set_var, set=colnames(metadata)))
      return(NULL)

    # make the data.frame and plot
    data.frame(reduction_coords, feature_value=feature_values, metadata) %>%
      mutate_(cluster_id=cluster_identity_set_var) %>%
      mutate_(is_selected=formatted_cell_filter) %>%
      mutate(text=sprintf(fmt='Cluster: %s\n%s: %.2f', cluster_id, feature_name, feature_value)) %>%
      mutate(feature_value=squish(x=feature_value, range=limits)) %>%
      arrange(is_selected, feature_value) %>%
      (function(input_data)
        plot_ly(source='event_source') %>%
        event_register('plotly_relayout') %>%
        layout(paper_bgcolor=panel_background_rgb,
               showlegend=FALSE,
               scene=list(xaxis=list(visible=FALSE),
                          yaxis=list(visible=FALSE),
                          zaxis=list(visible=FALSE)),
               modebar=list(orientation='v',
                            activecolor=plotly_config$modebar$activecolor,
                            color=plotly_config$modebar$color,
                            bgcolor=plotly_config$modebar$bgcolor),
               legend=list(orientation='h',
                           xanchor='center',
                           x=0.5),
               hoverlabel=list(bgcolor='white')) %>%
        config(scrollZoom=FALSE,
               displaylogo=FALSE,
               modeBarButtonsToRemove=c('zoom2d', 'tableRotation', 'resetCameraLastSave3d'),
               displayModeBar=TRUE) %>%
        add_markers(data=filter(input_data, is_selected),
                    x=~x, y=~y, z=~z,
                    color=~feature_value, colors=colour_gradient,
                    text=~text,
                    marker=list(symbol='circle-dot',
                                size=input_point_size*2,
                                opacity=1,
                                line=list(width=0)),
                    hoverinfo='text') %>%
        add_markers(data=filter(input_data, !is_selected),
                    x=~x, y=~y, z=~z,
                    color=~feature_value, colors=colour_gradient,
                    text=~text,
                    marker=list(symbol='circle-dot',
                                size=input_point_size*2,
                                opacity=0.05,
                                line=list(width=0)),
                    hoverinfo='text') %>%
        hide_colorbar())})



  ## make cluster identity scatterplots
  ### 2D ggplot
  output$cluster_scatterplot <- renderPlot({
    log_message('(output$cluster_scatterplot) making 2d cluster scatterplot')

    app_data <- app_data()
    reduction_coords <- reduction_coords()
    formatted_cell_filter <- formatted_cell_filter()
    cluster_identity_set_index <- input_cluster_identity_set_index()

    # define variables for function
    metadata <- app_data$metadata
    reduction_coords %<>% pluck('d2')
    input_point_size <- input_point_size()

    app_data$cluster_identity_sets %>% pluck(cluster_identity_set_index, 'var') -> cluster_identity_set_var

    # check that variables are where they should be
    if(is.null(cluster_identity_set_var) || !is.element(el=cluster_identity_set_var, set=colnames(metadata)))
      return(NULL)

    metadata %<>% mutate_(cluster_id=cluster_identity_set_var)
    cluster_idents <- metadata %>% pluck('cluster_id') %>% levels()
    n_clusters <- cluster_idents %>% length()

    # make a colour scale to match the plotly 3D version
    colorRampPalette(brewer.pal(n=8, name='Dark2'))(pmax(8, n_clusters)) %>%
      head(n=n_clusters) %>%
      set_names(cluster_idents) -> colour_scale_values

    # make the data.frame and plot
    data.frame(reduction_coords, metadata) %>%
      mutate_(is_selected=formatted_cell_filter) %>%
      arrange(is_selected, cluster_id) %>%
      {ggplot(data=.) +
       aes(x=x, y=y, colour=cluster_id, alpha=is_selected) +
       labs(title='Cell clusters', subtitle=sprintf(fmt='n=%s, N=%s', {sum(.$is_selected) %>% comma()}, {nrow(.) %>% comma()})) +
       geom_point(size=input_point_size) +
       guides(colour=guide_legend(override.aes=list(size=2))) +
       scale_colour_manual(values=colour_scale_values) +
       scale_alpha_manual(values=c(`TRUE`=1, `FALSE`=0.05)) +
       theme_void() +
       theme(aspect.ratio=1,
             legend.position='none',
             legend.title=element_blank(),
             panel.background=element_rect(fill=panel_background_rgb, colour=NA),
             panel.border=element_rect(fill=NA, colour=NA),
             text=element_text(size=14))}}, bg='transparent')



  ### 3D plotly
  output$cluster_scatterplot_3d <- renderPlotly({
    log_message('(output$cluster_scatterplot_3d) making 3d cluster scatterplot')

    app_data <- app_data()
    reduction_coords <- reduction_coords()
    input_point_size <- input_point_size()
    formatted_cell_filter <- formatted_cell_filter()
    cluster_identity_set_index <- input_cluster_identity_set_index()

    app_data$cluster_identity_sets %>% pluck(cluster_identity_set_index, 'var') -> cluster_identity_set_var

    metadata <- isolate(app_data$metadata)
    reduction_coords %<>% pluck('d3')

    # check that variables are where they should be
    if(is.null(cluster_identity_set_var) || !is.element(el=cluster_identity_set_var, set=colnames(metadata)))
      return(NULL)

    # make the data.frame and plot
    data.frame(reduction_coords, metadata) %>%
      mutate_(cluster_id=cluster_identity_set_var) %>%
      mutate_(is_selected=formatted_cell_filter) %>%
      mutate(text=sprintf(fmt='Cluster: %s', cluster_id)) %>%
      arrange(is_selected, cluster_id) %>%
      (function(input_data)
        plot_ly(source='event_source') %>%
        event_register('plotly_relayout') %>%
        layout(paper_bgcolor=panel_background_rgb,
               showlegend=FALSE,
               scene=list(xaxis=list(visible=FALSE),
                          yaxis=list(visible=FALSE),
                          zaxis=list(visible=FALSE)),
               modebar=list(orientation='v',
                            activecolor=plotly_config$modebar$activecolor,
                            color=plotly_config$modebar$color,
                            bgcolor=plotly_config$modebar$bgcolor),
               legend=list(orientation='h',
                           xanchor='center',
                           x=0.5)) %>%
        config(scrollZoom=FALSE,
               displaylogo=FALSE,
               modeBarButtonsToRemove=c('zoom2d', 'tableRotation', 'resetCameraLastSave3d'),
               displayModeBar=TRUE) %>%
        add_markers(data=filter(input_data, is_selected),
                    x=~x, y=~y, z=~z,
                    color=~cluster_id, colors='Dark2',
                    text=~text,
                    marker=list(symbol='circle-dot',
                                size=input_point_size*2,
                                opacity=1,
                                line=list(width=0)),
                    hoverinfo='text') %>%
        add_markers(data=filter(input_data, !is_selected),
                    x=~x, y=~y, z=~z,
                    color=~cluster_id, colors='Dark2',
                    text=~text,
                    marker=list(symbol='circle-dot',
                                size=input_point_size*2,
                                opacity=0.05,
                                line=list(width=0)),
                    hoverinfo='text'))})



  ## make cluster/feature signal barplot
  output$grouped_feature_values_barplot <- renderPlot({
    log_message('(output$grouped_feature_values_barplot) making feature barplot')

    req(app_data())
    req(selected_feature())

    app_data <- app_data()
    selected_feature <- selected_feature()
    cluster_identity_set_index <- input_cluster_identity_set_index()

    if(list(app_data$metadata, selected_feature$values) %>% sapply(nrow) %>% Reduce(f='!=')) # check that cbind will have equal row number
      return(NULL)

    metadata <- app_data$metadata

    app_data$cluster_identity_sets %>% pluck(cluster_identity_set_index, 'var') -> cluster_identity_set_var

    feature_values <- selected_feature$values
    feature_name <- selected_feature$name
    feature_type <- app_data$feature_names_2_types[[feature_name]] %||% 'any'
    palette_package <- 'brewer'
    picked_palette <- 'YlOrRd'
    palette_direction <- 1

    feature_values %>% digest() %>% sprintf(fmt='(output$grouped_feature_values_barplot) values: %s') %>% log_message()
    sprintf(fmt='(output$grouped_feature_values_barplot) got a feature_type [%s] for feature [%s]', feature_type, feature_name) %>% log_message(prepend='===')

    # TODO: move this function and make is accessible by all
    get_gradient <- function(palette_package, picked_palette, palette_direction=1, range) {
      range %>%
        sign() %>%
        Reduce(f='*') %>%
        equals(-1) -> is_divergent

      if(is_divergent) {
        if(palette_package=='brewer')
          fill_gradient <- scale_fill_distiller(palette=picked_palette, direction=palette_direction,
                                                values={range %>% c(0) %>% sort() %>% rescale()},
                                                limits=range, breaks=0, oob=squish)
        else if(palette_package=='viridis')
          fill_gradient <- scale_fill_viridis(option=picked_palette, n=32, direction=palette_direction,
                                              values={range %>% c(0) %>% sort() %>% rescale()},
                                              limits=range, breaks=0, oob=squish)
        else
          fill_gradient <- scale_fill_gradient()
      } else {
        if(palette_package=='brewer')
          fill_gradient <- scale_fill_distiller(palette=picked_palette, direction=palette_direction, limits=range, oob=squish)
        else if(palette_package=='viridis')
          fill_gradient <- scale_fill_viridis_c(option=picked_palette, n=32, direction=palette_direction, limits=range, oob=squish)
        else
          fill_gradient <- scale_fill_gradient()
      }

      fill_gradient
    }

    # check that variables are where they should be
    if(!is.element(el=cluster_identity_set_var, set=colnames(metadata)))
      return(NULL)

    # make the data.frame and plot
    ### TODO: make this simpler, some parts can be shared surely
    if(feature_type %in% c('module_score')) {
      cbind(metadata, feature_value=feature_values) %>%
        mutate_(cluster_id=cluster_identity_set_var) %>%
        group_by(cluster_id) %>%
        summarise(mean_value=mean(feature_value),
                  median_value=median(feature_value),
                  lower_quartile=quantile(feature_value, p=0.25),
                  upper_quartile=quantile(feature_value, p=0.75)) %>%
        mutate(cluster_id=fct_relevel(cluster_id, rev)) %>%
        {ggplot(data=.) +
         aes(x=cluster_id, fill=mean_value) +
         labs(x='Group ID',
              y='Module score',
              fill='Mean',
              size='Absolute\nmedian',
              title=sprintf(fmt='%s in cell clusters', feature_name)) +
         geom_hline(yintercept=0, colour='grey85', linetype='33', size=rel(0.5)) +
         geom_linerange(mapping=aes(ymin=lower_quartile, ymax=upper_quartile), colour='grey0', linetype='solid', size=1) +
         geom_point(mapping=aes(y=median_value), shape=18, colour='grey0', size=3.5) +
         geom_point(mapping=aes(y=mean_value), shape=21, colour='grey0', stroke=1, size=5) +
         scale_x_discrete(drop=FALSE) +
         scale_y_continuous() +
         get_gradient(palette_package='brewer', picked_palette='Spectral', palette_direction=-1, range=range(.$mean_value)) +
         coord_flip() +
         guides(colour='none', 
                fill=guide_colourbar(order=2, frame.colour='black', frame.linewidth=2, ticks.colour='black', ticks.linewidth=2), 
                size=guide_legend(order=3)) +
         theme_bw() +
         theme(axis.text.y=element_text(face='bold', size=rel(1.5)),
               axis.ticks.x=element_line(size=rel(0.5)),
               axis.ticks.y=element_line(size=rel(3)),
               axis.title.y=element_blank(),
               legend.background=element_blank(),
               legend.key=element_blank(),
               panel.background=element_rect(fill=panel_background_rgb),
               panel.border=element_rect(size=1, colour='black'),
               panel.grid.major.x=element_blank(),
               panel.grid.major.y=element_line(colour='grey85'),
               panel.grid.minor.x=element_blank(),
               panel.grid.minor.y=element_blank(),
               plot.background=element_blank(),
               text=element_text(size=14))} %>%
        ggplotGrob() -> gg
    } else {
      cbind(metadata, feature_value=feature_values) %>%
        mutate_(cluster_id=cluster_identity_set_var) %>%
        add_column(feature_name=feature_name) %>%
        group_by(cluster_id, feature_name) %>%
        mutate(cells_in_group=n()) %>%
        filter(feature_value>0) %>% # only use cells with non-zero expression
        group_by(cells_in_group, add=TRUE) %>%
        summarise(expressing_cells=n(),
                  mean_value=mean(feature_value),
                  median_value=median(feature_value),
                  lower_quartile=quantile(feature_value, 0.25),
                  upper_quartile=quantile(feature_value, 0.75)) %>%
        mutate(cluster_id=fct_relevel(cluster_id, rev)) %>%
        {ggplot(data=.) +
         aes(x=cluster_id, y=expressing_cells/cells_in_group*100, fill=mean_value) +
         labs(x='Group ID',
              y='Detected in cells within cluster',
              fill=sprintf(fmt='%s\n(mean)', feature_name),
              title=sprintf(fmt='%s in cell clusters', feature_name)) +
         geom_point(shape=21, colour='grey0', stroke=1, size=5) +
         scale_x_discrete(drop=FALSE) +
         scale_y_continuous(labels=function(y) str_c(y, '%')) +
         get_gradient(palette_package='brewer', picked_palette='YlOrRd', range=range(.$mean_value)) +
         coord_flip() +
         guides(colour='none', 
                fill=guide_colourbar(order=2, frame.colour='black', frame.linewidth=2, ticks.colour='black', ticks.linewidth=2), 
                size=guide_legend(order=3)) +
         theme_bw() +
         theme(axis.text.y=element_text(face='bold', size=rel(1.5)),
               axis.ticks.x=element_line(size=rel(0.5)),
               axis.ticks.y=element_line(size=rel(3)),
               axis.title.y=element_blank(),
               legend.background=element_blank(),
               legend.key=element_blank(),
               panel.background=element_rect(fill=panel_background_rgb),
               panel.border=element_rect(size=1, colour='black'),
               panel.grid.major.x=element_blank(),
               panel.grid.major.y=element_line(colour='grey85'),
               panel.grid.minor.x=element_blank(),
               panel.grid.minor.y=element_blank(),
               plot.background=element_blank(),
               text=element_text(size=14))} %>%
        ggplotGrob() -> gg
    }

    ### modify the (rotated) y-axis label grobs so the text is coloured to match the cluster_id of the scatterplot(s)
    #### get names of cluster identities (y-axis labels) in plot
    metadata %>% pluck(cluster_identity_set_var) %>% levels() -> cluster_idents
    cluster_idents %>% length() -> n_clusters

    #### get locations of grobs to modify in the gtable
    gg_left_axis <- gg$layout$name %>% str_which('axis-l')
    gg_left_axis_labels <- gg$grobs[[gg_left_axis]]$children %>% sapply(is.gtable) %>% which()
    gg_left_axis_labels_titleGrob <- 1
    gg_left_axis_labels_titleGrob_text <- 1

    #### make a colour scale to match the plotly 3D version
    colorRampPalette(brewer.pal(n=8, name='Dark2'))(pmax(8, n_clusters)) %>%
      head(n=n_clusters) %>%
      set_names(cluster_idents) -> colour_scale_values

    #### get the y-axis labels
    gg$grobs[[gg_left_axis]]$children[[gg_left_axis_labels]]$grobs[[gg_left_axis_labels_titleGrob]]$children[[gg_left_axis_labels_titleGrob_text]]$label -> cluster_ids

    #### lookup colours for y-axis labels
    gg$grobs[[gg_left_axis]]$children[[gg_left_axis_labels]]$grobs[[gg_left_axis_labels_titleGrob]]$children[[gg_left_axis_labels_titleGrob_text]]$gp$col <- colour_scale_values[cluster_ids]

    ### draw the grid
    grid.draw(gg)}, bg='transparent')



  ## tie scenes of 3d plots together
  ### make proxy connections to outputs
  feature_scatterplot_3d.proxy <- plotlyProxy(outputId='feature_scatterplot_3d', session=session, deferUntilFlush=TRUE)
  cluster_scatterplot_3d.proxy <- plotlyProxy(outputId='cluster_scatterplot_3d', session=session, deferUntilFlush=TRUE)

  observe(x={
    input_match_scenes_3d_plotly <- input_match_scenes_3d_plotly()
    if(!input_match_scenes_3d_plotly)
     return(NULL)

    #### get the scene
    plot_scene <- event_data(source='event_source', event='plotly_relayout', session=session)

    #### update both plots
    plotlyProxyInvoke(p=feature_scatterplot_3d.proxy, method='relayout', plot_scene)
    plotlyProxyInvoke(p=cluster_scatterplot_3d.proxy, method='relayout', plot_scene)})
}




# https://github.com/plotly/plotly.js/blob/master/src/plot_api/plot_config.js
