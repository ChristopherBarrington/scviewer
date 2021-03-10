
#                                       _                                    
#      o O O   ___     __     __ __    (_)     ___   __ __ __  ___      _ _  
#     o       (_-<    / _|    \ V /    | |    / -_)  \ V  V / / -_)    | '_| 
#    TS__[O]  /__/_   \__|_   _\_/_   _|_|_   \___|   \_/\_/  \___|   _|_|_  
#   {======|_|"""""|_|"""""|_|"""""|_|"""""|_|"""""|_|"""""|_|"""""|_|"""""| 
#  ./o--000'"`-0-0-'"`-0-0-'"`-0-0-'"`-0-0-'"`-0-0-'"`-0-0-'"`-0-0-'"`-0-0-' 
# ===========================================================================
#

.libPaths(c(.libPaths(), 'lib'))

library(rhdf5)
library(shiny)
library(dqshiny)
library(shinyWidgets)
library(shinydashboard)
library(dashboardthemes)
library(plotly)
library(RColorBrewer)
library(scales) # for comma()
library(esquisse) # for palettePicker()
library(waiter)

library(magrittr)
library(tidyverse)

if(packageVersion('dqshiny') < '0.0.5')
    packageVersion('dqshiny') %>% sprintf(fmt='!!! dqshiny version 0.0.5 is required but %s was loaded!') %>% stop()

message('/// ----- ----- ----- ----- -----')
str_c('/// started at:', date(), sep=' ') %>% message()
options(warn=-1,
        dplyr.summarise.inform=FALSE,
        scviewer.verbose=FALSE)

# load the project description configuration file
app_config <- yaml::read_yaml(file='config.yaml')

## define the dataset choices from the config file; make a nested list of L1/L2 with L1$L2 as the value
map_depth(.x=app_config$datasets, .depth=2, .f=pluck, 'file') %>%
  map_depth(.depth=1, function(x) x[names(x)!='config']) %>%
  plyr::ldply(function(x) data.frame(name=names(x))) %>%
  unite('key', .id, name, sep='$', remove=FALSE) %>%
  mutate_if(is.factor, as.character) %>%
  plyr::dlply(~.id, select, name, key) %>%
  plyr::llply(deframe) -> dataset_choices

# define the UI
## header
dashboardHeader(disable=FALSE,
                title=app_config$title,
                titleWidth='1000px',
                tags$li(a(onclick='history.go(-1); return false;', href=NULL,
                          icon(name='home', lib='font-awesome'), title='Back', style='cursor: pointer; font-size: larger;'),
                        class='dropdown')) -> ui_header

## sidebar
dashboardSidebar(disable=FALSE,
                 tags$script("$(document).on('shiny:busy', function() {
                   var currentdate = new Date(); 
                   var msg = '+++ shiny:busy: ' + currentdate.getHours().toString().padStart(2, '0') + ':' + currentdate.getMinutes().toString().padStart(2, '0') + ':' + currentdate.getSeconds().toString().padStart(2, '0');
                   console.log(msg);});"),
                 tags$script("$(document).on('shiny:idle', function() {
                   var currentdate = new Date(); 
                   var msg = '--- shiny:idle: ' + currentdate.getHours().toString().padStart(2, '0') + ':' + currentdate.getMinutes().toString().padStart(2, '0') + ':' + currentdate.getSeconds().toString().padStart(2, '0');
                   console.log(msg);});"),
                 tags$style(type='text/css', '.sidebar-toggle {visibility: hidden !important;}'),
                 tags$style(type='text/css', '.main-header .logo {text-align:left !important;}'),
                 tags$style(type='text/css', '.irs-grid-text {visibility: hidden !important;}'),
                 tags$style(type='text/css', '.autocomplete-items div:hover {background-color: #DDDDDD;}'),
                 selectizeInput(inputId='filename',
                                label='Select a dataset',
                                choices=dataset_choices,
                                options=list(placeholder='Datasets', onInitialize=I('function() { this.setValue(""); }'))),
                 autocomplete_input(id='feature', label='Feature', placeholder='Feature', options='', value=''),
                 sliderInput(inputId='feature_value_limits', label='Feature signal limits', min=0, max=1, step=0.05, value=c(0,0)),
                 selectInput(inputId='reduction_method', label='Dimension reduction method', choices=list(PCA='pca', UMAP='umap', tSNE='tsne'), selected='umap'),
                 prettyCheckboxGroup(inputId='cell_filter', label='Cell filter', choices='No filtering', selected='No filtering'),
                 {list(`Brewer [sequential]`=list(`brewer:Blues:f`=brewer_pal(palette='Blues', direction=1)(8),
                                                  `brewer:BuPu:f`=brewer_pal(palette='BuPu', direction=1)(8),
                                                  `brewer:GnBu:f`=brewer_pal(palette='GnBu', direction=1)(8),
                                                  `brewer:Greens:f`=brewer_pal(palette='Greens', direction=1)(8),
                                                  `brewer:Greys:f`=brewer_pal(palette='Greys', direction=1)(8),
                                                  `brewer:Oranges:f`=brewer_pal(palette='Oranges', direction=1)(8),
                                                  `brewer:Purples:f`=brewer_pal(palette='Purples', direction=1)(8),
                                                  `brewer:RdPu:f`=brewer_pal(palette='RdPu', direction=1)(8),
                                                  `brewer:Reds:f`=brewer_pal(palette='Reds', direction=1)(8),
                                                  `brewer:YlGn:f`=brewer_pal(palette='YlGn', direction=1)(8),
                                                  `brewer:YlGnBu:f`=brewer_pal(palette='YlGnBu', direction=1)(8)),
                       `Viridis [sequential]`=list(`viridis:magma:f`=viridis_pal(option='magma', direction=1)(8),
                                                   `viridis:plasma:f`=viridis_pal(option='plasma', direction=1)(8),
                                                   `viridis:inferno:f`=viridis_pal(option='inferno', direction=1)(8),
                                                   `viridis:viridis:f`=viridis_pal(option='viridis', direction=1)(8)),
                       `Brewer [divergent]`=list(`brewer:RdYlGn:r`=brewer_pal(palette='RdYlGn', direction=-1)(8),
                                                 `brewer:PRGn:r`=brewer_pal(palette='PRGn', direction=-1)(8),
                                                 `brewer:RdYlBu:r`=brewer_pal(palette='RdYlBu', direction=-1)(8),
                                                 `brewer:RdGy:r`=brewer_pal(palette='RdGy', direction=-1)(8),
                                                 `brewer:RdBu:r`=brewer_pal(palette='RdBu', direction=-1)(8),
                                                 `brewer:PuOr:r`=brewer_pal(palette='PuOr', direction=-1)(8),
                                                 `brewer:PiYG:r`=brewer_pal(palette='PiYG', direction=-1)(8),
                                                 `brewer:BrBG:r`=brewer_pal(palette='BrBG', direction=-1)(8),
                                                 `brewer:Spectral:r`=brewer_pal(palette='Spectral', direction=-1)(8))) %>%
                 palettePicker(inputId='predefined_palette', label='Colour palette', 
                               selected='brewer:YlGnBu:f', textColor=rgb(red=0, green=0, blue=0, alpha=0),
                               pickerOpts=list(`live-search`=FALSE, size=10))},
                 sliderInput(inputId='point_size', label='Size of cells', min=0.3, max=1.5, step=0.05, value=1.0)) -> ui_sidebar

## main body, plots
dashboardBody(shinyDashboardThemes(theme='grey_light'),
              fillPage(use_waiter(), waiter_on_busy(html=tagList(spin_atebits()), color=rgb(red=1, green=1, blue=1, alpha=0.5)),
                       tags$style(type='text/css', '#cluster_scatterplot {width: 100% !important; height: calc(50vh - 40px) !important;}'),
                       tags$style(type='text/css', '#cluster_scatterplot_3d {width: 100% !important; height: calc(50vh - 40px) !important;}'),
                       tags$style(type='text/css', '#feature_scatterplot {width: 100% !important; height: calc(50vh - 40px) !important;}'),
                       tags$style(type='text/css', '#feature_scatterplot_3d {width: 100% !important; height: calc(50vh - 40px) !important;}'),
                       tags$style(type='text/css', '#grouped_feature_values_barplot {width: 100% !important; height: calc(100vh - 80px) !important;}'),
                       fillRow(column(width=12,
                                      plotOutput('cluster_scatterplot', height='100%'),
                                      plotlyOutput('cluster_scatterplot_3d', height='100%')),
                               column(width=12,
                                      plotOutput('feature_scatterplot', height='100%'),
                                      plotlyOutput('feature_scatterplot_3d', height='100%')),
                               plotOutput('grouped_feature_values_barplot', height='100%')))) -> ui_body

## bring elements together into the app ui
dashboardPage(header=ui_header, sidebar=ui_sidebar, body=ui_body, title=NULL) -> ui

# define the server
server <- function(input, output, session) {
  ## helper functions
  get_config_values <- function(config, key)
    reshape2::melt(config) %>%
      filter_at(vars(-value), any_vars(.==key)) %>%
      select_if(~all(.!=key | is.na(.))) %>%
      unite(index, L2, L3, sep='$') %>%
      spread(key=index, value=value) %>%
      rename_at(vars(matches('^NA\\$NA$')), function(x) 'default') %>%
      purrr::set_names(str_remove, pattern='\\$config') %>%
      mutate_all(as.character)

  get_prioritised_value <- function(values, priority)
    priority %>%
      sapply(pluck, .x=values, .default=NA, simplify=TRUE) %>%
      na.omit() %>%
      head(n=1) %>%
      unname()

  log_message <- function(text, prepend='///')
    str_c(prepend, {Sys.time() %>% format('%H:%M:%S')}, text, sep=' ') %>%
      message()

  ## get UI inputs
  ### load the dataset file
  initialised <- reactiveValues()
  reactive(x={
    req(input$filename)

    input_dataset_key <- input$filename

    sprintf(fmt='(app_data) initialising from: %s', input_dataset_key) %>% log_message()

    #### parse dropdown key to give levels 1 and 2 from the datasets key of the yaml config
    input_dataset_key %>%
      stringr::str_split('\\$') %>%
      purrr::pluck(1) %>%
      purrr::set_names('L1','L2') %>%
      as.list() -> dataset_selection

    #### get the h5 file from the config file
    app_config %>%
      pluck('datasets', dataset_selection$L1, dataset_selection$L2, 'file') %T>%
      (. %>% sprintf(fmt='(app_data) h5_file: %s') %>% log_message(prepend='+++')) -> h5_file

    #### get the initial feature from the config file
    get_config_values(app_config, 'initial_feature') %>%
      get_prioritised_value(priority=c(input_dataset_key, dataset_selection$L1, 'default')) %T>%
      (. %>% sprintf(fmt='(app_data) initial_feature: %s') %>% log_message(prepend='+++')) -> initial_feature

    #### get and evaluate any group bands
    #### this is a vector of the number of rows in each band
    #### the yaml should be a comma-separated string
    # get_config_values(app_config, 'group_bands') %>%
    #   mutate(default='') %>%
    #   get_prioritised_value(priority=c(input_dataset_key, dataset_selection$L1, 'default')) %T>%
    #   (. %>% sprintf(fmt='(app_data) group_bands: %s') %>% log_message(prepend='+++')) %>%
    #   sprintf(fmt='c(%s)') %>%
    #   parse(text=.) %>%
    #   eval() -> group_bands

    #### load metadata table
    metadata_list <- h5read(file=h5_file, name='metadata')
    if(is.null(metadata_list$data$cell_filter))
      metadata_list$data %<>% add_column(cell_filter=factor('No filtering'))

    ##### for each would-be-factor variable in metadata$data, update the factor levels using metadata$factor_levels
    for(i in names(metadata_list$factor_levels))
      metadata_list$data %<>%
        mutate(across(.cols=i, function(x) factor(metadata_list$factor_levels[[i]][x], levels=metadata_list$factor_levels[[i]])))

    #### pull out the list of reductions
    reductions <- h5read(file=h5_file, name='reductions')

    #### update UI elements
    ##### features dropdown
    all_features <- h5read(file=h5_file, name='features/names')
    update_autocomplete_input(session=session, id='feature',
                              options=all_features,
                              value=initial_feature)

    ##### cell filter selection
    metadata_list$data %>%
      pluck('cell_filter') %>%
      levels() %>%
      replace(is.null(.), 'no filtering') %>%
      updatePrettyCheckboxGroup(session=session, inputId='cell_filter',
                                choices=., selected=.,
                                prettyOptions=list(icon=icon('check-square-o'), status='primary',
                                                   outline=TRUE, animation='jelly', bigger=TRUE, inline=TRUE))

    #### add to reactive values list
    list(initial_feature=initial_feature,
         reductions=reductions,
         metadata=metadata_list$data,
         # group_bands=group_bands,
         h5_file=h5_file,
         dataset_key=input_dataset_key)}) -> app_data

  observe(x={if(getOption('scviewer.verbose', default=FALSE)) reactiveValuesToList(app_data) %>% lapply(head) %>% print()})

  ### collect the user-specified feature name
  reactive(x={
    app_data <- app_data()

    input_feature <- input$feature
    initial_feature <- app_data$initial_feature
    h5_file <- app_data$h5_file

    input_feature <- if_else(input_feature=='', initial_feature, input_feature)

    sprintf('(selected_feature) reading feature [%s] from: %s', input_feature, h5_file) %>% log_message()

    h5_name <- input_feature %>% str_to_lower() %>% sprintf(fmt='features/values/%s')
    feature_values <- h5read(file=h5_file, name=h5_name)
     
    slider_min <- min(feature_values) %>% subtract(0.05) %>% round(digits=1)
    slider_max <- max(feature_values) %>% add(0.05) %>% round(digits=1)

    sprintf('(selected_feature) setting value limits for %s to [%s]', input_feature, str_c(slider_min, slider_max, sep=',')) %>% log_message()
   
    # sprintf(fmt='%s signal limits', input_feature) %>%
    #   updateSliderInput(session=session, inputId='feature_value_limits')
   
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

    name_2d <- reduction_method
    name_3d <- str_c(reduction_method, '_3d')

    reductions[c(name_2d, name_3d)] %>%
      purrr::set_names(c('d2','d3'))}) -> reduction_coords

  ### collect the point size and reduce reactivity
  reactive(x={
    input$point_size %T>%
      (. %>% sprintf(fmt='(input_point_size) set point_size [%s]') %>% log_message())}) %>%
    debounce(500) -> input_point_size

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

  ### (pretend to) collect the variable by which cell clusters are coloured
  cluster_variable <- function(...) 'cluster_id'

  ### collect the cell filtering values
  reactive(x={
    req(input$cell_filter)

    input$cell_filter %T>%
      (. %>% str_c(collapse=', ') %>% sprintf(fmt='(input_cell_filter) set cell_filter to [%s]') %>% log_message())}) %>%
    debounce(500) -> input_cell_filter

  ## on startup, show reminder to load a dataset
  sendSweetAlert(
    session=session,
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
    input_cell_filter <- input_cell_filter()
    input_feature_value_limits <- input_feature_value_limits()
    input_point_size <- input_point_size()
    reduction_coords <- reduction_coords()
    selected_feature <- isolate(selected_feature())
    selected_palette <- selected_palette()

    metadata <- app_data$metadata
    reduction_coords %<>% pluck('d2')
    feature_values <- selected_feature$values
    feature_name <- selected_feature$name
    limits <- input_feature_value_limits$limits
    palette_package <- selected_palette$package
    picked_palette <- selected_palette$name
    palette_direction <- selected_palette$direction

    feature_values %>% digest::digest() %>% sprintf(fmt='(output$feature_scatterplot) feature_values: %s') %>% log_message()

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

    data.frame(reduction_coords, feature_value=feature_values, metadata) %>%
      arrange(feature_value) %>%
      filter(cell_filter %in% input_cell_filter) %>%
      {ggplot(data=.) +
       aes(x=x, y=y, colour=feature_value) +
       labs(title=sprintf(fmt='%s in cells', feature_name), subtitle={nrow(.) %>% comma() %>% sprintf(fmt='n=%s')}) +
       geom_point(size=input_point_size) +
       colour_gradient +
       guides(color=guide_colourbar(label.position='bottom', frame.colour='black', frame.linewidth=2, ticks.colour='black', ticks.linewidth=2)) +
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

  # ### 3D plotly
  output$feature_scatterplot_3d <- renderPlotly({
    log_message('(output$feature_scatterplot_3d) making 3d feature scatterplot')

    app_data <- app_data()
    input_cell_filter <- input_cell_filter()
    input_feature_value_limits <- input_feature_value_limits()
    reduction_coords <- reduction_coords()
    selected_feature <- isolate(selected_feature())
    selected_palette <- selected_palette()

    metadata <- app_data$metadata
    reduction_coords %<>% pluck('d3')
    feature_values <- selected_feature$values
    feature_name <- selected_feature$name
    limits <- input_feature_value_limits$limits
    palette_package <- selected_palette$package
    picked_palette <- selected_palette$name
    palette_direction <- selected_palette$direction

    feature_values %>% digest::digest() %>% sprintf(fmt='(output$feature_scatterplot_3d) feature_values: %s') %>% log_message()

    if(palette_package=='brewer') {
      colour_gradient <- brewer_pal(palette=picked_palette, direction=palette_direction)(8)
    } else if(palette_package=='viridis') {
      colour_gradient <- viridis_pal(option=picked_palette, direction=1)(32)
    }
 
    data.frame(reduction_coords, feature_value=feature_values, metadata) %>%
      mutate(text=sprintf(fmt='Cluster: %s\nGroup: %s\n%s: %.2f', cluster_id, group_id, feature_name, feature_value)) %>%
      mutate(feature_value=squish(x=feature_value, range=limits)) %>%
      arrange(feature_value) %>%
      filter(cell_filter %in% input_cell_filter) %>%
      plot_ly() %>%
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
      add_markers(x=~x, y=~y, z=~z,
                  color=~feature_value, colors=colour_gradient,
                  text=~text,
                  marker=list(symbol='circle-dot',
                              size=input_point_size()*2,
                              line=list(width=0)),
                  hoverinfo='text') %>%
      hide_colorbar()})

  ## make cluster identity scatterplots
  ### 2D ggplot
  output$cluster_scatterplot <- renderPlot({
    log_message('(output$cluster_scatterplot) making 2d cluster scatterplot')

    app_data <- app_data()
    reduction_coords <- reduction_coords()

    metadata <- app_data$metadata
    reduction_coords %<>% pluck('d2')
    cell_colour_variable <- cluster_variable()
    n_clusters <- metadata %>% pluck(cell_colour_variable) %>% levels() %>% length()

    data.frame(reduction_coords, metadata) %>%
      rename(.id=cell_colour_variable) %>%
      arrange(.id) %>%
      # filter(cell_filter %in% input_cell_filter()) %>%
      {ggplot(data=.) +
       aes(x=x, y=y, colour=.id) +
       labs(title='Cell clusters', subtitle={nrow(.) %>% comma() %>% sprintf(fmt='n=%s')}) +
       geom_point(size=input_point_size()) +
       scale_colour_manual(values={colorRampPalette(brewer.pal(n=8, name='Set2'))(n_clusters)}) +
       theme_void() +
       theme(aspect.ratio=1,
             legend.position='none',
             panel.background=element_rect(fill=panel_background_rgb, colour=NA),
             panel.border=element_rect(fill=NA, colour=NA),
             text=element_text(size=14))}}, bg='transparent')

  ### 3D plotly
  output$cluster_scatterplot_3d <- renderPlotly({
    log_message('(output$cluster_scatterplot_3d) making 3d cluster scatterplot')

    app_data <- app_data()
    reduction_coords <- reduction_coords()

    metadata <- isolate(app_data$metadata)
    reduction_coords %<>% pluck('d3')
    cell_colour_variable <- cluster_variable()

    data.frame(reduction_coords, metadata) %>%
      mutate(text=sprintf(fmt='Cluster: %s\nGroup: %s', cluster_id, group_id)) %>%
      rename(.id=cell_colour_variable) %>%
      arrange(.id) %>%
      # filter(cell_filter %in% input_cell_filter()) %>%
      plot_ly() %>%
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
      add_markers(x=~x, y=~y, z=~z,
                  color=~.id, colors='Set2',
                  text=~text,
                  marker=list(symbol='circle-dot',
                              size=input_point_size()*2,
                              line=list(width=0)),
                  hoverinfo='text')})

  ## make cluster/feature signal barplot
  output$grouped_feature_values_barplot <- renderPlot({
    log_message('(output$grouped_feature_values_barplot) making feature barplot')

    app_data <- app_data()
    selected_feature <- selected_feature()

    if(list(app_data$metadata, selected_feature$values) %>% sapply(nrow) %>% Reduce(f='!=')) # check that cbind will have equal row number
      return(NULL)

    metadata <- app_data$metadata
    # group_bands <- app_data$group_bands
    feature_values <- selected_feature$values
    feature_name <- selected_feature$name
    palette_package <- 'brewer'
    picked_palette <- 'YlOrRd'
    palette_direction <- 1

    feature_values %>% digest::digest() %>% sprintf(fmt='(output$grouped_feature_values_barplot) values: %s') %>% log_message()
    
    if(palette_package=='brewer') {
      fill_gradient <- scale_fill_distiller(palette=picked_palette, direction=palette_direction)
    } else if(palette_package=='viridis') {
      fill_gradient <- scale_fill_viridis_c(option=picked_palette, direction=1)
    } else {
      fill_gradient <- scale_fill_gradient()
    }

    ### collect positions of bands to group (cell types)
    ### disabled, use geom_rect in plot
    # group_bands %>%
    #   c(0.5, .) %>%
    #   cumsum() %>%
    #   (function(x) data.frame(xmin=head(x, n=-1), xmax=tail(x, n=-1))) %>%
    #   mutate(colour=rep(c('white','grey95'), length.out=n())) %>%
    #   rbind(.[NA,]) -> group_band_positions

    cbind(metadata, feature_value=feature_values) %>%
      # filter(cell_filter %in% input_cell_filter()) %>%
      add_column(feature_name=feature_name) %>%
      group_by(group_id, feature_name) %>%
      mutate(cells_in_group=n()) %>%
      filter(feature_value>0) %>% # only use cells with non-zero expression
      group_by(cells_in_group, add=TRUE) %>%
      summarise(expressing_cells=n(),
                mean_value=mean(feature_value),
                median_value=median(feature_value),
                from_value=quantile(feature_value, 0.25),
                to_value=quantile(feature_value, 0.75)) %>%
      mutate(group_id=fct_relevel(group_id, rev)) %>%
      {ggplot(data=.) +
       aes(x=group_id, y=expressing_cells/cells_in_group*100, fill=mean_value) +
       labs(x='Cell types',
            y='Detected in cells within type',
            fill=sprintf(fmt='%s\n(mean)', feature_name),
            title=sprintf(fmt='%s in cell type', feature_name)) +
       # geom_rect(data=group_band_positions, mapping=aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf), fill=group_band_positions$colour, inherit.aes=FALSE) +
       # geom_vline(xintercept={levels(.$group_id) %>% length() %>% seq()}, colour='grey60', size=0.5) +
       geom_point(shape=21, colour='grey0', stroke=1, size=5) +
       scale_x_discrete(drop=FALSE) +
       scale_y_continuous(labels=function(y) str_c(y, '%')) +
       fill_gradient +
       coord_flip() +
       guides(colour='none', 
              fill=guide_colourbar(order=2, frame.colour='black', frame.linewidth=2, ticks.colour='black', ticks.linewidth=2), 
              size=guide_legend(order=3)) +
       theme_bw() +
       theme(axis.ticks=element_line(size=1),
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
             text=element_text(size=14))}}, bg='transparent')
}

# start the app
shinyApp(ui, server)

# https://github.com/plotly/plotly.js/blob/master/src/plot_api/plot_config.js
