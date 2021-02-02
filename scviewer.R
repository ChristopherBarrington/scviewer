
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
library(scales) # for comma()
library(esquisse) # for palettePicker()
library(waiter)

library(magrittr)
library(tidyverse)

message('/// ----- ----- ----- ----- -----')
str_c('/// started at:', date(), sep=' ') %>% message()
options(warn=-1,
        dplyr.summarise.inform=FALSE,
        scviewer.verbose=FALSE)
shinyOptions(cache = cachem::cache_disk("myapp-cache"))
# load the project description configuration file
app_config <- yaml::read_yaml(file='test_config.yaml')

# define the UI
## header
dashboardHeader(disable=FALSE,
                title=app_config$title,
                titleWidth='1000px') -> ui_header

## sidebar
dashboardSidebar(disable=FALSE,
                 tags$style(type='text/css', '.sidebar-toggle {visibility: hidden !important;}'),
                 tags$style(type='text/css', '.main-header .logo {text-align:left !important;}'),
                 tags$style(type='text/css', '.irs-grid-text {visibility: hidden !important;}'),
                 tags$style(type='text/css', '.autocomplete-items div:hover {background-color: #DDDDDD;}'),
                 selectizeInput(inputId='filename',
                                label='Select a dataset',
                                choices=map_depth(.x=app_config$datasets, .depth=2, .f=pluck, 'file'),
                                options=list(placeholder='Datasets', onInitialize=I('function() { this.setValue(""); }'))),
                 autocomplete_input(id='feature', label='Feature', placeholder='Feature', options='', value=''),
                 sliderInput(inputId='feature_value_limits', label='Feature signal limits', min=0, max=1, step=0.05, value=c(0,0)),
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
                 selectInput(inputId='reduction_method', label='Dimension reduction method', choices=list(PCA='pca', UMAP='umap', tSNE='tsne'), selected='umap'),
                 sliderInput(inputId='point_size', label='Size of cells', min=0.3, max=1.5, step=0.05, value=1.0),
                 prettyCheckboxGroup(inputId='cell_filter', label='Cell filter', choices=NULL, selected=NULL)) -> ui_sidebar

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

  ## get UI inputs
  ### load the dataset file
  app_data <- reactiveValues()
  observe(x={
    req(input$filename)
    message('/// initialising from: ', input$filename)

    selected_feature$name <- NULL
    selected_feature$values <- NULL

    app_data$reduction_2d <- NULL
    app_data$reduction_3d <- NULL

    h5read(file=input$filename, name='metadata_group_id_names') -> group_id_levels
    h5read(file=input$filename, name='metadata_cluster_id_names') -> cluster_id_levels
    h5read(file=input$filename, name='metadata_cell_filter_names') -> cell_filter_levels

    h5read(file=input$filename, name='metadata') %>%
      mutate(cluster_id=factor(cluster_id_levels[cluster_id], levels=cluster_id_levels),
             group_id=factor(group_id_levels[group_id], levels=group_id_levels),
             cell_filter=factor(cell_filter_levels[cell_filter], levels=cell_filter_levels)) -> app_data$metadata
    app_data$h5_file <- input$filename

    all_features <- h5read(file=app_data$h5_file, name='features/names')
    update_autocomplete_input(session=session, id='feature',
                              options=all_features,
                              value=app_config$initial_feature)

    app_data$metadata %>%
      pluck('cell_filter') %>%
      levels() %>%
      updatePrettyCheckboxGroup(session=session, inputId='cell_filter',
                                choices=., selected=.,
                                prettyOptions=list(icon=icon('check-square-o'), status='primary',
                                                   outline=TRUE, animation='jelly', bigger=TRUE, inline=TRUE))})

  observe(x={if(getOption('scviewer.verbose', default=FALSE)) reactiveValuesToList(app_data) %>% lapply(head) %>% print()})

  ### collect the user-specified feature name
  selected_feature <- reactiveValues()
  observe(x={
    req(app_data$h5_file)
    input_feature <- if_else(input$feature=='', app_config$initial_feature, input$feature)
    sprintf('/// reading feature [%s] from: %s', input_feature, app_data$h5_file) %>% message()

    h5_file <- app_data$h5_file
    h5_name <- input_feature %>% str_to_lower() %>% sprintf(fmt='features/values/%s')
    feature_values <- h5read(file=h5_file, name=h5_name)
     
      slider_min <- min(feature_values) %>% subtract(0.05) %>% round(digits=1)
      slider_max <- max(feature_values) %>% add(0.05) %>% round(digits=1)
     
      sprintf(fmt='%s signal limits', input_feature) %>%
        updateSliderInput(session=session, inputId='feature_value_limits')
     
      updateSliderInput(session=session, inputId='feature_value_limits',
                        min=slider_min, max=slider_max,
                        value=c(slider_min, slider_max))

      selected_feature$name <- input_feature
      selected_feature$values <- feature_values})
  
  ### collect the colour scale limits
  input_feature_value_limits <- reactiveValues()
  observeEvent(eventExpr=input$feature_value_limits, ignoreInit=TRUE, handlerExpr={
    req(input$feature_value_limits)
    req(selected_feature$name)
    sprintf('/// setting value limits for %s', isolate(selected_feature$name)) %>% message()

    input_feature_value_limits$min <- input$feature_value_limits[1]
    input_feature_value_limits$max <- input$feature_value_limits[2]
    input_feature_value_limits$limits <- input$feature_value_limits})
 
  ### collect the reduction method
  observe(x={
    req(app_data$h5_file)
    req(input$reduction_method)
    sprintf('/// reading reduction [%s] from: %s', input$reduction_method, app_data$h5_file) %>% message()

    h5_file <- app_data$h5_file
    h5_name_2d <- input$reduction_method %>% sprintf(fmt='reductions/%s') #%T>% print()
    h5_name_3d <- str_c(input$reduction_method, '_3d') %>% sprintf(fmt='reductions/%s') #%T>% print()

    app_data$reduction_2d <- h5read(file=h5_file, name=h5_name_2d)
    app_data$reduction_3d <- h5read(file=h5_file, name=h5_name_3d)})

  ### collect the point size and reduce reactivity
  input_point_size <- reactive(x={
    sprintf('/// set point size [%s]', input$point_size) %>% message()
    input$point_size}) %>%
    debounce(500)

  ### collect the selected colour palette
  selected_palette <- reactiveValues()
  observe(x={
    req(input$predefined_palette)
    sprintf('/// selected palette [%s]', input$predefined_palette) %>% message()
 
    #### split the palette source and name from the string
    input$predefined_palette %>% str_split(pattern=':') %>% pluck(1) -> sp
 
    #### save palette source and name into the reactive values list
    selected_palette$package <- sp[[1]]
    selected_palette$name <- sp[[2]]
    selected_palette$type <- sp[[3]]
    selected_palette$direction <- sp[[3]] %>% switch(f=1, r=-1)})

  ### collect the variable by which cell clusters are coloured
  cluster_variable <- function(...) 'cluster_id'

  ### collect the cell filtering values
  input_cell_filter <- reactive(x={
    sprintf('/// set cell_filter to [%s]', str_c(input$cell_filter, collapse=', ')) %>% message()
    input$cell_filter}) %>%
    debounce(500)

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
    req(app_data$reduction_2d)
    req(input_feature_value_limits$min)
    req(input_feature_value_limits$max)
    req(isolate(selected_feature$values))
    req(isolate(selected_feature$name))
    req(isolate(app_data$metadata))

    if(is.null(selected_palette$package) || (input_feature_value_limits$min==0 && input_feature_value_limits$max==0))
      return(NULL)

    message('/// making 2d feature scatterplot')

    palette_package <- selected_palette$package
    picked_palette <- selected_palette$name
    palette_direction <- selected_palette$direction

    get_labels <- function(x) {
      x %>% is.na() %>% not() %>% which() %>% range() -> idx
      x[-idx] <- ''
      x
    }

    if(palette_package=='brewer') {
      colour_gradient <- scale_colour_distiller(palette=picked_palette, direction=palette_direction, labels=get_labels, limits=input_feature_value_limits$limits, oob=squish)
    } else if(palette_package=='viridis') {
      colour_gradient <- scale_colour_viridis_c(option=picked_palette, direction=1, labels=get_labels, limits=input_feature_value_limits$limits, oob=squish)
    } else {
      colour_gradient <- scale_colour_gradient()
    }

    data.frame(app_data$reduction_2d, feature_value=isolate(selected_feature$values), isolate(app_data$metadata)) %>%
      arrange(feature_value) %>%
      filter(cell_filter %in% input_cell_filter()) %>%
      {ggplot(data=.) +
       aes(x=x, y=y, colour=feature_value) +
       labs(title=sprintf(fmt='%s in cells', isolate(selected_feature$name))) +
       geom_point(size=input_point_size()) +
       annotate(geom='text', x=-Inf, y=-Inf, label={nrow(.) %>% comma() %>% sprintf(fmt='n=%s')}, hjust=-0.1, vjust=-1) +
       colour_gradient +
       guides(color=guide_colourbar(label.position='bottom')) +
       theme_void() +
       theme(legend.box.margin=margin(r=10, b=10, t=0, l=0),
             legend.position=c(1,0),
             legend.justification=c(1,0),
             legend.direction='horizontal',
             legend.title=element_blank(),
             panel.border=element_rect(fill=NA),
             panel.background=element_rect(fill=panel_background_rgb),
             text=element_text(size=14))}}, bg='transparent')

  # ### 3D plotly
  output$feature_scatterplot_3d <- renderPlotly({
    req(app_data$reduction_3d)
    req(app_data$metadata)
    req(input_feature_value_limits$min)
    req(isolate(selected_feature$values))
    req(isolate(selected_feature$name))
    message('/// making 3d feature scatterplot')

    if(is.null(selected_palette$package) || (input_feature_value_limits$min==0 && input_feature_value_limits$max==0))
      return(NULL)
 
    palette_package <- selected_palette$package
    picked_palette <- selected_palette$name
    palette_direction <- selected_palette$direction
 
    if(palette_package=='brewer') {
      colour_gradient <- brewer_pal(palette=picked_palette, direction=palette_direction)(8)
    } else if(palette_package=='viridis') {
      colour_gradient <- viridis_pal(option=picked_palette, direction=1,)
    }
 
    data.frame(app_data$reduction_3d, feature_value=isolate(selected_feature$values), app_data$metadata) %>%
      mutate(text=sprintf(fmt='Cluster: %s\nGroup: %s\n%s: %.2f', cluster_id, group_id, selected_feature$name, feature_value)) %>%
      mutate(feature_value=squish(x=feature_value, range=input_feature_value_limits$limits)) %>%
      arrange(feature_value) %>%
      filter(cell_filter %in% input_cell_filter()) %>%
      plot_ly() %>%
      layout(paper_bgcolor=panel_background_rgb,
             scene=list(xaxis=list(visible=FALSE),
                        yaxis=list(visible=FALSE),
                        zaxis=list(visible=FALSE)),
             modebar=list(orientation='v',
                          activecolor=plotly_config$modebar$activecolor,
                          color=plotly_config$modebar$color,
                          bgcolor=plotly_config$modebar$bgcolor)) %>%
      config(scrollZoom=FALSE,
             displaylogo=FALSE,
             modeBarButtonsToRemove=c('zoom2d', 'tableRotation', 'resetCameraLastSave3d'),
             displayModeBar=TRUE) %>%
      add_markers(x=~x, y=~y, z=~z,
                  color=~feature_value,
                  text=~text,
                  colors=colour_gradient,
                  marker=list(symbol='circle-dot',
                              size=input_point_size()*2,
                              line=list(width=0)),
                  hoverinfo='text') %>%
      # colorbar(title=input_feature(),
      #          yanchor='center', y=0.5)
      hide_colorbar()})
 
  ## make cluster identity scatterplots
  ### 2D ggplot
  output$cluster_scatterplot <- renderPlot({
    req(app_data$reduction_2d)
    req(app_data$metadata)
    message('/// making 2d cluster scatterplot')

    cell_colour_variable <- cluster_variable()
    n_clusters <- app_data$metadata %>% pluck(cell_colour_variable) %>% levels() %>% length()

    data.frame(app_data$reduction_2d, app_data$metadata) %>%
      rename(.id=cell_colour_variable) %>%
      arrange(.id) %>%
      # filter(cell_filter %in% input_cell_filter()) %>%
      {ggplot(data=.) +
       aes(x=x, y=y, colour=.id) +
       labs(title='Cell clusters') +
       geom_point(size=input_point_size()) +
       annotate(geom='text', x=-Inf, y=-Inf, label={nrow(.) %>% comma() %>% sprintf(fmt='n=%s')}, hjust=-0.1, vjust=-1) +
       scale_colour_manual(values={colorRampPalette(brewer.pal(n=8, name='Set2'))(n_clusters)}) +
       theme_void() +
       theme(legend.position='none',
             panel.border=element_rect(fill=NA),
             panel.background=element_rect(fill=panel_background_rgb),
             text=element_text(size=14))}}, bg='transparent')

  ### 3D plotly
  output$cluster_scatterplot_3d <- renderPlotly({
    req(app_data$reduction_3d)
    req(app_data$metadata)
    message('/// making 3d cluster scatterplot')

    cell_colour_variable <- cluster_variable()

    data.frame(app_data$reduction_3d, app_data$metadata) %>%
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
    req(app_data$metadata)
    req(selected_feature$values)
    req(selected_feature$name)
    message('/// making feature barplot')

    palette_package <- 'brewer'
    picked_palette <- 'YlOrRd'
    palette_direction <- 1
    
    if(palette_package=='brewer') {
      fill_gradient <- scale_fill_distiller(palette=picked_palette, direction=palette_direction)
    } else if(palette_package=='viridis') {
      fill_gradient <- scale_fill_viridis_c(option=picked_palette, direction=1)
    } else {
      fill_gradient <- scale_fill_gradient()
    }

    cbind(app_data$metadata, feature_value=selected_feature$values) %>%
      # filter(cell_filter %in% input_cell_filter()) %>%
      add_column(feature_name=selected_feature$name) %>%
      group_by(group_id, feature_name) %>%
      mutate(cells_in_group=n()) %>%
      filter(feature_value>0) %>% # only use cells with non-zero expression
      group_by(cells_in_group, add=TRUE) %>%
      summarise(expressing_cells=n(),
                mean_value=mean(feature_value),
                median_value=median(feature_value),
                from_value=quantile(feature_value, 0.25),
                to_value=quantile(feature_value, 0.75)) %>%
      filter(group_id!='NULL' & !str_detect(group_id, 'Null')) %>%
      mutate(group_id=fct_relevel(group_id, rev)) %>%
      ggplot() +
      aes(x=group_id, fill=mean_value, colour=group_id) +
      labs(x='Cell types', y=sprintf(fmt='Median %s signal ± quartile', selected_feature$name),
           fill=sprintf(fmt='Mean signal', selected_feature$name),
           size='Reported by\ncells in group', colour='Cell type',
           title=sprintf(fmt='%s in cell groups', selected_feature$name)) +
      geom_linerange(mapping=aes(ymin=from_value, ymax=to_value), size=1.4, colour='grey30') +
      geom_point(mapping=aes(y=median_value, size=expressing_cells/cells_in_group*100), shape=21, colour='grey0', stroke=1) +
      scale_x_discrete() +
      scale_y_continuous() +
      scale_size_continuous(labels=function(l) str_c(l, '%'), range=c(2,8)) +
      fill_gradient +
      coord_flip() +
      guides(colour='none', 
             fill=guide_colourbar(order=2), 
             size=guide_legend(order=3)) +
      theme_bw() +
      theme(axis.title.y=element_blank(),
            legend.background=element_blank(),
            legend.key=element_blank(),
            panel.background=element_rect(fill=panel_background_rgb),
            panel.grid.major.x=element_blank(),
            plot.background=element_blank(),
            text=element_text(size=14))}, bg='transparent')
}

# start the app
shinyApp(ui, server)


# https://github.com/plotly/plotly.js/blob/master/src/plot_api/plot_config.js
