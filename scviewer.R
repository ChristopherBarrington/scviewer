.libPaths(c(.libPaths(), 'lib'))

library(shiny)
library(htmlwidgets)
library(shinydashboard)
library(dashboardthemes)
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(scales)
library(esquisse)
library(gtools)
library(shinyWidgets)
library(waiter)
library(magrittr)
library(tidyverse)

message('/// ----- ----- ----- ----- -----')
str_c('/// started at:', date(), sep=' ') %>% message()

options(warn=-1,
        dplyr.summarise.inform=FALSE)

# define the loading object screen
reading_rds_screen <- tagList(
  spin_atebits(),
  h5('Reading data into session')) 

redrawing_plot <- tagList(
  spin_3k())

# load the project description configuration file
app_config <- yaml::read_yaml(file='config.yaml')

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
                 selectizeInput(inputId='rds_to_load',
                                label='Select a dataset',
                                choices={app_config$datasets %>% map_depth(2, pluck, 'rds_file')},
                                options=list(placeholder='Datasets', onInitialize=I('function() { this.setValue(""); }'))),
                 textInput(inputId='feature', label='Feature', value=app_config$initial_feature, placeholder='Feature'),
                 sliderInput(inputId='feature_value_limits', label='Feature signal limits', min=-1, max=0, step=0.05, value=c(-1,0)),
                 {list(`Brewer [sequential]`=list(`brewer:Blues`=brewer_pal(palette='Blues')(8),
                                                  `brewer:BuPu`=brewer_pal(palette='BuPu')(8),
                                                  `brewer:GnBu`=brewer_pal(palette='GnBu')(8),
                                                  `brewer:Greens`=brewer_pal(palette='Greens')(8),
                                                  `brewer:Greys`=brewer_pal(palette='Greys')(8),
                                                  `brewer:Oranges`=brewer_pal(palette='Oranges')(8),
                                                  `brewer:Purples`=brewer_pal(palette='Purples')(8),
                                                  `brewer:RdPu`=brewer_pal(palette='RdPu')(8),
                                                  `brewer:Reds`=brewer_pal(palette='Reds')(8),
                                                  `brewer:YlGn`=brewer_pal(palette='YlGn')(8),
                                                  `brewer:YlGnBu`=brewer_pal(palette='YlGnBu')(8)),
                       `Viridis [sequential]`=list(`viridis:magma`=viridis_pal(option='magma', direction=1)(8),
                                                   `viridis:plasma`=viridis_pal(option='plasma', direction=1)(8),
                                                   `viridis:inferno`=viridis_pal(option='inferno', direction=1)(8),
                                                   `viridis:viridis`=viridis_pal(option='viridis', direction=1)(8))) %>%
                     palettePicker(inputId='predefined_palette', label='Colour palette', 
                                   selected='brewer:YlGnBu', textColor=rgb(red=0, green=0, blue=0, alpha=0),
                                   pickerOpts=list(`live-search`=FALSE, size=10))},
                 selectInput(inputId='reduction_method', label='Dimension reduction method', choices=list(PCA='pca', UMAP='umap', tSNE='tsne'), selected='umap'),
                 sliderInput(inputId='point_size', label='Size of cells', min=0.3, max=1.5, step=0.05, value=1.0)) -> ui_sidebar

## main body, plots
dashboardBody(shinyDashboardThemes(theme='grey_light'),
              fillPage(use_waiter(),
                       tags$style(type='text/css', '#cluster_scatterplot {width: 100% !important; height: calc(50vh - 40px) !important;}'),
                       tags$style(type='text/css', '#cluster_scatterplot_3d {width: 100% !important; height: calc(50vh - 40px) !important;}'),
                       tags$style(type='text/css', '#feature_scatterplot {width: 100% !important; height: calc(50vh - 40px) !important;}'),
                       tags$style(type='text/css', '#feature_scatterplot_3d {width: 100% !important; height: calc(50vh - 40px) !important;}'),
                       tags$style(type='text/css', '#cluster_feature_barplot {width: 100% !important; height: calc(100vh - 80px) !important;}'),
                       fillRow(column(width=12,
                                      plotOutput('cluster_scatterplot', height='100%'),
                                      plotlyOutput('cluster_scatterplot_3d', height='100%')),
                               column(width=12,
                                      plotOutput('feature_scatterplot', height='100%'),
                                      plotlyOutput('feature_scatterplot_3d', height='100%')),
                               plotOutput('cluster_feature_barplot', height='100%')))) -> ui_body

## bring elements together into the app ui
dashboardPage(header=ui_header, sidebar=ui_sidebar, body=ui_body, title=NULL) -> ui

# define the server
server <- function(input, output, session) {
  ## get UI inputs
  ### load the dataset rds file
  input_object <- reactive({
    req(input$rds_to_load)

    #### cover the screen with the loading rds waiter
    waiter_show(html=reading_rds_screen, color=rgb(red=1, green=1, blue=1, alpha=0.5))

    #### load the specified rds file
    sprintf(fmt='/// loading %s', input$rds_to_load) %>% message()
    object <- readRDS(input$rds_to_load)

    #### change the feature names in the data matrix to all lower case
    #### - feature names don't need to be input as case-sensitive
    colnames(object$feature_values) %<>% str_to_lower()
    
    #### stop the waiter
    waiter_hide()

    #### return the loaded rds object
    object})
  
  ### collect the user-specified feature name and reduce reactivity
  input_feature <- reactive({
    sprintf(fmt='%s signal limits', input$feature) %>%
      updateSliderInput(session=session, inputId='feature_value_limits')
    input$feature}) %>%
    debounce(1000)
  
  ### collect the reduction method
  input_reduction_method <- reactive({
    input$reduction_method})

  ### collect the point size and reduce reactivity
  input_point_size <- reactive({
    input$point_size}) %>%
    debounce(500)
  
  ### collect the colour scale limits
  input_feature_value_limits <- reactiveValues()
  observe({
    input_feature_value_limits$min <- input$feature_value_limits[1]
    input_feature_value_limits$max <- input$feature_value_limits[2]
    input_feature_value_limits$limits <- input$feature_value_limits})

  ### collect the selected colour palette
  selected_palette <- reactiveValues()
  observe({
    req(input$predefined_palette)

    #### split the palette source and name from the string
    input$predefined_palette %>% str_split(pattern=':') %>% pluck(1) -> sp

    #### save palette source and name into the reactive values list
    selected_palette$package <- sp[[1]]
    selected_palette$name <- sp[[2]]})

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
                          
  ### a waiter screen for plot panels being redrawn
  Waiter$new(id=c('cluster_feature_barplot',
                  'feature_scatterplot', 'feature_scatterplot_3d',
                  'cluster_scatterplot', 'cluster_scatterplot_3d'),
             html=redrawing_plot,
             color=rgb(red=1, green=1, blue=1, alpha=0.6)) -> waiter

  ## use inputs to get an input dataset
  data_to_plot <- reactive(x={
    req(input_object)
    object <- input_object()
    feature <- input_feature() %>% str_to_lower()

    ### show the loading screens
    waiter$show()

    ### check that the feature is in the matrix
    if(!is.element(el=feature, set=colnames(object$feature_values))) {
      sendSweetAlert(session=session,
                     title='Feature not found!',
                     text=sprintf('The specified feature "%s" was not found!', feature),
                     type='error')
      updateTextInput(session=session, inputId='feature', value=app_config$initial_feature)
      return(NULL)
    }
      
    ### make and return a data.frame that is used to make plots
    data.frame(object$reductions[[input_reduction_method()]] %>% set_names(str_c, '2d', sep='.'),
               object$reductions[[str_c(input_reduction_method(), '3d', sep='_')]] %>% set_names(str_c, '3d', sep='.'),
               feature_value=object$feature_values[,feature],
               object$metadata) %>%
      mutate(seurat_clusters=sprintf(fmt='Cluster %s: a cell type', seurat_clusters) %>% factor() %>% fct_relevel(mixedsort) %>% fct_relevel(rev)) %>%
      arrange(feature_value) -> data_to_plot

    ### update the slider
    slider_min <- min(data_to_plot$feature_value) %>% subtract(0.05) %>% round(digits=1)
    slider_max <- max(data_to_plot$feature_value) %>% add(0.05) %>% round(digits=1)
    updateSliderInput(session=session, inputId='feature_value_limits',
                      min=slider_min, max=slider_max,
                      value=c(slider_min, slider_max))

    ### return the data
    data_to_plot})

  ## make selected feature scatterplots
  ### 2D ggplot
  output$feature_scatterplot <- renderPlot({
    if(is.null(selected_palette$package) || input_feature_value_limits$min<0)
      return(NULL)

    palette_package <- selected_palette$package
    picked_palette <- selected_palette$name
    get_labels <- function(x) {
      x %>% is.na() %>% not() %>% which() %>% range() -> idx
      x[-idx] <- ''
      x
    }

    if(palette_package=='brewer') {
      colour_gradient <- scale_colour_distiller(palette=picked_palette, direction='reverse', labels=get_labels, limits=input_feature_value_limits$limits, oob=squish)
    } else if(palette_package=='viridis') {
      colour_gradient <- scale_colour_viridis_c(option=picked_palette, direction=1, labels=get_labels, limits=input_feature_value_limits$limits, oob=squish)
    } else {
      colour_gradient <- scale_colour_gradient()
    }
    
    ggplot(data=data_to_plot()) +
      aes(x=x.2d, y=y.2d, colour=feature_value) +
      labs(title=sprintf(fmt='%s in cells', input_feature())) +
      geom_point(size=input_point_size()) +
      colour_gradient +
      guides(color=guide_colourbar(label.position='bottom')) +
      theme_void() +
      theme(#legend.box.background=element_blank(),
            legend.box.margin=margin(r=10, b=10, t=0, l=0),
            legend.position=c(1,0),
            legend.justification=c(1,0),
            legend.direction='horizontal',
            legend.title=element_blank(),
            panel.border=element_rect(fill=NA),
            panel.background=element_rect(fill=panel_background_rgb),
            text=element_text(size=14))}, bg='transparent')

  ### 3D plotly
  output$feature_scatterplot_3d <- renderPlotly({
    if(is.null(selected_palette$package) || input_feature_value_limits$min<0)
      return(NULL)

    data_to_plot() %>%
      mutate(feature_value=squish(x=feature_value, range=input_feature_value_limits$limits)) %>%
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
             modeBarButtonsToRemove=c('zoom2d', 'resetCameraLastSave3d'),
             displayModeBar=TRUE) %>%
      add_markers(x=~x.3d, y=~y.3d, z=~z.3d,
                  color=~feature_value,
                  colors=selected_palette$name,
                  text=~seurat_clusters,
                  marker=list(symbol='circle-dot',
                              size=input_point_size()*2,
                              line=list(width=0)),

                  hoverinfo='text') %>%
      # colorbar(title=input_feature(),
      #          yanchor='center', y=0.5)
      hide_colorbar()
    })
  
  ## make cluster identity scatterplots
  ### 2D ggplot
  output$cluster_scatterplot <- renderPlot({
    data_to_plot <- data_to_plot()
    n_clusters <- levels(data_to_plot$seurat_clusters) %>% length()
    
    ggplot(data=data_to_plot) +
      aes(x=x.2d, y=y.2d, colour=seurat_clusters) +
      labs(title='Cell clusters') +
      geom_point(size=input_point_size()) +
      scale_colour_manual(values={colorRampPalette(brewer.pal(n=8, name='Set2'))(n_clusters)}) +
      theme_void() +
      theme(legend.position='none',
            panel.border=element_rect(fill=NA),
            panel.background=element_rect(fill=panel_background_rgb),
            text=element_text(size=14))}, bg='transparent')
  
  ### 3D plotly
  output$cluster_scatterplot_3d <- renderPlotly({
    data_to_plot <- data_to_plot()

    plot_ly(data=data_to_plot) %>%
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
             modeBarButtonsToRemove=c('zoom2d', 'resetCameraLastSave3d'),
             displayModeBar=TRUE) %>%
      add_markers(x=~x.3d, y=~y.3d, z=~z.3d,
                  color=~seurat_clusters, colors='Set2',
                  text=~seurat_clusters,
                  marker=list(symbol='circle-dot',
                              size=input_point_size()*2,
                              line=list(width=0)),
                  hoverinfo='text')})
  
  ## make cluster/feature signal barplot
  output$cluster_feature_barplot <- renderPlot({
    palette_package <- selected_palette$package
    picked_palette <- selected_palette$name
  
    data_to_plot <- data_to_plot()
    n_clusters <- levels(data_to_plot$seurat_clusters) %>% length()
    
    if(palette_package=='brewer') {
      fill_gradient <- scale_fill_distiller(palette=picked_palette, direction='reverse')
    } else if(palette_package=='viridis') {
      fill_gradient <- scale_fill_viridis_c(option=picked_palette, direction=1)
    } else {
      fill_gradient <- scale_fill_gradient()
    }

    data_to_plot %>%
      mutate(feature_name=input_feature()) %>%
      group_by(seurat_clusters, feature_name) %>%
      mutate(cells_in_cluster=n()) %>%
      filter(feature_value>0) %>% # only use cells with non-zero expression
      group_by(cells_in_cluster, add=TRUE) %>%
      summarise(expressing_cells=n(),
                mean_value=mean(feature_value),
                median_value=median(feature_value),
                from_value=quantile(feature_value, 0.25),
                to_value=quantile(feature_value, 0.75)) %>%
      ggplot() +
      aes(x=seurat_clusters, fill=mean_value, colour=seurat_clusters) +
      labs(x='Cell types', y=sprintf(fmt='Median %s signal ± quartile', input_feature()), fill=sprintf(fmt='Mean signal', input_feature()), size='Cells in cluster', colour='Cell type', title=sprintf(fmt='%s in clusters', input_feature())) +
      geom_linerange(mapping=aes(ymin=from_value, ymax=to_value), size=2) +
      geom_point(mapping=aes(y=median_value, size=expressing_cells/cells_in_cluster*100), shape=21, colour='darkgrey', stroke=1) +
      scale_size_continuous(labels=function(l) str_c(l, '%'), range=c(2,8)) +
      scale_colour_manual(values={colorRampPalette(brewer.pal(n=8, name='Set2'))(n_clusters)}, breaks={levels(data_to_plot$seurat_clusters) %>% rev()}) +
      fill_gradient +
      coord_flip() +
      guides(colour=guide_legend(order=1), 
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
