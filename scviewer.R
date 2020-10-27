library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(scales)
library(esquisse)
library(gtools)
library(shinyWidgets)
library(magrittr)
library(tidyverse)

# define the UI
ui <- dashboardPage(
  dashboardHeader(title="the title of the manuscript will go in this placeholder text once there is a title and I am just padding the text now and there are some extra words here and here and there too", disable=FALSE, titleWidth='1500px'),
  dashboardSidebar(disable=FALSE,
    selectizeInput(inputId='rds_to_load', label='Select a dataset', choices={yaml::read_yaml(file='samplesheet.yaml')$datasets %>% map_depth(2, pluck, 'rds_file')},
      options = list(
        placeholder = 'Datasets',
        onInitialize = I('function() { this.setValue(""); }'))),
    textInput(inputId='feature', label='Feature', value='SOX2', placeholder='Feature'),
    selectInput(inputId='reduction_method', label='Dimension reduction method', choices=list(PCA='pca', UMAP='umap', tSNE='tsne'), selected='umap'),
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
          `Viridis [sequential]`=list(`viridis:magma`=viridis_pal(option='magma')(8),
                                      `viridis:plasma`=viridis_pal(option='plasma')(8),
                                      `viridis:inferno`=viridis_pal(option='inferno')(8),
                                      `viridis:viridis`=viridis_pal(option='viridis')(8))) %>%
        palettePicker(inputId='predefined_palette', label='Colour palette', 
                      selected='brewer:YlGnBu', textColor=rgb(red=0, green=0, blue=0, alpha=0),
                      pickerOpts=list(`live-search`=FALSE, size=10))},
    sliderInput(inputId='point_size', label='Size of cells', min=0.3, max=1.5, step=0.05, value=1.0)),
  dashboardBody(
    shinyDashboardThemes(theme="grey_light"),
    fillPage(
      tags$style(type = "text/css", "#cluster_scatterplot {width: 100% !important; height: calc(50vh - 40px) !important;}"),
      tags$style(type = "text/css", "#cluster_scatterplot_3d {width: 100% !important; height: calc(50vh - 40px) !important;}"),
      tags$style(type = "text/css", "#feature_scatterplot {width: 100% !important; height: calc(50vh - 40px) !important;}"),
      tags$style(type = "text/css", "#feature_scatterplot_3d {width: 100% !important; height: calc(50vh - 40px) !important;}"),
      tags$style(type = "text/css", "#cluster_feature_barplot {width: 100% !important; height: calc(100vh - 80px) !important;}"),
      fillRow(
    column(width=12,
      plotOutput("cluster_scatterplot", height='100%'),
      plotlyOutput("cluster_scatterplot_3d", height='100%'),
    ),
    column(width=12,
      plotOutput("feature_scatterplot", height='100%'),
      plotlyOutput("feature_scatterplot_3d", height='100%'),
    ),
    plotOutput("cluster_feature_barplot", height='100%')
  )))
)

# define the server
server <- function(input, output, session){
  ## get UI inputs
  input_object <- reactive({
    req(input$rds_to_load)
    sprintf(fmt='/// loading %s', input$rds_to_load) %>% print()
    object <- readRDS(input$rds_to_load)
    colnames(object$rna_data) %<>% str_to_lower()
    object})
  
  input_feature <- reactive({
    input$feature}) %>%
    debounce(1000)
  
  input_reduction_method <- reactive({
    input$reduction_method})

    input_point_size <- reactive({
      input$point_size}) %>%
      debounce(500)
  
  selected_palette <- reactiveValues()
  observe({
    req(input$predefined_palette)
    input$predefined_palette %>% str_split(pattern=':') %>% pluck(1) -> sp
    selected_palette$package <- sp[[1]]
    selected_palette$name <- sp[[2]]})

  ## show notice to load a dataset
  observe({
    if(input$rds_to_load=='')
      sendSweetAlert(
        session=session,
        title='Getting started',
        text='Select a dataset to view using the "Datasets" dropdown',
        type='info')})
  
  ## common elements
  panel_background_rgb <- rgb(1, 1, 1, 0)
  
  ## use inputs to get an input dataset
  dimred <- reactive({
    req(input_object)
    object <- input_object()
    feature <- input_feature() %>% str_to_lower()
    if(!is.element(el=feature, set=colnames(object$rna_data))) {
      sendSweetAlert(session=session,
                     title='Feature not found!',
                     text=sprintf('The specified feature "%s" was not found!', feature),
                     type='error')
      updateTextInput(session=session, inputId='feature', value='SOX2')
      return(NULL)
    }
    
    data.frame(object$reductions[[input_reduction_method()]] %>% set_names(str_c, '2d', sep='.'),
               object$reductions[[str_c(input_reduction_method(), '3d', sep='_')]] %>% set_names(str_c, '3d', sep='.'),
               feature_value=object$rna_data[,feature],
               object$metadata) %>%
      mutate(seurat_clusters=sprintf(fmt='Cluster %s: a cell type', seurat_clusters) %>% factor() %>% fct_relevel(mixedsort)) %>%
      arrange(feature_value)})

  ## make selected feature scatterplots
  ### 2D ggplot
  output$feature_scatterplot <- renderPlot({
    if(is.null(selected_palette$package))
      return(NULL)
    
    palette_package <- selected_palette$package
    picked_palette <- selected_palette$name
    
    if(palette_package=='brewer') {
      colour_gradient <- scale_colour_distiller(palette=picked_palette, direction='reverse', oob=squish)
    } else if(palette_package=='viridis') {
      colour_gradient <- scale_colour_viridis_c(option=picked_palette, n=32, direction=1, oob=squish)
    } else {
      colour_gradient <- scale_colour_gradient()
    }
    
    ggplot(data=dimred()) +
      aes(x=x.2d, y=y.2d, colour=feature_value) +
      labs(title=sprintf(fmt='%s in cells', input_feature())) +
      geom_point(size=input_point_size()) +
      colour_gradient +
      guides(color=guide_colourbar(label.position='bottom')) +
      theme_void() +
      theme(legend.position=c(1,0),
            legend.justification=c(1,0),
            legend.direction='horizontal',
            legend.title=element_blank(),
            panel.border=element_rect(fill=NA),
            panel.background=element_rect(fill=panel_background_rgb),
            text=element_text(size=14))}, bg='transparent')

  ### 3D plotly
  output$feature_scatterplot_3d <- renderPlotly({
    plot_ly(data=dimred(), colors=selected_palette$name) %>%
      layout(paper_bgcolor=panel_background_rgb,
             scene=list(xaxis=list(visible=FALSE),
                        yaxis=list(visible=FALSE),
                        zaxis=list(visible=FALSE)),
             modebar=list(orientation='v',
                          activecolor='orange',
                          color='lightgrey',
                          bgcolor='transparent')) %>%
      config(scrollZoom=FALSE,
             displaylogo=FALSE,
             modeBarButtonsToRemove=c('zoom2d', 'resetCameraLastSave3d'),
             displayModeBar=TRUE) %>%
      add_markers(x=~x.3d, y=~y.3d, z=~z.3d,
                  color=~feature_value,
                  text=~seurat_clusters,
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
    data_to_plot <- dimred()
    n_clusters <- levels(data_to_plot$seurat_clusters) %>% length()
    
    ggplot(data=dimred()) +
      aes(x=x.2d, y=y.2d, colour=seurat_clusters) +
      labs(title='Cell clusters') +
      geom_point(size=input_point_size()) +
      scale_color_brewer(palette='Set2') +
      scale_colour_manual(values={colorRampPalette(brewer.pal(n=8, name='Set2'))(n_clusters)}) +
      theme_void() +
      theme(legend.position='none',
            panel.border=element_rect(fill=NA),
            panel.background=element_rect(fill=panel_background_rgb),
            text=element_text(size=14))}, bg='transparent')
  
  ### 3D plotly
  output$cluster_scatterplot_3d <- renderPlotly({
    plot_ly(data=dimred()) %>%
      layout(paper_bgcolor=panel_background_rgb,
             #showlegend=FALSE,
             scene=list(xaxis=list(visible=FALSE),
                        yaxis=list(visible=FALSE),
                        zaxis=list(visible=FALSE)),
             modebar=list(orientation='v',
                          activecolor='orange',
                          color='lightgrey',
                          bgcolor='transparent'),
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
  
    data_to_plot <- dimred()
    n_clusters <- levels(data_to_plot$seurat_clusters) %>% length()
    
    if(palette_package=='brewer') {
      fill_gradient <- scale_fill_distiller(palette=picked_palette, direction='reverse', oob=squish)
    } else if(palette_package=='viridis') {
      fill_gradient <- scale_fill_viridis_c(option=picked_palette, n=32, direction=1, oob=squish)
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
                summary_value=mean(feature_value),
                from_value=quantile(feature_value, 0.25),
                to_value=quantile(feature_value, 0.75)) %>%
      ggplot() +
      aes(x=seurat_clusters, fill=summary_value, colour=seurat_clusters) +
      labs(x='Cell types', y=sprintf(fmt='Mean %s signal', input_feature()), fill=sprintf(fmt='Mean signal', input_feature()), size='Cells in cluster', colour='Cell type') +
      geom_linerange(mapping=aes(ymin=from_value, ymax=to_value), size=2) +
      geom_point(mapping=aes(y=summary_value, size=expressing_cells/cells_in_cluster*100), shape=21, colour='darkgrey', stroke=1) +
      scale_size_continuous(labels=function(l) str_c(l, '%'), range=c(2,8)) +
      scale_colour_manual(values={colorRampPalette(brewer.pal(n=8, name='Set2'))(n_clusters)}) +
      fill_gradient +
      coord_flip() +
      theme_bw() +
      theme(axis.title.y=element_blank(),
            # legend.position='none',
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
