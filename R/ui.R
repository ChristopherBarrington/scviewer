
ui <- function() {
  config <- system.file('config.yaml', package='scviewer') %>% load_app_config()
  dataset_choices <- get_dataset_choices(config=config)

  ## header
  dashboardHeader(disable=FALSE,
                  title=config$title,
                  tags$li(a(onclick='history.go(-1); return false;', href=NULL,
                            icon(name='home', lib='font-awesome'), title='Back', style='cursor: pointer'),
                          class='dropdown'),
                  titleWidth='1000px') -> ui_header



  ## sidebar
  dashboardSidebar(disable=FALSE,
                   tags$head(tags$link(rel='shortcut icon', href='https://www.crick.ac.uk/themes/custom/crick/favicons/favicon.ico'),
                             includeHTML(config$tracker)),
                   tags$style(type='text/css', '.sidebar-toggle {visibility: hidden !important;}'),
                   tags$style(type='text/css', '.main-header .logo {text-align:left !important; background: #455a64 !important; color: rgba(255, 255, 255, 0.8) !important}'),
                   tags$style(type='text/css', '.main-header .logo:hover {color: white !important}'),
                   tags$style(type='text/css', '.navbar {background: #455a64 !important; box-shadow: none !important}'),
                   tags$style(type='text/css', 'i.fa.fa-home {color: rgba(255, 255, 255, 0.8) !important; font-size: larger !important;}'),
                   tags$style(type='text/css', '.irs-grid-text {visibility: hidden !important;}'),
                   tags$style(type='text/css', '.autocomplete-items div:hover {background-color: #DDDDDD;}'),


                   selectizeInput(inputId='filename', label='Select a dataset', choices=dataset_choices,
                                  options=list(placeholder='Datasets', onInitialize=I('function() { this.setValue(""); }'))),
                   autocomplete_input(id='feature', label='Feature', placeholder='Feature', options='', value=''),
                   sliderInput(inputId='feature_value_limits', label='Feature signal limits', min=0, max=1, step=0.05, value=c(0,0)),
                   selectInput(inputId='reduction_method', label='Dimension reduction method', choices=NULL, selected=NULL),
                   div(id='cluster_sets'),
                   div(id='cluster_identities'),
                   div(id='filters'),
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
                                 selected='viridis:plasma:f', textColor=rgb(red=0, green=0, blue=0, alpha=0),
                                 pickerOpts=list(`live-search`=FALSE, size=10))},
                   sliderInput(inputId='point_size', label='Size of cells', min=0.3, max=1.5, step=0.05, value=1.0),
                   prettySwitch(inputId='match_scenes_3d_plotly', label='Match 3D plot layouts', value=TRUE, status='success', fill=TRUE)) -> ui_sidebar



  ## main body, plots
  dashboardBody(shinyDashboardThemes(theme='grey_light'),
                fillPage(use_waiter(), #waiter_on_busy(html=tagList(spin_atebits()), color=rgb(red=1, green=1, blue=1, alpha=0.5)),
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
  dashboardPage(header=ui_header, sidebar=ui_sidebar, body=ui_body, title=NULL)
}

