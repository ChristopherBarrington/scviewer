.onLoad <- function(...) {
  options(warn=-1,
          dplyr.summarise.inform=FALSE,
          scviewer.verbose=!interactive())
}
