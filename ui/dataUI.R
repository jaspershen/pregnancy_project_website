ui.data <- function(){
  tagList(
    fluidPage(
      column(width = 8, offset = 2,
             includeMarkdown("./data/markdown/demoData.Rmd"),
             # includeMarkdown("./data/markdown/demoData.html"),
             br(), br(), br()
      )
    )
  )
}