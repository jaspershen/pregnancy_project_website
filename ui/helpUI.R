ui.help <- function(){
  tagList(
    fluidPage(
      column(width = 10, offset = 1,
             # includeMarkdown("./data/markdown/MetDNA.instruction.Rmd")
             # includeMarkdown("./data/markdown/metflow_tutorial/metflow_tutorial.Rmd"),
             includeHTML("./data/markdown/metflow_tutorial/metflow.tutorial1.1.html"),
             br(), br(), br()
             # uiOutput(outputId = "help.page.rmd")
             )
    )
  )
}