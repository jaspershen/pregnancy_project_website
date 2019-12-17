
fluidPage(
      shinyjs::useShinyjs(),
      shinysky::actionButton(inputId = "one.dc.generate.analysis.result",
                             label = "Generate Analysis Result", styleclass = "info"),
      downloadButton(outputId = "one.dc.result.download", "Download Analysis Result"),
      helpText("Please click", strong("Generate Analysis Result"),
               "to generate analysis result, then click",
               strong("Download Analysis Result"),
               "to download."),
  br(), br(), br()
)
