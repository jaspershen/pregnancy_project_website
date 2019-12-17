###data cleaning UI
ui.pathwayAnalysis <- function(){
  fluidPage(
    tabsetPanel(id = "PAtab",
                selected = "PAenter", type = "tabs",

                tabPanel(value = "PAenter",
                         title = "1.Paste Differential Metabolites/Peaks",
                         # icon = icon("upload")
                         source("ui/pathway_analysis/pa_enter_ui.R", local = TRUE)$value
                ),

                ##check data
                tabPanel(value = "PAdataCheck",
                         title = "2.Check Data Files",
                         # icon = icon("cog"),
                         source("ui/pathway_analysis/pa_data_check_ui.R", local = TRUE)$value
                ),


                ##pathway enrichment
                tabPanel(value = "PApathwayEnrichment",
                         title = "3.Pathway Enrichment",
                         # icon = icon("cog"),
                         source("ui/pathway_analysis/pa_pathway_enrichment_ui.R", local = TRUE)$value
                ),
                ##download result
                tabPanel(value = "PAdownload",
                         title = "4.Result Download",
                         # icon = icon("download"),
                         source("ui/pathway_analysis/pa_download_result_ui.R", local = TRUE)$value
                )
                # )
    )
  )
}









