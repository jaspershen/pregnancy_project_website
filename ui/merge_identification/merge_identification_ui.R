###data cleaning UI
ui.mergeidentification <- function(){
  fluidPage(
    tabsetPanel(id = "mergeIdenTab",
                selected = "mergeIdenUpload", type = "tabs",
                ##upload data and data check
                tabPanel(value = "mergeIdenUpload",
                         title = "1.Upload Data Files",
                         # icon = icon("upload"),
                         source("ui/merge_identification/merge_iden_upload_ui.R", local = TRUE)$value
                ),

                ##set parameters
                tabPanel(value = "mergeIdenParameter",
                         title = "2.Parameter Setting",
                         # icon = icon("cog"),
                         source("ui/merge_identification/merge_iden_parameter_ui.R", local = TRUE)$value
                ),
                ##download result
                tabPanel(value = "mergeIdenDownload",
                         title = "3.Result Download",
                         # icon = icon("download"),
                         source("ui/merge_identification/merge_iden_download_ui.R", local = TRUE)$value
                )
                # )
    )
  )
}


