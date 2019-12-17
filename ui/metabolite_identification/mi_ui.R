###data cleaning UI
ui.metaboliteidentification <- function(){
  fluidPage(
    shinyjs::useShinyjs(),
    navlistPanel(id = "MItab",
                 well = TRUE,
                 widths = c(3,9),
                 ##upload data
                 tabPanel(value = "MIupload",
                          title = "1.Data Files Upload",
                          # icon = icon("upload"),
                          source("ui/metabolite_identification/mi_upload_ui.R", local = TRUE)$value
                 ),

                 ##data check
                 tabPanel(value = "MIdataCheck",
                          title = "2.Data Cheak",
                          # icon = icon("check-square"),
                          source("ui/metabolite_identification/mi_data_check_ui.R", local = TRUE)$value
                          # uiOutput(outputId = ".mi.data.check.area")
                 ),


                 ##m/z match
                 tabPanel(value = "MImzMatch",
                          title = "3.m/z Match",
                          # icon = icon("database"),
                          source("ui/metabolite_identification/mi_mz_match_ui.R", local = TRUE)$value
                 ),

                 ##ms/ms match
                 tabPanel(value = "MIms2Match",
                          title = "4.MS/MS Match",
                          # icon = icon("database"),
                          source("ui/metabolite_identification/mi_ms2_match_ui.R", local = TRUE)$value
                 )

                ##data download
                # tabPanel(value = "MIdownload",
                #          title = "5.Result Download",
                #          icon = icon("download")
                #          # source("ui/metabolite_identification/mi_download_result_ui.R", local = TRUE)$value
                #          # uiOutput(outputId = ".mi.data.profile.area")
                # )
                )
  )
}












