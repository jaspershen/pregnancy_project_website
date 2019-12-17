

###data cleaning UI
ui.datacleaning <- function(){
  fluidPage(
    navlistPanel(id = "DCtab",
                 well = TRUE,
                 widths = c(3,9),
                 ##upload data
                 tabPanel(value = "DCupload",
                          title = "1.Upload Data Files",
                          # icon = icon("upload"),
                          source("ui/data_cleaning/dc_upload_ui.R", local = TRUE)$value
                 ),

                 ##data check
                 tabPanel(value = "DCdataCheck",
                          title = "2.Check Data Files",
                          # icon = icon("check-square"),
                          source("ui/data_cleaning/dc_data_check_ui.R", local = TRUE)$value
                          # uiOutput(outputId = ".dc.data.check.area")
                 ),

                 ##batch alginment
                 tabPanel(value = "DCbatchAlignment",
                          title = "3.Batch Alignment",
                          # icon = icon("align-justify"),
                          source("ui/data_cleaning/dc_batch_alignment_ui.R", local = TRUE)$value
                 ),

                 ##data quality assessment, before data cleaning
                 tabPanel(value = "DCdataQualityBefore",
                          title = "4.Data Quality Check",
                          # icon = icon("flag-checkered"),
                          source("ui/data_cleaning/dc_qa1_ui.R", local = TRUE)$value
                 ),

                 ##missing value processing
                 ##
                 tabPanel(value = "DCmvProcessing",
                          title = "5.Missing Value Processing",
                          # icon = icon("puzzle-piece"),
                          source("ui/data_cleaning/dc_mv_processing_ui.R", local = TRUE)$value
                 ),

                ##zero value processing
                tabPanel(value = "DCzeroProcessing",
                         title = "6.Zero Value Processing",
                         # icon = icon("puzzle-piece"),
                         source("ui/data_cleaning/dc_zero_processing_ui.R", local = TRUE)$value
                ),

                ##data normalization
                tabPanel(value = "DCdn",
                         title = "7.Data Normalization",
                         # icon = icon("eraser"),
                         source("ui/data_cleaning/dc_data_normalization_ui.R", local = TRUE)$value
                ),

                ##data integration
                tabPanel(value = "DCdi",
                         title = "8.Data Integration",
                         # icon = icon("paperclip"),
                         source("ui/data_cleaning/dc_data_integration_ui.R", local = TRUE)$value
                ),

                ##outlier processing
                tabPanel(value = "DCos",
                         title = "9.Outlier Removal",
                         # icon = icon("filter"),
                         source("ui/data_cleaning/dc_outlier_sample_ui.R", local = TRUE)$value
                ),

                ##data quality assessment
                tabPanel(value = "DCdataQualityAfter",
                         title = "10.Data Quality Visualization",
                         # icon = icon("flag-checkered"),
                         source("ui/data_cleaning/dc_qa2_ui.R", local = TRUE)$value
                ),

                ##data download
                tabPanel(value = "DCdownload",
                         title = "11.Result Download",
                         # icon = icon("download"),
                         source("ui/data_cleaning/dc_download_result_ui.R", local = TRUE)$value
                         # uiOutput(outputId = ".dc.data.profile.area")
                )
                )
  )
}












