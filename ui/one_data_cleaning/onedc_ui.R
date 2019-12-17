###data cleaning UI
ui.oneDatacleaning <- function(){
  fluidPage(
  tabsetPanel(id = "oneDCtab",
              selected = "oneDCupload", type = "tabs",
    # navlistPanel(id = "oneDCtab",
    #              well = TRUE,
    #              widths = c(3,9),
                 ##upload data and data check
                 tabPanel(value = "oneDCupload",
                          title = "1.Data Files Upload",
                          # icon = icon("upload"),
                          source("ui/one_data_cleaning/one_dc_upload_ui.R", local = TRUE)$value
                 ),

                 ##set parameters
                 tabPanel(value = "oneDCparameter",
                          title = "2.Parameter Setting",
                          # icon = icon("cog"),
                          source("ui/one_data_cleaning/one_dc_parameter_ui.R", local = TRUE)$value
                 ),
                 ##download result
                 tabPanel(value = "oneDCdownload",
                          title = "3.Result Download",
                          # icon = icon("download"),
                          source("ui/one_data_cleaning/one_dc_download_ui.R", local = TRUE)$value
                 )
                 # )
                 )
  )
}


