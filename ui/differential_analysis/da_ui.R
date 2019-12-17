###data cleaning UI
ui.differentialanalysis <- function(){
  fluidPage(
    shinyjs::useShinyjs(),
    navlistPanel(id = "DAtab",
                 well = TRUE,
                 widths = c(3,9),
                 ##upload data
                 tabPanel(value = "DAupload",
                          title = "1.Upload Data Files",
                          # icon = icon("upload")
                          source("ui/differential_analysis/da_upload_ui.R", local = TRUE)$value
                 ),

                 ##data check
                 tabPanel(value = "DAdataCheck",
                          title = "2.Check Data Flies",
                          # icon = icon("check-square")
                          source("ui/differential_analysis/da_data_check_ui.R", local = TRUE)$value
                          # uiOutput(outputId = ".da.data.check.area")
                 ),

                 ##Univariate Analysis
                 tabPanel(value = "DAunivariateAnalysis",
                          title = "3.Univariate Analysis",
                          # icon = icon("align-justify"),
                          source("ui/differential_analysis/da_univariate_analysis_ui.R", local = TRUE)$value
                 ),

                 ##Multivariate Analysis
                 tabPanel(value = "DAmultivariateAnalysis",
                          title = "4.Multivariate Analysis",
                          # icon = icon("flag-checkered"),
                          source("ui/differential_analysis/da_multivariate_analysis_ui.R", local = TRUE)$value
                 ),

                 ##Multivariate Analysis
                 tabPanel(value = "DAdifferentialMetaboliteSelection",
                          title = "5.Differential Metaboloite Selection",
                          # icon = icon("flag-checkered"),
                          source("ui/differential_analysis/da_differential_metabolite_selection_ui.R", local = TRUE)$value
                 ),

                 ##Validation
                 tabPanel(value = "DAvalidation",
                          title = "6.Performance Validation",
                          # icon = icon("puzzle-piece"),
                          source("ui/differential_analysis/da_validation_ui.R", local = TRUE)$value
                 ),

                ##data download
                tabPanel(value = "DAdownload",
                         title = "7.Result Download",
                         # icon = icon("download"),
                         source("ui/differential_analysis/da_download_result_ui.R", local = TRUE)$value
                         # uiOutput(outputId = ".da.data.profile.area")
                )
                )
  )
}












