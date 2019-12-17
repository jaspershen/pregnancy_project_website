# dc.upload.ui <- function(){
  fluidPage(
    # headerPanel(title = "Upload data"),
    column(width = 4,
           wellPanel(
             textInput(inputId = "oneDCprojectID",
                       label = h5(
                         "Project name",
                         shinyBS::tipify(
                           el = icon(name = "info-circle"),
                           placement = "bottom",
                           trigger = "hover",
                           title = "Please text your project name"
                         )
                       ),
                       value = "",
                       placeholder = "Project name"
                       ),
             #ms1. peak table
             fileInput(
               inputId = "oneDCms1PeakTable",
               label = h5(
                 "MS1 peak table",
                 shinyBS::tipify(
                   el = icon(name = "info-circle"),
                   placement = "bottom",
                   trigger = "hover",
                   title = "The first column must be peak name"
                 )
               ),
               multiple = TRUE,
               accept = c("txt/csv", "text/comma-separated-values,text/plain", ".csv"),
               buttonLabel = "Browser"
             ),
             #sample information
             fileInput(
               inputId = "oneDCsampleInfo",
               label = h5(
                 "Sample information",
                 shinyBS::tipify(
                   el = icon(name = "info-circle"),
                   placement = "bottom",
                   trigger = "hover",
                   title = "Column 1: sample name, column 2: group"
                 )
               ),
               multiple = FALSE,
               accept = c("txt/csv", "text/comma-separated-values,text/plain", ".csv"),
               buttonLabel = "Browser"
             ),

             # ms2. peak table
             fileInput(
               inputId = "oneDCms2PeakTable",
               label = h5(
                 "Peak table with identifications (from MetAnalyzer)",
                 shinyBS::tipify(
                   el = icon(name = "info-circle"),
                   placement = "bottom",
                   trigger = "hover",
                   title = "Must be from MetAnalyzer"
                 )
               ),
               multiple = TRUE,
               accept = c("txt/csv", "text/comma-separated-values,text/plain", ".csv"),
               buttonLabel = "Browser"
             ),
             helpText(
               a(
                 "Click Here to Download Demo data",
                 href = "http://pgbwxuy70.bkt.clouddn.com/demo_data_for_one_step_data_cleaning.zip",
                 target = "_blank"
               )
             ),
             hr(),

             useShinyalert(),
               actionButton(
                 inputId = 'one.dc.upload.button',
                 label = "Upload",
                 styleclass = "info",
                 # class = "btn-primary",
                 icon = icon('play-circle')
               ),
             actionButton(inputId = "one.dc.data.check.2.parameter",
                          label = "Next",
                          styleclass = "warning"),
             helpText("If there are Errors in your data, please check
                      your data and upload again.")
             # hr(),
             # helpText(
             #   a(
             #     "Click Here to Download Demo data",
             #     href = "http://oxr5qv74w.bkt.cloudmi.com/Data_normalization.zip",
             #     target = "_blank"
             #   )
             # )

           )
    ),

    column(width = 7,
           DT::dataTableOutput(outputId = "one.dc.data.check.result")
    )

  )
# }