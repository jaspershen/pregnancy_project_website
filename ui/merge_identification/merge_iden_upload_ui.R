# dc.upload.ui <- function(){
  fluidPage(
    # headerPanel(title = "Upload data"),
    column(width = 4,
           wellPanel(
             textInput(inputId = "mergeIdenProjectID",
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
               inputId = "mergeIdenMS1PeakTable",
               label = h5(
                 "MS1 peak table",
                 shinyBS::tipify(
                   el = icon(name = "info-circle"),
                   placement = "bottom",
                   trigger = "hover",
                   title = "The first column must be peak name"
                 )
               ),
               multiple = FALSE,
               accept = c("txt/csv", "text/comma-separated-values,text/plain", ".csv"),
               buttonLabel = "Browser"
             ),

             # ms2. peak table
             fileInput(
               inputId = "mergeIdenMS2PeakTable",
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
             # helpText(
             #   a(
             #     "Click Here to Download Demo data",
             #     href = "http://pgbwxuy70.bkt.clouddn.com/demo_data_for_one_step_data_cleaning.zip",
             #     target = "_blank"
             #   )
             # ),
             # hr(),

             useShinyalert(),
               actionButton(
                 inputId = 'merge.iden.upload.button',
                 label = "Upload",
                 styleclass = "info",
                 # class = "btn-primary",
                 icon = icon('play-circle')
               ),
             actionButton(inputId = "merge.iden.data.check.2.parameter",
                          label = "Next",
                          styleclass = "warning"),
             helpText("If there are Errors in your data, please check
                      your data and upload again.")

           )
    ),

    column(width = 7,
           span(textOutput(outputId = "merge.iden.data.check.result.info"), style = "color:red"),
           DT::dataTableOutput(outputId = "merge.iden.data.check.result")
    )

  )
# }