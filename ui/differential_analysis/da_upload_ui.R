# dc.upload.ui <- function(){
  fluidPage(
    # headerPanel(title = "Upload data"),
    column(width = 4,
           wellPanel(
             textInput(inputId = "DAprojectID",
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
               inputId = "DAms1PeakTable",
               label = h5(
                 "MS1 peak table(discovery)",
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

             #sample information
             fileInput(
               inputId = "DAsampleInfo",
               label = h5(
                 "Sample information(discovery)",
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

             checkboxInput(inputId = "da.use.demo.data",
                           label = "Use demo data", value = FALSE),

             useShinyalert(),
               actionButton(
                 inputId = 'da.upload.button',
                 label = "Submit",
                 styleclass = "info",
                 # class = "btn-primary",
                 icon = icon('play-circle')
               )

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
             includeHTML(path = "data/html/introduction_dc_data_upload.html"),
           br(), br(), br()
    )

  )
# }