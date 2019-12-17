# mi.upload.ui <- function(){
  fluidPage(
    # headerPanel(title = "Upload data"),
    column(width = 4,
           wellPanel(
             textInput(inputId = "MIprojectID",
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
               inputId = "MIms1PeakTable",
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

             #MS/MS data
             radioButtons(inputId = "MIhasMS2",
                          label = "Upload MS/MS data?",
                          choices = c("Yes", "No"),
                          selected = "No"),
             conditionalPanel(
               condition = "input.MIhasMS2 == 'Yes'",
               fileInput(
                 inputId = "MIms2Data",
                 label = h5(
                   "MS/MS data",
                   shinyBS::tipify(
                     el = icon(name = "info-circle"),
                     placement = "bottom",
                     trigger = "hover",
                     title = "MS/MS data, mgf or msp file"
                   )
                 ),
                 multiple = TRUE,
                 accept = c("mgf/msp", ".mgf", ".msp"),
                 buttonLabel = "Browser"
               ),

               selectInput(inputId = "mi.ms2.type", label = "MS/MS data type",
                           choices = c(
                             "MGF" = "mgf",
                             "MSP" = "msp"
                           ), selected = "mgf",
                           multiple = FALSE
               ),

               numericInput(inputId = "mi.ms1.ms2.match.mz.tol",
                            label = "MS1 peak and MS/MS spectrum match m/z tolerance (ppm)",
                            value = 25, min = 1, max = 25),
               numericInput(inputId = "mi.ms1.ms2.match.rt.tol",
                            label = "MS1 peak and MS/MS spectrum match RT tolerance (second)",
                            value = 10, min = 1, max = 120)
             ),

             checkboxInput(inputId = "mi.use.demo.data",
                           label = "Use demo data", value = FALSE),

             useShinyalert(),
               actionButton(
                 inputId = 'mi.upload.button',
                 label = "Submit",
                 styleclass = "info",
                 # class = "btn-primary",
                 icon = icon('play-circle')
               ),

             hr(),
             helpText(
               a(
                 "Click Here to Download Demo data",
                 href = "http://oxr5qv74w.bkt.cloudmi.com/Data_normalization.zip",
                 target = "_blank"
               )
             )

           )
    ),

    column(width = 7,
             includeHTML(path = "data/html/introduction_dc_data_upload.html")
    )

  )
# }