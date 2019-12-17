fluidPage(
  # tags$hr(style = "border-color:black;"),
  column(width = 3, offset = 0,
         # selectInput("hasIS", label = "Data with internal standard?",
         #             choices = c(
         #               "Yes" = "yes",
         #               "No" = "no"
         #             ),selected = "no", multiple = FALSE
         # ),
         #---------------------------------------------------------------------
         textOutput(outputId = "oneDCparamFrom"),
         h4("Global Parameters", style = "color:black;"),
         fileInput(
           inputId = "oneDCparamTable",
           label = h5(
             "Parameter Table",
             shinyBS::tipify(
               el = icon(name = "info-circle"),
               placement = "bottom",
               trigger = "hover",
               title = "You can upload parameter table for all parameters"
             )
           ),
           multiple = FALSE,
           accept = c("csv",".xlsx"),
           # accept = c("txt/csv", "text/comma-separated-values,text/plain", ".csv"),
           buttonLabel = "Browser"
         ),

         helpText(
           a("Click Here to Download Demo Parameter Table",
             href = "http://pgbwxuy70.bkt.clouddn.com/parameter_table1.xlsx",
             target = "_blank"
           )
         ),

         hr(),

         useShinyalert(),
         actionButton(
           inputId = 'one.dc.upload.param.button',
           label = "Upload",
           styleclass = "info",
           icon = icon('play-circle')
         ),
         #view parameter table
         actionButton(inputId = "oneDCshowParamTable",
                      label = "View",
                      styleclass = "warning"),
         bsModal(id = "paramTable",
                 title = "Parameter Table",
                 trigger = "oneDCshowParamTable",
                 size = "large",
                 DT::dataTableOutput(outputId = "one.dc.param.table")
         ),
         helpText("Select the parameter table and upload."),

         selectInput(inputId = "oneDCpolarity",
                     label = "Polarity",
                     choices = c(
                       "Positive" = "positive",
                       "Negative" = "negative"
                     ), selected = "positive", multiple = FALSE),

         # selectInput("hasQC", label = "Data with QC samples?",
         #             choices = c(
         #               "Yes" = "yes",
         #               "No" = "no"
         #             ),selected = "yes", multiple = FALSE
         # ),
         tags$hr(style = "border-color:black;"),
         ###-------------------------------------------------------------------
         ###batch alignment
         h4("Batch alignment", style = "color:black;"),
         numericInput(inputId = "one.dc.ba.mz.tol",
                      label = "m/z tolerance (ppm)",
                      value = 25, min = 1, max = 50, step = 1),

         numericInput(inputId = "one.dc.ba.rt.tol",
                      label = "Retention tolerance (second)",
                      value = 30, min = 1, max = 300, step = 1),

         tags$hr(style = "border-color:black;"),
         h4("Metabolite identification", style = "color:black;"),
         numericInput(inputId = "one.dc.mi.mz.tol",
                      label = "m/z tolerance (ppm)",
                      value = 25, min = 1, max = 50, step = 1),

         numericInput(inputId = "one.dc.mi.rt.tol",
                      label = "Retention tolerance (second)",
                      value = 180, min = 1, max = 300, step = 1),
         br(), br(), br()
  ),


  column(width = 3,
         #---------------------------------------------------------------------
         #missing value processing
         h4("Missing Value Processing", style = "color:black;"),
         sliderInput("one.dc.var.mv.cutoff", label = "Peak will be removed if MV ratio > %",
                     min = 1, max = 80, value = 50, step = 1),

         selectInput(inputId = "imputationMethod",
                     label = "MV imputation method",
                     choices = c(
                       "Zero value" = "zero",
                       "Mean" = "mean",
                       "Median" = "meadian",
                       "Minimum" = "minimum",
                       "KNN" = "knn",
                       "missForest" = "rf",
                       "BPCA" = "bpca",
                       "PPCA" = "ppca",
                       "SVD" = "svd"
                     ),
                     selected = "knn",
                     multiple = FALSE),

         conditionalPanel(condition = "input.imputationMethod == 'knn'",
                          sliderInput(inputId = "one.dc.k",
                                      label = "Number of neighbors",
                                      min = 2, max = 20, value = 10,
                                      step = 1, round = TRUE),
                          sliderInput(inputId = "one.dc.rowmax",
                                      label = "The maximum percent missing data allowed in any row",
                                      min = 1, max = 70, value = 50,
                                      step = 0.5, round = FALSE),
                          sliderInput(inputId = "one.dc.colmax",
                                      label = "The maximum percent missing data allowed in any column",
                                      min = 1, max = 90, value = 80,
                                      step = 0.5, round = FALSE)
         ),
         conditionalPanel(
           condition = "input.imputationMethod == 'rf'",
           sliderInput(inputId = "one.dc.mv.ntree",
                       label = "Number of trees to grow in each forest",
                       min = 1, max = 30, value = 10,
                       step = 1, round = TRUE),
           checkboxInput(inputId = "one.dc.mvReplace",
                         label = "Bootstrap sampling (with replacements) is performed",
                         value = TRUE)
         ),
         conditionalPanel(
           condition = "input.imputationMethod == 'bpca' | input.imputationMethod == 'ppca' | input.imputationMethod == 'svd'",
           sliderInput(inputId = "one.dc.nPcs",
                       label = "Number of principal components to calculate",
                       min = 1, max = 5, value = 2,
                       step = 1, round = TRUE)
         ),
         # conditionalPanel(
         #   condition = "input.imputationMethod == 'ppca'",
         #   sliderInput(inputId = "one.dc.ppca.nPcs",
         #               label = "Number of principal components to calculate",
         #               min = 1, max = 5, value = 2,
         #               step = 1, round = TRUE)
         # ),
         #
         # conditionalPanel(
         #   condition = "input.imputationMethod == 'svd'",
         #   sliderInput(inputId = "one.dc.svd.nPcs",
         #               label = "Number of principal components to calculate",
         #               min = 1, max = 5, value = 2,
         #               step = 1, round = TRUE)
         # ),
         br(), br(), br()
  ),


  column(width = 3,
         ##---------------------------------------------------------------------
         ##zero value processing
         h4("Zero Value Processing", style = "color:black;"),
         sliderInput("one.dc.var.zero.cutoff",
                     label = "Peak removed if zero ratio > %",
                     min = 1, max = 100, value = 50, step = 1),
         tags$hr(style = "border-color:black;"),

         #----------------------------------------------------------------------
         #data normalization
         h4("Data Normalization", style = "color:black;"),
         radioButtons(
           inputId = "oneDCdnHasQC",
           label = "Method based on QC sample",
           choices = list("YES" = "hasQC",
                          "NO" = "noQC"),
           selected = "hasQC"
         ),

         conditionalPanel(
           condition = "input.oneDCdnHasQC == 'hasQC'",
           selectInput(
             inputId = "oneDCnormalizationMethod1",
             label = "Normalization method",
             choices = c("QC LOESS" = "loess",
                         "QC SVR (MetNormalizer)" = "svr"),
             selected = "svr"
           ),
           conditionalPanel(
             condition = "input.oneDCnormalizationMethod1 == 'loess'",
             checkboxInput(
               inputId = "one.dc.loess.kepp.dimension",
               label = "KEPP dimension or not?",
               value = TRUE
             ),
             checkboxInput(
               inputId = "one.dc.parameter.optimization",
               label = "Optimize parameters?",
               value = TRUE
             ),
             sliderInput(
               inputId = "one.dc.begin.end",
               label = "Beigin and End",
               min = 0.5,
               max = 1,
               value = c(0.5, 1),
               step = 0.1,
               round = FALSE
             ),
             sliderInput(
               inputId = "one.dc.loess.step",
               label = "Step of LOESS",
               min = 0.1,
               max = 0.4,
               value = 0.2,
               step = 0.1,
               round = FALSE
             )
           ),
           conditionalPanel(
             condition = "input.oneDCnormalizationMethod1 == 'svr'",
             checkboxInput(
               inputId = "one.dc.svr.kepp.dimension",
               label = "KEPP dimension or not?",
               value = TRUE
             ),
             sliderInput(
               inputId = "one.dc.svr.multiple",
               label = "How many peaks used?",
               min = 1,
               max = 10,
               value = 5,
               step = 1,
               round = TRUE
             )
           )
         ),
         conditionalPanel(
           condition = "input.DCdnHasQC == 'noQC'",
           selectInput(
             inputId = "normalizationMethod2",
             label = "Normalization method",
             choices = c(
               # "None" = "no",
               "Mean" = "mean",
               "Median" = "median",
               "Total" = "total",
               "Probabilistic Quotient Normalization(PQN)" = "pqn",
               "Quantile" = "quantile"
             ),
             selected = "median"
           )
         ),
         tags$hr(style = "border-color:black;"),
         #---------------------------------------------------------------------
         #data integration
         h4("Data Integration", style = "color:black;"),
         radioButtons(
           inputId = "oneDCdiHasQC",
           label = "Method based on QC sample",
           choices = list("YES" = "hasQC",
                          "NO" = "noQC"),
           selected = "hasQC"
         ),

         conditionalPanel(
           condition = "input.oneDCdiHasQC == 'hasQC'",
           selectInput(
             inputId = "oneDCintegrationMethod1",
             label = "Integration method",
             choices = c("QC mean" = "qc.mean",
                         "QC median" = "qc.median",
                         "None" = "none"),
             selected = "qc.median"
           )
         ),

         conditionalPanel(
           condition = "input.oneDCdiHasQC == 'noQC'",
           selectInput(
             inputId = "oneDCintegrationMethod2",
             label = "Integration method",
             choices = c("Subject mean" = "subject.mean",
                         "Subject median" = "subject.median",
                         "None" = "none"),
             selected = "subject.median"
           )
         ),
         br(), br(), br()
  ),

  column(width = 3,
         #----------------------------------------------------------------------
         #outlier processing
         h4("Outlier Processing", style = "color:black;"),
         selectInput(
           inputId = "one.dc.os.pca.log",
           label = "Logarithm method",
           choices = c(
             "No log" = "no",
             "Log 2" = "log2",
             "Log e" = "loge",
             "Log 10" = "log10"
           ),
           selected = "log10"
         ),

         selectInput(
           inputId = "one.dc.os.pca.scale",
           label = "Scale method",
           choices = c(
             "Pareto scale" = "pareto",
             "Auto scale" = "auto",
             "No scale" = "no"
           ),
           selected = "auto"
         ),

         checkboxInput(
           inputId = "one.dc.os.pca.center",
           label = "Center or not",
           value = TRUE
         ),

         sliderInput(
           inputId = "one.dc.os.pca.ci.tol",
           label = "Samples will be considered as outliers outside % CI",
           min = 90,
           max = 100,
           value = 95,
           step = 1,
           round = TRUE
         ),
         sliderInput(
           inputId = "one.dc.os.zero.tol",
           label = "Samples will be considered as outliers with zero value ratio > %",
           min = 0,
           max = 100,
           value = 50,
           step = 0.5,
           round = TRUE
         ),

         checkboxInput("one.dc.qc.outlier.filter", label = "Filter QC outliers", value = FALSE),
         checkboxInput("one.dc.subject.outlier.filter", label = "Filter subject outliers", value = FALSE),
         tags$hr(style = "border-color:black;"),
         ##-------------------------------------------------------------------
         ##submit
         shinyalert::useShinyalert(),
         shinysky::actionButton(inputId = "oneDCsubmit", label = "Submit",
                                styleclass = "info",icon = icon("submit")),
         shinysky::actionButton(inputId = "oneDCparameter2resultDownload", label = "Next",
                                styleclass = "warning",icon = icon("submit")),
         helpText("Click", strong("Submit"), "to analyzer"),
         # textOutput(outputId = "metcleaning.info"),
         span(textOutput(outputId = "metcleaning.info"), style = "color:red"),
         br(), br(), br()
  )
)