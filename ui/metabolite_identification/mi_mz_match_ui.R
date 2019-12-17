
fluidPage(
  # headerPanel(title = "Upload data"),
  column(width = 4,
         wellPanel(
           tabPanel(
            "Parameter Setting",
             value = "mi.mz.parameter.setting",
             selectInput(inputId = "miMZpolarity",
                         label = "Ionization polarity",
                         choices = c(
                           "Positive" = "positive",
                           "Negative" = "negative"
                         ),
                         selected = "positive"),

             conditionalPanel(condition = "input.miMZpolarity == 'positive'",
                              selectInput(inputId = "mi.mz.pos.adduct",
                                          label = "Adduct type",
                                          choices = c(
                                            "[M+H]+" = "mi.plus.h",
                                            "[M+K]+" = "mi.plus.k",
                                            "[M+Na]+" = "mi.plus.na",
                                            "[M+NH4]+" = "mi.plus.nh4"
                                          ),
                                          selected = c("mi.plus.h",
                                                          "mi.plus.k",
                                                          "mi.plus.na",
                                                          "mi.plus.nh4"),
                                          multiple = TRUE)
                              ),

             conditionalPanel(condition = "input.miMZpolarity == 'negative'",
                              selectInput(inputId = "mi.mz.neg.adduct",
                                          label = "Adduct type",
                                          choices = c(
                                            "[M-H]-" = "mi.minus.h",
                                            "[M+Cl]+" = "mi.plus.cl",
                                            "[M+CH3COO]+" = "mi.plus.ch3coo"
                                          ),
                                          selected = c("mi.minus.h",
                                                          "mi.plus.cl",
                                                          "mi.plus.ch3coo"),
                                          multiple = TRUE)
                              ),

             sliderInput(inputId = "mi.mz.match.mz.tol",
                         label = "m/z macth tolerance (ppm)",
                         min = 1, max = 200, value = 25),

             selectInput(inputId = "mi.mz.match.library",
                         label = "MS1 library",
                         choices = c(
                           # "HMDB" = "hmdb",
                           "KEGG" = "kegg"
                         ),
                         selected = c("kegg"),
                         multiple = FALSE
                         ),
             actionButton(
               inputId = 'mi.mz.match.button',
               label = "Submit",
               styleclass = "info",
               icon = icon('play-circle')
             ),
             actionButton(inputId = "mi.mz.match.2.ms2.match",
                         label = "Next", styleclass = "warning"),

             helpText("Click", strong("Submit"), "to identified metabolite."),
             br(),
             br(),
             br()
             )
           )
  ),

  column(width = 7,
         actionButton(inputId = "miMZgo1",
                      label = "Identification information",
                      styleclass = "info"),
         br(), br(),
         bsModal(id = "miExample1",
                 title = "Match information",
                 trigger = "miMZgo1",
                 size = "large",
                 textOutput(outputId = "mi.peak.ms1.match.info1"),
                 DT::dataTableOutput(outputId = "mi.peak.ms1.match.info2")
         ),
         DT::dataTableOutput(outputId = "mi.mz.match.result")
  )
)
