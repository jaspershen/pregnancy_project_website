fluidPage(
  column(width = 4,
         wellPanel(
           textInput(inputId = "PAprojectID",
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

           selectInput(inputId = "paMetaboliteType",
                       label = "Metabolite type",
                       choices = c(
                         # "Metabolite name" = "pa.met.name",
                         "KEGG ID" = "kegg.id",
                         "Peak (m/z)" = "peak.mz"
                       ),
                       selected = "kegg.id"),

           # conditionalPanel(condition = "input.paMetaboliteType == 'kegg.id'",
                            textAreaInput(inputId = "pa.met.name.list",
                                          label = h5(
                                            "Metabolite list",
                                            shinyBS::tipify(
                                              el = icon(name = "info-circle"),
                                              placement = "bottom",
                                              trigger = "hover",
                                              title = "One column, no header like example shows"
                                            )
                                          ),
                                          value = example.compound.list,
                                          width = "400px", height = "200px"),
                            # ),

           conditionalPanel(condition = "input.paMetaboliteType == 'peak.mz'",

                            # textAreaInput(inputId = "pa.met.name.list2",
                            #               label = h5(
                            #                 "Metabolite list",
                            #                 shinyBS::tipify(
                            #                   el = icon(name = "info-circle"),
                            #                   placement = "bottom",
                            #                   trigger = "hover",
                            #                   title = "One column, no header like example shows"
                            #                 )
                            #               ),
                            #               value = example.compound.list2,
                            #               width = "400px", height = "200px"),



                            selectInput(inputId = "paMZpolarity",
                                        label = "Ionization polarity",
                                        choices = c(
                                          "Positive" = "positive",
                                          "Negative" = "negative"
                                        ),
                                        selected = "positive"),

                            conditionalPanel(condition = "input.paMZpolarity == 'positive'",
                                             selectInput(inputId = "pa.mz.pos.adduct",
                                                         label = "Adduct type",
                                                         choices = c(
                                                           "[M+H]+" = "M+H",
                                                           "[M+K]+" = "M+K",
                                                           "[M+Na]+" = "M+Na",
                                                           "[M+NH4]+" = "M+NH4"
                                                         ),
                                                         selected = c("M+H",
                                                                      "M+K",
                                                                      "M+Na",
                                                                      "M+NH4"),
                                                         multiple = TRUE)
                            ),

                            conditionalPanel(condition = "input.paMZpolarity == 'negative'",
                                             selectInput(inputId = "pa.mz.neg.adduct",
                                                         label = "Adduct type",
                                                         choices = c(
                                                           "[M-H]-" = "M-H",
                                                           "[M+Cl]+" = "M+Cl",
                                                           "[M+CH3COO]+" = "M+CH3COO"
                                                         ),
                                                         selected = c("pa.minus.h",
                                                                      "pa.plus.cl",
                                                                      "pa.plus.ch3coo"),
                                                         multiple = TRUE)
                            ),

                            sliderInput(inputId = "pa.mz.match.mz.tol",
                                        label = "m/z macth tolerance (ppm)",
                                        min = 1, max = 50, value = 25),

                            selectInput(inputId = "pa.mz.match.library",
                                        label = "Database for metabolite identification",
                                        choices = c(
                                          # "HMDB" = "hmdb",
                                          "KEGG" = "kegg"
                                        ),
                                        selected = c("kegg"),
                                        multiple = FALSE
                            )
                            ),

           useShinyalert(),
           actionButton(
             inputId = 'pa.enter.button',
             label = "Submit",
             styleclass = "info",
             # class = "btn-primary",
             icon = icon('play-circle')
           )
         )
  ),

  column(width = 7,
         includeHTML(path = "data/html/introduction_pa_data_upload.html"),
         br(), br(), br()
  )

)
# }