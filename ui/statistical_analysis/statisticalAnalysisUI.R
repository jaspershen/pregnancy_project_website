###statistical analysis UI
ui.statisticalanalysis <- function(){
  tagList(
    # tabPanel(title = "Analysis", icon = icon(name = "calculator"),
             tabsetPanel(
                         id = "saTab",
                         type = "pills",
                         tabPanel(title = "Intrduction",
                                  icon = icon("question-circle"),
                                  uiOutput("sa.introduction.area")),
                         navbarMenu(title = "Univariate analysis",
                                    # id = "uaTab",
                                    # id = "uaTab",
                                    # selected = "uaFCtab",
                                    tabPanel(value = "uaFCtab",
                                             title = "Fold change",
                                             sidebarLayout(
                                               sidebarPanel = sidebarPanel(
                                               uiOutput(outputId = "ua.fc.area")
                                               ),
                                               mainPanel = mainPanel(
                                                 uiOutput(outputId = "ua.fc.result.area")
                                               ))
                                             ),
                                    tabPanel(value = "uaTtestTab",
                                             title = "Student's t test",
                                             sidebarLayout(
                                               sidebarPanel = sidebarPanel(
                                                 uiOutput(outputId = "ua.ttest.area")
                                               ),
                                               mainPanel = mainPanel(
                                                 uiOutput(outputId = "ua.ttest.result.area")
                                               ))
                                             ),
                                    tabPanel(value = "uaWilcoxTab",
                                             title = "Wilcoxson test",
                                             sidebarLayout(
                                               sidebarPanel = sidebarPanel(
                                                 uiOutput(outputId = "ua.wilcox.area")
                                               ),
                                               mainPanel = mainPanel(
                                                 uiOutput(outputId = "ua.wilcox.result.area")
                                               ))
                                             ),
                                    tabPanel(value = "uaANOVAtab",
                                             title = "ANOVA",
                                             sidebarLayout(
                                               sidebarPanel = sidebarPanel(
                                                 uiOutput(outputId = "ua.anova.area")
                                               ),
                                               mainPanel = mainPanel(
                                                 uiOutput(outputId = "ua.anova.result.area")
                                               ))
                                             )
                                    ),
                         navbarMenu(title = "Multiple variate analysis",
                                    # id = "maTab",
                                    # selected = "maPCAtab",
                                    tabPanel(value = "maPCAtab",
                                                title = "PCA analysis",
                                             sidebarLayout(
                                               sidebarPanel = sidebarPanel(
                                                 uiOutput(outputId = "ma.pca.area")
                                               ),
                                               mainPanel = mainPanel(
                                                 uiOutput(outputId = "ma.pca.result.area")
                                               ))
                                             ),
                                    tabPanel(value = "maPLStab",
                                                title = "PLS-DA analysis",
                                             sidebarLayout(
                                               sidebarPanel = sidebarPanel(
                                                 # "test"
                                                 uiOutput(outputId = "ma.pls.area")
                                               ),
                                               mainPanel = mainPanel(
                                                 # "test"
                                                 uiOutput(outputId = "ma.pls.result.area")
                                               ))
                                             ),
                                    tabPanel(value = "maHCAtab",
                                                title = "Hierarchical clustering analysis",
                                             sidebarLayout(
                                               sidebarPanel = sidebarPanel(
                                                 uiOutput(outputId = "ma.hca.area")
                                               ),
                                               mainPanel = mainPanel(
                                                 uiOutput(outputId = "ma.hca.result.area")
                                               ))
                                             )
                                    )
             ),
             br(), br(), br()
    # )
  )}












