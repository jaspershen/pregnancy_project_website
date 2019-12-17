# library(xcms)
# library(CAMERA)
library(shiny)
library(shinythemes)
library(shinysky)#github:devtools::install_github("AnalytixWare/ShinySky")
library(digest)
library(shinyIncubator)#github:devtools::install_github("rstudio/shiny-incubator")
library(colourpicker)
library(shinyBS)
# library(impute)#bioconductor
# library(missForest)
# library(pcaMethods)#bioconductor
library(shinyalert)
library(stats)
# library(beeswarm)
library(shinyWidgets)
# library(ellipse)
# library(pheatmap)#need install the newest version
# library(plsdepot)
# library(pls)
library(knitr)
library(pryr)
# library(Rdisop)
library(stringr)
library(plotly)
library(PerformanceAnalytics)
library(psych)
library(kableExtra)
# library(e1071)
# library(randomForest)
# library(pROC)
library(ggcorrplot)
library(pbapply)
library(zip)
library(DT)
# library(MetCleaning)#devtools::install_githb("jaspershen/MetCleaning")

##source functions
# source("function/tools.R")
# source("function/paraTrans.R")
# source("function/helper.R")

## function for data cleaning
# source("function/data_cleaning/checkData.R")
# source("function/data_cleaning/batchAlignment.R")
# source("function/data_cleaning/dataIntegration.R")
# source("function/data_cleaning/dataNormalization.R")
# source("function/data_cleaning/findOutlier.R")
# source("function/data_cleaning/mvFunction.R")
# source("function/data_cleaning/pcaFunction.R")
# source("function/data_cleaning/zeroFunction.R")

##function for metabolite identification
# source("function/metabolite_identification/miFunction.R")
# source("function/metabolite_identification/miCheckMS1.R")
# source("function/metabolite_identification/miCheckMS2.R")
# source("function/metabolite_identification/ms2MzRtPlot.R")
# source("function/metabolite_identification/formulationFunctions.R")

##function for data cleaning, one step
# source("function/one_data_cleaning/checkParamTable.R")

##function for merge identification
# source("function/merge_identification/checkDataMerge.R")
# source("function/merge_identification/mergeIden.R")

##function for pathway analysis
# source("function/pathway_analysis/pathwayEnrichment.R")
# source("function/pathway_analysis/paCheckData.R")

###source UI
source("ui/accountUI.R")
source("ui/commentUI.R")
source("ui/faqsUI.R")
source("ui/helpUI.R")
source("ui/introductionUI.R")
source("ui/linkUI.R")
source("ui/logInSignUpUI.R")
source("ui/otherUI.R")
source("ui/demoDataUI.R")

##data cleaning UI
##data cleaning UI
# source("ui/data_cleaning/dc_batch_alignment_ui.R")
# source("ui/data_cleaning/dc_data_check_ui.R")
# source("ui/data_cleaning/dc_data_integration_ui.R")
# source("ui/data_cleaning/dc_data_normalization_ui.R")
# source("ui/data_cleaning/dc_download_result_ui.R")
# source("ui/data_cleaning/dc_mv_processing_ui.R")
# source("ui/data_cleaning/dc_outlier_sample_ui.R")
# source("ui/data_cleaning/dc_qa1_ui.R")
# source("ui/data_cleaning/dc_qa2_ui.R")
# source("ui/data_cleaning/dc_ui.R")
# source("ui/one_data_cleaning/onedc_ui.R")
# source("ui/data_cleaning/dc_upload_ui.R")
# source("ui/data_cleaning/dc_zero_processing_ui.R")
# source("ui/metabolite_identification/mi_ui.R")
# source("ui/differential_analysis/da_ui.R")
# source("ui/merge_identification/merge_identification_ui.R")




shinyUI(
  fluidPage(
    title = "pPOP",
    # tags$head(
    #   tags$link(href = "style.css", rel = "stylesheet"),
    # 
    #   # Favicon
    #   tags$link(rel = "shortcut icon",
    #             type="image/x-icon",
    #             # href="http://daattali.com/shiny/img/favicon.ico"
    #             href = "metflow.ico"
    #             ),
    # 
    #   # Facebook OpenGraph tags
    #   tags$meta(property = "og:title", content = share$title),
    #   tags$meta(property = "og:type", content = "website"),
    #   # tags$meta(property = "og:url", content = share$url),
    #   # tags$meta(property = "og:image", content = share$image),
    #   # tags$meta(property = "og:description", content = share$description),
    # 
    #   # Twitter summary cards
    #   tags$meta(name = "twitter:card", content = "summary"),
    #   tags$meta(name = "twitter:site", content = paste0("@", share$twitter_user)),
    #   tags$meta(name = "twitter:creator", content = paste0("@", share$twitter_user)),
    #   tags$meta(name = "twitter:title", content = share$title),
    #   tags$meta(name = "twitter:description", content = share$description),
    #   tags$meta(name = "twitter:image", content = share$image)
    # ),

    # tags$a(
    #   href="http://www.zhulab.cn/",
    #   tags$img(style = "position: absolute; top: 4%; right: 1%; border: 0;",
    #            src = "zhulab logo1.png",
    #            alt = "Fork me on GitHub",
    #            height = "120",
    #            # weight = "100",
    #            align = "right"
    #   )
    # ),

    # tags$a(
    #   href="https://github.com/jaspershen",
    #   tags$img(style = "position: absolute; top: 2%; left: 1%; border: 0;",
    #            src = "metflow logo2.png",
    #            alt = "Fork me on GitHub",
    #            height = "150",
    #            # weight = "100",
    #            align = "left"
    #            )
    # ),

    # div(id = "header",
    #     div(id = "title",
    #         "Pregnancy Project at Stanford University"
    #     ),
    #     div(id = "subtitle",
    #         "Pregnancy Project at Stanford University"),
    #     div(id = "subsubtitle",
    #         "By",
    #         tags$a(href = "http://shenxt.sxl.cn/", "Xiaotao Shen",
    #                target = "_blank"),
    #         "and",
    #         tags$a(href = "http://www.zhulab.cn/members_show.php?id=1", "Dr. Liang Liang",
    #                target = "_blank"),
    #         "in",
    #         tags$a(href = "http://snyderlab.stanford.edu/", "Professor Michael Snyder lab",
    #                target = "_blank")
    #         # HTML("&bull;"),
    #         # "Available",
    #         # tags$a(href = "https://github.com/daattali/timevis", "on GitHub"),
    #         # HTML("&bull;"),
    #         # tags$a(href = "http://www.zhulab.cn/software.php", "More software",
    #         #        target = "_blank"), "by Zhu lab"
    #     )
    # ),

      tags$footer(h5("©",
                     # tags$a(href = "http://www.zhulab.cn/", "Zhu lab",
                     #        target = "_blank"),
                     "Snyder lab",
                     ", Department of Genetics, School of Medicine, Stanford University. All Rights Reserved.",
                     align = "center",
                     style = "font-weight: normal; font-family: arial; font-weight:400;"),
                     align = "center",
  style = 
  "
  position:fixed;
  bottom:0;
  left:0;
  right:0;
  color: white;
  padding: 0px;
  margin:0;
  background-color: black;
  box-sizing:border-box;
  z-index: 1000;
  height: 30px;
  margin-bottom: 0;
  "
  ),

  #avoid error
  # tags$style(type="text/css",
  #            ".shiny-output-error { visibility: hidden; }",
  #            ".shiny-output-error:before { visibility: hidden; }"
  # ),


    navbarPage(id = "initialTab",
               title = "",
               header = NULL,
               # shinythemes::themeSelector(),
               theme = shinythemes::shinytheme("cosmo"),
               inverse = FALSE,
               fluid = TRUE,
               selected = "Home",
               position = "static-top",
               # footer = "test",

               #---------------------------------------------------------------
               ##Home
               tabPanel(title = "Home",
                        icon = icon("home"),
                        uiOutput("introduction.area")
                        # style='width: 50%;'
               ),

               #---------------------------------------------------------------
               #log in and account
               tabPanel(value = "logInTab",
                        title = "Log in & Account",
                        icon = icon(name = "user-circle"),
                        uiOutput("account_area")
               ),

               #---------------------------------------------------------------
               #Analysis
               # navbarMenu(
               #   title = "Analysis",
               #   icon = icon(name = "calculator"),
               #   tabPanel("Data Cleaning",
               #            uiOutput("datacleaning.area")
               #            ),
               #   # tabPanel("Metabolite Identification",
               #   #          uiOutput("metaboliteidentification.area")
               #   # ),
               # 
               #   tabPanel("Differential Metabolite Discovery",
               #            uiOutput("differentialanalysis.area")
               #            ),
               # 
               #   # tabPanel("Data Cleaning(One Step)",
               #   #          uiOutput("oneDatacleaning.area")
               #   # ),
               # 
               #   tabPanel("Pathway Enrichment Analysis",
               #            uiOutput("pathwayAnalysis.area")
               #   )
               # 
               #   ),

               #---------------------------------------------------------------
               #Additional tools
               # navbarMenu(
               #            title = "Additional tools",
               #            icon = icon(name = "wrench"),
               #            tabPanel("Merge identification",
               #                     uiOutput("mergeidentification.area")
               #            )
               #            # tabPanel("Worklist generator",
               #            #          uiOutput("worklist.area")
               #            #          ),
               #            # tabPanel("MS2 plot generator"
               #            #          # uiOutput("ms2plot.area")
               #            #          )
               #            # tabPanel("MS/MS spectrum match",
               #            #          uiOutput("ms2.match.area")
               #            ),

               #---------------------------------------------------------------
               #Help
               # navbarMenu(icon = icon(name = "info", lib = "font-awesome"),
               #            title = "Help",
               #            tabPanel(title = a("MetFlow Tutorial",
               #                               href = "http://www.zhulab.cn/uploads/file/MetFlow/metflow_tutorial.html",
               #                               target = "_blank")
               #                     ),
               # 
               #            tabPanel(title = a("MetFlow Tutorial (pdf download)",
               #                               href = "http://www.zhulab.cn/uploads/file/MetFlow/metflow_tutorial.pdf",
               #                               target = "_blank")
               #                     ),
               #            tabPanel(title = "Demo data",
               #                     # icon = icon("database"),
               #                     uiOutput("demo.data.area")
               #                     )
               # ),
               
               #---------------------------------------------------------------
               #Data
               tabPanel(title = "Data",
                        icon = icon(name = "archive"),
                        fluidPage(
                          uiOutput("data.area")
                        )
               ),

               #---------------------------------------------------------------
               #Comments
               # tabPanel(title = "Comments",
               #          icon = icon(name = "comments"),
               #          fluidPage(
               #            uiOutput("comment.area")
               #          )
               # ),

               #---------------------------------------------------------------
               #Link
               # tabPanel(title = "Links",
               #          icon = icon(name = "link"),
               #          uiOutput("link.area")
               # ),

               #---------------------------------------------------------------
               #Sign up
               tabPanel(value = "signUpTab",
                        title = "Sign up",
                        icon = icon(name = "user-plus"),
                        uiOutput("signup_area")
               )

               #---------------------------------------------------------------
               #Reset password
               # tabPanel(value = "resetPassword",
               #          title = "Reset password",
               #          icon = icon(name = "key"),
               #          uiOutput("resetPassword_area")
               # )

                )
  )
)
