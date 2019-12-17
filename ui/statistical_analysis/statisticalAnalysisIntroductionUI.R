ui.saIntroduction <- function(){
  tagList(
    fluidPage(
      sidebarLayout(sidebarPanel = sidebarPanel(
        tags$style(".well {background-color: white;}"),
        # br(), br(),
        h4("News", align = "left", style = "color:lightseagreen"),
        h5("Version 0.99.1 (2018.2.14)", align = "left",
           style="font-weight: normal;line-height:20px;"),
        h5("New feature: The data cleaning methods are added.", align = "justify",
           style="font-weight: normal;line-height:20px;")
      ),
      mainPanel = mainPanel(includeHTML(path = "./data/html/statisticalIntroduction.html")
                            ),
      position = "right"
      )
      )
    )
}