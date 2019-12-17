ui.account <- function(){
  tagList(
    fluidPage(
          column(width = 8,
                 actionButton(inputId = "logout",
                              label = "Log out",
                              styleclass = "info",
                              icon = icon("power-off")),
                 h4("Profile", style = "color:red"),
                           hr(),
                           textOutput(outputId = "profileUser"),
                           hr(),
                           textOutput(outputId = "profileCountry"),
                           hr(),
                           textOutput(outputId = "profileOrg"),
                           hr(),
                 textOutput(outputId = "profileEmail"),
                 # hr(),
                 # h4("Your project", style = "color:red"),
                 # hr(),
                 # dataTableOutput("yourProject"),
                 # br(), br(), br(),
                 offset = 2
          )
    )
  )
}