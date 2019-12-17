############################################################################
##log in UI
uiLogin <- function(){
  tagList(
    fluidPage(
      br(), br(),br(),
      wellPanel(
        # span(h5("(Test account: user name: test, password: test)"), style = "color:red"),
        textInput(inputId = "user_name",
                  label = "User name",
                  placeholder = "Your user name",
                  width = "800px"),
        passwordInput(inputId = "password",
                      label = "Password",
                      placeholder = "Your password",
                      width = "800px"),
        useShinyalert(),
        actionButton("login_button",
                     "Log in",
                     styleclass = "info"),

        actionButton("jumpToSignUpTab",
                     label = "Sign up",
                     block = FALSE,
                     icon = "upload",
                     styleclass = "info"),
        h5("If you don't have a account,",br(),
           "Please sign up first!", style = "color:black;font-weight: normal;")
        # actionButton("jumpToResetPassword",
        #              label = "Reset password",
        #              block = FALSE,
        #              icon = "upload",
        #              styleclass = "warning"),
        # h5("If you forget your password,",br(),
        #    "Please Click Reset password.", style = "color:black;font-weight: normal;")
      )
    )
  )
}



#####Ui for not log in of analysis page
not_logged_in2 <- function(){
  tagList(
    div(img(src='sorry2.gif',
            # height = 400,
            width = 400
    ), style="text-align: center;"),

    h3("Please click Log in & Account to log in first.",
      align = "center"),

     fluidRow(column(6, align = "center", offset = 3,
                     actionButton("jumpToInitialTab2",
                                  label = "Log in",
                                  styleclass = "info")
     ))
  )
}


not_logged_in3 <- function(){
  tagList(
    div(img(src='sorry2.gif',
            # height = 400,
            width = 400
    ), style="text-align: center;"),

    h3("Please click Log in & Account to log in first.",
       align = "center"),

    fluidRow(column(6, align = "center", offset = 3,
                    actionButton("jumpToInitialTab3",
                                 label = "Log in",
                                 styleclass = "info")
    ))
  )
}

not_logged_in4 <- function(){
  tagList(
    div(img(src='sorry2.gif',
            # height = 400,
            width = 400
    ), style="text-align: center;"),

    h3("Please click Log in & Account to log in first.",
       align = "center"),

    fluidRow(column(6, align = "center", offset = 3,
                    actionButton("jumpToInitialTab4",
                                 label = "Log in",
                                 styleclass = "info")
    ))
  )
}

not_logged_in5 <- function(){
  tagList(
    div(img(src='sorry2.gif',
            # height = 400,
            width = 400
    ), style="text-align: center;"),

    h3("Please click Log in & Account to log in first.",
       align = "center"),

    fluidRow(column(6, align = "center", offset = 3,
                    actionButton("jumpToInitialTab5",
                                 label = "Log in",
                                 styleclass = "info")
    ))
  )
}



not_logged_in6 <- function(){
  tagList(
    div(img(src='sorry2.gif',
            # height = 400,
            width = 400
    ), style="text-align: center;"),

    h3("Please click Log in & Account to log in first.",
       align = "center"),

    fluidRow(column(6, align = "center", offset = 3,
                    actionButton("jumpToInitialTab6",
                                 label = "Log in",
                                 styleclass = "info")
    ))
  )
}


not_logged_in7 <- function(){
  tagList(
    div(img(src='sorry2.gif',
            # height = 400,
            width = 400
    ), style="text-align: center;"),

    h3("Please click Log in & Account to log in first.",
       align = "center"),

    fluidRow(column(6, align = "center", offset = 3,
                    actionButton("jumpToInitialTab7",
                                 label = "Log in",
                                 styleclass = "info")
    ))
  )
}


not_logged_in8 <- function(){
  tagList(
    div(img(src='sorry2.gif',
            # height = 400,
            width = 400
    ), style="text-align: center;"),
    
    h3("Please click Log in & Account to log in first.",
       align = "center"),
    
    fluidRow(column(6, align = "center", offset = 3,
                    actionButton("jumpToInitialTab8",
                                 label = "Log in",
                                 styleclass = "info")
    ))
  )
}



################################################################################
#sign up UI
uiSignup <- function(){
  tagList(
    fluidPage(
      br(), br(), br(),
      wellPanel(
        textInput(inputId = "username.signup",
                  label = "User name",
                  placeholder = "Example: Jasper Shen",
                  width = "800px"),
        passwordInput(inputId = "passwd.signup",
                      label = "Password",
                      placeholder = "More than 6",
                      width = "800px"),
        selectInput(inputId = "country.signup",
                    label = "Country or region",
                    choices = country,
                    selected = "United States", multiple = FALSE,
                    width = "800px"),
        textInput(inputId = "org.signup",
                    label = "Organization",
                  placeholder = "For example: Stanford University",
                  width = "800px"),
        textInput("email.signup",
                  "Email address",
                  placeholder = "For example: user_name@stanford.edu",
                  width = "800px"),
        # br(),br(),
        actionButton("signup_button",
                     "Sign up",
                     styleclass = "info")
      )
    )
  )
}



##reset UI
uiReset <- function(){
  tagList(
    fluidPage(
      wellPanel(
        textInput(inputId = "username.reset",
                  label = "User name",
                  placeholder = "Your user name",
                  width = "800px"),
        textInput("email.reset",
                  "Email address",
                  placeholder = "Your email",
                  width = "800px"),

        passwordInput(inputId = "passwd.reset",
                      label = "New password",
                      placeholder = "New password",
                      width = "800px"),
        # br(),br(),
        actionButton("reset_button",
                     "Submit",
                     styleclass = "info")
      )
    )
  )
}



##sign up information
# uiSignup <- function(){
#   tagList(
#     fluidPage(
#       wellPanel(
#         textInput("username.signup",
#                   "User name", width = "800px"),
#         passwordInput("passwd.signup",
#                       "Password", width = "800px"),
#         selectInput(inputId = "country.signup",
#                     label = "Country or region",
#                     choices = country,
#                     selected = "China", multiple = FALSE,
#                     width = "800px"),
#         textInput("org.signup",
#                   "Organization",
#                   width = "800px"),
#         textInput("email.signup",
#                   "Email address", width = "800px"),
#         actionButton("signup_button",
#                      "Submit",
#                      styleclass = "info")
#       )
#     )
#   )
# }


ui.signup.success <- function(){
  tagList(
    br(), br(), br(),
    div(img(src='congratugation.jpg',
            height = 240, width = 889
    ), style="text-align: center;"),
    h3(
      "Thank you for your register!",
      align = "center"),
    fluidRow(column(6, align = "center", offset = 3,
                    actionButton("jumpToInitialTab7",
                                 label = "Log in",
                                 styleclass = "info")
    ))

  )
}


ui.reset.success <- function(){
  tagList(
    br(), br(), br(),
    div(img(src='congratugation.jpg',
            height = 240, width = 889
    ), style="text-align: center;"),
    h3(
      "Please refresh the page and use your new password to log in.",
      align = "center")
  )
}
