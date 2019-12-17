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
source("ui/dataUI.R")
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




##main function
shinyServer(function(session, input, output) {

  ##upload data size limination, max limination is 100 M
  options(shiny.maxRequestSize = 50*1024^2)

  ##UI for introduction page
  output$introduction.area <- renderUI(ui.introduction())

  ##time
  output$time <- renderText({
    invalidateLater(as.integer(1), session)
    format(Sys.time())
  })

  ## user number
  output$user.num <- renderText({
    credentials <- readRDS("credentials/credentials.rds")
    paste("Registered Users: ", nrow(credentials))
    # rm(credentials)
  })


  ## jobNumber number
  # output$job.num <- renderText({
  #   load("credentials/jobNumber")
  #   paste("Completed Jobs: ", jobNumber)
  # })

  # output$job.num <- renderText({
  #   credentials <- readRDS("credentials/credentials.rds")
  #   jobNumber <- sum(as.numeric(credentials$job.number))
  #   paste("Completed Jobs: ", jobNumber)
  # })


  # output$help.area <- renderUI(ui.help())
  # output$faqs.area <- renderUI(ui.faqs())
  output$link.area <- renderUI(ui.link())
  # output$data.area <- renderUI(ui.data())
  # output$demo.data.area <- renderUI(ui.demo.data())

  # output$metflow.tutoriala <- renderUI({
  #   tags$iframe(src = "https://cn.bing.com/")
  # })

###########################################################################
################log in function############################################
  ##### UI code for login page
  output$not_logged_in <- renderUI(fluidPage(
      column(width = 3, offset = 4,
             uiLogin(),
             uiOutput("pass")
      )
  ))

  observeEvent(input$jumpToInitialTab2, {
    updateTabsetPanel(session, "initialTab",
                      selected = "logInTab")
  })

  observeEvent(input$jumpToInitialTab3, {
    updateTabsetPanel(session, "initialTab",
                      selected = "logInTab")
  })

  observeEvent(input$jumpToInitialTab4, {
    updateTabsetPanel(session, "initialTab",
                      selected = "logInTab")
  })

  observeEvent(input$jumpToInitialTab5, {
    updateTabsetPanel(session, "initialTab",
                      selected = "logInTab")
  })

  observeEvent(input$jumpToInitialTab6, {
    updateTabsetPanel(session, "initialTab",
                      selected = "logInTab")
  })

  observeEvent(input$jumpToInitialTab7, {
    updateTabsetPanel(session, "initialTab",
                      selected = "logInTab")
  })

  observeEvent(input$jumpToInitialTab8, {
    updateTabsetPanel(session, "initialTab",
                      selected = "logInTab")
  })


  observeEvent(input$jumpToSignUpTab, {
    updateTabsetPanel(session, "initialTab",
                      selected = "signUpTab")
  })

  observeEvent(input$jumpToResetPassword, {
    updateTabsetPanel(session, "initialTab",
                      selected = "resetPassword")
  })


  #### PASSWORD server code ----------------------------------------------------
  # reactive value containing user's authentication status
  user_input <- reactiveValues(authenticated = FALSE,
                               valid_credentials = FALSE,
                               user_locked_out = FALSE,
                               status = "",
                               username = "")

  # user_input <- reactiveValues(authenticated = TRUE,
  #                              valid_credentials = TRUE,
  #                              user_locked_out = FALSE,
  #                              status = "",
  #                              username = "shen")

  ##############################################################################

  # authenticate user by:
  #   1. checking whether their user name and password are in the credentials
  #       data frame and on the same row (credentials are valid)
  #   2. if credentials are valid, retrieve their lockout status from the data frame
  #   3. if user has failed login too many times and is not currently locked out,
  #       change locked out status to TRUE in credentials DF and save DF to file
  #   4. if user is not authenticated, determine whether the user name or the password
  #       is bad (username precedent over pw) or he is locked out. set status value for
  #       error message code below
  observeEvent(input$login_button, {
    credentials <- readRDS("credentials/credentials.rds")
    row_username <- which(credentials$user == input$user_name)
    row_password <- which(credentials$pw == digest(input$password)) # digest() makes md5 hash of password

    # if user name row and password name row are same, credentials are valid
    #   and retrieve locked out status
    if (length(row_username) == 1 &&
        length(row_password) >= 1 &&  # more than one user may have same pw
        (row_username %in% row_password)) {
      user_input$valid_credentials <- TRUE
      user_input$user_locked_out <- credentials$locked_out[row_username]
    }

    # if user is not currently locked out but has now failed login too many times:
    #   1. set current lockout status to TRUE
    #   2. if username is present in credentials DF, set locked out status in
    #     credentials DF to TRUE and save DF
    if (input$login_button == num_fails_to_lockout &
        user_input$user_locked_out == FALSE) {

      user_input$user_locked_out <- TRUE

      if (length(row_username) == 1) {
        credentials$locked_out[row_username] <- TRUE
        saveRDS(credentials, "credentials/credentials.rds")
      }
    }

    # if a user has valid credentials and is not locked out, he is authenticated
    if (user_input$valid_credentials == TRUE & user_input$user_locked_out == FALSE) {
      user_input$authenticated <- TRUE
      user_input$username <- input$user_name
    } else {
      user_input$authenticated <- FALSE
    }

    # if user is not authenticated, set login status variable for error messages below
    if (user_input$authenticated == FALSE) {
      if (user_input$user_locked_out == TRUE) {
        user_input$status <- "locked_out"
      } else if (length(row_username) > 1) {
        user_input$status <- "credentials_data_error"
      } else if (input$user_name == "" || length(row_username) == 0) {
        user_input$status <- "bad_user"
      } else if (input$password == "" || length(row_password) == 0) {
        user_input$status <- "bad_password"
      }
    }
  })


  ###log out
  observeEvent(eventExpr = input$logout, {
    user_input$authenticated <- FALSE
  })


  # red error message if bad credentials
  output$pass <- renderUI({
    if (user_input$status == "locked_out") {
      h5(strong(paste0("Your account is locked because of too many\n",
                       "failed login attempts. Contact administrator(shenxt1990@163.com)."),
                style = "color:red"), align = "center")
    } else if (user_input$status == "credentials_data_error") {
      h5(strong("Credentials data error - contact administrator!", style = "color:red"), align = "center")
    } else if (user_input$status == "bad_user") {
      h5(strong("User name not found!", style = "color:red"), align = "center")
    } else if (user_input$status == "bad_password") {
      h5(strong("Incorrect password!", style = "color:red"), align = "center")
    } else {
      ""
    }
  })


  observeEvent(input$login_button, {
    if(user_input$authenticated == TRUE){
      shinyalert(title = paste("Welcome back!", user_input$username),
                 text = "Now you can click Analysis to analyze your data.", type = "success")
    }
  })






#------------------------------------------------------------------------------
  ##### UI code for Signup page
  output$not_signed_up <- renderUI(
    fluidPage(
      column(width = 3, offset = 4,
             uiSignup(),
             uiOutput("signup.info")
      )

  ))

  output$signed_up <- renderUI(ui.signup.success())


  output$signup_area <- renderUI({
    if (user_signup$valid.signup == TRUE){
      uiOutput("signed_up")
    }else{
      uiOutput("not_signed_up")
    }
  })



  #reset password
  # user_reset <- reactiveValues(valid.reset = FALSE,
  #                              status = "")
  # 
  # observeEvent(eventExpr = input$jumpToResetPassword, {
  #   user_reset$valid.reset <- FALSE
  #   user_reset$status <- ""
  # })
  # 
  # 
  # output$reset_success <- renderUI(ui.reset.success())
  # 
  # output$not_reset_success <- renderUI(
  #   fluidPage(
  #     column(width = 3, offset = 4,
  #            uiReset(),
  #            uiOutput("reset.info")
  #     )
  # 
  #   ))
  # output$resetPassword_area <- renderUI({
  #   if (user_reset$valid.reset == TRUE){
  #     uiOutput("reset_success")
  #   }else{
  #     uiOutput("not_reset_success")
  #   }
  # })



  ###account page
  output$account_area <- renderUI({
    if (user_input$authenticated == TRUE){
      ui.account()
    }else{
      uiOutput("not_logged_in")
    }
  })

  ##user information
  credentials <- eventReactive(eventExpr = input$login_button,{
    credentials <- readRDS("credentials/credentials.rds")
    credentials
  })

  output$profileUser <- renderText(
    paste("User name: ",
          as.character(credentials()[credentials()[,1] == input$user_name,"user"]),
          sep = "")
  )

  output$profileCountry <- renderText(
    paste("Country: ",
          as.character(credentials()[credentials()[,1] == input$user_name,"country"]),
          sep = "")
  )

  output$profileOrg <- renderText(
    paste("Organization: ",
          as.character(credentials()[credentials()[,1] == input$user_name,"org"]),
          sep = "")
  )

  output$profileEmail <- renderText(
    paste("Email: ",
          as.character(credentials()[credentials()[,1] == input$user_name,"email"]),
          sep = "")
  )


  #### PASSWORD server code ----------------------------------------------------
  # reactive value containing user's sign up status
  user_signup <- reactiveValues(valid.signup = FALSE,
                                status = "")

  # observeEvent(input$signup_button, {
  #   user_signup$valid.signup <- FALSE
  #   user_signup$status <- ""
  # })

  # valid signup by:
  #   1. username.signup, passwd.signup, country, org.singup and email.signup are valid.
  #   2. username is not in the credentials

  observeEvent(input$signup_button, {
    credentials <- readRDS("credentials/credentials.rds")
     if(
       (input$username.signup != "") &
       (input$passwd.signup != "") &
       (input$country.signup != "") &
       (input$org.signup != "") &
       (input$email.signup != "")
     ){
       if(!any(input$username.signup %in% credentials$user)){
       newuser <- c(input$username.signup,#user name
                    digest::digest(input$passwd.signup),#pass word
                    FALSE,#lock out
                    input$country.signup,#country
                    input$org.signup,#organiation
                    input$email.signup,#email
                    0#job.number
                    )
       credentials <- rbind(credentials, newuser)
       credentials$job.number <- as.numeric(credentials$job.number)
       credentials$locked_out <- as.logical(credentials$locked_out)
       saveRDS(credentials, "credentials/credentials.rds")
       user_signup$valid.signup <- TRUE
       dir.create(file.path("./user_data", input$username.signup))
       }
     }

    # if user is not authenticated, set login status variable for error messages below
    if (user_signup$valid.signup == FALSE) {
      if(input$email.signup == "") {user_signup$status <- "no.email"}
      if(input$org.signup == "") {user_signup$status <- "no.org"}
      if(input$country.signup == "") {user_signup$status <- "no.country"}
      if(input$passwd.signup == "") {user_signup$status <- "no.passwd"}
      if(input$username.signup == "") {user_signup$status <- "no.username"}

      if(input$username.signup != ""){
        if(any(input$username.signup %in% credentials$user)){
          user_signup$status <- "duplicated.username"
            # paste0("User [", input$username.signup, "] already exist. Choose different user names.")
        }
      }

    }
  })

  # red error message if bad sigh up
  output$signup.info <- renderUI({
    if(user_signup$status == "no.username" |
       user_signup$status == "no.passwd" |
       user_signup$status == "no.country" |
       user_signup$status == "no.org" |
       user_signup$status == "no.email"
       ) {
      h5(strong("User name, password, country, organization and email address should not be space!", style = "color:red"), align = "center")
    }else{
      if(user_signup$status == "duplicated.username") {
        h5(strong("This user name already exists-choose a different user name.", style = "color:red"), align = "center")
      }
    }


  })

###reset password
  # observeEvent(input$reset_button, {
  #   credentials <- readRDS("credentials/credentials.rds")
  #   # cat("test1\n")
  #   if(input$username.reset != "" & input$email.reset != "" & input$passwd.reset != ""){
  #     # cat("test2\n")
  #     temp.idx1 <- grep(input$username.reset, credentials$user)
  #     temp.idx2 <- grep(input$email.reset, credentials$email)
  #     temp.idx <- intersect(temp.idx1, temp.idx2)[1]
  #     if(length(temp.idx) != 0){
  #       credentials$pw[temp.idx] <- digest::digest(input$passwd.reset)
  #       saveRDS(credentials, "credentials/credentials.rds")
  #       user_reset$valid.reset <- TRUE
  #       # cat("test3\n")
  #     }else{
  #       user_reset$status <- "wrong.username"
  #       user_reset$status <- "wrong.email"
  #       # cat("test4\n")
  #     }
  #   }
  # 
  #   # if user is not authenticated, set login status variable for error messages below
  #   if (user_reset$valid.reset == FALSE) {
  #     if(input$username.reset == "") {user_reset$status <- "wrong.username"; return()}
  #     if(input$email.reset == "") {user_reset$status <- "wrong.email"; return()}
  #     if(input$passwd.reset == "") {user_reset$status <- "no.password"; return()}
  #   }else{
  #     user_reset$status <- ""
  #   }
  # })


  # red error message if bad reset
  # output$reset.info <- renderUI({
  #   if(user_reset$status == "wrong.username" | user_reset$status == "wrong.email") {
  #     h5(strong("User name or email is wrong", style = "color:red"), align = "center")
  #   }else{
  #     if(user_reset$status == "no.password"){
  #       h5(strong("Please enter new password!", style = "color:red"), align = "center")
  #     }else{
  #       ""
  #     }
  #   }
  # })


  ##create folder
  # observe({
  #   if(user_input$authenticated == TRUE){
  #     dir.create(file.path("./user_data", user_input$username))
  #     dir.create(file.path("./user_data", user_input$username, "test"))
  #     dir.create(file.path("./user_data", user_input$username, "test/data_cleaning"))
  #   }
  # })


  #-----------------------------------------------------------------------------
  ## data cleaning functions, in the R file named dataCleaning
  # source("function/data_cleaning/dataCleaning.R", local = TRUE)$value

  #-----------------------------------------------------------------------------
  ## data cleaning functions, one step
  # source("function/one_data_cleaning/oneDataCleaning.R", local = TRUE)$value


  #-----------------------------------------------------------------------------
  ##metabolite identification functions, in the R file named metaboliteIdentification
  # source("function/metabolite_identification/metaboliteIdentification.R", local = TRUE)$value


  #-----------------------------------------------------------------------------
  ##differential analysis functions, in the R file named differentialAnalysis
  # source("function/differential_analysis/differentialAnalysis.R", local = TRUE)$value


  #-----------------------------------------------------------------------------
  ##pathway enrichment analysis
  # source("function/pathway_analysis/pathwayAnalysis.R", local = TRUE)$value

  ##merge identification
  # source("function/merge_identification/merge_identification.R", local = TRUE)$value
  
  
  output$data.area <- renderUI({
    if (user_input$authenticated == TRUE){
      ui.data()
    }else{
      not_logged_in8()
    }
  })
  
}
)