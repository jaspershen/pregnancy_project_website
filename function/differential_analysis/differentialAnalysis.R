#####################################upload data###############################
output$differentialanalysis.area <- renderUI({
  if (user_input$authenticated == TRUE){
    ui.differentialanalysis()
  }else{
    not_logged_in6()
  }
})

##warning if no data selected or project name is null
observeEvent(eventExpr = input$da.upload.button, {
  #if don't use demo data and don't upload data or give project name, error
  if(input$da.use.demo.data == FALSE){
    if(is.null(input$DAms1PeakTable) | is.null(input$DAsampleInfo) | input$DAprojectID == ""){
      shinyalert::shinyalert(title = "Error",
                             text = "Project name, MS1 peak table and Sample information are required.",
                             type = "error")
    }
  }
})


observeEvent(eventExpr = input$da.upload.button, {
  #if don't use demo data and don't upload data or give project name, error
  if(input$da.use.demo.data == TRUE){
    if(input$DAprojectID == ""){
      shinyalert::shinyalert(title = "Error",
                             text = "Project name is required.",
                             type = "error")
    }
  }
})


##create folder
observeEvent(eventExpr = input$da.upload.button, {
  if(!is.null(input$DAprojectID)){
    if(input$DAprojectID != ""){
      dir.create(file.path("./user_data", user_input$username))
      dir.create(file.path("./user_data", user_input$username, input$DAprojectID))
      dir.create(file.path("./user_data", user_input$username, input$DAprojectID, "differential_analysis"))
    }
  }
})

##user.path
user.path <- reactive({
  req(user_input$username)
  req(input$DAprojectID)
  file.path("user_data", user_input$username, input$DAprojectID, "differential_analysis")
})

###record job number
# observeEvent(input$da.upload.button,{
#   if(!is.null(input$DAprojectID)){
#     if(input$DAprojectID != ""){
#       load("credentials/jobNumber")
#       jobNumber <- jobNumber + 1
#       save(jobNumber, file = "credentials/jobNumber")
#     }
#   }
# })


# ###record job number
observeEvent(input$da.upload.button,{
  if(!is.null(input$DAprojectID)){
    if(input$DAprojectID != ""){
      credentials <- readRDS("credentials/credentials.rds")
      credentials$job.number[match(user_input$username, credentials$user)] <- as.numeric(credentials$job.number[match(user_input$username, credentials$user)])+1
      saveRDS(credentials, "credentials/credentials.rds")
    }
  }
})



















# -------------------------------------------------------------------------------
###read MS1 peak table
da.ms1.raw <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$da.upload.button, {
  withProgress(message = 'Upload MS1 peak table...',
               detail = 'This may take a while',
               # style= "old",
               value = 0, {
                 if(input$da.use.demo.data == TRUE){##use demo data
                   # temp.file <- dir("./data/demo_data/Data_Cleaning")
                   da.ms1.raw$data <- lapply(1:length(1), function(idx){
                     # x <- temp.file[idx]
                     incProgress(1/length(1),
                                 detail = "This may take a while")
                     as.data.frame(readr::read_csv("./data/demo_data/Differential_Analysis/data.dis.csv",
                                                   col_types = readr::cols()))
                   })
                 }else{#upload data
                   if (!is.null(input$DAms1PeakTable) & input$DAprojectID != "") {
                     # temp.file <- input$DAms1PeakTable$datapath
                     da.ms1.raw$data <- lapply(1:length(1), function(idx){
                       incProgress(1/length(1),
                                   detail = "This may take a while")
                       as.data.frame(readr::read_csv(input$DAms1PeakTable$datapath,
                                                     col_types = readr::cols()))
                     })
                   }
                 }
               })
})


## read sample.info
da.sample.info.raw <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$da.upload.button, {
  if(input$da.use.demo.data == TRUE){
    da.sample.info.raw$data <- as.data.frame(readr::read_csv("./data/demo_data/Differential_Analysis/sample.info.dis.csv",
                                                             col_types = readr::cols()))
    da.sample.info.raw$data[,"group"] <- as.character(da.sample.info.raw$data[,"group"])

  }else{
    if (!is.null(input$DAsampleInfo) & input$DAprojectID != "") {
      # da.sample.info.raw$data <- NULL
      da.sample.info.raw$data <-
        as.data.frame(readr::read_csv(input$DAsampleInfo$datapath,col_types = readr::cols()))
      # da.sample.info.raw$data[,"group"] <- as.character(da.sample.info.raw$data[,"group"])
    }

  }
})


##jump to data check page from upload data page
observeEvent(input$da.upload.button, {
  if(!is.null(da.ms1.raw$data) & !is.null(da.sample.info.raw$data) & input$DAprojectID != ""){
    updateTabsetPanel(session, inputId = "DAtab",
                      selected = "DAdataCheck")
  }

})



##save data user upload
observeEvent(eventExpr = input$da.upload.button, {
  if(input$DAprojectID != ""){
    withProgress(message = 'Save data...',
                 detail = 'This may take a while',
                 # style= "old",
                 value = 0, {
                     lapply(1:length(1), function(idx){
                       incProgress(1/length(1),
                                   detail = "This may take a while")
                       if(!is.null(da.ms1.raw$data)){
                         da.ms1.raw <- da.ms1.raw$data
                         save(da.ms1.raw, file = file.path("user_data", user_input$username,
                                                           input$DAprojectID,
                                                           "differential_analysis",
                                                           "da.ms1.raw"))
                         rm(list= c("da.ms1.raw"))
                       }

                       if(!is.null(da.sample.info.raw$data)){
                         da.sample.info.raw <- da.sample.info.raw$data
                         save(da.sample.info.raw, file = file.path("user_data", user_input$username,
                                                                   input$DAprojectID,
                                                                   "differential_analysis",
                                                                   "da.sample.info.raw"))
                         rm(list= c("da.sample.info.raw"))
                       }
                     })
                 })
  }
})


















#------------------------------------------------------------------------------
##Check data
da.data.check.result <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$da.upload.button,{
  if(!is.null(da.ms1.raw$data) & !is.null(da.sample.info.raw$data) & input$DAprojectID != ""){
    check.result <- try({checkData(peak.table = da.ms1.raw$data,
                              sample.info = da.sample.info.raw$data,
                              step = "biomarker")})
    if(class(check.result)[1] == "try-error"){
      output$da.data.check.result.message <- renderText({paste("Data check: ",check.result[[1]])})
      check.result <- data.frame("Data.File" = "Data",
                                 "Information" = "There are errors in your data",
                                 "Result" = "Error",
                                 stringsAsFactors = FALSE)
    }else{
      output$da.data.check.result.message <- renderText({""})
    }
  }else{
    check.result <- data.frame("Data.File" = "Data",
                               "Information" = "You don't provide MS1 peak table or sample information",
                               "Result" = "Error",
                               stringsAsFactors = FALSE)
  }

  check.result <- as.data.frame(check.result)
  da.data.check.result$data <- check.result
})


output$da.data.check.result <- DT::renderDataTable(
  DT::datatable(da.data.check.result$data,
                class = 'cell-border stripe',
                editable = FALSE,
                selection = "single",
                # caption = 'Raw data containing MVs',
                escape = FALSE,
                # filter = "top",
                filter = "none",
                rownames = FALSE,
                extensions = list("ColReorder" = NULL,
                                  "Buttons" = NULL,
                                  "FixedColumns" = list(leftColumns=2)),
                options = list(
                  dom = 'BRrltpi',##remove serach options
                  # autoWidth = TRUE,
                  lengthMenu = list(c(15, 50, -1), c('15', '50', 'All')),
                  ColReorder = TRUE,
                  buttons =
                    list(
                      list(
                        extend = 'collection',
                        buttons = list(list(extend="csv",filename = "Data.check.result"),
                                       list(extend='excel',filename = "Data.check.result")),
                        text = 'Download'
                      ),
                      I('colvis')
                    ),
                  scrollX = TRUE
                )
  ) %>% DT::formatStyle(
    'Result',
    target = 'row',
    backgroundColor = DT::styleEqual(c("Error: NA in sample.information",
                                       "Error: space in sample.information",
                                       "Error: No sample.name",
                                       "Error: No injection.order",
                                       "Error: No class",
                                       "Error: No batch",
                                       "Error: No group",
                                       "Error: No QC or Subject",
                                       "Error: The sample names in sample.inforamtion and data are not same",
                                       "Error: No name",
                                       "Error: No mz",
                                       "Error: No rt",
                                       "Error: NA in MS1 peak table"),
                                     c(rep('#FC4B4E', 13)))
  ),
  server = FALSE
)


##jump to upload data page from data check page
observeEvent(input$da.data.check.2.da.upload, {
  updateTabsetPanel(session, "DAtab",
                    selected = "DAupload")

})

##jump to batch alignment page from data check
observeEvent(input$da.data.check.2.uni.analysis, {
  if(!is.null(da.data.check.result$data)){
    if(length(grep(pattern = "Error",da.data.check.result$data[,3])) == 0){
      updateTabsetPanel(session, "DAtab",
                        selected = "DAunivariateAnalysis")
    }
  }
})

##error if there is error in data
observeEvent(eventExpr = input$da.data.check.2.uni.analysis, {
  if(!is.null(da.data.check.result$data)){
    if(length(grep(pattern = "Error",da.data.check.result$data[,3])) != 0){
      shinyalert::shinyalert(title = "Error",
                             text = "There are errors in you files, please click Previous to check and
                             upload again.",
                             type = "error")
    }
  }
})






















#-------------------------------------------------------------------------------
#univariate analysis

##fold change
###group area
##FC group area
da.group.name <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$da.data.check.2.uni.analysis,{
  temp <- unique(da.sample.info.raw$data[,"group"])
  if(input$da.use.demo.data == TRUE) {
    temp <- c("Control", "QC", "Case")
  }
  if(any(temp == "Control")){
    temp[grep("Control", temp)] <- temp[1]
    temp[1] <- "Control"
  }

  if(any(temp == "Case")){
    temp[grep("Case", temp)] <- temp[length(temp)]
    temp[length(temp)] <- "Case"
  }

  da.group.name$data <- temp
})

output$da.group.area <- renderUI({
  fluidPage(
           selectInput(inputId = "da.ua.control",
                       label = "Control group",
                       choices = da.group.name$data),
           selectInput(inputId = "da.ua.case",
                       label = "Case group",
                       choices = rev(da.group.name$data))
  )

})


##save parameter
da.ua.params <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$daUAsubmitButton,{
  temp <- try({
    daUAparams <- DAuaParam( da.ua.control = input$da.ua.control,
                             da.ua.case = input$da.ua.case,
                             daUAlog = input$daUAlog,
                             # daUAscale = input$daUAscale,
                             # daUAcenter = input$daUAcenter,
                             daUAfcWhich = input$daUAfcWhich,
                             daUAhypothesisTesting = input$daUAhypothesisTesting,
                             daUAalternative = input$daUAalternative,
                             daUApaired = input$daUApaired,
                             daUAadjust = input$daUAadjust)
    da.ua.params$data <- daUAparams
    save(daUAparams, file = file.path("user_data", user_input$username,
                                      input$DAprojectID, "differential_analysis/daUAparams"))
  })

  if(class(temp)[1] == "try-error"){
    info <- temp[[1]]
    output$da.ua.params.message <- renderText({paste("Univaraite analysis parameters: ",info)})
    rm(list="temp")
  }else{
    output$da.ua.params.message <- renderText({""})
    rm(list="temp")
  }

})

###log, scale or center sample
daUAfc <- reactiveValues(data = NULL)
daUAp <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$daUAsubmitButton, {
if(is.null(da.ms1.raw$data) | is.null(da.sample.info.raw$data)) return(NULL)
  sample.info <- da.sample.info.raw$data
  data <- da.ms1.raw$data[[1]]
  sample <- data[,match(sample.info[,1], colnames(data)), drop = FALSE]
  tags <- data[,-match(sample.info[,1], colnames(data)), drop = FALSE]

  ##log
sample <- sxtLog(sample = sample,  method = input$daUAlog)
##scale
# sample <- sxtScale(sample = sample, method = input$daUAscale, center = input$daUAcenter)

control.group <- input$da.ua.control
case.group <- input$da.ua.case
control.group.name <- sample.info[,"sample.name"][which(sample.info[,"group"]==control.group)]
case.group.name <- sample.info[,"sample.name"][which(sample.info[,"group"]==case.group)]

control.group.idx <- match(control.group.name, colnames(sample))
case.group.idx <- match(case.group.name, colnames(sample))

##fold change
withProgress(message = 'Fold change...',
             detail = 'This may take a while',
             # style= "old",
             value = 0, {
               lapply(1:length(1), function(idx){
                 incProgress(1/length(1),
                             detail = "This may take a while")
                 if(input$daUAfcWhich == "median"){
                   temp <- try({
                     apply(sample, 1, function(x){
                     median(x[case.group.idx])/median(x[control.group.idx])
                   })
                     })
                   if(class(temp)[1] == "try-error"){
                     info <- temp[[1]]
                     output$daUAfc.info <- renderText({paste("Fold change: ",info)})
                     rm(list = c("temp"))
                   }else{
                     output$daUAfc.info <- renderText({""})
                     daUAfc$data <- temp
                     rm(list = c("temp"))
                   }

                 }else{
                   temp <- try({
                     apply(sample, 1, function(x){
                     mean(x[case.group.idx])/mean(x[control.group.idx])})
                   })
                   if(class(temp)[1] == "try-error"){
                     info <- temp[[1]]
                     output$daUAfc.info <- renderText({paste("Fold change: ",info)})
                     rm(list = c("temp"))
                   }else{
                     output$daUAfc.info <- renderText({""})
                     daUAfc$data <- temp
                     rm(list = c("temp"))
                   }
                 }

                 daUAfc$data[is.na(daUAfc$data)] <- 1
                 daUAfc$data[is.nan(daUAfc$data)] <- 1
                 daUAfc$data[is.infinite(daUAfc$data)] <- max(daUAfc$data, na.rm = TRUE)
                 names(daUAfc$data) <- tags[,"name"]

               })
             })



##p value
withProgress(message = 'Hypothesis test...',
             detail = 'This may take a while',
             # style= "old",
             value = 0, {
               if(input$daUAhypothesisTesting == "t"){
                 daUAp$data <- apply(sample, 1, function(x){
                   control <- as.numeric(x[control.group.idx])
                   case <- as.numeric(x[case.group.idx])
                   if(all(is.na(control))) control[is.na(control)] <- 1
                   if(all(is.na(case))) case[is.na(case)] <- 1
                   control[is.na(control)] <- mean(control, na.rm = TRUE)
                   case[is.na(case)] <- mean(case, na.rm = TRUE)
                   temp <- try({
                     t.test(x = control,
                          y = case,
                          alternative = input$daUAalternative,
                          paired = input$daUApaired)}, silent = TRUE)
                   if(class(temp) == "try-error"){
                     1
                   }else{
                     temp$p.value
                   }
                 })
               }else{
                 daUAp$data <- apply(sample, 1, function(x){
                   control <- as.numeric(x[control.group.idx])
                   case <- as.numeric(x[case.group.idx])
                   if(all(is.na(control))) control[is.na(control)] <- 1
                   if(all(is.na(case))) case[is.na(case)] <- 1
                   control[is.na(control)] <- mean(control, na.rm = TRUE)
                   case[is.na(case)] <- mean(case, na.rm = TRUE)
                   temp <- try({wilcox.test(x = control,
                               y = case,
                               alternative = input$daUAalternative,
                               paired = input$daUApaired)}, silent = TRUE)
                   if(class(temp) == "try-error"){
                     1
                   }else{
                     temp$p.value
                   }
                 })
               }
             })

names(daUAp$data) <- tags[,"name"]

if(input$daUAadjust == "no"){
  daUAp$data <- daUAp$data
}else{
  daUAp$data <- p.adjust(daUAp$data, method = input$daUAadjust)
}
})





da.ms1.p.fc <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$daUAsubmitButton, {
if(is.null(daUAfc$data) | is.null(daUAp$data)) return(NULL)
  temp <- data.frame(names(daUAfc$data), daUAfc$data, daUAp$data, stringsAsFactors = FALSE)
  colnames(temp) <- c("name", "Fold-change", "P-value")
  da.ms1.p.fc$data <- temp
})




##volcano plot
da.volcano.plot <- reactiveValues(data = NULL)
# da.volcano.3d.plot <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$daUAsubmitButton, {
  if(is.null(da.ms1.p.fc$data)) return(NULL)
  withProgress(message = 'Volcano plot...',
               detail = 'This may take a while',
               # style= "old",
               value = 0, {
                 lapply(1:length(1), function(idx){
                   incProgress(1/length(1),
                               detail = "This may take a while")

                   temp <- try({volcanoPlot(object = da.ms1.p.fc$data,
                                            control.group.name = input$da.ua.control,
                                            case.group.name = input$da.ua.case,
                                            p.adjust.method = input$daUAadjust,
                                            p.cutoff = input$daDMpCutoff1,
                                            fc.cutoff = input$daDMfcCutoff1)})
                   if(class(temp)[1] == "try-error"){
                     info <- temp[[1]]
                     output$da.volcano.info <- renderText({paste("Volcano plot: ",info)})
                     rm(list = c("temp"))
                   }else{
                     da.volcano.plot$data <- temp
                     output$da.volcano.info <- renderText({""})
                     rm(list = c("temp"))
                   }
                 })
               })
})


output$da.volcano.plot <- renderPlotly({
  if(!is.null(da.volcano.plot$data)){
    ggplotly(da.volcano.plot$data
             # tooltip = c("Sample", "Injection.order", "Intensity"),
             # source = "da.volcano.plot"
    )
  }
})

output$da.ms1.p.fc <- DT::renderDataTable(
  DT::datatable(da.ms1.p.fc$data,
                class = 'cell-border stripe',
                editable = FALSE,
                selection = "single",
                # caption = 'Raw data containing MVs',
                escape = FALSE,
                # filter = "top",
                filter = "none",
                rownames = FALSE,
                extensions = list("ColReorder" = NULL,
                                  "Buttons" = NULL,
                                  "FixedColumns" = list(leftColumns=2)),
                options = list(
                  dom = 'BRrltpi',
                  # autoWidth = TRUE,
                  lengthMenu = list(c(10, 50, -1), c('10', '50', 'All')),
                  ColReorder = TRUE,
                  buttons =
                    list(
                      list(
                        extend = 'collection',
                        buttons = list(list(extend="csv",filename = "Potential.marker.univariate.analysis"),
                                       list(extend='excel',filename = "Potential.marker.univariate.analysis")),
                        text = 'Download'
                      ),
                      I('colvis')
                    ),
                  scrollX = TRUE
                )
  ),
  server = FALSE
)



##jump to multivariate analysis
observeEvent(input$daUA2MA, {
  if(!is.null(da.ms1.p.fc$data)){
    updateTabsetPanel(session, "DAtab",
                      selected = "DAmultivariateAnalysis")
  }
})

##error if there is no marker1
observeEvent(eventExpr = input$daUA2MA, {
  if(is.null(da.ms1.p.fc$data)){
    shinyalert::shinyalert(title = "Error",
                           text = "Please click Submit first.",
                           type = "error")
  }
})


















#-------------------------------------------------------------------------------
#multivariate analysis
##save parameter
da.ma.params <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$daMAsubmitButton,{
  temp <- try({
    daMAparams <- DAmaParam(
      da.ma.control = input$da.ua.control,
      da.ma.case = input$da.ua.case,
      daMAlog = input$daMAlog,
      daMAscale = input$daMAscale,
      daMAcenter = input$daMAcenter,
      daMAncomp = input$daMAncomp,
      daMAhcaClusteringDistanceRows = input$daMAhcaClusteringDistanceRows,
      daMAhcaClusteringDistanceCols = input$daMAhcaClusteringDistanceCols,
      daMAhcaClusteringMethod = input$daMAhcaClusteringMethod)
    da.ma.params$data <- daMAparams
    save(daMAparams, file = file.path("user_data", user_input$username,
                                      input$DAprojectID, "differential_analysis/daMAparams"))
  })

  if(class(temp)[1] == "try-error"){
    info <- temp[[1]]
    output$da.ma.params.message <- renderText({paste("Multivaraite analysis parameters: ",info)})
    rm(list="temp")
  }else{
    output$da.ma.params.message <- renderText({""})
    rm(list="temp")
  }

})


#PCA analysis
daMApca <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$daMAsubmitButton,{
if(!is.null(da.ms1.raw$data) & !is.null(da.sample.info.raw$data)){
  withProgress(message = 'PCA analysis...',
               detail = 'This may take a while',
               # style= "old",
               value = 0, {
                 lapply(1:length(1), function(idx){
                   incProgress(1/length(1),
                               detail = "This may take a while")
                   temp <- try({
                     data <- da.ms1.raw$data[[1]]
                     sample.info <- da.sample.info.raw$data
                     control.group <- input$da.ua.control
                     case.group <- input$da.ua.case
                     sample.info <- sample.info[sample.info$group %in% c(control.group, case.group),]
                     sample <- data[,match(sample.info$sample.name, colnames(data))]

                     ##log
                     sample <- sxtLog(sample = sample, method = input$daMAlog)
                     ##scale
                     sample <- sxtScale(sample = sample, method = input$daMAscale, center = input$daMAcenter)

                     prcomp(data.frame(t(sample)), retx = TRUE,
                                            center = FALSE,
                                            scale = FALSE)
                   })

                   if(class(temp)[1] == "try-error"){
                     info <- temp[[1]]
                     output$daMApca.message <- renderText({paste("PCA: ",info)})
                     rm(list = c("temp"))
                   }else{
                     daMApca$data <- temp
                     output$daMApca.message <- renderText({""})
                     rm(list = c("temp"))
                   }

                   rm(list = c("data", "sample.info", "control.group", "case.group", "sample"))
                 })
               })
}
})


daMApcaPlot <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$daMAsubmitButton, {
if(!is.null(daMApca$data)){
  sample.info <- da.sample.info.raw$data
  control.group <- input$da.ua.control
  case.group <- input$da.ua.case
  sample.info <- sample.info[sample.info$group %in% c(control.group, case.group),]
  temp <- try({
    pcaScorePlot2(pca.object = daMApca$data, sample.info = sample.info)
  })

  if(class(temp)[1] == "try-error"){
    info <- temp[[1]]
    output$daMApcaPlot.message <- renderText({paste("PCA plot: ",info)})
    rm(list = c("temp"))
  }else{
    daMApcaPlot$data <- temp
    output$daMApcaPlot.message <- renderText({""})
    rm(list = c("temp"))
  }

  rm(list = c("sample.info", "control.group", "case.group"))

}
})



output$daMApcaPlot <- renderPlotly({
  if(!is.null(daMApcaPlot$data)){
    ggplotly(daMApcaPlot$data,
             # tooltip = c("Sample", "Injection.order", "Batch"),
             source = "daMApcaPlot")
  }
})



#PLS analysis
daMApls <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$daMAsubmitButton,{
  if(!is.null(da.ms1.raw$data) & !is.null(da.sample.info.raw$data)){
    withProgress(message = 'PLS analysis...',
                 detail = 'This may take a while',
                 # style= "old",
                 value = 0, {
                   lapply(1:length(1), function(idx){
                     incProgress(1/length(1),
                                 detail = "This may take a while")

                     temp <- try({

                       data <- da.ms1.raw$data[[1]]
                       sample.info <- da.sample.info.raw$data
                       control.group <- input$da.ua.control
                       case.group <- input$da.ua.case
                       sample.info <- sample.info[sample.info$group %in% c(control.group, case.group),]
                       sample <- data[,match(sample.info$sample.name, colnames(data))]

                       ##log
                       sample <- sxtLog(sample = sample, method = input$daMAlog)
                       ##scale
                       sample <- sxtScale(sample = sample, method = input$daMAscale, center = input$daMAcenter)

                       pls.Y <- as.numeric(as.factor(sample.info[,"group"]))

                       ncompa <- min(nrow(sample), ncol(sample))
                       if(ncompa > 30) ncompa <- 30

                       plsdepot::plsreg1(t(sample), pls.Y, comps = ncompa)
                     })

                     if(class(temp)[1] == "try-error"){
                       info <- temp[[1]]
                       output$daMApls.message <- renderText({paste("PLS: ",info)})
                       rm(list = c("temp"))
                     }else{
                       daMApls$data <- temp
                       output$daMApls.message <- renderText({""})
                       rm(list = c("temp"))
                     }

                     rm(list = c("data", "sample.info", "control.group",
                                 "case.group", "sample", "pls.Y"))
                   })
                 })
  }
})


daMAplsQ2Plot <- reactiveValues(data = NULL)
observe({
  if(!is.null(daMApls$data)){
  temp <- try({
    plsQ2barplot(pls.object = daMApls$data, comps.number = 10)
  })

  if(class(temp)[1] == "try-error"){
    info <- temp[[1]]
    output$daMAplsQ2Plot.message <- renderText({paste("PLS Q2 plot: ",info)})
    rm(list = c("temp"))
  }else{
    daMAplsQ2Plot$data <- temp
    output$daMAplsQ2Plot.message <- renderText({""})
    rm(list = c("temp"))
  }
  }
})

output$daMAplsQ2Plot <- renderPlotly({
  if(!is.null(daMAplsQ2Plot$data)){
    ggplotly(daMAplsQ2Plot$data,
             # tooltip = c("Sample", "Injection.order", "Batch"),
             source = "daMAplsQ2Plot")
  }
})


###PLS analysis using user imput ncomp
daMApls2 <- reactiveValues(data1 = NULL, data2 = NULL)
toListen <- reactive({
  list(input$daMAsubmitButton, input$daMAplsSubmitButton)
})
observeEvent(eventExpr = toListen(),{
# observeEvent(eventExpr = input$daMAplsSubmitButton,{
  if(!is.null(input$daMAsubmitButton) & !is.null(input$daMAplsSubmitButton)){
    if(input$daMAsubmitButton == 0 && input$daMAplsSubmitButton == 0){
      return()
    }else{
      withProgress(message = 'PLS analysis...',
                   detail = 'This may take a while',
                   # style= "old",
                   value = 0, {
                     lapply(1:length(1), function(idx){
                       incProgress(1/length(1),
                                   detail = "This may take a while")

                       temp <- try({

                         data <- da.ms1.raw$data[[1]]
                         sample.info <- da.sample.info.raw$data
                         control.group <- input$da.ua.control
                         case.group <- input$da.ua.case
                         sample.info <- sample.info[sample.info$group %in% c(control.group, case.group),]
                         sample <- data[,match(sample.info$sample.name, colnames(data))]

                         ##log
                         sample <- sxtLog(sample = sample, method = input$daMAlog)
                         ##scale
                         sample <- sxtScale(sample = sample, method = input$daMAscale, center = input$daMAcenter)


                         pls.Y <- as.numeric(as.factor(sample.info[,"group"]))

                         ncompa <- input$daMAncomp
                         if(ncompa > min(c(nrow(sample), ncol(sample)))) ncompa <- min(c(nrow(sample), ncol(sample)))

                         dummy <- SXTdummy(pls.Y)
                         int.dummy <- dummy
                         pls.object1 <- plsdepot::plsreg1(t(sample), pls.Y, comps = ncompa)
                         pls.object2 <- plsdepot::plsreg2(t(sample), int.dummy, comps = ncompa)
                       })

                       if(class(temp)[1] == "try-error"){
                         info <- temp[[1]]
                         output$daMApls2.message <- renderText({paste("PLS2: ",info)})
                         rm(list = c("temp"))
                       }else{
                         daMApls2$data1 <- pls.object1
                         daMApls2$data2 <- pls.object2
                         output$daMApls2.message <- renderText({""})
                         rm(list = c("temp"))
                       }

                       rm(list = c("data", "sample.info", "control.group", "case.group",
                                   "sample", "pls.Y", "dummy", "int.dummy"))
                     })
                   })
    }
  }

})


##Q2cum and R2
daMAplsQ2R2Plot <- reactiveValues(data = NULL)
observe({
  if(!is.null(daMApls2$data1)){
    temp <- try({
      plsQ2R2barplot(pls.object = daMApls2$data1)
    })
    if(class(temp)[1] == "try-error"){
      info <- temp[[1]]
      output$daMAplsQ2R2Plot.message <- renderText({paste("PCA plot: ",info)})
      rm(list = c("temp"))
    }else{
      daMAplsQ2R2Plot$data <- temp
      output$daMAplsQ2R2Plot.message <- renderText({""})
      rm(list = c("temp"))
    }
  }
})


output$daMAplsQ2R2Plot <- renderPlotly({
  if(!is.null(daMAplsQ2R2Plot$data)){
    ggplotly(daMAplsQ2R2Plot$data,
             # tooltip = c("Sample", "Injection.order", "Batch"),
             source = "daMAplsQ2R2Plot")
  }
})

##PLS score plot
daMAplsPlot <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$daMAsubmitButton, {
if(!is.null(daMApls2$data1)){
  temp <- try({
    sample.info <- da.sample.info.raw$data
    control.group <- input$da.ua.control
    case.group <- input$da.ua.case
    sample.info <- sample.info[sample.info$group %in% c(control.group, case.group),]
    plsScorePlot(pls.object = daMApls2$data1, sample.info = sample.info)
  })

  if(class(temp)[1] == "try-error"){
    info <- temp[[1]]
    output$daMAplsPlot.message <- renderText({paste("PCA plot: ",info)})
    rm(list = c("temp"))
  }else{
    daMAplsPlot$data <- temp
    output$daMAplsPlot.message <- renderText({""})
    rm(list = c("temp"))
  }

  rm(list = c("sample.info", "control.group", "case.group"))

}
})


output$daMAplsPlot <- renderPlotly({
  if(!is.null(daMAplsPlot$data)){
    ggplotly(daMAplsPlot$data,
             # tooltip = c("Sample", "Injection.order", "Batch"),
             source = "daMAplsPlot")
  }
})



###VIP value
daMAvip <- reactiveValues(data = NULL)
daMAfcPvip <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$daMAsubmitButton, {
  if(is.null(daMApls2$data2)) return(NULL)
  temp <- try({
    vip <- apply(daMApls2$data2$VIP, 1, mean)
    temp1 <- data.frame(da.ms1.p.fc$data, vip, stringsAsFactors = FALSE)
    colnames(temp1)[4] <- "VIP"
  })

  if(class(temp)[1] == "try-error"){
    info <- temp[[1]]
    output$daMAvip.message <- renderText({paste("VIP: ",info)})
    rm(list = c("temp"))
  }else{
    daMAvip$data <- vip
    daMAfcPvip$data <- temp1
    output$daMAvip.message <- renderText({""})
    rm(list = c("temp"))
  }
})



output$daMAfcPvip <- DT::renderDataTable(
  DT::datatable(daMAfcPvip$data,
                class = 'cell-border stripe',
                editable = FALSE,
                selection = "single",
                # caption = 'Raw data containing MVs',
                escape = FALSE,
                # filter = "top",
                filter = "none",
                rownames = FALSE,
                extensions = list("ColReorder" = NULL,
                                  "Buttons" = NULL,
                                  "FixedColumns" = list(leftColumns=2)),
                options = list(
                  dom = 'BRrltpi',
                  # autoWidth = TRUE,
                  lengthMenu = list(c(10, 50, -1), c('10', '50', 'All')),
                  ColReorder = TRUE,
                  buttons =
                    list(
                      list(
                        extend = 'collection',
                        buttons = list(list(extend="csv",filename = "Potential.marker.multivariate.analysis"),
                                       list(extend='excel',filename = "Potential.marker.multivariate.analysis")),
                        text = 'Download'
                      ),
                      I('colvis')
                    ),
                  scrollX = TRUE
                )
  ),
  server = FALSE
)





##HCA analysis
daMAhca <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$daMAhcaSubmitButton,{
  if(!is.null(da.ms1.raw$data) & !is.null(da.sample.info.raw$data)){
    temp <- try({
      data <- da.ms1.raw$data[[1]]
      sample.info <- da.sample.info.raw$data
      control.group <- input$da.ua.control
      case.group <- input$da.ua.case
      sample.info <- sample.info[sample.info$group %in% c(control.group, case.group),]
      sample <- data[,match(sample.info$sample.name, colnames(data))]

      ##log
      sample <- sxtLog(sample = sample, method = input$daMAlog)
      ##scale
      sample <- sxtScale(sample = sample, method = input$daMAscale, center = input$daMAcenter)
      group <- c(control.group, case.group)

      heatMap(sample = sample,
              sample.info = sample.info,
              group = group,
              int.col = c(input$daMAhcaLowCol, input$daMAhcaMiddleCol, input$daMAhcaHighCol),
              color = c(input$daMAhcaGroup1Col, input$daMAhcaGroup2Col),
              show_rownames = input$daMAhcaShowRowNames,
              show_colnames = input$daMAhcaShowColNames,
              cluster_rows = input$daMAhcaClusterRows,
              cluster_cols = input$daMAhcaClusterCols,
              clustering_distance_rows = input$daMAhcaClusteringDistanceRows,
              clustering_distance_cols = input$daMAhcaClusteringDistanceCols,
              clustering_method = input$daMAhcaClusteringMethod)
    })

    if(class(temp)[1] == "try-error"){
      info <- temp[[1]]
      output$daMAhca.message <- renderText({paste("HCA: ",info)})
      rm(list = c("temp"))
    }else{
      daMAhca$data <- temp
      output$daMAhca.message <- renderText({""})
      rm(list = c("temp"))
    }

    rm(list = c("data", "sample.info", "control.group", "case.group",
                "sample", "group"))
  }
})


daMAhcaHeatmap <- reactiveValues(data = NULL)
observe({
  if(!is.null(daMAhca$data)){
    daMAhcaHeatmap$data <- daMAhca$data
  }
})

output$daMAhcaHeatmap <- renderPlot({
  if(!is.null(daMAhcaHeatmap$data)){
    daMAhcaHeatmap$data
  }
})


# output$daMAhcaHeatmapDownload <- downloadHandler(
#   filename = function() { "Heatmap.pdf" },
#   content = function(file) {
#     pdf(file = file,
#         width = input$daMAhcaHeatmapWidth,
#         height = input$daMAhcaHeatmapHeight)
#     daMAhcaHeatmap$data
#     dev.off()
#     # readr::write_csv(as.data.frame(data()), file)
#   }, contentType = "pdf")

# output$daMAhcaHeatmapDownload = downloadHandler(
#   filename = 'Heatmap.pdf',
#   content = function(file) {
#     pdf(file = file,
#         width=input$daMAhcaHeatmapWidth,
#         height = input$daMAhcaHeatmapHeight)
#     daMAhca$data
#     dev.off()
#   })



output$daMAhcaHeatmapDownload <- downloadHandler(
  filename = function() { "Heatmap.pdf" },
  content = function(file) {
    device <- function(..., width , height) {
      grDevices::pdf(..., width = input$daMAhcaHeatmapWidth,
                     height = input$daMAhcaHeatmapHeight)
    }
    ggsave(file,
           plot = daMAhca$data,
           device = device)
  }, contentType = "pdf")






##jump to differential metabolite selection
observeEvent(input$daMA2DM, {
  if(!is.null(daMAfcPvip$data)){
    updateTabsetPanel(session, "DAtab",
                      selected = "DAdifferentialMetaboliteSelection")
  }
})

##error if there is no marker1
observeEvent(eventExpr = input$daMA2DM, {
  if(is.null(daMAfcPvip$data)){
    shinyalert::shinyalert(title = "Error",
                           text = "Please click Submit first.",
                           type = "error")
  }
})



















##differential metabolite selection
##save parameter
da.dm.params <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$daDMsubmitButton,{
  temp <- try({
    daDMparams <- DAdmParam(
      daDMpCutoff = input$daDMpCutoff,
      daDMfcCutoff = input$daDMfcCutoff,
      daDMvipCutoff = input$daDMvipCutoff)
    da.dm.params$data <- daDMparams
    save(daDMparams, file = file.path("user_data", user_input$username,
                                      input$DAprojectID, "differential_analysis/daDMparams"))
  })

  if(class(temp)[1] == "try-error"){
    info <- temp[[1]]
    output$da.dm.params.message <- renderText({paste("Differential metabolite selection parameters: ",info)})
    rm(list="temp")
  }else{
    output$da.dm.params.message <- renderText({""})
    rm(list="temp")
  }

})








##3D volcano plot
da.volcano.3d.plot <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$daDMsubmitButton, {
  if(is.null(daMAfcPvip$data)) return(NULL)
  withProgress(message = '3D plot...',
               detail = 'This may take a while',
               # style= "old",
               value = 0, {
                 lapply(1:length(1), function(idx){
                   incProgress(1/length(1),
                               detail = "This may take a while")

                   temp <- try({
                     volcanoPlot3d(object = daMAfcPvip$data,
                                   control.group.name = input$da.ua.control,
                                   case.group.name = input$da.ua.case,
                                   p.adjust.method = input$daUAadjust,
                                   p.cutoff = input$daDMpCutoff,
                                   fc.cutoff = input$daDMfcCutoff,
                                   vip.cutoff = input$daDMvipCutoff)
                     })
                   if(class(temp)[1] == "try-error"){
                     info <- temp[[1]]
                     output$da.volcano.3d.plot.message <- renderText({paste("3D plot: ",info)})
                     rm(list = c("temp"))
                   }else{
                     da.volcano.3d.plot$data <- temp
                     output$da.volcano.3d.plot.message <- renderText({""})
                     rm(list = c("temp"))
                   }
                 })
               })
})


output$da.volcano.3d.plot <- renderPlotly({
  if(!is.null(da.volcano.3d.plot$data)){
    da.volcano.3d.plot$data
  }
})




##potential biomarker
daDifferentialMetabolite <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$daDMsubmitButton, {
  if(is.null(daMAfcPvip$data)) return(NULL)

  withProgress(message = "Different metabolite selection...",
               detail = 'This may take a while',
               # style= "old",
               value = 0, {
                 lapply(1:length(1), function(idx){
                   incProgress(1/length(1),
                               detail = "This may take a while")
                   temp <- try({
                     temp.idx1 <- which(daMAfcPvip$data[,3] < input$daDMpCutoff & daMAfcPvip$data[,2] > input$daDMfcCutoff)
                     temp.idx2 <- which(daMAfcPvip$data[,3] < input$daDMpCutoff & daMAfcPvip$data[,2] < 1/input$daDMfcCutoff)
                     temp.idx3 <- which(daMAfcPvip$data[,4] > input$daDMvipCutoff)
                     temp.idx <- sort(unique(c(temp.idx1, temp.idx2)))
                     temp.idx <- unique(intersect(temp.idx, temp.idx3))
                     sample.info <- da.sample.info.raw$data
                     data <- da.ms1.raw$data[[1]]
                     sample <- data[,match(sample.info[,1], colnames(data))]
                     tags <- data[,-match(sample.info[,1], colnames(data))]
                     tags <- data.frame(tags, "P-value" = daMAfcPvip$data[,3],
                                        "Fold-change" = daMAfcPvip$data[,2],
                                        "VIP" = daMAfcPvip$data[,4],
                                        stringsAsFactors = FALSE)
                     data.frame(tags, sample, stringsAsFactors = FALSE)[temp.idx,]
                   })

                   if(class(temp)[1] == "try-error"){
                     info <- temp[[1]]
                     output$daDifferentialMetabolite.message <- renderText({paste("Differential metabolites: ",info)})
                     rm(list = c("temp"))
                   }else{
                     daDifferentialMetabolite$data <- temp
                     marker.number <- nrow(temp)
                     output$daDifferentialMetabolite.message <- renderText({paste("Differential metabolites number: ",marker.number)})
                     rm(list = c("temp"))
                   }
                   rm(list = c("temp.idx1", "temp.idx2", "temp.idx3", "temp.idx",
                               "sample.info", "data", "sample", "tags"))
                 })
               })
})


output$daDifferentialMetabolite <- DT::renderDataTable(
  DT::datatable(daDifferentialMetabolite$data,
                class = 'cell-border stripe',
                editable = FALSE,
                selection = "single",
                # caption = 'Raw data containing MVs',
                escape = FALSE,
                # filter = "top",
                filter = "none",
                rownames = FALSE,
                extensions = list("ColReorder" = NULL,
                                  "Buttons" = NULL,
                                  "FixedColumns" = list(leftColumns=2)),
                options = list(
                  dom = 'BRrltpi',
                  # autoWidth = TRUE,
                  lengthMenu = list(c(10, 50, -1), c('10', '50', 'All')),
                  ColReorder = TRUE,
                  buttons =
                    list(
                      list(
                        extend = 'collection',
                        buttons = list(list(extend="csv",filename = "Different.metabolites"),
                                       list(extend='excel',filename = "Different.metabolites")),
                        text = 'Download'
                      ),
                      I('colvis')
                    ),
                  scrollX = TRUE
                )
  ),
  server = FALSE
)


##jump to differential metabolite selection
observeEvent(input$daDM2validation, {
  if(!is.null(daDifferentialMetabolite$data)){
    updateTabsetPanel(session, "DAtab",
                      selected = "DAvalidation")
  }
})

##error if there is no marker1
observeEvent(eventExpr = input$daDM2validation, {
  if(is.null(daDifferentialMetabolite$data)){
    shinyalert::shinyalert(title = "Error",
                           text = "Please click Submit first.",
                           type = "error")
  }

})



















##validation
##warning if no data selected
observeEvent(eventExpr = input$daValidationUploadButton, {
    if(is.null(input$DAms1PeakTableValidation) | is.null(input$DAsampleInfoValidation)){
      shinyalert::shinyalert(title = "Error",
                             text = "MS1 peak table and Sample information for validation are required.",
                             type = "error")
    }

})


##error if no marker
observeEvent(eventExpr = input$daValidationUploadButton, {
  if(!is.null(daDifferentialMetabolite$data)){
    if(nrow(daDifferentialMetabolite$data) == 0){
      shinyalert::shinyalert(title = "Error",
                             text = "The number of differential metabolites is zero!",
                             type = "error")
    }
  }
})

observeEvent(eventExpr = input$daValidationROCanalysis, {
  if(!is.null(daDifferentialMetabolite$data)){
    if(nrow(daDifferentialMetabolite$data) == 0){
      shinyalert::shinyalert(title = "Error",
                             text = "The number of differential metabolites is zero!",
                             type = "error")
    }
  }
})


##save parameter
da.validation.params <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$daValidationSubmitButton,{
  temp <- try({
    daValidationParams <-
      DAvalidationParam(daValidationPredictionModel = input$daValidationPredictionModel)
    da.validation.params$data <- daValidationParams
    save(daValidationParams, file = file.path("user_data", user_input$username,
                                              input$DAprojectID, "differential_analysis/daValidationParams"))
  })

  if(class(temp)[1] == "try-error"){
    info <- temp[[1]]
    output$da.validation.params.message <- renderText({paste("Validation parameters: ",info)})
    rm(list="temp")
  }else{
    output$da.validation.params.message <- renderText({""})
    rm(list="temp")
  }

})


###read MS1 peak table and sample.information for validation
da.ms1.validation <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$daValidationUploadButton, {
  withProgress(message = 'Upload MS1 peak table...',
               detail = 'This may take a while',
               # style= "old",
               value = 0, {
                   if (!is.null(input$DAms1PeakTableValidation)) {
                     da.ms1.validation$data <- lapply(1:length(1), function(idx){
                       incProgress(1/length(1),
                                   detail = "This may take a while")
                       as.data.frame(readr::read_csv(input$DAms1PeakTableValidation$datapath,
                                                     col_types = readr::cols()))
                     })
                   }
               })
})


## read sample.info
da.sample.info.validation <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$daValidationUploadButton, {
    if (!is.null(input$DAsampleInfoValidation)) {
      da.sample.info.validation$data <-
        as.data.frame(readr::read_csv(input$DAsampleInfoValidation$datapath,col_types = readr::cols()))
    }
})

##check result
da.data.check.result.validation <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$daValidationUploadButton,{
  if(!is.null(da.ms1.validation$data) & !is.null(da.sample.info.validation$data)){
    check.result <- checkData(peak.table = da.ms1.validation$data,
                              sample.info = da.sample.info.validation$data,
                              step = "biomarker")
  }else{
    check.result <- data.frame("Data.File" = "Data",
                               "Information" = "You don't provide MS1 peak table or sample information",
                               "Result" = "Warning",
                               stringsAsFactors = FALSE)
  }

  check.result <- as.data.frame(check.result)
  da.data.check.result.validation$data <- check.result
})


##error if there is error in data
observeEvent(eventExpr = input$daValidationSubmitButton, {
  if(!is.null(da.data.check.result.validation$data)){
    if(length(grep(pattern = "Error",da.data.check.result.validation$data[,3])) != 0){
      shinyalert::shinyalert(title = "Error",
                             text = "There are errors in you files, please check and upload again.",
                             type = "error")
    }
  }

})



#PCA analysis
#1 means discovery and 2 means validation
daValidationPCA1 <- reactiveValues(data = NULL)
daValidationPCA2 <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$daValidationSubmitButton,{
if(is.null(daDifferentialMetabolite$data)) return(NULL)
if(nrow(daDifferentialMetabolite$data) == 0) return(NULL)
##discovery dataset
  withProgress(message = 'PCA analysis for discovery dataset...',
               detail = 'This may take a while',
               # style= "old",
               value = 0, {
                 lapply(1:length(1), function(idx){
                   incProgress(1/length(1),
                               detail = "This may take a while")
                   temp <- try({
                     sample.info <- da.sample.info.raw$data
                     control.group <- input$da.ua.control
                     case.group <- input$da.ua.case
                     sample.info <- sample.info[sample.info$group %in% c(control.group, case.group),]
                     marker <- daDifferentialMetabolite$data

                     data1 <- da.ms1.raw$data[[1]]
                     sample1 <- data1[,match(sample.info$sample.name, colnames(data1))]
                     rownames(sample1) <- data1$name
                     sample1 <- sample1[which(rownames(sample1) %in% marker$name),]


                     ##log
                     sample1 <- sxtLog(sample = sample1, method = input$daMAlog)
                     ##scale
                     sample1 <- sxtScale(sample = sample1, method = input$daMAscale, center = input$daMAcenter)

                     prcomp(data.frame(t(sample1)), retx = TRUE, center = FALSE, scale = FALSE)
                   })

                   if(class(temp)[1] == "try-error"){
                     info <- temp[[1]]
                     output$daValidationPCA1.message <- renderText({paste("PCA discovery: ",info)})
                     rm(list = c("temp"))
                   }else{
                     daValidationPCA1$data <- temp
                     output$daValidationPCA1.message <- renderText({""})
                     rm(list = c("temp"))
                   }

                   rm(list = c("sample.info", "control.group", "case.group",
                               "marker", "data1", "sample1"))

                 })
               })

  if(!is.null(da.ms1.validation$data) & !is.null(da.sample.info.validation$data)){
    ##validation dataset
    withProgress(message = 'PCA analysis for validation dataset...',
                 detail = 'This may take a while',
                 # style= "old",
                 value = 0, {
                   lapply(1:length(1), function(idx){
                     incProgress(1/length(1),
                                 detail = "This may take a while")
                     temp <- try({
                       sample.info <- da.sample.info.validation$data
                       control.group <- input$da.ua.control
                       case.group <- input$da.ua.case
                       sample.info <- sample.info[sample.info$group %in% c(control.group, case.group),]
                       marker <- daDifferentialMetabolite$data

                       data2 <- da.ms1.validation$data[[1]]
                       sample2 <- data2[,match(sample.info$sample.name, colnames(data2))]
                       rownames(sample2) <- data2$name
                       sample2 <- sample2[which(rownames(sample2) %in% marker$name),]

                       sample2 <- sxtLog(sample = sample2, method = input$daMAlog)
                       ##scale
                       sample2 <- sxtScale(sample = sample2, method = input$daMAscale, center = input$daMAcenter)

                       prcomp(data.frame(t(sample2)), retx = TRUE, center = FALSE, scale = FALSE)
                     })

                     if(class(temp)[1] == "try-error"){
                       info <- temp[[1]]
                       output$daValidationPCA2.message <- renderText({paste("PCA validation: ",info)})
                       rm(list = c("temp"))
                     }else{
                       daValidationPCA2$data <- temp
                       output$daValidationPCA2.message <- renderText({""})
                       rm(list = c("temp"))
                     }

                     rm(list = c("sample.info", "control.group", "case.group",
                                 "marker", "data2", "sample2"))

                   })
                 })
  }

})


daValidationPCAplot1 <- reactiveValues(data = NULL)
observe({
  temp <- try({
    sample.info <- da.sample.info.raw$data
    control.group <- input$da.ua.control
    case.group <- input$da.ua.case
    sample.info <- sample.info[sample.info$group %in% c(control.group, case.group),]
    pcaScorePlot2(pca.object = daValidationPCA1$data, sample.info = sample.info)
  })

  if(class(temp)[1] == "try-error"){
    info <- temp[[1]]
    output$daValidationPCAplot1.message <- renderText({paste("PCA plot discovery: ",info)})
    rm(list = c("temp"))
  }else{
    daValidationPCAplot1$data <- temp
    output$daValidationPCAplot1.message <- renderText({""})
    rm(list = c("temp"))
  }
  rm(list = c("sample.info", "control.group", "case.group"))
})




output$daValidationPCAplot1 <- renderPlotly({
  if(!is.null(daValidationPCAplot1$data)){
    ggplotly(daValidationPCAplot1$data,
             # tooltip = c("Sample", "Injection.order", "Batch"),
             source = "daValidationPCAplot1")
  }
})


daValidationPCAplot2 <- reactiveValues(data = NULL)
observe({
  if(is.null(da.ms1.validation$data) | is.null(da.sample.info.validation$data)) return(NULL)
  temp <- try({
    sample.info <- da.sample.info.validation$data
    control.group <- input$da.ua.control
    case.group <- input$da.ua.case
    sample.info <- sample.info[sample.info$group %in% c(control.group, case.group),]
    pcaScorePlot2(pca.object = daValidationPCA2$data, sample.info = sample.info)
  })

  if(class(temp)[1] == "try-error"){
    info <- temp[[1]]
    output$daValidationPCAplot2.message <- renderText({paste("PCA plot discovery: ",info)})
    rm(list = c("temp"))
  }else{
    daValidationPCAplot2$data <- temp
    output$daValidationPCAplot2.message <- renderText({""})
    rm(list = c("temp"))
  }
  rm(list = c("sample.info", "control.group", "case.group"))

})



output$daValidationPCAplot2 <- renderPlotly({
  if(!is.null(daValidationPCAplot2$data)){
    ggplotly(daValidationPCAplot2$data,
             # tooltip = c("Sample", "Injection.order", "Batch"),
             source = "daValidationPCAplot2")
  }
})




###PLS analysis
daValidationPLS1 <- reactiveValues(data = NULL)
daValidationPLS2 <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$daValidationSubmitButton,{
  if(!is.null(da.ms1.raw$data) & !is.null(da.sample.info.raw$data)){
    if(is.null(daDifferentialMetabolite$data)) return(NULL)
    if(nrow(daDifferentialMetabolite$data) == 0) return(NULL)
    withProgress(message = 'PLS analysis for validation...',
                 detail = 'This may take a while',
                 # style= "old",
                 value = 0, {
                   lapply(1:length(1), function(idx){
                     incProgress(1/length(1),
                                 detail = "This may take a while")
                     temp <- try({
                       sample.info <- da.sample.info.raw$data
                       control.group <- input$da.ua.control
                       case.group <- input$da.ua.case
                       sample.info <- sample.info[sample.info$group %in% c(control.group, case.group),]
                       marker <- daDifferentialMetabolite$data

                       data1 <- da.ms1.raw$data[[1]]
                       sample1 <- data1[,match(sample.info$sample.name, colnames(data1))]
                       rownames(sample1) <- data1$name
                       sample1 <- sample1[which(rownames(sample1) %in% marker$name),]


                       ##log
                       sample1 <- sxtLog(sample = sample1, method = input$daMAlog)
                       ##scale
                       sample1 <- sxtScale(sample = sample1, method = input$daMAscale,
                                           center = input$daMAcenter)

                       pls.Y <- as.numeric(as.factor(sample.info[,"group"]))

                       plsdepot::plsreg1(t(sample1), pls.Y, comps = 3)
                     })

                     if(class(temp)[1] == "try-error"){
                       info <- temp[[1]]
                       output$daValidationPLS1.message <- renderText({paste("PLS discovery: ",info)})
                       rm(list = c("temp"))
                     }else{
                       daValidationPLS1$data <- temp
                       output$daValidationPLS1.message <- renderText({""})
                       rm(list = c("temp"))
                     }

                     rm(list = c("sample.info", "control.group", "case.group", "marker",
                                 "data1", "sample1", "pls.Y"))
                   })
                 })
  }

  if(!is.null(da.ms1.validation$data) & !is.null(da.sample.info.validation$data)){
    withProgress(message = 'PLS analysis for validation...',
                 detail = 'This may take a while',
                 # style= "old",
                 value = 0, {
                   lapply(1:length(1), function(idx){
                     incProgress(1/length(1),
                                 detail = "This may take a while")
                     temp <- try({
                       sample.info <- da.sample.info.validation$data
                       control.group <- input$da.ua.control
                       case.group <- input$da.ua.case
                       sample.info <- sample.info[sample.info$group %in% c(control.group, case.group),]
                       marker <- daDifferentialMetabolite$data

                       data2 <- da.ms1.validation$data[[1]]
                       sample2 <- data2[,match(sample.info$sample.name, colnames(data2))]
                       rownames(sample2) <- data2$name
                       sample2 <- sample2[which(rownames(sample2) %in% marker$name),]

                       sample2 <- sxtLog(sample = sample2, method = input$daMAlog)
                       ##scale
                       sample2 <- sxtScale(sample = sample2, method = input$daMAscale, center = input$daMAcenter)

                       pls.Y <- as.numeric(as.factor(sample.info[,"group"]))

                       plsdepot::plsreg1(t(sample2), pls.Y, comps = 3)
                     })

                     if(class(temp)[1] == "try-error"){
                       info <- temp[[1]]
                       output$daValidationPLS2.message <- renderText({paste("PLS validation: ",info)})
                       rm(list = c("temp"))
                     }else{
                       daValidationPLS2$data <- temp
                       output$daValidationPLS2.message <- renderText({""})
                       rm(list = c("temp"))
                     }
                     rm(list = c("sample.info", "control.group", "case.group", "marker",
                                 "data2", "sample2", "pls.Y"))
                   })
                 })
  }

})


##PLS score plot
daValidationPLSplot1 <- reactiveValues(data = NULL)
observe({
  temp <- try({
    sample.info <- da.sample.info.raw$data
    control.group <- input$da.ua.control
    case.group <- input$da.ua.case
    sample.info <- sample.info[sample.info$group %in% c(control.group, case.group),]
    plsScorePlot(pls.object = daValidationPLS1$data, sample.info = sample.info)
  })

  if(class(temp)[1] == "try-error"){
    info <- temp[[1]]
    output$daValidationPLSplot1.message <- renderText({paste("PLS plot discovery: ",info)})
    rm(list = c("temp"))
  }else{
    daValidationPLSplot1$data <- temp
    output$daValidationPLSplot1.message <- renderText({""})
    rm(list = c("temp"))
  }
  rm(list = c("sample.info", "control.group", "case.group"))
})



output$daValidationPLSplot1 <- renderPlotly({
  if(!is.null(daValidationPLSplot1$data)){
    ggplotly(daValidationPLSplot1$data,
             source = "daValidationPLSplot1")
  }
})


daValidationPLSplot2 <- reactiveValues(data = NULL)
observe({
  if(is.null(da.sample.info.validation$data) | is.null(da.sample.info.validation$data)) return(NULL)
  temp <- try({
    sample.info <- da.sample.info.validation$data
    control.group <- input$da.ua.control
    case.group <- input$da.ua.case
    sample.info <- sample.info[sample.info$group %in% c(control.group, case.group),]
    plsScorePlot(pls.object = daValidationPLS2$data, sample.info = sample.info)
  })

  if(class(temp)[1] == "try-error"){
    info <- temp[[1]]
    output$daValidationPLSplot2.message <- renderText({paste("PLS plot validation: ",info)})
    rm(list = c("temp"))
  }else{
    daValidationPLSplot2$data <- temp
    output$daValidationPLSplot2.message <- renderText({""})
    rm(list = c("temp"))
  }
  rm(list = c("sample.info", "control.group", "case.group"))
})





output$daValidationPLSplot2 <- renderPlotly({
  if(!is.null(daValidationPLSplot2$data)){
    ggplotly(daValidationPLSplot2$data,
             source = "daValidationPLSplot1")
  }
})


##HCA analysis
daValidationHCA1 <- reactiveValues(data = NULL)
daValidationHCA2 <- reactiveValues(data = NULL)

observeEvent(eventExpr = input$daValidationSubmitButton,{
  if(!is.null(da.ms1.raw$data) & !is.null(da.sample.info.raw$data)){
    if(is.null(daDifferentialMetabolite$data)) return(NULL)
    if(nrow(daDifferentialMetabolite$data) == 0) return(NULL)
    withProgress(message = 'HCA for validation...',
                 detail = 'This may take a while',
                 # style= "old",
                 value = 0, {
                   lapply(1:length(1), function(idx){
                     incProgress(1/length(1),
                                 detail = "This may take a while")
                     temp <- try({
                       sample.info <- da.sample.info.raw$data
                       control.group <- input$da.ua.control
                       case.group <- input$da.ua.case
                       sample.info <- sample.info[sample.info$group %in% c(control.group, case.group),]
                       marker <- daDifferentialMetabolite$data

                       data1 <- da.ms1.raw$data[[1]]
                       sample1 <- data1[,match(sample.info$sample.name, colnames(data1))]
                       rownames(sample1) <- data1$name
                       sample1 <- sample1[which(rownames(sample1) %in% marker$name),]


                       ##log
                       sample1 <- sxtLog(sample = sample1, method = input$daMAlog)
                       ##scale
                       sample1 <- sxtScale(sample = sample1, method = input$daMAscale,
                                           center = input$daMAcenter)

                       group <- c(control.group, case.group)

                                                 heatMap(sample = sample1,
                                                        sample.info = sample.info,
                                                        group = group,
                                                        int.col = c(input$daMAhcaLowCol, input$daMAhcaMiddleCol, input$daMAhcaHighCol),
                                                        color = c(input$daMAhcaGroup1Col, input$daMAhcaGroup2Col),
                                                        show_rownames = input$daMAhcaShowRowNames,
                                                        show_colnames = input$daMAhcaShowColNames,
                                                        cluster_rows = input$daMAhcaClusterRows,
                                                        cluster_cols = input$daMAhcaClusterCols,
                                                        clustering_distance_rows = input$daMAhcaClusteringDistanceRows,
                                                        clustering_distance_cols = input$daMAhcaClusteringDistanceCols,
                                                        clustering_method = input$daMAhcaClusteringMethod)
                     })


                     if(class(temp)[1] == "try-error"){
                       info <- temp[[1]]
                       output$daValidationHCA1.message <- renderText({paste("HCA discovery: ",info)})
                       rm(list = c("temp"))
                     }else{
                       daValidationHCA1$data <- temp
                       output$daValidationHCA1.message <- renderText({""})
                       rm(list = c("temp"))
                     }
                     rm(list = c("sample.info", "control.group", "case.group",
                                 "marker", "data1", "sample1", "group"))
                   })
                 })
  }

  if(!is.null(da.ms1.validation$data) & !is.null(da.sample.info.validation$data)){
    withProgress(message = 'HCA for validation...',
                 detail = 'This may take a while',
                 # style= "old",
                 value = 0, {
                   lapply(1:length(1), function(idx){
                     incProgress(1/length(1),
                                 detail = "This may take a while")
                     temp <- try({
                       sample.info <- da.sample.info.validation$data
                       control.group <- input$da.ua.control
                       case.group <- input$da.ua.case
                       sample.info <- sample.info[sample.info$group %in% c(control.group, case.group),]
                       marker <- daDifferentialMetabolite$data

                       data2 <- da.ms1.validation$data[[1]]
                       sample2 <- data2[,match(sample.info$sample.name, colnames(data2))]
                       rownames(sample2) <- data2$name
                       sample2 <- sample2[which(rownames(sample2) %in% marker$name),]

                       sample2 <- sxtLog(sample = sample2, method = input$daMAlog)
                       ##scale
                       sample2 <- sxtScale(sample = sample2, method = input$daMAscale, center = input$daMAcenter)

                       group <- c(control.group, case.group)

                                                heatMap(sample = sample2,
                                                        sample.info = sample.info,
                                                        group = group,
                                                        int.col = c(input$daMAhcaLowCol, input$daMAhcaMiddleCol, input$daMAhcaHighCol),
                                                        color = c(input$daMAhcaGroup1Col, input$daMAhcaGroup2Col),
                                                        show_rownames = input$daMAhcaShowRowNames,
                                                        show_colnames = input$daMAhcaShowColNames,
                                                        cluster_rows = input$daMAhcaClusterRows,
                                                        cluster_cols = input$daMAhcaClusterCols,
                                                        clustering_distance_rows = input$daMAhcaClusteringDistanceRows,
                                                        clustering_distance_cols = input$daMAhcaClusteringDistanceCols,
                                                        clustering_method = input$daMAhcaClusteringMethod)
                     })

                     if(class(temp)[1] == "try-error"){
                       info <- temp[[1]]
                       output$daValidationHCA2.message <- renderText({paste("HCA validation: ",info)})
                       rm(list = c("temp"))
                     }else{
                       daValidationHCA2$data <- temp
                       output$daValidationHCA2.message <- renderText({""})
                       rm(list = c("temp"))
                     }
                     rm(list = c("sample.info", "control.group", "case.group",
                                 "marker", "data2", "sample2", "group"))
                   })
                 })
  }
})



daValidationHCAheatmap1 <- reactiveValues(data = NULL)
observe({
    daValidationHCAheatmap1$data <- daValidationHCA1$data
})


output$daValidationHCAheatmap1 <- renderPlot({
    daValidationHCAheatmap1$data
})


# output$daValidationHCAheatmap1Download1 <- downloadHandler(
#   filename = function() { "Heatmap.pdf" },
#   content = function(file) {
#     pdf(file = file,
#         width = input$daValidationHCAheatmapWidth1,
#         height = input$daValidationHCAheatmapWidth1)
#     daValidationHCAheatmap1$data
#     dev.off()
#   }, contentType = "pdf")


# output$daValidationHCAheatmap1Download1 = downloadHandler(
#   filename = 'Heatmap.pdf',
#   content = function(file) {
#     pdf(file = file, width=input$daValidationHCAheatmapWidth1,
#         height = input$daValidationHCAheatmapWidth1)
#     daValidationHCA1$data
#     dev.off()
#   })


output$daValidationHCAheatmap1Download1 <- downloadHandler(
  filename = function() { "Heatmap.pdf" },
  content = function(file) {
    device <- function(..., width , height) {
      grDevices::pdf(..., width = input$daValidationHCAheatmapWidth1,
                     height = input$daValidationHCAheatmapWidth1)
    }
    ggsave(file, plot =  daValidationHCA1$data, device = device)
  }, contentType = "pdf")



daValidationHCAheatmap2 <- reactiveValues(data = NULL)
observe({
  daValidationHCAheatmap2$data <- daValidationHCA2$data
})


output$daValidationHCAheatmap2 <- renderPlot({
    daValidationHCAheatmap2$data
})




# output$daValidationHCAheatmap1Download2 <- downloadHandler(
#   filename = function() { "Heatmap.pdf" },
#   content = function(file) {
#     pdf(file = file,
#         width = input$daValidationHCAheatmapWidth2,
#         height = input$daValidationHCAheatmapWidth2)
#     daValidationHCAheatmap2$data
#     dev.off()
#     # readr::write_csv(as.data.frame(data()), file)
#   }, contentType = "pdf")



# output$daValidationHCAheatmap1Download2 = downloadHandler(
#   filename = 'Heatmap.pdf',
#   content = function(file) {
#     pdf(file = file,
#         width=input$daValidationHCAheatmapWidth2,
#         height = input$daValidationHCAheatmapWidth2)
#     daValidationHCA2$data
#     dev.off()
#   })



output$daValidationHCAheatmap1Download2 <- downloadHandler(
  filename = function() { "Heatmap.pdf" },
  content = function(file) {
    device <- function(..., width , height) {
      grDevices::pdf(..., width = input$daValidationHCAheatmapWidth2,
                     height = input$daValidationHCAheatmapWidth2)
    }
    ggsave(file, plot =  daValidationHCA2$data, device = device)
  }, contentType = "pdf")


###ROC analysis
daValidationROC1 <- reactiveValues(data = NULL)
daValidationROC2 <- reactiveValues(data = NULL)

observeEvent(eventExpr = input$daValidationROCanalysis, {
  if(!is.null(daDifferentialMetabolite$data)){
    if(nrow(daDifferentialMetabolite$data) == 0) return(NULL)
    ##construction model
    withProgress(message = 'ROC analysis...',
                 detail = 'This may take a while',
                 # style= "old",
                 value = 0, {
                   lapply(1:1, function(idx){
                     temp <- try({
                       sample.info <- da.sample.info.raw$data
                       control.group <- input$da.ua.control
                       case.group <- input$da.ua.case
                       sample.info <- sample.info[sample.info$group %in% c(control.group, case.group),]
                       marker <- daDifferentialMetabolite$data

                       data1 <- da.ms1.raw$data[[1]]
                       sample1 <- data1[,match(sample.info$sample.name, colnames(data1))]
                       rownames(sample1) <- data1$name
                       sample1 <- sample1[which(rownames(sample1) %in% marker$name),]

                       ##log
                       sample1 <- sxtLog(sample = sample1, method = input$daMAlog)
                       ##scale
                       sample1 <- sxtScale(sample = sample1, method = input$daMAscale,
                                           center = input$daMAcenter)

                       ##prediction model
                       Y <- as.numeric(as.factor(sample.info[,"group"]))
                       Y <- Y - 1

                       if(!is.null(da.ms1.validation$data) & !is.null(da.sample.info.validation$data)){
                         sample.info2 <- da.sample.info.validation$data
                         sample.info2 <- sample.info2[sample.info2$group %in% c(control.group, case.group),]

                         data2 <- da.ms1.validation$data[[1]]
                         sample2 <- data2[,match(sample.info2$sample.name, colnames(data2))]
                         rownames(sample2) <- data2$name
                         sample2 <- sample2[which(rownames(sample2) %in% marker$name),]

                         sample2 <- sxtLog(sample = sample2, method = input$daMAlog)
                         ##scale
                         sample2 <- sxtScale(sample = sample2, method = input$daMAscale, center = input$daMAcenter)
                         Y2 <- as.numeric(as.factor(sample.info2[,"group"]))
                         Y2 <- Y2 - 1
                       }

                       if(input$daValidationPredictionModel == "pls"){
                         prediction.model <- pls::plsr(Y ~ t(sample1), scale = FALSE,
                                                       validation = "CV", ncomp = 3, method = "oscorespls")
                         prediction.Y <- predict(prediction.model, t(sample1), ncomp = 3)
                         roc1 <- pROC::roc(Y, prediction.Y, ci = TRUE)
                         if(!is.null(da.ms1.validation$data) & !is.null(da.sample.info.validation$data)){
                           prediction.Y2 <- predict(prediction.model, t(sample2), ncomp = 3)
                           roc2 <- pROC::roc(Y2, prediction.Y2, ci = TRUE)
                         }
                       }

                       if(input$daValidationPredictionModel == "rf"){
                         prediction.model <- randomForest(t(sample1), Y, prox = TRUE,
                                                          importance = TRUE)
                         prediction.Y <- predict(prediction.model, t(sample1))
                         roc1 <- pROC::roc(Y, prediction.Y, ci = TRUE)
                         if(!is.null(da.ms1.validation$data) & !is.null(da.sample.info.validation$data)){
                           prediction.Y2 <- predict(prediction.model, t(sample2))
                           roc2 <- pROC::roc(Y2, prediction.Y2, ci = TRUE)
                         }
                       }

                       if(input$daValidationPredictionModel == "svm"){
                         prediction.model <- e1071::svm(t(sample1),Y,scale = FALSE)
                         prediction.Y <- predict(prediction.model, t(sample1))
                         roc1 <- pROC::roc(Y, prediction.Y, ci = TRUE)
                         if(!is.null(da.ms1.validation$data) & !is.null(da.sample.info.validation$data)){
                           prediction.Y2 <- predict(prediction.model, t(sample2))
                           roc2 <- pROC::roc(Y2, prediction.Y2, ci = TRUE)
                         }
                       }

                       if(input$daValidationPredictionModel == "lr"){
                         dis.data <- data.frame(Y, t(sample1), stringsAsFactors = FALSE)
                         prediction.model <- glm(Y ~ ., family = binomial,
                                                 data = dis.data)
                         prediction.Y <- predict(prediction.model, as.data.frame(t(sample1)))
                         roc1 <- pROC::roc(Y, prediction.Y, ci = TRUE)
                         if(!is.null(da.ms1.validation$data) & !is.null(da.sample.info.validation$data)){
                           prediction.Y2 <- predict(prediction.model, as.data.frame(t(sample2)))
                           roc2 <- pROC::roc(Y2, prediction.Y2, ci = TRUE)
                         }
                       }
                     })

                     if(class(temp)[1] == "try-error"){
                       info <- temp[[1]]
                       output$ROC.message <- renderText({paste("ROC: ",info)})
                       daValidationROC1$data <- NULL
                       daValidationROC2$data <- NULL
                       rm(list = c("temp"))
                     }else{
                       output$ROC.message <- renderText({""})
                       daValidationROC1$data <- roc1
                       rm(list = c("temp"))
                       if(!is.null(da.ms1.validation$data) & !is.null(da.sample.info.validation$data)){
                         daValidationROC2$data <- roc2
                       }
                     }

                     rm(list = c("sample.info", "control.group", "case.group", "marker",
                                 "data1", "sample1", "Y", "prediction.model", "prediction.Y"))

                     if(!is.null(da.ms1.validation$data) & !is.null(da.sample.info.validation$data)){
                       rm(list = c("sample.info2",
                                   "data2", "sample2", "Y2", "prediction.Y2"))
                     }

                     if(input$daValidationPredictionModel == "lr"){
                       rm(list = c("dis.data"))
                     }
                   })
                 })
  }
})


daValidationROClplot1 <- reactiveValues(data = NULL)
observe({
  # if(!is.null(daValidationROC1$data)){
 temp <- try({
   ROCplot(roc.object = daValidationROC1$data)
})
 if(class(temp)[1] == "try-error"){
   info <- temp[[1]]
   output$daValidationROClplot1.message <- renderText({paste("ROC plot discovery: ",info)})
   rm(list = c("temp"))
 }else{
   daValidationROClplot1$data <- temp
   output$daValidationROClplot1.message <- renderText({""})
   rm(list = c("temp"))
 }
  # }
})

daValidationROClplot2 <- reactiveValues(data = NULL)
observe({
  # if(!is.null(daValidationROC2$data)){
    temp <- try({
      ROCplot(roc.object = daValidationROC2$data)
    })
    if(class(temp)[1] == "try-error"){
      info <- temp[[1]]
      output$daValidationROClplot2.message <- renderText({paste("ROC plot validation: ",info)})
      rm(list = c("temp"))
    }else{
      daValidationROClplot2$data <- temp
      output$daValidationROClplot2.message <- renderText({""})
      rm(list = c("temp"))
    }
  # }
})





output$daValidationROClplot1 <- renderPlotly(expr = {
  if(!is.null(daValidationROClplot1$data)){
    ggplotly(daValidationROClplot1$data)
  }
})


output$daValidationROClplot2 <- renderPlotly(expr = {
  if(!is.null(daValidationROClplot2$data)){
    ggplotly(daValidationROClplot2$data)
  }
})


##jump to result download
observeEvent(input$daValidation2Download, {
  updateTabsetPanel(session, "DAtab",
                    selected = "DAdownload")

})



















##download
####generate analysis report
observeEvent(eventExpr = input$da.generate.analysis.report, {
  if(is.null(daDifferentialMetabolite$data)) return(NULL)
  now.path <- getwd()
  user.path <- file.path("user_data", user_input$username, input$DAprojectID, "differential_analysis")
  tempReport <- file.path(now.path, user.path, "DAreport.temp.Rmd")
  file.copy("data/markdown/DAreport.Rmd", tempReport, overwrite = TRUE)

  params <- list(##parameters
                   da.ua.params = da.ua.params$data,
                   da.ma.params = da.ma.params$data,
                   da.dm.params = da.dm.params$data,
                   da.validation.params = da.validation.params$data,
                   da.vocano.plot = da.volcano.plot$data,
                   daMApcaPlot = daMApcaPlot$data,
                   daMAplsPlot = daMAplsPlot$data,
                   daMAhcaHeatmap = daMAhcaHeatmap$data,
                   daValidationPCAplot1 = daValidationPCAplot1$data,
                   daValidationPCAplot2 = daValidationPCAplot2$data,
                   daValidationPLSplot1 = daValidationPLSplot1$data,
                   daValidationPLSplot2 = daValidationPLSplot2$data,
                   daValidationHCAheatmap1 = daValidationHCAheatmap1$data,
                   daValidationHCAheatmap2 = daValidationHCAheatmap2$data,
                   daValidationROClplot1 = daValidationROClplot1$data,
                   daValidationROClplot2 = daValidationROClplot2$data
                   )
  # save(params, file = "params")
  withProgress(message = 'Generate analysis report...',
               detail = 'This may take a while',
               # style= "old",
               value = 0, {
                 lapply(1:length(tempReport), function(idx){
                   incProgress(1/length(tempReport),
                               detail = "This may take a while")
                   temp <- try({
                     rmarkdown::render(tempReport,
                                       output_file = file.path(now.path, user.path,
                                                               "Analysis.Report.of.Differential.Metabolite.Discovery.html"),
                                       params = params,
                                       envir = new.env(parent = globalenv())
                     )
                   })

                   if(class(temp)[1] == "try-error"){
                     info <- temp[[1]]
                     output$generateReport.message <- renderText({paste("Generate report: ",info)})
                     rm(list = c("temp"))
                   }else{
                     output$generateReport.message <- renderText({""})
                     rm(list = c("temp"))
                   }

                 })
               })
})


####download analysis report
output$da.report.download <- downloadHandler(
  filename = "Analysis.Report.of.Differential.Metabolite.Discovery.html",
  content = function(file){
    file.copy(file.path("user_data", user_input$username,
                        input$DAprojectID, "differential_analysis",
                        "Analysis.Report.of.Differential.Metabolite.Discovery.html"), file)
  }
)

####generate analysis result
observeEvent(eventExpr = input$da.generate.analysis.result, {

  if(!is.null(daDifferentialMetabolite$data)){
    #ZIP data
    withProgress(message = 'Generate analysis result...',
                 detail = 'This may take a while',
                 # style= "old",
                 value = 0, {
                   lapply(1:length(1), function(idx){
                     incProgress(1/length(1),
                                 detail = "This may take a while")

                     temp <- try({
                       now.path <- getwd()
                       temp.path <- file.path(now.path, "user_data", user_input$username,
                                              input$DAprojectID, "differential_analysis","Differential_Metabolite_Discovery_Result")
                       dir.create(temp.path)
                       temp.path1 <- file.path(temp.path, "Figures")
                       dir.create(temp.path1)


                       readr::write_csv(daDifferentialMetabolite$data,
                                        file.path(temp.path, "Differential.metabolites.csv"))

                       ##volcanplot
                       ggsave(da.volcano.plot$data,
                              file = file.path(temp.path1, "Volcano.plot.pdf"), width = 7, height = 7)

                       ggsave(daMApcaPlot$data,
                              file = file.path(temp.path1, "PCA.score.plot.using.all.peaks.pdf"), width = 7, height = 7)

                       ggsave(daMAplsPlot$data,
                              file = file.path(temp.path1, "PLS.score.plot.using.all.peaks.pdf"), width = 7, height = 7)

                       ggsave(daMAhcaHeatmap$data,
                              file = file.path(temp.path1, "Heatmap.using.all.peaks.pdf"), width = 7, height = 7)

                       # pdf(file = file.path(temp.path1, "Heatmap.using.all.peaks.pdf"),
                       #     width = 7, height = 7)
                       # daMAhcaHeatmap$data
                       # dev.off()

                       ggsave( daValidationHCAheatmap1$data,
                              file = file.path(temp.path1, "Heatmap.using.potential.markers.in.discovery.dataset.pdf"),
                              width = 7, height = 7)

                       # pdf(file = file.path(temp.path1, "Heatmap.using.potential.markers.in.discovery.dataset.pdf"), width = 7, height = 7)
                       # daValidationHCAheatmap1$data
                       # dev.off()

                       ggsave( daValidationHCAheatmap2$data,
                               file = file.path(temp.path1, "Heatmap.using.potential.markers.in.validation.dataset.pdf"),
                               width = 7, height = 7)

                       # pdf(file = file.path(temp.path1, "Heatmap.using.potential.markers.in.validation.dataset.pdf"), width = 7, height = 7)
                       # daValidationHCAheatmap2$data
                       # dev.off()

                       ggsave(daValidationPCAplot1$data,
                              file = file.path(temp.path1, "PCA.score.plot.using.potential.markers.in.discovery.dataset.pdf"), width = 7, height = 7)

                       ggsave(daValidationPCAplot2$data,
                              file = file.path(temp.path1, "PCA.score.plot.using.potential.markers.in.validation.dataset.pdf"), width = 7, height = 7)

                       ggsave(daValidationPLSplot1$data,
                              file = file.path(temp.path1, "PLS.score.plot.using.potential.markers.in.discovery.dataset.pdf"), width = 7, height = 7)

                       ggsave(daValidationPLSplot2$data,
                              file = file.path(temp.path1, "PLS.score.plot.using.potential.markers.in.validation.dataset.pdf"), width = 7, height = 7)

                       ggsave(daValidationROClplot1$data,
                              file = file.path(temp.path1, "AUC.using.potential.markers.in.discovery.dataset.pdf"), width = 7, height = 7)

                       ggsave(daValidationROClplot2$data,
                              file = file.path(temp.path1, "AUC.using.potential.markers.in.validation.dataset.pdf"), width = 7, height = 7)


                       setwd(file.path("user_data", user_input$username, input$DAprojectID, "differential_analysis"))
                       zip::zip(zipfile = file.path(now.path, "user_data", user_input$username, input$DAprojectID, "differential_analysis",
                                                                    "Differential_Metabolite_Discovery_Result.zip"),
                                                files = "Differential_Metabolite_Discovery_Result",
                                                recurse = TRUE
                     )
                     })

                     if(class(temp) == "try-error"){
                       info <- temp[[1]]
                       setwd(now.path)
                       output$generateResult.message <- renderText({paste("Generate result: ",info)})
                       rm(list = c("temp"))
                     }else{
                       setwd(now.path)
                       output$generateResult.message <- renderText({""})
                       rm(list = c("temp"))
                     }
                   })
                 })
  }
})


####download analysis result
output$da.result.download <- downloadHandler(
  filename = "Differential_Metabolite_Discovery_Result.zip",
  content = function(file){
    file.copy(file.path("user_data", user_input$username,
                        input$DAprojectID, "differential_analysis",
                        "Differential_Metabolite_Discovery_Result.zip"), file)
  }
)



