#####################################upload data###############################
#####one step data cleaning
###one data cleaning.area
output$oneDatacleaning.area <- renderUI({
  if (user_input$authenticated == TRUE){
    ui.oneDatacleaning()
  }else{
    not_logged_in5()
  }
})


##warning if no data selected or project name is null
observeEvent(eventExpr = input$one.dc.upload.button, {
    if(is.null(input$oneDCms1PeakTable) | is.null(input$oneDCsampleInfo) | input$oneDCprojectID == ""){
      shinyalert::shinyalert(title = "Error",
                             text = "Project name, MS1 peak table and Sample information are required.",
                             type = "error")
    }
})


##create folder
observeEvent(eventExpr = input$one.dc.upload.button,{
  if(!is.null(input$oneDCprojectID)){
    if(input$oneDCprojectID != ""){
      dir.create(file.path("./user_data", user_input$username))
      dir.create(file.path("./user_data", user_input$username, input$oneDCprojectID))
      dir.create(file.path("./user_data", user_input$username, input$oneDCprojectID, "one_data_cleaning"))
    }
  }
})

##one.user.path
one.user.path <- reactive({
  req(user_input$username)
  req(input$oneDCprojectID)
  file.path("user_data", user_input$username, input$oneDCprojectID, "one_data_cleaning")
})

##warning if no data selected or project name is null
observeEvent(eventExpr = input$one.dc.data.check.2.parameter, {
  if(is.null(input$oneDCms1PeakTable) | is.null(input$oneDCsampleInfo) | input$oneDCprojectID == ""){
    shinyalert::shinyalert(title = "Error",
                           text = "Submit first",
                           type = "error")
  }
})


###record job number
observeEvent(input$one.dc.upload.button,{
  if(!is.null(input$oneDCprojectID)){
    if(input$oneDCprojectID != ""){
      credentials <- readRDS("credentials/credentials.rds")
      credentials$job.number[match(user_input$username, credentials$user)] <- as.numeric(credentials$job.number[match(user_input$username, credentials$user)])+1
      saveRDS(credentials, "credentials/credentials.rds")
    }
  }
})















###read MS1 peak table
one.dc.ms1.raw <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$one.dc.upload.button, {
  withProgress(message = 'Upload MS1 peak table...',
               detail = 'This may take a while',
               # style= "old",
               value = 0, {#upload data
                   if (!is.null(input$oneDCms1PeakTable) & input$oneDCprojectID != "") {
                     temp.file <- input$oneDCms1PeakTable$datapath
                     one.dc.ms1.raw$data <- lapply(1:length(temp.file), function(idx){
                       incProgress(1/length(temp.file),
                                   detail = paste("Data", idx))
                       as.data.frame(readr::read_csv(temp.file[idx],
                                                     col_types = readr::cols()))
                     })
                   }
               })
})


one.dc.ms2.raw <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$one.dc.upload.button, {
  withProgress(message = 'Upload MS2 identification result table...',
               detail = 'This may take a while',
               # style= "old",
               value = 0, {#upload data
                 if (!is.null(input$oneDCms2PeakTable) & input$oneDCprojectID != "") {
                   temp.file <- input$oneDCms2PeakTable$datapath
                   one.dc.ms2.raw$data <- lapply(1:length(temp.file), function(idx){
                     incProgress(1/length(temp.file),
                                 detail = paste("Data", idx))
                     as.data.frame(readr::read_csv(temp.file[idx],
                                                   col_types = readr::cols()))
                   })
                 }
               })
})


## read sample.info
one.dc.sample.info.raw <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$one.dc.upload.button, {
    if (!is.null(input$oneDCsampleInfo) & input$oneDCprojectID != "") {
      one.dc.sample.info.raw$data <-
        as.data.frame(readr::read_csv(input$oneDCsampleInfo$datapath,col_types = readr::cols()))
    }
})



















#------------------------------------------------------------------------------
##Check data
one.dc.data.check.result <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$one.dc.upload.button,{
  if(!is.null(one.dc.ms1.raw$data) & !is.null(one.dc.sample.info.raw$data) & input$oneDCprojectID != ""){
    one.dc.data.check.result$data <- checkData(peak.table = one.dc.ms1.raw$data,
                              sample.info = one.dc.sample.info.raw$data)
  }else{
    one.dc.data.check.result$data <- data.frame("Data.File" = "Data",
                               "Information" = "You don't provide MS1 peak table or sample information",
                               "Result" = "Error",
                               stringsAsFactors = FALSE)
  }
})


output$one.dc.data.check.result <- DT::renderDataTable(
  DT::datatable(one.dc.data.check.result$data,
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
                                       "Error: No rt"),
                                     c(rep('#FC4B4E', 12)))
  ),
  server = FALSE
)



##jump to parameter setting page from data check
observeEvent(input$one.dc.data.check.2.parameter, {
  if(length(grep(pattern = "Error",one.dc.data.check.result$data[,3])) == 0){
    if(!is.null(one.dc.ms1.raw$data) & !is.null(one.dc.sample.info.raw$data)){
      updateTabsetPanel(session, "oneDCtab",
                        selected = "oneDCparameter")
    }
  }
})

##error if there is error in data
observeEvent(eventExpr = input$one.dc.data.check.2.parameter, {
  if(length(grep(pattern = "Error",one.dc.data.check.result$data[,3])) != 0){
    shinyalert::shinyalert(title = "Error",
                           text = "There are errors in you files, please check and
                           upload again.",
                           type = "error")
  }

})


##write MS1 peak table
observeEvent(eventExpr = input$one.dc.data.check.2.parameter,{
  if(length(grep(pattern = "Error",one.dc.data.check.result$data[,3])) == 0){
    if(!is.null(one.dc.ms1.raw$data)){
      for(i in 1:length(one.dc.ms1.raw$data)){
        readr::write_csv(one.dc.ms1.raw$data[[i]],
                         file.path(one.user.path(), paste("batch", i, ".csv", sep = "")))
      }
    }

    if(!is.null(one.dc.sample.info.raw$data)){
      readr::write_csv(one.dc.sample.info.raw$data, file.path(one.user.path(),"sample.info.csv"))
    }
  }
})


##write MS2 peak table
observeEvent(eventExpr = input$one.dc.data.check.2.parameter,{
  if(length(grep(pattern = "Error",one.dc.data.check.result$data[,3])) == 0){
    if(!is.null(one.dc.ms2.raw$data)){
      dir.create(file.path(one.user.path(), "peak identification"))
      for(i in 1:length(one.dc.ms2.raw$data)){
        readr::write_csv(one.dc.ms2.raw$data[[i]],
                         file.path(one.user.path(), "peak identification", paste("ms2_", i, ".csv", sep = "")))
      }
    }
  }
})



















####paramete setting and run MetCleaning
##upload parameter table
one.dc.param.table <- reactiveValues(data = NULL)
one.dc.param.table.check.result <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$one.dc.upload.param.button,{
  if(!is.null(input$oneDCparamTable$datapath)){
    param.table <- as.data.frame(readxl::read_excel(input$oneDCparamTable$datapath))
    load("data/demo_data/demoParamTable")
    check.result <- checkParamTable(userParamTable = param.table,
                                    demoParamTable = demoParamTable)
    one.dc.param.table$data <- param.table
    one.dc.param.table.check.result$data <- check.result
  }else{
    shinyalert::shinyalert(title = "Error",
                           text = "Please select parameter table",
                           type = "error")
  }
})


###error if there is no parameter table
observeEvent(eventExpr = input$oneDCshowParamTable, {
  if(is.null(one.dc.param.table$data)){
    shinyalert::shinyalert(title = "Error",
                           text = "There is no parameter table",
                           type = "error")
  }
})

###error if there the paramTable is wrong
observeEvent(eventExpr = input$oneDCshowParamTable, {
  if(!is.null(one.dc.param.table$data)){
    if(one.dc.param.table.check.result$data != "OK"){
      shinyalert::shinyalert(title = "Error",
                             text = one.dc.param.table.check.result$data,
                             type = "error")
    }
  }
})


output$one.dc.param.table <- DT::renderDataTable(
  DT::datatable(one.dc.param.table$data,
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
                        buttons = list(list(extend="csv",filename = "Parameter.Table"),
                                       list(extend='excel',filename = "Parameter.Table")),
                        text = 'Download'
                      ),
                      I('colvis')
                    ),
                  scrollX = TRUE
                )
  ) %>% DT::formatStyle(
    'Parameter',
    target = 'row',
    backgroundColor = DT::styleEqual(c("Global Parameters",
                                       "Batch alignment",
                                       "Missing Value Processing",
                                       "Zero Value Processing",
                                       "Data Normalization",
                                       "Data Integration",
                                       "Outlier Processing",
                                       "Metabolite identification"),
                                     c(rep('#1E90FF', 8)))
  ),
  server = FALSE
)



###error if there the paramTable is wrong
observeEvent(eventExpr = input$one.dc.upload.param.button, {
  if(!is.null(one.dc.param.table$data)){
    if(one.dc.param.table.check.result$data == "OK"){
      shinyalert::shinyalert(title = "NOTE",
                             text = "The paramters are from Parameter Table!",
                             type = "info")
    }
  }
})



###notic that the parameter will form parameter table
oneDCparamFrom <- reactive({
  if(!is.null(one.dc.param.table$data)) return("NOTE: The paramters are from Parameter Table!")
})

output$oneDCparamFrom <- renderText(expr = {
  oneDCparamFrom()
})















##save parameter
# observeEvent(eventExpr = input$one.dc.data.check.result, {
# if(!is.null(one.dc.ms1.raw)){
#  save(one.dc.ms1.raw$data, file = file.path(one.user.path, "one.dc.ms1.raw"))
# }
# })

##run MetCleaning
##prepare parameter
oneDCparameter <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$oneDCsubmit,{
  # observeEvent(eventExpr = input$one.dc.data.check.2.parameter,{
  # observe({
if(!is.null(one.dc.param.table$data)){
  oneDCparameter$data <- as.character(one.dc.param.table$data[,2])
  names(oneDCparameter$data) <- as.character(one.dc.param.table$data[,1])
  oneDCparameter$data <- oneDCparameter$data[!is.na(oneDCparameter$data)]
  oneDCparameter$data <- sapply(oneDCparameter$data, list)
  oneDCparameter$data[[2]] <- as.numeric(oneDCparameter$data[[2]])
  oneDCparameter$data[[3]] <- as.numeric(oneDCparameter$data[[3]])
  oneDCparameter$data[[4]] <- as.numeric(oneDCparameter$data[[4]])
  oneDCparameter$data[[5]] <- paraTrans(ui.para = oneDCparameter$data[[5]])
  oneDCparameter$data[[6]] <- as.numeric(oneDCparameter$data[[6]])
  oneDCparameter$data[[7]] <- as.numeric(oneDCparameter$data[[7]])
  oneDCparameter$data[[8]] <- as.numeric(oneDCparameter$data[[8]])
  oneDCparameter$data[[9]] <- as.numeric(oneDCparameter$data[[9]])
  oneDCparameter$data[[10]] <- as.logical(oneDCparameter$data[[10]])
  oneDCparameter$data[[11]] <- as.numeric(oneDCparameter$data[[11]])
  oneDCparameter$data[[12]] <- as.numeric(oneDCparameter$data[[12]])
  oneDCparameter$data[[13]] <- as.character(oneDCparameter$data[[13]])
  oneDCparameter$data[[14]] <- paraTrans(oneDCparameter$data[[14]])
  oneDCparameter$data[[15]] <- as.logical(oneDCparameter$data[[15]])
  oneDCparameter$data[[16]] <- as.logical(oneDCparameter$data[[16]])
  oneDCparameter$data[[17]] <- as.numeric(strsplit(oneDCparameter$data[[17]], split = ",")[[1]])
  # oneDCparameter$data[[17]] <- as.character(oneDCparameter$data[[17]])
  oneDCparameter$data[[18]] <- as.numeric(oneDCparameter$data[[18]])
  oneDCparameter$data[[19]] <- as.numeric(oneDCparameter$data[[19]])
  # oneDCparameter$data[[20]] <- as.logical(oneDCparameter$data[[20]])
  oneDCparameter$data[[21]] <- paraTrans(oneDCparameter$data[[21]])
  oneDCparameter$data[[22]] <- paraTrans(oneDCparameter$data[[22]])
  oneDCparameter$data[[23]] <- paraTrans(oneDCparameter$data[[23]])
  oneDCparameter$data[[24]] <- as.logical(oneDCparameter$data[[24]])
  oneDCparameter$data[[25]] <- as.numeric(oneDCparameter$data[[25]])
  oneDCparameter$data[[26]] <- as.numeric(oneDCparameter$data[[26]])
  oneDCparameter$data[[27]] <- as.logical(oneDCparameter$data[[27]])
  oneDCparameter$data[[28]] <- as.logical(oneDCparameter$data[[28]])
  ##mz tolerance and rt tolerance for
  oneDCparameter$data[[29]] <- as.numeric(oneDCparameter$data[[29]])
  oneDCparameter$data[[30]] <- as.numeric(oneDCparameter$data[[30]])
  # temp1 <- oneDCparameter$data
  # save(temp1, file = "temp1")

}else{
oneDCparameter$data <- list(input$oneDCpolarity, input$one.dc.ba.mz.tol, input$one.dc.ba.rt.tol,#1:3
                         input$one.dc.var.mv.cutoff, input$imputationMethod, input$one.dc.k,#4:6
                         input$one.dc.rowmax, input$one.dc.colmax, input$one.dc.mv.ntree,#7:9
                         input$one.dc.mvReplace, input$one.dc.nPcs, input$one.dc.var.zero.cutoff,#10:12
                         input$oneDCdnHasQC, input$oneDCnormalizationMethod1, input$one.dc.svr.kepp.dimension,#13:15
                         input$one.dc.parameter.optimization, input$one.dc.begin.end, input$one.dc.loess.step,#16:18
                         input$one.dc.svr.multiple, input$oneDCdiHasQC, NA,#19:21
                         input$one.dc.os.pca.log, input$one.dc.os.pca.scale, input$one.dc.os.pca.center,#22:24
                         input$one.dc.os.pca.ci.tol, input$one.dc.os.zero.tol, input$one.dc.qc.outlier.filter,#25:27
                         input$one.dc.subject.outlier.filter, input$one.dc.mi.mz.tol, input$one.dc.mi.rt.tol#28:30
                         )
if(input$oneDCdiHasQC == "hasQC"){
  oneDCparameter$data[[21]] <- input$oneDCintegrationMethod1
}else{
  oneDCparameter$data[[21]] <- input$oneDCintegrationMethod2
}
}
})




observeEvent(eventExpr = input$oneDCsubmit, {

  # if(!is.null(oneDCparameter$data)){
  # save(oneDCparameter$data, file = file.path(one.user.path(), "oneDCparameter"))
  # }
  temp.file <- dir(one.user.path())
  temp.file <- setdiff(temp.file, "peak.table.after.batch.alignment.csv")
if(!is.null(one.dc.ms1.raw$data) & !is.null(one.dc.sample.info.raw$data)){
  # cat("test3\n")
  withProgress(message = 'Processing...',
               detail = 'This may take a while',
               # style= "old",

               value = 0, {#upload data
                   lapply(1:1, function(idx){
                     incProgress(1/1,
                                 detail = "This may take a while")
metcleaning.info <- try(expr = {metflowR::metflowR(
  data = grep("batch", x = temp.file, value = TRUE),
  sample.information = "sample.info.csv",
  polarity = oneDCparameter$data[[1]],
  hasIS = "no",
  hasQC = "yes",
  #batch alignment
  combine.mz.tol = oneDCparameter$data[[2]],
  combine.rt.tol = oneDCparameter$data[[3]],
  #MVFilter para
  mv.filter = TRUE,
  obs.mv.cutoff = 0,
  var.mv.cutoff = 1 - oneDCparameter$data[[3]]/100,
  #MVimputation
  imputation.method = oneDCparameter$data[[5]],
  k = oneDCparameter$data[[6]],
  rowmax = oneDCparameter$data[[7]],
  colmax = oneDCparameter$data[[8]],
  maxp = 1500,
  #ZeroFilter para
  zero.filter = TRUE,
  obs.zero.cutoff = 0,
  var.zero.cutoff = 1 - oneDCparameter$data[[12]]/100,
  #DataNormalization
  normalization = TRUE,
  method = oneDCparameter$data[[14]],
  multiple = oneDCparameter$data[[19]],
  threads = 2,
  #PeakIdentification
  hmdb.matching = FALSE,
  show = 5,
  mass.tolerance = 30,
  mz.tolerance = oneDCparameter$data[[29]],
  rt.tolerance = oneDCparameter$data[[30]],
  #DataOverview para
  met.plot = FALSE,
  path = one.user.path(),
  # worklist.from = "manual",
  #other slection
  qc.outlier.filter = TRUE,
  subject.outlier.filter = TRUE,
  integration = TRUE)},silent = FALSE)

if(class(metcleaning.info) == "try-error"){
  if(metcleaning.info[[1]] == "Error in UseMethod(\"layout\") : \n  no applicable method for 'layout' applied to an object of class \"c('double', 'numeric')\"\n"){
    output$metcleaning.info <- renderText({"MetCleaning is done!"})
  }else{
    output$metcleaning.info <- renderText({metcleaning.info[[1]]})
  }
}else{
  output$metcleaning.info <- renderText({"MetCleaning is done!"})
}
                   })
               })
}
})

###error if there is no results from metcleaning
observeEvent(eventExpr = input$oneDCparameter2resultDownload, {
  temp.file <- dir(one.user.path())
  if(all(temp.file != "10 RSD overview")){
      shinyalert::shinyalert(title = "Error",
                             text = "Please click Submit first!",
                             type = "error")

  }
})

##jump to result download from parameter setting
observeEvent(input$oneDCparameter2resultDownload, {
  temp.file <- dir(one.user.path())
  if(any(temp.file == "10 RSD overview")){
    updateTabsetPanel(session, "oneDCtab",
                      selected = "oneDCdownload")
  }
})




















##download result
observeEvent(eventExpr = input$one.dc.generate.analysis.result, {
  now.path <- getwd()
  temp.path <- file.path(one.user.path(),"Data_Cleaning_Result")
  dir.create(temp.path)
  temp.result <- c("1 MV overview", "2 MV filter", "3 Zero overview",
                   "4 Zero filter", "5 QC outlier filter", "6 Subject outlier finder",
                   "7 Normalization result", "8 Batch effect",
                   "10 RSD overview", "11 Data overview", "intermediate", "met.data.after.pre",
                   "peak.table.after.data.cleaning.csv", "qc.info.csv", "subject.info.csv",
                   "peak.table.after.batch.alignment.csv")

  ##copy data
  withProgress(message = 'Copy data...',
               detail = "",
               value = 0,{
                 lapply(1:length(temp.result), function(idx){
                   incProgress(1/length(temp.result),
                               detail = temp.result[idx])
                   file.copy(from = file.path(one.user.path(), temp.result[idx]),
                             to = temp.path, overwrite = TRUE, recursive = TRUE)
                 })
               })

  #ZIP data
  withProgress(message = 'Zip result...',
               detail = 'This may take a while',
               # style= "old",
               value = 0, {
                 lapply(1:length(1), function(idx){
                   incProgress(1/length(1))
                   setwd(one.user.path())
                   temp.error <- try(zip::zip(zipfile = "Data_Cleaning_Result.zip",
                                              files = "Data_Cleaning_Result",
                                              recurse = TRUE
                   ))

                   if(class(temp.error) == "try-error"){
                     setwd(now.path)
                   }
                   setwd(now.path)
                 })
               })

  ##remove some result
  withProgress(message = 'Delete results...',
               detail = "",
               value = 0,{
                 lapply(1:length(temp.result), function(idx){
                   incProgress(1/length(temp.result),
                               detail = temp.result[idx])
                   unlink(file.path(one.user.path(), temp.result[idx]), recursive = TRUE)
                 })
               })

  unlink(file.path(one.user.path(), "Data_Cleaning_Result"), recursive = TRUE)
  unlink(file.path(one.user.path(), "intermediate"), recursive = TRUE)
})



####download analysis result
output$one.dc.result.download <- downloadHandler(
  filename = "Data_Cleaning_Result.zip",
  content = function(file){
    file.copy(file.path(one.user.path(), "Data_Cleaning_Result.zip"), file)
  }
)


