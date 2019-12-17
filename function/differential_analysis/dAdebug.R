da.ms1.raw <- lapply(1:1, function(x)
  as.data.frame(readr::read_csv("D:/study/R/my git/metCube/version1.0.2/data/demo_data/Differential_Analysis/data.dis.csv")))
# da.ms1.raw <- as.data.frame(da.ms1.raw)

# da.sample.info.raw <- readr::read_csv("F:/data pre-processing/tujia/sample.info.demo-1.csv")
da.sample.info.raw <- readr::read_csv("D:/study/R/my git/metCube/version1.0.2/data/demo_data/Differential_Analysis/sample.info.dis.csv")

da.sample.info.raw <- as.data.frame(da.sample.info.raw)



    check.result <- checkData(peak.table = da.ms1.raw,
                              sample.info = da.sample.info.raw,
                              step = "biomarker")


  check.result <- as.data.frame(check.result)
  check.result


#-------------------------------------------------------------------------------
#univariate analysis

  temp <- unique(da.sample.info.raw[,"group"])
  da.group.name <- temp




##save parameter

  da.ua.control <- "Control"
  da.ua.case <- 'Case'
  daUAlog <- "no"
  daUAscale <- "auto"
  daUAcenter <- TRUE
  daUAfcWhich <- "mean"
  daUAhypothesisTesting <- "t"
  daUApaired <- FALSE
  daUAadjust <- "fdr"
  daUAalternative <- "two.sided"
    da.ua.params <- DAuaParam( da.ua.control = da.ua.control,
                                  da.ua.case = da.ua.case,
                                  daUAlog = daUAlog,
                                  daUAscale = daUAscale,
                                  daUAcenter = daUAcenter,
                                  daUAfcWhich = daUAfcWhich,
                                  daUAhypothesisTesting = daUAhypothesisTesting,
                                  daUAalternative = daUAalternative,
                                  daUApaired = daUApaired,
                                  daUAadjust = daUAadjust)

    da.ua.params


###log, scale or center sample
  sample.info <- da.sample.info.raw
  data <- da.ms1.raw[[1]]
  sample <- data[,match(sample.info[,1], colnames(data)), drop = FALSE]
  tags <- data[,-match(sample.info[,1], colnames(data)), drop = FALSE]

  ##log
  sample <- sxtLog(sample = sample,  method = daUAlog)
  ##scale
  sample <- sxtScale(sample = sample, method = daUAscale, center = daUAcenter)

  control.group <- da.ua.control
  case.group <- da.ua.case
  control.group.name <- sample.info[,"sample.name"][which(sample.info[,"group"]==control.group)]
  case.group.name <- sample.info[,"sample.name"][which(sample.info[,"group"]==case.group)]

  control.group.idx <- match(control.group.name, colnames(sample))
  case.group.idx <- match(case.group.name, colnames(sample))

  ##fold change
                     daUAfc <- try({apply(sample, 1, function(x){
                       median(x[case.group.idx])/median(x[control.group.idx])
                     })
                     })

  daUAfc[is.na(daUAfc)] <- 1
  daUAfc[is.nan(daUAfc)] <- 1
  daUAfc[is.infinite(daUAfc)] <- max(daUAfc, na.rm = TRUE)
  names(daUAfc) <- tags[,"name"]

  ##p value

                   daUAp <- apply(sample, 1, function(x){
                     control <- as.numeric(x[control.group.idx])
                     case <- as.numeric(x[case.group.idx])
                     if(all(is.na(control))) control[is.na(control)] <- 1
                     if(all(is.na(case))) case[is.na(case)] <- 1
                     control[is.na(control)] <- mean(control, na.rm = TRUE)
                     case[is.na(case)] <- mean(case, na.rm = TRUE)
                     temp <- try({wilcox.test(x = control,
                                         y = case,
                                         alternative = daUAalternative,
                                         paired = daUApaired)}, silent = TRUE)
                     if(class(temp) == "try-error"){
                       1
                     }else{
                       temp$p.value
                     }
                   })


  names(daUAp) <- tags[,"name"]


    daUAp <- p.adjust(daUAp, method = daUAadjust)








  temp <- data.frame(names(daUAfc), daUAfc, daUAp, stringsAsFactors = FALSE)
  colnames(temp) <- c("name", "Fold-change", "P-value")
  da.ms1.p.fc <- temp



  ##volcano plot
  daDMpCutoff <- 0.05
  daDMfcCutoff <- 2

  da.volcano.plot <- try({volcanoPlot(object = da.ms1.p.fc,
                                      control.group.name = da.ua.control,
                                      case.group.name = da.ua.case,
                                      p.adjust.method = daUAadjust,
                                      p.cutoff = daDMpCutoff,
                                      fc.cutoff = daDMfcCutoff)})
  if(class(da.volcano.plot)[1] == "try-error"){
    output$da.volcano.info <- renderText({paste("Volcano plot: ",da.volcano.plot[[1]])})
  }else{
    da.volcano.plot <- da.volcano.plot
  }





  ggplotly(da.volcano.plot)



#-------------------------------------------------------------------------------
#multivariate analysis



##save parameter
  daMAlog <- "log10"
  daMAscale <- "auto"
  daMAcenter <- TRUE
  daMAncomp <- 3
  daMAhcaClusteringDistanceRows <- "euclidean"
  daMAhcaClusteringDistanceCols <- "euclidean"
  daMAhcaClusteringMethod <- "ward.D"

  da.ma.params <- DAmaParam(
    da.ma.control = da.ua.control,
    da.ma.case = da.ua.case,
    daMAlog = daMAlog,
    daMAscale = daMAscale,
    daMAcenter = daMAcenter,
    daMAncomp = daMAncomp,
    daMAhcaClusteringDistanceRows = daMAhcaClusteringDistanceRows,
    daMAhcaClusteringDistanceCols = daMAhcaClusteringDistanceCols,
    daMAhcaClusteringMethod = daMAhcaClusteringMethod)

  da.ma.params

#PCA analysis

                     data <- da.ms1.raw[[1]]
                     sample.info <- da.sample.info.raw
                     control.group <- da.ua.control
                     case.group <- da.ua.case
                     sample.info <- sample.info[sample.info$group %in% c(control.group, case.group),]
                     sample <- data[,match(sample.info$sample.name, colnames(data))]

                     ##log
                     sample <- sxtLog(sample = sample, method = daMAlog)
                     ##scale
                     sample <- sxtScale(sample = sample, method = daMAscale, center = daMAcenter)


                     daMApca <- prcomp(data.frame(t(sample)),
                                            retx = TRUE,
                                            center = FALSE,
                                            scale = FALSE)




  sample.info <- da.sample.info.raw
  control.group <- da.ua.control
  case.group <- da.ua.case
  sample.info <- sample.info[sample.info$group %in% c(control.group, case.group),]
  daMApcaPlot <- pcaScorePlot2(pca.object = daMApca, sample.info = sample.info)




  ggplotly(daMApcaPlot,
           # tooltip = c("Sample", "Injection.order", "Batch"),
           source = "daMApcaPlot")




#PLS analysis

                     data <- da.ms1.raw[[1]]
                     sample.info <- da.sample.info.raw
                     control.group <- da.ua.control
                     case.group <- da.ua.case
                     sample.info <- sample.info[sample.info$group %in% c(control.group, case.group),]
                     sample <- data[,match(sample.info$sample.name, colnames(data))]

                     ##log
                     sample <- sxtLog(sample = sample, method = daMAlog)
                     ##scale
                     sample <- sxtScale(sample = sample, method = daMAscale, center = daMAcenter)


                     pls.Y <- as.numeric(as.factor(sample.info[,"group"]))


                     ncompa <- min(nrow(sample), ncol(sample))
                     if(ncompa > 30) ncompa <- 30

                     pls.object <- plsdepot::plsreg1(t(sample), pls.Y, comps = ncompa)
                     daMApls <- pls.object





  ggplotly(plsQ2barplot(pls.object = daMApls, comps.number = 10),
           # tooltip = c("Sample", "Injection.order", "Batch"),
           source = "daMAplsQ2Plot")



###PLS analysis using user imput ncomp
daMApls2 <- list(data1 = NULL, data2 = NULL)

                       data <- da.ms1.raw[[1]]
                       sample.info <- da.sample.info.raw
                       control.group <- da.ua.control
                       case.group <- da.ua.case
                       sample.info <- sample.info[sample.info$group %in% c(control.group, case.group),]
                       sample <- data[,match(sample.info$sample.name, colnames(data))]

                       ##log
                       sample <- sxtLog(sample = sample, method = daMAlog)
                       ##scale
                       sample <- sxtScale(sample = sample, method = daMAscale, center = daMAcenter)


                       pls.Y <- as.numeric(as.factor(sample.info[,"group"]))

                       ncompa <- daMAncomp
                       if(ncompa > min(c(nrow(sample), ncol(sample)))) ncompa <- min(c(nrow(sample), ncol(sample)))

                       dummy <- SXTdummy(pls.Y)
                       int.dummy <- dummy
                       pls.object1 <- plsdepot::plsreg1(t(sample), pls.Y, comps = ncompa)
                       pls.object2 <- plsdepot::plsreg2(t(sample), int.dummy, comps = ncompa)
                       daMApls2$data1 <- pls.object1
                       daMApls2$data2 <- pls.object2



##Q2cum and R2
  ggplotly(plsQ2R2barplot(pls.object = daMApls2$data1),
           # tooltip = c("Sample", "Injection.order", "Batch"),
           source = "daMAplsQ2R2Plot")


##PLS score plot
  sample.info <- da.sample.info.raw
  control.group <- da.ua.control
  case.group <- da.ua.case
  sample.info <- sample.info[sample.info$group %in% c(control.group, case.group),]
  daMAplsPlot <-plsScorePlot(pls.object = daMApls2$data1, sample.info = sample.info)



  ggplotly(daMAplsPlot,
           # tooltip = c("Sample", "Injection.order", "Batch"),
           source = "daMAplsPlot")





###VIP value

  vip <- apply(daMApls2$data2$VIP, 1, mean)
  daMAvip <- vip
  temp <- data.frame(da.ms1.p.fc, vip, stringsAsFactors = FALSE)
  colnames(temp)[4] <- "VIP"
  daMAfcPvip <- temp







##HCA analysis

  data <- da.ms1.raw[[1]]
  sample.info <- da.sample.info.raw
  control.group <- da.ua.control
  case.group <- da.ua.case
  sample.info <- sample.info[sample.info$group %in% c(control.group, case.group),]
  sample <- data[,match(sample.info$sample.name, colnames(data))]

  ##log
  sample <- sxtLog(sample = sample, method = daMAlog)
  ##scale
  sample <- sxtScale(sample = sample, method = daMAscale, center = daMAcenter)
  group <- c(control.group, case.group)


  daMAhca <- heatMap(sample = sample,
                          sample.info = sample.info,
                          group = group
                          # int.col = c(daMAhcaLowCol, daMAhcaMiddleCol, daMAhcaHighCol),
                          # color = c(daMAhcaGroup1Col, daMAhcaGroup2Col),
                          # show_rownames = daMAhcaShowRowNames,
                          # show_colnames = daMAhcaShowColNames,
                          # cluster_rows = daMAhcaClusterRows,
                          # cluster_cols = daMAhcaClusterCols,
                          # clustering_distance_rows = daMAhcaClusteringDistanceRows,
                          # clustering_distance_cols = daMAhcaClusteringDistanceCols,
                          # clustering_method = daMAhcaClusteringMethod
                     )



  par(mar = c(5,5,4,2))
  daMAhca























##differential metabolite selection
##save parameter
  daDMpCutoff <- 0.05
  daDMfcCutoff <- 2
  daDMvipCutoff <- 1
  da.dm.params <- DAdmParam(
    daDMpCutoff = daDMpCutoff,
    daDMfcCutoff = daDMfcCutoff,
    daDMvipCutoff = daDMvipCutoff)

da.dm.params








##potential biomarker
daDifferentialMetabolite <- reactiveValues(data = NULL)
observeEvent(eventExpr = daDMsubmitButton, {
  if(is.null(daMAfcPvip)) return(NULL)

  withProgress(message = "Different metabolite selection...",
               detail = 'This may take a while',
               # style= "old",
               value = 0, {
                 lapply(1:length(1), function(idx){
                   incProgress(1/length(1),
                               detail = "This may take a while")
                   temp.idx1 <- which(daMAfcPvip[,3] < daDMpCutoff & daMAfcPvip[,2] > daDMfcCutoff)
                   temp.idx2 <- which(daMAfcPvip[,3] < daDMpCutoff & daMAfcPvip[,2] < 1/daDMfcCutoff)
                   temp.idx3 <- which(daMAfcPvip[,4] > daDMvipCutoff)
                   temp.idx <- sort(unique(c(temp.idx1, temp.idx2)))
                   temp.idx <- unique(intersect(temp.idx, temp.idx3))
                   sample.info <- da.sample.info.raw
                   data <- da.ms1.raw[[1]]
                   sample <- data[,match(sample.info[,1], colnames(data))]
                   tags <- data[,-match(sample.info[,1], colnames(data))]
                   tags <- data.frame(tags, "P-value" = daMAfcPvip[,3],
                                      "Fold-change" = daMAfcPvip[,2],
                                      "VIP" = daMAfcPvip[,4],
                                      stringsAsFactors = FALSE)
                   daDifferentialMetabolite <- data.frame(tags, sample, stringsAsFactors = FALSE)[temp.idx,]
                   rm(list = c("tags", "sample", "data", "sample.info"))
                 })
               })
})


output$daDifferentialMetabolite <- DT::renderDataTable(
  DT::datatable(daDifferentialMetabolite,
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


##jump to differential metabolite selection
observeEvent(daDM2validation, {
  if(!is.null(daDifferentialMetabolite)){
    # temp.path <- user.path()
    # save(temp.path, file = "temp.path")
    updateTabsetPanel(session, "DAtab",
                      selected = "DAvalidation")
  }
})

##error if there is no marker1
observeEvent(eventExpr = daDM2validation, {
  if(is.null(daDifferentialMetabolite)){
    shinyalert::shinyalert(title = "Error",
                           text = "Please click Submit first.",
                           type = "error")
  }

})



















##validation
##warning if no data selected
observeEvent(eventExpr = daValidationUploadButton, {
  #if don't use demo data and don't upload data or give project name, error
  # if(daValidationUseDemoData == FALSE){
  if(is.null(DAms1PeakTableValidation) | is.null(DAsampleInfoValidation)){
    shinyalert::shinyalert(title = "Error",
                           text = "MS1 peak table and Sample information for validation are required.",
                           type = "error")
  }
  # }
})


##save parameter
da.validation.params <- reactiveValues(data = NULL)
observeEvent(eventExpr = daValidationSubmitButton,{
  da.validation.params <- DAvalidationParam( daValidationPredictionModel = daValidationPredictionModel)
})


###read MS1 peak table and sample.information for validation
da.ms1.validation <- reactiveValues(data = NULL)
observeEvent(eventExpr = daValidationUploadButton, {
  withProgress(message = 'Upload MS1 peak table...',
               detail = 'This may take a while',
               # style= "old",
               value = 0, {
                 # if(daValidationUseDemoData == TRUE){##use demo data
                 #   # temp.file <- dir("./data/demo_data/Data_Cleaning")
                 #   da.ms1.validation <- lapply(1:length(1), function(idx){
                 #     # x <- temp.file[idx]
                 #     incProgress(1/length(1),
                 #                 detail = "This may take a while")
                 #     as.data.frame(readr::read_csv("./data/demo_data/Differential_Analysis/data.val.csv",
                 #                                   col_types = readr::cols()))
                 #   })
                 # }else{#upload data
                 if (!is.null(DAms1PeakTableValidation)) {
                   # temp.file <- DAms1PeakTablepath
                   da.ms1.validation <- lapply(1:length(1), function(idx){
                     incProgress(1/length(1),
                                 detail = "This may take a while")
                     as.data.frame(readr::read_csv(DAms1PeakTableValidationpath,
                                                   col_types = readr::cols()))
                   })
                 }
                 # }
               })
})


## read sample.info
da.sample.info.validation <- reactiveValues(data = NULL)
observeEvent(eventExpr = daValidationUploadButton, {
  # if(daValidationUseDemoData == TRUE){
  #   da.sample.info.validation <- as.data.frame(readr::read_csv("./data/demo_data/Differential_Analysis/sample.info.val.csv",
  #                                                            col_types = readr::cols()))
  #   da.sample.info.validation[,"group"] <- as.character(da.sample.info.validation[,"group"])
  #
  # }else{
  if (!is.null(DAsampleInfoValidation)) {
    da.sample.info.validation <-
      as.data.frame(readr::read_csv(DAsampleInfoValidationpath,col_types = readr::cols()))
  }
  # }
})

##check result
da.data.check.result.validation <- reactiveValues(data = NULL)
observeEvent(eventExpr = daValidationUploadButton,{
  if(!is.null(da.ms1.validation) & !is.null(da.sample.info.validation)){
    check.result <- checkData(peak.table = da.ms1.validation,
                              sample.info = da.sample.info.validation,
                              step = "biomarker")
  }else{
    check.result <- data.frame("Data.File" = "Data",
                               "Information" = "You don't provide MS1 peak table or sample information",
                               "Result" = "Warning",
                               stringsAsFactors = FALSE)
  }

  check.result <- as.data.frame(check.result)
  da.data.check.result.validation <- check.result
})


##error if there is error in data
observeEvent(eventExpr = daValidationSubmitButton, {
  if(!is.null(da.data.check.result.validation)){
    if(length(grep(pattern = "Error",da.data.check.result.validation[,3])) != 0){
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
observeEvent(eventExpr = daValidationSubmitButton,{
  if(is.null(da.ma.marker)) return(NULL)
  if(nrow(da.ma.marker) == 0) return(NULL)
  ##discovery dataset
  withProgress(message = 'PCA analysis for discovery dataset...',
               detail = 'This may take a while',
               # style= "old",
               value = 0, {
                 lapply(1:length(1), function(idx){
                   incProgress(1/length(1),
                               detail = "This may take a while")

                   sample.info <- da.sample.info.raw
                   control.group <- da.ua.control
                   case.group <- da.ua.case
                   sample.info <- sample.info[sample.info$group %in% c(control.group, case.group),]
                   marker <- da.ma.marker

                   data1 <- da.ms1.raw[[1]]
                   sample1 <- data1[,match(sample.info$sample.name, colnames(data1))]
                   rownames(sample1) <- data1$name
                   sample1 <- sample1[which(rownames(sample1) %in% marker$name),]


                   ##log
                   sample1 <- sxtLog(sample = sample1, method = daMAlog)
                   ##scale
                   sample1 <- sxtScale(sample = sample1, method = daMAscale, center = daMAcenter)

                   daValidationPCA1 <- prcomp(data.frame(t(sample1)),
                                                   retx = TRUE,
                                                   center = FALSE,
                                                   scale = FALSE)
                 })
               })

  if(!is.null(da.ms1.validation) & !is.null(da.sample.info.validation)){
    ##validation dataset
    withProgress(message = 'PCA analysis for validation dataset...',
                 detail = 'This may take a while',
                 # style= "old",
                 value = 0, {
                   lapply(1:length(1), function(idx){
                     incProgress(1/length(1),
                                 detail = "This may take a while")

                     sample.info <- da.sample.info.validation
                     control.group <- da.ua.control
                     case.group <- da.ua.case
                     sample.info <- sample.info[sample.info$group %in% c(control.group, case.group),]
                     marker <- da.ma.marker

                     data2 <- da.ms1.validation[[1]]
                     sample2 <- data2[,match(sample.info$sample.name, colnames(data2))]
                     rownames(sample2) <- data2$name
                     sample2 <- sample2[which(rownames(sample2) %in% marker$name),]

                     sample2 <- sxtLog(sample = sample2, method = daMAlog)
                     ##scale
                     sample2 <- sxtScale(sample = sample2, method = daMAscale, center = daMAcenter)

                     daValidationPCA2 <- prcomp(data.frame(t(sample2)),
                                                     retx = TRUE,
                                                     center = FALSE,
                                                     scale = FALSE)
                   })
                 })
  }

})


daValidationPCAplot1 <- function(){
  sample.info <- da.sample.info.raw
  control.group <- da.ua.control
  case.group <- da.ua.case
  sample.info <- sample.info[sample.info$group %in% c(control.group, case.group),]
  pcaScorePlot2(pca.object = daValidationPCA1, sample.info = sample.info)
}


output$daValidationPCAplot1 <- renderPlotly({
  ggplotly(daValidationPCAplot1(),
           # tooltip = c("Sample", "Injection.order", "Batch"),
           source = "daValidationPCAplot1")
})


daValidationPCAplot2 <- function(){
  if(is.null(da.sample.info.validation) | is.null(da.sample.info.validation)) return(NULL)
  sample.info <- da.sample.info.validation
  control.group <- da.ua.control
  case.group <- da.ua.case
  sample.info <- sample.info[sample.info$group %in% c(control.group, case.group),]
  pcaScorePlot2(pca.object = daValidationPCA2, sample.info = sample.info)
}


output$daValidationPCAplot2 <- renderPlotly({
  ggplotly(daValidationPCAplot2(),
           # tooltip = c("Sample", "Injection.order", "Batch"),
           source = "daValidationPCAplot2")
})





###PLS analysis
daValidationPLS1 <- reactiveValues(data = NULL)
daValidationPLS2 <- reactiveValues(data = NULL)
observeEvent(eventExpr = daValidationSubmitButton,{
  if(!is.null(da.ms1.raw) & !is.null(da.sample.info.raw)){
    if(is.null(da.ma.marker)) return(NULL)
    if(nrow(da.ma.marker) == 0) return(NULL)
    withProgress(message = 'PLS analysis for validation...',
                 detail = 'This may take a while',
                 # style= "old",
                 value = 0, {
                   lapply(1:length(1), function(idx){
                     incProgress(1/length(1),
                                 detail = "This may take a while")
                     sample.info <- da.sample.info.raw
                     control.group <- da.ua.control
                     case.group <- da.ua.case
                     sample.info <- sample.info[sample.info$group %in% c(control.group, case.group),]
                     marker <- da.ma.marker

                     data1 <- da.ms1.raw[[1]]
                     sample1 <- data1[,match(sample.info$sample.name, colnames(data1))]
                     rownames(sample1) <- data1$name
                     sample1 <- sample1[which(rownames(sample1) %in% marker$name),]


                     ##log
                     sample1 <- sxtLog(sample = sample1, method = daMAlog)
                     ##scale
                     sample1 <- sxtScale(sample = sample1, method = daMAscale,
                                         center = daMAcenter)

                     pls.Y <- as.numeric(as.factor(sample.info[,"group"]))

                     pls.object <- plsdepot::plsreg1(t(sample1), pls.Y, comps = 3)
                     daValidationPLS1 <- pls.object
                   })
                 })
  }

  if(!is.null(da.ms1.validation) & !is.null(da.sample.info.validation)){
    withProgress(message = 'PLS analysis for validation...',
                 detail = 'This may take a while',
                 # style= "old",
                 value = 0, {
                   lapply(1:length(1), function(idx){
                     incProgress(1/length(1),
                                 detail = "This may take a while")
                     sample.info <- da.sample.info.validation
                     control.group <- da.ua.control
                     case.group <- da.ua.case
                     sample.info <- sample.info[sample.info$group %in% c(control.group, case.group),]
                     marker <- da.ma.marker

                     data2 <- da.ms1.validation[[1]]
                     sample2 <- data2[,match(sample.info$sample.name, colnames(data2))]
                     rownames(sample2) <- data2$name
                     sample2 <- sample2[which(rownames(sample2) %in% marker$name),]

                     sample2 <- sxtLog(sample = sample2, method = daMAlog)
                     ##scale
                     sample2 <- sxtScale(sample = sample2, method = daMAscale, center = daMAcenter)

                     pls.Y <- as.numeric(as.factor(sample.info[,"group"]))

                     pls.object <- plsdepot::plsreg1(t(sample2), pls.Y, comps = 3)
                     daValidationPLS2 <- pls.object
                   })
                 })
  }

})


##PLS score plot
daValidationPLSplot1 <- function(){
  sample.info <- da.sample.info.raw
  control.group <- da.ua.control
  case.group <- da.ua.case
  sample.info <- sample.info[sample.info$group %in% c(control.group, case.group),]
  plsScorePlot(pls.object = daValidationPLS1, sample.info = sample.info)
}



output$daValidationPLSplot1 <- renderPlotly({
  ggplotly(daValidationPLSplot1(),
           source = "daValidationPLSplot1")
})


daValidationPLSplot2 <- function(){
  if(is.null(da.sample.info.validation) | is.null(da.sample.info.validation)) return(NULL)
  sample.info <- da.sample.info.validation
  control.group <- da.ua.control
  case.group <- da.ua.case
  sample.info <- sample.info[sample.info$group %in% c(control.group, case.group),]
  plsScorePlot(pls.object = daValidationPLS2, sample.info = sample.info)
}



output$daValidationPLSplot2 <- renderPlotly({
  ggplotly(daValidationPLSplot2(),
           source = "daValidationPLSplot1")
})


##HCA analysis
daValidationHCA1 <- reactiveValues(data = NULL)
daValidationHCA2 <- reactiveValues(data = NULL)

observeEvent(eventExpr = daValidationSubmitButton,{
  if(!is.null(da.ms1.raw) & !is.null(da.sample.info.raw)){
    if(is.null(da.ma.marker)) return(NULL)
    if(nrow(da.ma.marker) == 0) return(NULL)
    withProgress(message = 'HCA for validation...',
                 detail = 'This may take a while',
                 # style= "old",
                 value = 0, {
                   lapply(1:length(1), function(idx){
                     incProgress(1/length(1),
                                 detail = "This may take a while")
                     sample.info <- da.sample.info.raw
                     control.group <- da.ua.control
                     case.group <- da.ua.case
                     sample.info <- sample.info[sample.info$group %in% c(control.group, case.group),]
                     marker <- da.ma.marker

                     data1 <- da.ms1.raw[[1]]
                     sample1 <- data1[,match(sample.info$sample.name, colnames(data1))]
                     rownames(sample1) <- data1$name
                     sample1 <- sample1[which(rownames(sample1) %in% marker$name),]


                     ##log
                     sample1 <- sxtLog(sample = sample1, method = daMAlog)
                     ##scale
                     sample1 <- sxtScale(sample = sample1, method = daMAscale,
                                         center = daMAcenter)

                     group <- c(control.group, case.group)

                     daValidationHCA1 <- heatMap(sample = sample1,
                                                      sample.info = sample.info,
                                                      group = group,
                                                      int.col = c(daMAhcaLowCol, daMAhcaMiddleCol, daMAhcaHighCol),
                                                      color = c(daMAhcaGroup1Col, daMAhcaGroup2Col),
                                                      show_rownames = daMAhcaShowRowNames,
                                                      show_colnames = daMAhcaShowColNames,
                                                      cluster_rows = daMAhcaClusterRows,
                                                      cluster_cols = daMAhcaClusterCols,
                                                      clustering_distance_rows = daMAhcaClusteringDistanceRows,
                                                      clustering_distance_cols = daMAhcaClusteringDistanceCols,
                                                      clustering_method = daMAhcaClusteringMethod
                     )



                   })
                 })
  }

  if(!is.null(da.ms1.validation) & !is.null(da.sample.info.validation)){
    withProgress(message = 'HCA for validation...',
                 detail = 'This may take a while',
                 # style= "old",
                 value = 0, {
                   lapply(1:length(1), function(idx){
                     incProgress(1/length(1),
                                 detail = "This may take a while")
                     sample.info <- da.sample.info.validation
                     control.group <- da.ua.control
                     case.group <- da.ua.case
                     sample.info <- sample.info[sample.info$group %in% c(control.group, case.group),]
                     marker <- da.ma.marker

                     data2 <- da.ms1.validation[[1]]
                     sample2 <- data2[,match(sample.info$sample.name, colnames(data2))]
                     rownames(sample2) <- data2$name
                     sample2 <- sample2[which(rownames(sample2) %in% marker$name),]

                     sample2 <- sxtLog(sample = sample2, method = daMAlog)
                     ##scale
                     sample2 <- sxtScale(sample = sample2, method = daMAscale, center = daMAcenter)

                     group <- c(control.group, case.group)

                     daValidationHCA2 <- heatMap(sample = sample2,
                                                      sample.info = sample.info,
                                                      group = group,
                                                      int.col = c(daMAhcaLowCol, daMAhcaMiddleCol, daMAhcaHighCol),
                                                      color = c(daMAhcaGroup1Col, daMAhcaGroup2Col),
                                                      show_rownames = daMAhcaShowRowNames,
                                                      show_colnames = daMAhcaShowColNames,
                                                      cluster_rows = daMAhcaClusterRows,
                                                      cluster_cols = daMAhcaClusterCols,
                                                      clustering_distance_rows = daMAhcaClusteringDistanceRows,
                                                      clustering_distance_cols = daMAhcaClusteringDistanceCols,
                                                      clustering_method = daMAhcaClusteringMethod
                     )
                   })
                 })
  }
})


daValidationHCAheatmap1 <- function(){
  par(mar = c(5,5,4,2))
  daValidationHCA1
}


output$daValidationHCAheatmap1 <- renderPlot({
  daValidationHCAheatmap1()
})


output$daValidationHCAheatmap1Download1 <- downloadHandler(
  filename = function() { "Heatmap.pdf" },
  content = function(file) {
    pdf(file = file,
        width = daValidationHCAheatmapWidth1,
        height = daValidationHCAheatmapWidth1)
    daValidationHCAheatmap1()
    dev.off()
    # readr::write_csv(as.data.frame(data()), file)
  }, contentType = "pdf")


daValidationHCAheatmap2 <- function(){
  par(mar = c(5,5,4,2))
  daValidationHCA2
}


output$daValidationHCAheatmap2 <- renderPlot({
  daValidationHCAheatmap2()
})


output$daValidationHCAheatmap1Download2 <- downloadHandler(
  filename = function() { "Heatmap.pdf" },
  content = function(file) {
    pdf(file = file,
        width = daValidationHCAheatmapWidth2,
        height = daValidationHCAheatmapWidth2)
    daValidationHCAheatmap2()
    dev.off()
    # readr::write_csv(as.data.frame(data()), file)
  }, contentType = "pdf")



###ROC analysis
daValidationROC1 <- reactiveValues(data = NULL)
daValidationROC2 <- reactiveValues(data = NULL)

observeEvent(eventExpr = daValidationROCanalysis, {
  if(!is.null(da.ma.marker)){
    if(nrow(da.ma.marker) == 0) return(NULL)
    ##construction model
    withProgress(message = 'ROC analysis...',
                 detail = 'This may take a while',
                 # style= "old",
                 value = 0, {
                   lapply(1:length(1), function(idx){
                     incProgress(1/length(1),
                                 detail = "This may take a while")
                     sample.info <- da.sample.info.raw
                     control.group <- da.ua.control
                     case.group <- da.ua.case
                     sample.info <- sample.info[sample.info$group %in% c(control.group, case.group),]
                     marker <- da.ma.marker

                     data1 <- da.ms1.raw[[1]]
                     sample1 <- data1[,match(sample.info$sample.name, colnames(data1))]
                     rownames(sample1) <- data1$name
                     sample1 <- sample1[which(rownames(sample1) %in% marker$name),]

                     ##log
                     sample1 <- sxtLog(sample = sample1, method = daMAlog)
                     ##scale
                     sample1 <- sxtScale(sample = sample1, method = daMAscale,
                                         center = daMAcenter)

                     if(!is.null(da.ms1.validation) & !is.null(da.sample.info.validation)){
                       sample.info2 <- da.sample.info.validation
                       sample.info2 <- sample.info2[sample.info2$group %in% c(control.group, case.group),]

                       data2 <- da.ms1.validation[[1]]
                       sample2 <- data2[,match(sample.info2$sample.name, colnames(data2))]
                       rownames(sample2) <- data2$name
                       sample2 <- sample2[which(rownames(sample2) %in% marker$name),]

                       sample2 <- sxtLog(sample = sample2, method = daMAlog)
                       ##scale
                       sample2 <- sxtScale(sample = sample2, method = daMAscale, center = daMAcenter)
                       Y2 <- as.numeric(as.factor(sample.info2[,"group"]))
                       Y2 <- Y2 - 1
                     }

                     # group <- c(control.group, case.group)

                     ##prediction model
                     Y <- as.numeric(as.factor(sample.info[,"group"]))
                     Y <- Y - 1

                     if(daValidationPredictionModel == "pls"){
                       prediction.model <- pls::plsr(Y ~ t(sample1), scale = FALSE,
                                                     validation = "CV", ncomp = 3, method = "oscorespls")
                       prediction.Y <- predict(prediction.model, t(sample1), ncomp = 3)
                       daValidationROC1 <- pROC::roc(Y, prediction.Y, ci = TRUE)
                       if(!is.null(da.ms1.validation) & !is.null(da.sample.info.validation)){
                         prediction.Y2 <- predict(prediction.model, t(sample2), ncomp = 3)
                         daValidationROC2 <- pROC::roc(Y2, prediction.Y2, ci = TRUE)
                       }
                     }

                     if(daValidationPredictionModel == "rf"){
                       prediction.model <- randomForest(t(sample1), Y, prox = TRUE,
                                                        importance = TRUE)
                       prediction.Y <- predict(prediction.model, t(sample1))
                       daValidationROC1 <- pROC::roc(Y, prediction.Y, ci = TRUE)
                       if(!is.null(da.ms1.validation) & !is.null(da.sample.info.validation)){
                         prediction.Y2 <- predict(prediction.model, t(sample2))
                         daValidationROC2 <- pROC::roc(Y2, prediction.Y2, ci = TRUE)
                       }
                     }

                     if(daValidationPredictionModel == "svm"){
                       prediction.model <- e1071::svm(t(sample1),Y,scale = FALSE)
                       prediction.Y <- predict(prediction.model, t(sample1))
                       daValidationROC1 <- pROC::roc(Y, prediction.Y, ci = TRUE)
                       if(!is.null(da.ms1.validation) & !is.null(da.sample.info.validation)){
                         prediction.Y2 <- predict(prediction.model, t(sample2))
                         daValidationROC2 <- pROC::roc(Y2, prediction.Y2, ci = TRUE)
                       }
                     }

                     if(daValidationPredictionModel == "lr"){
                       dis.data <- data.frame(Y, t(sample1), stringsAsFactors = FALSE)
                       prediction.model <- glm(Y ~ ., family = binomial,
                                               data = dis.data)
                       prediction.Y <- predict(prediction.model, as.data.frame(t(sample1)))
                       daValidationROC1 <- pROC::roc(Y, prediction.Y, ci = TRUE)
                       if(!is.null(da.ms1.validation) & !is.null(da.sample.info.validation)){
                         prediction.Y2 <- predict(prediction.model, as.data.frame(t(sample2)))
                         daValidationROC2 <- pROC::roc(Y2, prediction.Y2, ci = TRUE)
                       }
                     }


                   })
                 })
  }

  ##validation dateset prediction
  # if(!is.null(da.ms1.validation) & !is.null(da.sample.info.validation)){
  #   withProgress(message = 'ROC analysis...',
  #                detail = 'This may take a while',
  #                # style= "old",
  #                value = 0, {
  #                  lapply(1:length(1), function(idx){
  #                    incProgress(1/length(1),
  #                                detail = "This may take a while")
  #                    sample.info <- da.sample.info.validation
  #                    control.group <- da.ua.control
  #                    case.group <- da.ua.case
  #                    sample.info <- sample.info[sample.info$group %in% c(control.group, case.group),]
  #                    marker <- da.ma.marker
  #
  #                    data2 <- da.ms1.validation[[1]]
  #                    sample2 <- data2[,match(sample.info$sample.name, colnames(data2))]
  #                    rownames(sample2) <- data2$name
  #                    sample2 <- sample2[which(rownames(sample2) %in% marker$name),]
  #
  #                    sample2 <- sxtLog(sample = sample2, method = daMAlog)
  #                    ##scale
  #                    sample2 <- sxtScale(sample = sample2, method = daMAscale, center = daMAcenter)
  #
  #
  #                    ##prediction in validation dataset
  #                    if(daValidationPredictionModel == "pls"){
  #                      prediction.Y <- predict(prediction.model, t(sample2), ncomp = 3)
  #                      daValidationROC2 <- pROC::roc(Y, prediction.Y, ci = TRUE)
  #                    }
  #
  #                    if(daValidationPredictionModel == "rf"){
  #                      prediction.Y <- predict(prediction.model, t(sample2))
  #                      daValidationROC2 <- pROC::roc(Y, prediction.Y, ci = TRUE)
  #                    }
  #
  #                    if(daValidationPredictionModel == "svm"){
  #                      prediction.Y <- predict(prediction.model, t(sample2))
  #                      daValidationROC2 <- pROC::roc(Y, prediction.Y, ci = TRUE)
  #                    }
  #
  #                    if(daValidationPredictionModel == "lr"){
  #                      prediction.Y <- predict(prediction.model, t(sample2))
  #                      daValidationROC2 <- pROC::roc(Y, prediction.Y, ci = TRUE)
  #                    }
  #
  #                  })
  #                })
  # }
})



daValidationROClplot1 <- function(){
  if(is.null(daValidationROC1)) return(NULL)
  ROCplot(roc.object = daValidationROC1)
}

daValidationROClplot2 <- function(){
  if(is.null(daValidationROC2)) return(NULL)
  ROCplot(roc.object = daValidationROC2)
}


output$daValidationROClplot1 <- renderPlotly(expr = {
  ggplotly(daValidationROClplot1())
})


output$daValidationROClplot2 <- renderPlotly(expr = {
  ggplotly(daValidationROClplot2())
})



###error if there is no markers are selected

##warning if no data selected or project name is null
observeEvent(eventExpr = daValidationSubmitButton, {
  if(!is.null(da.ma.marker)){
    if(nrow(da.ma.marker) == 0){
      shinyalert::shinyalert(title = "Error",
                             text = "There is 0 marker!",
                             type = "error")
    }
  }
})


observeEvent(eventExpr = daValidationROCanalysis, {
  if(!is.null(da.ma.marker)){
    if(nrow(da.ma.marker) == 0){
      shinyalert::shinyalert(title = "Error",
                             text = "There is 0 marker!",
                             type = "error")
    }
  }
})


##jump to result download
observeEvent(daValidation2Download, {
  updateTabsetPanel(session, "DAtab",
                    selected = "DAdownload")

})



















##download
####generate analysis report
observeEvent(eventExpr = da.generate.analysis.report, {
  now.path <- getwd()
  user.path <- file.path("user_data", user_username, DAprojectID, "differential_analysis")
  tempReport <- file.path(now.path, user.path, "DAreport.temp.Rmd")
  file.copy("data/markdown/DAreport.Rmd", tempReport, overwrite = TRUE)

  params <- list(##parameters
    da.ua.params = da.ua.params,
    da.ma.params = da.ma.params,
    da.validation.params = da.validation.params,
    da.vocano.plot = da.volcano.plot,
    daMApcaPlot = daMApcaPlot(),
    daMAplsPlot = daMAplsPlot(),
    daMAhcaHeatmap = daMAhcaHeatmap(),
    daValidationPCAplot1 = daValidationPCAplot1(),
    daValidationPCAplot2 = daValidationPCAplot2(),
    daValidationPLSplot1 = daValidationPLSplot1(),
    daValidationPLSplot2 = daValidationPLSplot2(),
    daValidationHCAheatmap1 = daValidationHCAheatmap1(),
    daValidationHCAheatmap2 = daValidationHCAheatmap2(),
    daValidationROClplot1 = daValidationROClplot1(),
    daValidationROClplot2 = daValidationROClplot2()
  )
  # save(params, file = "params")
  withProgress(message = 'Generate analysis report...',
               detail = 'This may take a while',
               # style= "old",
               value = 0, {
                 lapply(1:length(tempReport), function(idx){
                   incProgress(1/length(tempReport),
                               detail = "This may take a while")
                   rmarkdown::render(tempReport,
                                     output_file = file.path(now.path, user.path,
                                                             "Analysis.Report.of.Differential.Metabolite.Discovery.html"),
                                     params = params,
                                     envir = new.env(parent = globalenv())
                   )
                 })
               })
})


####download analysis report
output$da.report.download <- downloadHandler(
  filename = "Analysis.Report.of.Differential.Metabolite.Discovery.html",
  content = function(file){
    file.copy(file.path("user_data", user_username,
                        DAprojectID, "differential_analysis",
                        "Analysis.Report.of.Differential.Metabolite.Discovery.html"), file)
  }
)

####generate analysis result
observeEvent(eventExpr = da.generate.analysis.result, {

  if(!is.null(da.ma.marker)){
    #ZIP data
    withProgress(message = 'Generate analysis result...',
                 detail = 'This may take a while',
                 # style= "old",
                 value = 0, {
                   lapply(1:length(1), function(idx){
                     incProgress(1/length(1),
                                 detail = "This may take a while")

                     now.path <- getwd()
                     temp.path <- file.path(now.path, "user_data", user_username,
                                            DAprojectID, "differential_analysis","Differential_Metabolite_Discovery_Result")
                     dir.create(temp.path)
                     # load(file.path(user.path(), "da.ms1.os"))
                     readr::write_csv(da.ua.marker,
                                      file.path(temp.path, "Potential.marker.univariate.analysis.csv"))

                     readr::write_csv(da.ma.marker,
                                      file.path(temp.path, "Potential.marker.multivariate.analysis.csv"))

                     ##volcanplot
                     ggsave(da.volcano.plot,
                            file = file.path(temp.path, "Volcano.plot.pdf"), width = 7, height = 7)

                     ggsave(daMApcaPlot(),
                            file = file.path(temp.path, "PCA.score.plot.using.all.peaks.pdf"), width = 7, height = 7)

                     ggsave(daMAplsPlot(),
                            file = file.path(temp.path, "PLS.score.plot.using.all.peaks.pdf"), width = 7, height = 7)

                     pdf(file = file.path(temp.path, "Heatmap.using.all.peaks.pdf"), width = 7, height = 7)
                     daMAhcaHeatmap()
                     dev.off()

                     pdf(file = file.path(temp.path, "Heatmap.using.potential.markers.in.discovery.dataset.pdf"), width = 7, height = 7)
                     daValidationHCAheatmap1()
                     dev.off()

                     pdf(file = file.path(temp.path, "Heatmap.using.potential.markers.in.validation.dataset.pdf"), width = 7, height = 7)
                     daValidationHCAheatmap2()
                     dev.off()

                     ggsave(daValidationPCAplot1(),
                            file = file.path(temp.path, "PCA.score.plot.using.potential.markers.in.discovery.dataset.pdf"), width = 7, height = 7)

                     ggsave(daValidationPCAplot2(),
                            file = file.path(temp.path, "PCA.score.plot.using.potential.markers.in.validation.dataset.pdf"), width = 7, height = 7)

                     ggsave(daValidationPLSplot1(),
                            file = file.path(temp.path, "PLS.score.plot.using.potential.markers.in.discovery.dataset.pdf"), width = 7, height = 7)

                     ggsave(daValidationPLSplot2(),
                            file = file.path(temp.path, "PLS.score.plot.using.potential.markers.in.validation.dataset.pdf"), width = 7, height = 7)

                     ggsave(daValidationROClplot1(),
                            file = file.path(temp.path, "AUC.using.potential.markers.in.discovery.dataset.pdf"), width = 7, height = 7)

                     ggsave(daValidationROClplot2(),
                            file = file.path(temp.path, "AUC.using.potential.markers.in.validation.dataset.pdf"), width = 7, height = 7)

                     setwd(file.path("user_data", user_username, DAprojectID, "differential_analysis"))
                     temp.error <- try(zip::zip(zipfile = file.path(now.path, "user_data", user_username, DAprojectID, "differential_analysis",
                                                                    "Differential_Metabolite_Discovery_Result.zip"),
                                                files = "Differential_Metabolite_Discovery_Result",
                                                recurse = TRUE
                     ))

                     if(class(temp.error) == "try-error"){
                       setwd(now.path)
                     }
                     setwd(now.path)
                   })
                 })
  }
})


####download analysis result
output$da.result.download <- downloadHandler(
  filename = "Differential_Metabolite_Discovery_Result.zip",
  content = function(file){
    file.copy(file.path("user_data", user_username, DAprojectID, "differential_analysis", "Differential_Metabolite_Discovery_Result.zip"), file)
  }
)



