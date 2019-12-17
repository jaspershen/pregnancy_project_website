##data cleaning

# temp.file <- dir(file.path("F:/data pre-processing/demo data of MetFlow/metflow_dataset1_raw"),
#                  full.names = TRUE)
# temp.file <- dir(file.path("D:/study/R/my git/metCube/version1.0.2/data/demo_data/Data_Cleaning"),
#                  full.names = TRUE)
# temp.file <- "F:/data pre-processing/demo data for metCube/GCMS/MTBLS321/GC_demo_data.csv"
# temp.file <- dir(file.path("F:/data pre-processing/tujia"),
#                  full.names = TRUE)
# temp.file <- grep("batch", temp.file, value = TRUE)

# dc.ms1.raw <- lapply(temp.file, function(x){
#   as.data.frame(readr::read_csv(x))
# })

# dc.sample.info.raw <- read.csv("F:/data pre-processing/demo data for metCube/GCMS/MTBLS321/GC_demo_sample_info.csv",
#                                stringsAsFactors = FALSE)

# dc.sample.info.raw <- read.csv("F:/data pre-processing/demo data of MetFlow/metflow_dataset1_raw/sample.info.csv",
#                                stringsAsFactors = FALSE)
# dc.sample.info.raw <- read.csv("D:/study/R/my git/metCube/version1.0.2/data/demo_data/Data_Cleaning/sample.information.csv",
#                                stringsAsFactors = FALSE)
# dc.sample.info.raw <- read.csv("F:/data pre-processing/tujia/sample.info.demo-1.csv",
#                                stringsAsFactors = FALSE)


setwd("F:\\test\\metflow")
load("dc.ms1.raw")
load("dc.sample.info.raw")
check.result <- checkData(peak.table = dc.ms1.raw,
                          sample.info = dc.sample.info.raw, step = "cleaning")

check.result




##dc.data.info is the information of dataset.
    dc.data.info <- paste("There are", length(dc.ms1.raw), "batches.",
                               "And there are",
                               ifelse(any(dc.sample.info.raw$class == "QC"),
                                      "QC sample", "no QC samples."), "in your data.")


    dc.data.info



















#------------------------------------------------------------------------------
#batch alignment
###rough match
    dc.ba.mz.tol <- 25
    dc.ba.rt.tol <- 30

                   temp <- try({lapply(1:1, function(idx){
                     roughAlign(peak.table = dc.ms1.raw,
                                combine.mz.tol = dc.ba.mz.tol,
                                combine.rt.tol = dc.ba.rt.tol)
                   })[[1]]
                   })

                   if(class(temp)[1] == "try-error"){
                     dc.batch.alignment.result1.info <- renderText({paste("Roungh alignment: ",temp[[1]])})
                   }

                   dc.batch.alignment.result1 <- temp
                   rm(list = c("temp"))




  dc.ba.mz.plot <- baMZplot(dc.batch.alignment.result1)
  dc.ba.mz.plot
  ggplotly(dc.ba.mz.plot,
           tooltip = c("mz", "mz.error"),
           source = "dc.ba.mz.plot")



  dc.ba.rt.plot <- baRTplot(dc.batch.alignment.result1)
  dc.ba.rt.plot
  ggplotly(dc.ba.rt.plot,
           tooltip = c("rt", "rt.error"),
           source = "dc.ba.rt.plot")




  dc.ba.int.plot <- baINTplot(dc.batch.alignment.result1)
  dc.ba.int.plot
  ggplotly(dc.ba.int.plot, tooltip = c(
    "int",
    "int.error"),
    source = "dc.ba.int.plot")



###accurate match

                   temp <- try({
                     lapply(1:1, function(idx){
                     accurateAlign(peak.table = dc.ms1.raw,
                                   simple.data = dc.batch.alignment.result1)
                   })[[1]]
                   })
                   if(class(temp)[1] == "try-error"){
                     dc.batch.alignment.result2.info <- renderText({paste("Accurate alignment: ",temp[[1]])})
                   }

                   dc.batch.alignment.result2 <- temp
                   rm(list = c("temp"))





##save information of batch alginment


    temp <- try({getBatchAlignmentInfo(dc.ms1.raw,
                                       dc.batch.alignment.result1,
                                       dc.batch.alignment.result2)})

    if(class(temp)[1] == "try-error"){
      dc.ba.info.message <- renderText({paste("Batch alignment info: ",temp[[1]])})
    }
    dc.ba.info <- temp
    rm(list = c("temp"))






















#-------------------------------------------------------------------------------
#data quality assessment, before data cleaning
## peak profile
    temp <- try({dataInformation(object = dc.batch.alignment.result2)})
    if(class(temp)[1] == "try-error"){
      dc.data.profile.plot.message <- renderText({paste("Data profile: ",temp[[1]])})
    }
    dc.data.profile.plot <- temp
    rm(list = c("temp"))

    dc.data.profile.plot



  ggplotly(dc.data.profile.plot, tooltip = c(
    "rt",
    "mz","int.log"),
    source = "dc.data.profile.plot")



## MV ratio in peaks

    temp <- try({mvPeakPlot(data = dc.batch.alignment.result2)})
    if(class(temp)[1] == "try-error"){
      dc.peak.mv.ratio.plot.message <- renderText({paste("MV ratio plot: ",temp[[1]])})
    }
    dc.peak.mv.ratio.plot <- temp
    rm(list = c("temp"))

    dc.peak.mv.ratio.plot

  ggplotly(dc.peak.mv.ratio.plot, tooltip = c(
    "Index",
    "MV.ratio","Peak"),
    source = "dc.peak.mv.ratio.plot")



## MV ratio in Samples


    temp <- try({mvSamplePlot(data = dc.batch.alignment.result2)})
    if(class(temp)[1] == "try-error"){
      dc.sample.mv.ratio.plot.message <- renderText({paste("MV ratio plot: ",temp[[1]])})
    }
    dc.sample.mv.ratio.plot <- temp
    rm(list = c("temp"))

    dc.sample.mv.ratio.plot
  ggplotly(dc.sample.mv.ratio.plot,
           tooltip = c(
             "Index",
             "MV.ratio","Sample"),
           source = "dc.sample.mv.ratio.plot")




## zero ratio in peaks

    temp <- try({zeroPeakPlot(data = dc.batch.alignment.result2,
                              text = FALSE)})
    if(class(temp)[1] == "try-error"){
      dc.peak.zero.ratio.plot.message <- renderText({paste("Zero ratio plot: ",temp[[1]])})
    }

    dc.peak.zero.ratio.plot <- temp
    rm(list = c("temp"))
    dc.peak.zero.ratio.plot


  # ggplotly(dc.peak.zero.ratio.plot,
  #          tooltip = c(
  #            "Index",
  #            "zero.ratio","Peak"),
  #          source = "dc.peak.zero.ratio.plot")



## zero ratio in Samples

    temp <- try({zeroSamplePlot(data = dc.batch.alignment.result2,
                                text = FALSE)})
    if(class(temp)[1] == "try-error"){
      dc.sample.zero.ratio.plot.message <- renderText({paste("Zero ratio plot: ",temp[[1]])})
    }
    dc.sample.zero.ratio.plot <- temp
    rm(list = c("temp"))
    dc.sample.zero.ratio.plot

  # ggplotly(dc.sample.zero.ratio.plot,
  #          tooltip = c(
  #            "Index",
  #            "zero.ratio","Sample"),
  #          source = "dc.sample.zero.ratio.plot")



###group information in da_qa1_ui
dc.group.name <- unique(dc.sample.info.raw[,"group"])




###RSD distribution
      group <-  unique(dc.sample.info.raw[,"group"])[1]

    temp <- try({rsdDistribution(data = dc.batch.alignment.result2,
                                 sample.info = dc.sample.info.raw,
                                 group = group,
                                 rsd.tol = 30)})
    if(class(temp)[1] == "try-error"){
      dc.peak.rsd.plot.message <- renderText({paste("RSD plot: ",temp[[1]])})
    }
    dc.peak.rsd.plot <- temp
    rm(list = c("temp"))

    dc.peak.rsd.plot


    temp <- try({rsdDistribution(data = dc.batch.alignment.result2,
                                 sample.info = dc.sample.info.raw,
                                 group = "QC",
                                 rsd.tol = 30)})
    if(class(temp)[1] == "try-error"){
      dc.peak.rsd.plot.message <- renderText({paste("RSD plot: ",temp[[1]])})
    }
    dc.peak.rsd.plot <- temp

    dc.peak.rsd.plot





      temp.group <- "Control"


    temp <- try({rsdDistribution(data = dc.batch.alignment.result2,
                                 sample.info = dc.sample.info.raw,
                                 group = temp.group,
                                 rsd.tol = 30)})
    if(class(temp)[1] == "try-error"){
      dc.peak.rsd.plot.for.report.message <- renderText({paste("RSD plot for report: ",temp[[1]])})
    }
    dc.peak.rsd.plot.for.report <- temp
    rm(list = c("temp", "temp.group"))
    dc.peak.rsd.plot.for.report



  # ggplotly(dc.peak.rsd.plot,
  #          #        tooltip = c(
  #          # "Index",
  #          # "zero.ratio","Sample"),
  #          source = "dc.peak.rsd.plot")




#-------------------------------------------------------------------------------
#PCA score plot, should be fixed
  dc.peak.table <- dc.batch.alignment.result2
  dc.sample.info <- dc.sample.info.raw

  temp1 <- try({
    dc.sample <- dc.peak.table[,match(dc.sample.info[,"sample.name"],
                                      colnames(dc.peak.table)), drop = FALSE]
    dc.tags <- dc.peak.table[,-match(dc.sample.info[,"sample.name"],
                                     colnames(dc.peak.table)), drop = FALSE]
    ##log
    ##if there are NA is dc.sample, impute it
    if(sum(is.na(dc.sample)) != 0){
      mv.ratio.peak <- apply(dc.sample, 1, function(x) sum(is.na(x))/ncol(dc.sample))
      idx1 <- which(mv.ratio.peak > 0.5)

      if(length(idx1) > 0){
        dc.sample <- dc.sample[-idx1,]
        dc.tags <- dc.tags[-idx1,]
      }
      mv.ratio.sample <- apply(dc.sample, 2, function(x) sum(is.na(x))/nrow(dc.sample))
      idx2 <- which(mv.ratio.sample > 0.5)
      if(length(idx2) > 0){
        dc.sample <- dc.sample[,-idx2]
        dc.sample.info <- dc.sample.info[-idx2,]
      }

      temp <- impute::impute.knn(data = as.matrix(dc.sample), k = 10)
      dc.sample <- as.data.frame(temp$data)
      rm(list = "temp")
    }
    ##remove some peaks with large zero value ratio
    zero.ratio.peak <- apply(dc.sample, 1, function(x) sum(x==0)/ncol(dc.sample))
    idx1 <- which(zero.ratio.peak > 0.5)
    if(length(idx1) > 0){
      dc.sample <- dc.sample[-idx1,]
      dc.tags <- dc.tags[-idx1,]
    }

    dc.sample <- sxtLog(sample = dc.sample, method = "log10")
    ##scale
    dc.sample <- sxtScale(sample = dc.sample,
                          method = "auto",
                          center = TRUE)
    pca.object <- prcomp(data.frame(t(dc.sample)),
                         retx = TRUE,
                         center = FALSE,
                         scale = FALSE)
  })

  if(class(temp1)[1] == "try-error"){
    dc.qa1.pca.object.message <- renderText({paste("PCA analysis: ",temp1[[1]])})
  }else{
    dc.qa1.pca.object <- pca.object

  }
  rm(list = c("dc.peak.table", "dc.sample", "dc.tags", "pca.object"))
  rm(list = c("temp1"))




    temp <- try({
      pcaScorePlot(pca.object = dc.qa1.pca.object,
                   batch.info = dc.sample.info.raw[,c(1,4)],
                   class.info = dc.sample.info.raw[,c(1,3)])
    })
    if(class(temp)[1] == "try-error"){
      dc.qa1.pca.score.plot.message <- renderText({paste("PCA plot: ",temp[[1]])})
    }else{
      dc.qa1.pca.score.plot <- temp
    }
    rm(list = c("temp"))

    dc.qa1.pca.score.plot
    # plotly::ggplotly(dc.qa1.pca.score.plot, source = "temp")



    temp <- try({pcaScorePlot(pca.object = dc.qa1.pca.object,
                              batch.info = dc.sample.info.raw[,c(1,4)],
                              class.info = dc.sample.info.raw[,c(1,3)])})
    if(class(temp)[1] == "try-error"){
      dc.qa1.pca.score.plot.for.report.message <- renderText({paste("PCA plot for report: ",temp[[1]])})
    }else{
      dc.qa1.pca.score.plot.for.report <- temp
    }
    dc.qa1.pca.score.plot.for.report
    rm(list = c("temp"))

###Boxplots for QC samples
    temp <- try({
      qcIntBoxplot(data = dc.batch.alignment.result2,
                              sample.info = dc.sample.info.raw)
      })
    if(class(temp)[1] == "try-error"){
      dc.qc.int.boxplot.message <- renderText({paste("QC intensity boxplot: ",temp[[1]])})
    }else{
      dc.qc.int.boxplot <- temp
    }
    rm(list = c("temp"))

    dc.qc.int.boxplot
  ggplotly(dc.qc.int.boxplot,
           source = "dc.qc.int.boxplot")



###QC correlation


    temp <- try({qcCorPlot(data = dc.batch.alignment.result2,
                           sample.info = dc.sample.info.raw)})
    if(class(temp)[1] == "try-error"){
      dc.qc.cor.plot.message <- renderText({paste("QC cor plot: ",temp[[1]])})
    }else{
      dc.qc.cor.plot <- temp
    }
    rm(list = c("temp"))



    dc.qc.cor.plot




    temp <- try({qcCor(data = dc.batch.alignment.result2,
                       sample.info = dc.sample.info.raw)})
    if(class(temp)[1] == "try-error"){
      dc.qc.cor.message <- renderText({paste("QC cor: ",temp[[1]])})
    }else{
      dc.qc.cor <- temp
    }
    rm(list = c("temp"))






















#-------------------------------------------------------------------------------
#MV processing
                   temp <- try({
                     lapply(1:1, function(idx){
                       dc.sample <-
                         dc.batch.alignment.result2[,match(dc.sample.info.raw[,"sample.name"], colnames(dc.batch.alignment.result2))]
                       dc.tags <-
                         dc.batch.alignment.result2[,-match(dc.sample.info.raw[,"sample.name"], colnames(dc.batch.alignment.result2))]
                       mvImputation(sample = dc.sample,
                                    tags = dc.tags,
                                    sample.info = dc.sample.info.raw,
                                    mv.peak.remove.tol = 50,
                                    method = "KNN",
                                    rowmax = 0.5,
                                    colmax = 0.5,
                                    k = 10
                                    # ntree = input$mvNtree,
                                    # replace = input$mvReplace,
                                    # bpca.npcs = input$bpca.nPcs,
                                    # ppca.npcs = input$ppca.npcs,
                                    # svd.npcs = input$svd.npcs
                                    )
                     })[[1]]
                   })



                   if(class(temp)[1] == "try-error"){
                     dc.ms1.after.mv.message <- renderText({paste("MV processing: ",temp[[1]])})
                   }else{
                     dc.ms1.after.mv <- temp
                   }
                   rm(list = c("temp"))
                   rm(list = c("dc.sample", "dc.tags"))





  # dcMVparams <- DCmvParam(
  #   50,
  #   "knn",
  #   input$k,
  #   input$mvNtree,
  #   input$mvReplace,
  #   input$bpca.nPcs,
  #   input$ppca.nPcs,
  #   input$svd.nPcs)














#-------------------------------------------------------------------------------
#zero value processing

                 temp <- try({
                   lapply(1:1, function(idx){
                     dc.sample <-
                       dc.ms1.after.mv[,match(dc.sample.info.raw[,"sample.name"], colnames(dc.ms1.after.mv))]
                     dc.tags <-
                       dc.ms1.after.mv[,-match(dc.sample.info.raw[,"sample.name"], colnames(dc.ms1.after.mv))]

                     zeroFilter(sample = dc.sample, tags = dc.tags,
                                sample.info = dc.sample.info.raw,
                                zero.peak.remove.tol = 10)
                   })[[1]]
                 })

                 if(class(temp)[1] == "try-error"){
                   dc.ms1.after.zero.message <- renderText({paste("Zero processing: ",temp[[1]])})
                 }else{
                   dc.ms1.after.zero <- temp
                 }
                 rm(list = c("temp"))
                 rm(list = c("dc.sample", "dc.tags"))



######save parameters of zero value processing

  dcZeroParams <- data.frame("Parameter" = "Peaks will be removed if zero ratio more than %",
                             "Setting" = 50,
                             stringsAsFactors = FALSE)

  dcZeroParams


#-------------------------------------------------------------------------------
#data normalization
######save parameters of data normalization


  # dcDNparams <- DCdnParam(
  #   input$DCdnHasQC,
  #   input$normalizationMethod1,
  #   input$normalizationMethod2,
  #   input$loess.kepp.dimension,
  #   input$parameter.optimization,
  #   input$begin.end,
  #   input$loess.step,
  #   input$svr.kepp.dimension,
  #   input$svr.multiple)


# DCwithQC <- reactive({
#   dc.sample.info <- dc.sample.info.raw$data
#   group <- unique(dc.sample.info[,"group"])
#   if(all(group != "QC")) return(0)
#   return(1)
# })


  dc.peak.table <- dc.ms1.after.zero
  dc.sample.info <- dc.sample.info.raw

  dc.sample <- dc.peak.table[,match(dc.sample.info[,"sample.name"], colnames(dc.peak.table)), drop = FALSE]
  dc.tags <- dc.peak.table[,-match(dc.sample.info[,"sample.name"], colnames(dc.peak.table)), drop = FALSE]


  DCdnHasQC <- "noQC"
  temp <- try({
    if(DCdnHasQC == "noQC"){
      dataNormalization(sample = dc.sample,
                        tags = dc.tags,
                        sample.info = dc.sample.info,
                        method = "mean")
    }else{
      dataNormalization(sample = dc.sample,
                        tags = dc.tags,
                        sample.info = dc.sample.info,
                        method = "svr",
                        optimization = TRUE,
                        begin = 0.2,
                        end = 0.7,
                        step = 0.2,
                        dimension1 = TRUE,
                        dimension2 = TRUE,
                        multiple = 5)
    }


  })
  if(class(temp)[1] == "try-error"){
    dc.ms1.dn.message <- renderText({paste("Data normalization: ",temp[[1]])})
  }else{
    dc.ms1.dn <- temp
  }
  rm(list = c("temp"))
  rm(list = c("dc.peak.table", "dc.sample.info", "dc.sample", "dc.tags"))




output$dc.ms1.dn <- DT::renderDataTable(
  DT::datatable(dc.ms1.dn$data,
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
                        buttons = list(list(extend="csv",filename = "Data.after.normalization"),
                                       list(extend='excel',filename = "Data.after.normalization")),
                        text = 'Download'
                      ),
                      I('colvis')
                    ),
                  scrollX = TRUE
                )
  ),
  server = FALSE
)


###information for single peak before normalization
observeEvent(eventExpr = input$DCdnShowPeak1, {
  peak.table <- dc.ms1.after.zero$data
  sample.info <- dc.sample.info.raw$data
  sample <- peak.table[,match(sample.info[,"sample.name"], colnames(peak.table)), drop = FALSE]
  tags <- peak.table[,-match(sample.info[,"sample.name"], colnames(peak.table)), drop = FALSE]

  if (is.null(input$dc.ms1.dn_rows_selected)) {
    dc.single.peak.plot.dn1 <- function(){
      par( mar = c(5,5,4,2))
      ggplot2::ggplot(data = data.frame("x" = 1, "y" = 1, stringsAsFactors = FALSE), ggplot2::aes(x = x, y = y)) +
        ggplot2::theme_bw() +
        ggplot2::annotate(geom = "text", x = 1, y = 1, label = "Plese select a peak")
    }

  }else{
    peak.idx <- as.numeric(input$dc.ms1.dn_rows_selected)
    dc.single.peak.plot.dn1 <- function(){
      par(mar = c(5,5,4,2))
      plot <- dnSinglePeak(sample = sample, tags = tags,
                           sample.info = sample.info, peak.idx = peak.idx,
                           alpha = 0.8, size = 2, text = FALSE,
                           title = paste(tags[peak.idx, "name"])
      )
      plot
    }

    # rm(list = c("peak.table", "sample.info", "sample", "tags"))
  }


  output$dc.single.peak.plot.dn1 <- renderPlotly({
    ggplotly(dc.single.peak.plot.dn1(),
             tooltip = c("Sample", "Injection.order", "Intensity"),
             source = "dc.single.peak.plot.dn")
  })

  output$dc.single.peak.plot.dn1.download <- downloadHandler(
    filename = function() { "Peak.information.pdf" },
    content = function(file) {
      device <- function(..., width , height) {
        grDevices::pdf(..., width = input$dc.single.peak.plot.dn1.width,
                       height = input$dc.single.peak.plot.dn1.height)
      }
      ggsave(file, plot = dc.single.peak.plot.dn1(), device = device)
    }, contentType = "pdf")

})




###information for single peak after normalization
observeEvent(eventExpr = input$DCdnShowPeak2, {
  peak.table <- dc.ms1.dn$data
  sample.info <- dc.sample.info.raw$data
  sample <- peak.table[,match(sample.info[,"sample.name"], colnames(peak.table)), drop = FALSE]
  tags <- peak.table[,-match(sample.info[,"sample.name"], colnames(peak.table)), drop = FALSE]

  if (is.null(input$dc.ms1.dn_rows_selected)) {
    dc.single.peak.plot.dn2 <- function(){
      par( mar = c(5,5,4,2))
      ggplot2::ggplot(data = data.frame("x" = 1, "y" = 1, stringsAsFactors = FALSE), ggplot2::aes(x = x, y = y)) +
        ggplot2::theme_bw() +
        ggplot2::annotate(geom = "text", x = 1, y = 1, label = "Plese select a peak")
    }

  }else{
    peak.idx <- as.numeric(input$dc.ms1.dn_rows_selected)
    dc.single.peak.plot.dn2 <- function(){
      par(mar = c(5,5,4,2))
      plot <- dnSinglePeak(sample = sample, tags = tags,
                           sample.info = sample.info, peak.idx = peak.idx,
                           alpha = 0.8, size = 2, text = FALSE,
                           title = paste(tags[peak.idx, "name"])
      )
      plot
    }

    # rm(list = c("peak.table", "sample.info", "sample", "tags"))
  }


  output$dc.single.peak.plot.dn2 <- renderPlotly({
    ggplotly(dc.single.peak.plot.dn2(),
             tooltip = c("Sample", "Injection.order", "Intensity"),
             source = "dc.single.peak.plot.dn2")
  })

  output$dc.single.peak.plot.dn2.download <- downloadHandler(
    filename = function() { "Peak.information.after.normalization.pdf" },
    content = function(file) {
      device <- function(..., width , height) {
        grDevices::pdf(..., width = input$dc.single.peak.plot.dn2.width,
                       height = input$dc.single.peak.plot.dn2.height)
      }
      ggsave(file, plot = dc.single.peak.plot.dn2(), device = device)
    }, contentType = "pdf")

})



##group information for selection
dc.group.name <- reactive({
  dc.group.name <- unique(dc.sample.info.raw$data[,"group"])
  if(any(dc.group.name == "QC")){
    dc.group.name <- c("QC", dc.group.name[-which(dc.group.name == "QC")])
  }
  dc.group.name
})

output$dc.dn.group.area1 <- renderUI({
  fluidPage(
    shinyWidgets::multiInput(inputId = "dc.dn.group1",
                             label = "Show which group",
                             choices = dc.group.name(),
                             selected = dc.group.name()[1],
                             options = list(enable_search = FALSE,
                                            non_selected_header = "No selected:",
                                            selected_header = "Selected:")
    )
  )
})


output$dc.dn.group.area2 <- renderUI({
  fluidPage(
    shinyWidgets::multiInput(inputId = "dc.dn.group2",
                             label = "Show which group",
                             choices = dc.group.name(),
                             selected = dc.group.name()[1],
                             options = list(enable_search = FALSE,
                                            non_selected_header = "No selected:",
                                            selected_header = "Selected:")
    )
  )
})


output$dc.dn.group.area3 <- renderUI({
  fluidPage(
    shinyWidgets::multiInput(inputId = "dc.dn.group3",
                             label = "Show which group",
                             choices = dc.group.name(),
                             selected = dc.group.name()[1],
                             options = list(enable_search = FALSE,
                                            non_selected_header = "No selected:",
                                            selected_header = "Selected:")
    )
  )
})


output$dc.dn.group.area4 <- renderUI({
  fluidPage(
    shinyWidgets::multiInput(inputId = "dc.dn.group4",
                             label = "Show which group",
                             choices = dc.group.name(),
                             selected = dc.group.name()[1],
                             options = list(enable_search = FALSE,
                                            non_selected_header = "No selected:",
                                            selected_header = "Selected:")
    )
  )
})


###Box plot befor data normalization
# observeEvent(eventExpr = input$dc.dn.submit.button, {
dc.boxplot.dn1 <- function(){
  par(mar = c(5,5,4,2))
  if(is.null(dc.ms1.after.zero$data)) return(NULL)
  if(is.null(input$dc.dn.group1)){
    group <- unique(dc.sample.info.raw$data[,"group"])
    if(any(group == "QC")){
      group <- "QC"
    }
  }else{
    group <- input$dc.dn.group1
  }
  dc.boxplot.dn1 <- dnSampleBoxplot(sample = dc.ms1.after.zero$data,
                                    sample.info = dc.sample.info.raw$data,
                                    # group = input$dc.dn.group1,
                                    group = group,
                                    title = "Befor normalization"
  )
  dc.boxplot.dn1

}

output$dc.boxplot.dn1 <- renderPlotly({
  ggplotly(dc.boxplot.dn1(), tooltip = c("Sample"),
           source = "dc.boxplot.dn1")
})

# })


###Box plot after data normalization
# observeEvent(eventExpr = input$dc.dn.submit.button, {
dc.boxplot.dn2 <- function(){
  par(mar = c(5,5,4,2))
  if(is.null(dc.ms1.dn$data)) return(NULL)

  if(is.null(input$dc.dn.group2)){
    group <- unique(dc.sample.info.raw$data[,"group"])
    if(any(group == "QC")){
      group <- "QC"
    }
  }else{
    group <- input$dc.dn.group2
  }
  dc.boxplot.dn2 <- dnSampleBoxplot(sample = dc.ms1.dn$data,
                                    sample.info = dc.sample.info.raw$data,
                                    # group = input$dc.dn.group1,
                                    group = group,
                                    title = "After normalization"
  )
  dc.boxplot.dn2

}

output$dc.boxplot.dn2 <- renderPlotly({
  ggplotly(dc.boxplot.dn2(), tooltip = c("Sample"),
           source = "dc.boxplot.dn2")
})

# })


# download plot
output$dc.boxplot.dn1.download <- downloadHandler(
  filename = function() { "Boxplot.befor.data.normalization.pdf" },
  content = function(file) {
    device <- function(..., width , height) {
      grDevices::pdf(..., width = input$dc.boxplot.dn1.width,
                     height = input$dc.boxplot.dn1.height)
    }
    ggsave(file, plot = dc.boxplot.dn1(), device = device)
  }, contentType = "pdf")


output$dc.boxplot.dn2.download <- downloadHandler(
  filename = function() { "Boxplot.after.data.normalization.pdf" },
  content = function(file) {
    device <- function(..., width , height) {
      grDevices::pdf(..., width = input$dc.boxplot.dn2.width,
                     height = input$dc.boxplot.dn2.height)
    }
    ggsave(file, plot = dc.boxplot.dn2(), device = device)
  }, contentType = "pdf")





###RSD before and after
# observeEvent(eventExpr = input$DCdnShowRSD, {
dc.rsd.comparation <- function(){
  par(mar = c(5,5,4,2))
  if(is.null(dc.ms1.dn$data)) return(NULL)
  if(is.null(input$dc.dn.group3)){
    group <- unique(dc.sample.info.raw$data[,"group"])
    if(any(group == "QC")){
      group <- "QC"
    }
  }else{
    group <- input$dc.dn.group3
  }

  tags <- dc.ms1.dn$data[,-match(dc.sample.info.raw$data[,"sample.name"], colnames(dc.ms1.dn$data)), drop = FALSE]

  dc.rsd.comparation <- rsdBeforeAfter(sample1 = dc.ms1.after.zero$data,
                                       sample2 = dc.ms1.dn$data,
                                       tags = tags,
                                       sample.info = dc.sample.info.raw$data,
                                       alpha = 0.5, size = 2,
                                       group = group,
                                       title = "RSD comparation")
  rm(list = c("tags"))
  dc.rsd.comparation

}

output$dc.rsd.comparation <- renderPlotly({
  ggplotly(dc.rsd.comparation(),
           tooltip = c("Peak", "Before", "After", "Change"),
           source = "dc.rsd.comparation")
})

# })


# download plot
output$dc.rsd.comparation.download <- downloadHandler(
  filename = function() { "RSD.comparation.pdf" },
  content = function(file) {
    device <- function(..., width , height) {
      grDevices::pdf(..., width = input$dc.rsd.comparation.width,
                     height = input$dc.rsd.comparation.height)
    }
    ggsave(file, plot = dc.rsd.comparation(), device = device)
  }, contentType = "pdf")






###RSD distribution before and after normalization
observeEvent(eventExpr = input$DCdnShowRSD2, {
  dc.rsd.distribution1 <- function(){
    par(mar = c(5,5,4,2))
    if(is.null(dc.ms1.after.zero$data)) return(NULL)
    if(is.null(input$dc.dn.group4)){
      group <- unique(dc.sample.info.raw$data[,"group"])
      if(any(group == "QC")){
        group <- "QC"
      }
    }else{
      group <- input$dc.dn.group4
    }

    tags <- dc.ms1.after.zero$data[,-match(dc.sample.info.raw$data[,"sample.name"],
                                           colnames(dc.ms1.after.zero$data)), drop = FALSE]

    dc.rsd.distribution1 <- rsdDistribution2(sample = dc.ms1.after.zero$data,
                                             rsd.tol = input$dc.rsd.distribution.tol,
                                             tags = tags,
                                             sample.info = dc.sample.info.raw$data,
                                             alpha = 0.5, size = 2,
                                             group = group,
                                             title = "Before normalization")
    rm(list = c("tags"))
    dc.rsd.distribution1

  }

  output$dc.rsd.distribution1 <- renderPlotly({
    ggplotly(dc.rsd.distribution1(),
             tooltip = c("Peak", "RSD", "Group"),
             source = "dc.rsd.distribution1")
  })
})


# download plot
output$dc.rsd.distribution1 <- downloadHandler(
  filename = function() { "RSD.distributation.before.normalization.pdf" },
  content = function(file) {
    device <- function(..., width , height) {
      grDevices::pdf(..., width = input$dc.rsd.distribution.width,
                     height = input$dc.rsd.distribution.height)
    }
    ggsave(file, plot = dc.rsd.distribution1(), device = device)
  }, contentType = "pdf")



observeEvent(eventExpr = input$DCdnShowRSD2, {
  dc.rsd.distribution2 <- function(){
    par(mar = c(5,5,4,2))
    if(is.null(dc.ms1.dn$data)) return(NULL)
    if(is.null(input$dc.dn.group4)){
      group <- unique(dc.sample.info.raw$data[,"group"])
      if(any(group == "QC")){
        group <- "QC"
      }
    }else{
      group <- input$dc.dn.group4
    }

    tags <- dc.ms1.dn$data[,-match(dc.sample.info.raw$data[,"sample.name"],
                                   colnames(dc.ms1.dn$data)), drop = FALSE]

    dc.rsd.distribution2 <- rsdDistribution2(sample = dc.ms1.dn$data,
                                             rsd.tol = input$dc.rsd.distribution.tol,
                                             tags = tags,
                                             sample.info = dc.sample.info.raw$data,
                                             alpha = 0.5, size = 2,
                                             group = group,
                                             title = "After normalization")
    rm(list = c("tags"))
    dc.rsd.distribution2

  }

  output$dc.rsd.distribution2 <- renderPlotly({
    ggplotly(dc.rsd.distribution2(),
             tooltip = c("Peak", "RSD", "Group"),
             source = "dc.rsd.distribution2")
  })

})


# download plot
output$dc.rsd.distribution2 <- downloadHandler(
  filename = function() { "RSD.distributation.after.normalization.pdf" },
  content = function(file) {
    device <- function(..., width , height) {
      grDevices::pdf(..., width = input$dc.rsd.distribution.width,
                     height = input$dc.rsd.distribution.height)
    }
    ggsave(file, plot = dc.rsd.distribution2(), device = device)
  }, contentType = "pdf")


###Error if there are no data normalization, warninguser click Next befor click submit
observeEvent(eventExpr = input$dc.dn.2.di, {
  if(is.null(dc.ms1.dn$data)){
    shinyalert::shinyalert(title = "Error",
                           text = "Please click Submit first.",
                           type = "error")
  }

})

##jump to Data normalization page from zero value processing page
observeEvent(input$dc.dn.2.di, {
  if(!is.null(dc.ms1.dn$data)){
    updateTabsetPanel(session, "DCtab",
                      selected = "DCdi")
  }
})



















#-------------------------------------------------------------------------------
#data integration
###error information if there are no QC samples in your data
observeEvent(eventExpr = input$dc.di.submit.button, {
  if(input$DCdiHasQC == "hasQC" & DCwithQC() == 0){
    shinyalert::shinyalert("Error", "There are no QC samples in your data.",
                           type = "error")
  }
})

##save parameter
dc.di.params <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$dc.di.submit.button,{
  dcDIparams <- DCdiParam(DCdiHasQC = input$DCdiHasQC,
                          integrationMethod1 = input$integrationMethod1,
                          integrationMethod2 = input$integrationMethod2)

  dc.di.params$data <- dcDIparams

  save(dcDIparams, file = file.path("user_data", user_input$username,
                                    input$DCprojectID,
                                    "data_cleaning",
                                    "dcDIparams"))

})

##data integration
dc.ms1.di <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$dc.di.submit.button,{
  withProgress(message = 'Data integration...',
               detail = 'This may take a while',
               # style= "old",
               value = 0, {
                 temp <- try({
                   lapply(1:1, function(idx){
                     incProgress(1/length(idx),
                                 detail = "This may take a while")
                     dc.peak.table <- dc.ms1.dn$data
                     dc.sample.info <- dc.sample.info.raw$data
                     dc.sample <- dc.peak.table[,match(dc.sample.info.raw$data[,"sample.name"], colnames(dc.peak.table)), drop = FALSE]
                     dc.tags <- dc.peak.table[,-match(dc.sample.info.raw$data[,"sample.name"], colnames(dc.peak.table)), drop = FALSE]
                     if(input$DCdiHasQC == "hasQC"){
                       method <- input$integrationMethod1
                     }else{
                       method <- input$integrationMethod2
                     }

                     if(method != "no"){
                       temp.data <- dataIntegration(sample = dc.sample,
                                                    sample.info = dc.sample.info,
                                                    method = method)
                       data.frame(dc.tags, temp.data, stringsAsFactors = FALSE)
                     }else{
                       dc.ms1.dn$data
                     }
                   })[[1]]
                 })

                 if(class(temp)[1] == "try-error"){
                   dc.ms1.di.message <- renderText({paste("Data integration: ",temp[[1]])})
                 }else{
                   dc.ms1.di$data <- temp
                 }

                 rm(list = c("dc.peak.table", "dc.sample.info", "dc.sample", "dc.tags"))
               })
})


output$dc.ms1.di <- DT::renderDataTable(
  DT::datatable(dc.ms1.di$data,
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
                        buttons = list(list(extend="csv",filename = "Data.after.integration"),
                                       list(extend='excel',filename = "Data.after.integration")),
                        text = 'Download'
                      ),
                      I('colvis')
                    ),
                  scrollX = TRUE
                )
  ),
  server = FALSE
)



###information for single peak before integration
observeEvent(eventExpr = input$DCdiShowPeak1, {
  peak.table <- dc.ms1.dn$data
  sample.info <- dc.sample.info.raw$data
  sample <- peak.table[,match(sample.info[,1], colnames(peak.table)), drop = FALSE]
  tags <- peak.table[,-match(sample.info[,1], colnames(peak.table)), drop = FALSE]

  if (is.null(input$dc.ms1.di_rows_selected)) {
    dc.single.peak.plot.di1 <- function(){
      par( mar = c(5,5,4,2))
      ggplot2::ggplot(data = data.frame("x" = 1, "y" = 1, stringsAsFactors = FALSE), ggplot2::aes(x = x, y = y)) +
        ggplot2::theme_bw() +
        ggplot2::annotate(geom = "text", x = 1, y = 1, label = "Plese select a peak")
    }

  }else{
    peak.idx <- as.numeric(input$dc.ms1.di_rows_selected)
    dc.single.peak.plot.di1 <- function(){
      par(mar = c(5,5,4,2))
      plot <- dnSinglePeak(sample = sample, tags = tags,
                           sample.info = sample.info, peak.idx = peak.idx,
                           alpha = 0.8, size = 2, text = FALSE,
                           title = paste(tags[peak.idx, "name"])
      )
      plot
    }

    # rm(list = c("peak.table", "sample.info", "sample", "tags"))
  }

  output$dc.single.peak.plot.di1 <- renderPlotly({
    ggplotly(dc.single.peak.plot.di1(),
             tooltip = c("Sample", "Injection.order", "Batch"),
             source = "dc.single.peak.plot.di")
  })

  output$dc.single.peak.plot.di1.download <- downloadHandler(
    filename = function() { "Peak.information.pdf" },
    content = function(file) {
      device <- function(..., width , height) {
        grDevices::pdf(..., width = input$dc.single.peak.plot.di1.width,
                       height = input$dc.single.peak.plot.di1.height)
      }
      ggsave(file, plot = dc.single.peak.plot.di1(), device = device)
    }, contentType = "pdf")

})


###information for single peak after integration
observeEvent(eventExpr = input$DCdiShowPeak2, {
  peak.table <- dc.ms1.di$data
  sample.info <- dc.sample.info.raw$data
  sample <- peak.table[,match(sample.info[,1], colnames(peak.table)), drop = FALSE]
  tags <- peak.table[,-match(sample.info[,1], colnames(peak.table)), drop = FALSE]

  if (is.null(input$dc.ms1.di_rows_selected)) {
    dc.single.peak.plot.di2 <- function(){
      par( mar = c(5,5,4,2))
      ggplot2::ggplot(data = data.frame("x" = 1, "y" = 1, stringsAsFactors = FALSE), ggplot2::aes(x = x, y = y)) +
        ggplot2::theme_bw() +
        ggplot2::annotate(geom = "text", x = 1, y = 1, label = "Plese select a peak")
    }

  }else{
    peak.idx <- as.numeric(input$dc.ms1.di_rows_selected)
    dc.single.peak.plot.di2 <- function(){
      par(mar = c(5,5,4,2))
      plot <- dnSinglePeak(sample = sample, tags = tags,
                           sample.info = sample.info, peak.idx = peak.idx,
                           alpha = 0.8, size = 2, text = FALSE,
                           title = paste(tags[peak.idx, "name"])
      )
      plot
    }

    # rm(list = c("peak.table", "sample.info", "sample", "tags"))
  }


  output$dc.single.peak.plot.di2 <- renderPlotly({
    ggplotly(dc.single.peak.plot.di2(),
             tooltip = c("Sample", "Injection.order", "Intensity"),
             source = "dc.single.peak.plot.di2")
  })

  output$dc.single.peak.plot.di2.download <- downloadHandler(
    filename = function() { "Peak.information.after.integration.pdf" },
    content = function(file) {
      device <- function(..., width , height) {
        grDevices::pdf(..., width = input$dc.single.peak.plot.di2.width,
                       height = input$dc.single.peak.plot.di2.height)
      }
      ggsave(file, plot = dc.single.peak.plot.di2(), device = device)
    }, contentType = "pdf")

})



##group information for selection
dc.group.name <- reactive({
  dc.group.name <- unique(dc.sample.info.raw$data[,"group"])
  if(any(dc.group.name == "QC")){
    dc.group.name <- c("QC", dc.group.name[-which(dc.group.name == "QC")])
  }
  dc.group.name
})

output$dc.di.group.area1 <- renderUI({
  fluidPage(
    shinyWidgets::multiInput(inputId = "dc.di.group1",
                             label = "Show which group",
                             choices = dc.group.name(),
                             selected = dc.group.name()[1],
                             options = list(enable_search = FALSE,
                                            non_selected_header = "No selected:",
                                            selected_header = "Selected:")
    )
  )
})


output$dc.di.group.area2 <- renderUI({
  fluidPage(
    shinyWidgets::multiInput(inputId = "dc.di.group2",
                             label = "Show which group",
                             choices = dc.group.name(),
                             selected = dc.group.name()[1],
                             options = list(enable_search = FALSE,
                                            non_selected_header = "No selected:",
                                            selected_header = "Selected:")
    )
  )
})


output$dc.di.group.area3 <- renderUI({
  fluidPage(
    shinyWidgets::multiInput(inputId = "dc.di.group3",
                             label = "Show which group",
                             choices = dc.group.name(),
                             selected = dc.group.name()[1],
                             options = list(enable_search = FALSE,
                                            non_selected_header = "No selected:",
                                            selected_header = "Selected:")
    )
  )
})


output$dc.di.group.area4 <- renderUI({
  fluidPage(
    shinyWidgets::multiInput(inputId = "dc.di.group4",
                             label = "Show which group",
                             choices = dc.group.name(),
                             selected = dc.group.name()[1],
                             options = list(enable_search = FALSE,
                                            non_selected_header = "No selected:",
                                            selected_header = "Selected:")
    )
  )
})


###Box plot befor data integration
# observeEvent(eventExpr = input$dc.di.submit.button, {
dc.boxplot.di1 <- function(){
  par(mar = c(5,5,4,2))
  if(is.null(dc.ms1.dn$data)) return(NULL)
  if(is.null(input$dc.di.group1)){
    group <- unique(dc.sample.info.raw$data[,"group"])
    if(any(group == "QC")){
      group <- "QC"
    }
  }else{
    group <- input$dc.di.group1
  }
  dc.boxplot.di1 <- diSampleBoxplot(sample = dc.ms1.dn$data,
                                    sample.info = dc.sample.info.raw$data,
                                    # group = input$dc.di.group1,
                                    group = group,
                                    title = "Befor integration"
  )
  dc.boxplot.di1

}

output$dc.boxplot.di1 <- renderPlotly({
  ggplotly(dc.boxplot.di1(), tooltip = c("Sample"),
           source = "dc.boxplot.di1")
})

# })


###Box plot after data integration
# observeEvent(eventExpr = input$dc.di.submit.button, {
dc.boxplot.di2 <- function(){
  par(mar = c(5,5,4,2))
  if(is.null(dc.ms1.di$data)) return(NULL)

  if(is.null(input$dc.di.group2)){
    group <- unique(dc.sample.info.raw$data[,"group"])
    if(any(group == "QC")){
      group <- "QC"
    }
  }else{
    group <- input$dc.di.group2
  }
  dc.boxplot.di2 <- diSampleBoxplot(sample = dc.ms1.di$data,
                                    sample.info = dc.sample.info.raw$data,
                                    # group = input$dc.di.group1,
                                    group = group,
                                    title = "After integration"
  )
  dc.boxplot.di2

}

output$dc.boxplot.di2 <- renderPlotly({
  ggplotly(dc.boxplot.di2(), tooltip = c("Sample"),
           source = "dc.boxplot.di2")
})

# })


# download plot
output$dc.boxplot.di1.download <- downloadHandler(
  filename = function() { "Boxplot.befor.data.integration.pdf" },
  content = function(file) {
    device <- function(..., width , height) {
      grDevices::pdf(..., width = input$dc.boxplot.di1.width,
                     height = input$dc.boxplot.di1.height)
    }
    ggsave(file, plot = dc.boxplot.di1(), device = device)
  }, contentType = "pdf")


output$dc.boxplot.di2.download <- downloadHandler(
  filename = function() { "Boxplot.after.data.integration.pdf" },
  content = function(file) {
    device <- function(..., width , height) {
      grDevices::pdf(..., width = input$dc.boxplot.di2.width,
                     height = input$dc.boxplot.di2.height)
    }
    ggsave(file, plot = dc.boxplot.di2(), device = device)
  }, contentType = "pdf")



###RSD before and after
# observeEvent(eventExpr = input$DCdiShowRSD, {
dc.di.rsd.comparation <- function(){
  par(mar = c(5,5,4,2))
  if(is.null(dc.ms1.di$data)) return(NULL)
  if(is.null(input$dc.di.group3)){
    group <- unique(dc.sample.info.raw$data[,"group"])
    if(any(group == "QC")){
      group <- "QC"
    }
  }else{
    group <- input$dc.di.group3
  }

  tags <- dc.ms1.di$data[,-match(dc.sample.info.raw$data[,"sample.name"], colnames(dc.ms1.di$data)), drop = FALSE]

  dc.di.rsd.comparation <- rsdBeforeAfter(sample1 = dc.ms1.dn$data,
                                          sample2 = dc.ms1.di$data,
                                          tags = tags,
                                          sample.info = dc.sample.info.raw$data,
                                          alpha = 0.5, size = 2,
                                          group = group,
                                          title = "RSD comparation"
  )
  dc.di.rsd.comparation

}

output$dc.di.rsd.comparation <- renderPlotly({
  ggplotly(dc.di.rsd.comparation(),
           tooltip = c("Peak", "Before", "After", "Change"),
           source = "dc.di.rsd.comparation")
})

# download plot
output$dc.di.rsd.comparation.download <- downloadHandler(
  filename = function() { "RSD.comparation.pdf" },
  content = function(file) {
    device <- function(..., width , height) {
      grDevices::pdf(..., width = input$dc.di.rsd.comparation.width,
                     height = input$dc.di.rsd.comparation.height)
    }
    ggsave(file, plot = dc.di.rsd.comparation(), device = device)
  }, contentType = "pdf")



###RSD distribution before and after integration
observeEvent(eventExpr = input$DCdiShowRSD2, {
  dc.di.rsd.distribution1 <- function(){
    par(mar = c(5,5,4,2))
    if(is.null(dc.ms1.dn$data)) return(NULL)
    if(is.null(input$dc.di.group4)){
      group <- unique(dc.sample.info.raw$data[,"group"])
      if(any(group == "QC")){
        group <- "QC"
      }
    }else{
      group <- input$dc.di.group4
    }

    tags <- dc.ms1.dn$data[,-match(dc.sample.info.raw$data[,"sample.name"], colnames(dc.ms1.dn$data)), drop = FALSE]

    dc.di.rsd.distribution1 <- rsdDistribution2(sample = dc.ms1.dn$data,
                                                rsd.tol = input$dc.di.rsd.distribution.tol,
                                                tags = tags,
                                                sample.info = dc.sample.info.raw$data,
                                                alpha = 0.5, size = 2,
                                                group = group,
                                                title = "Before integration")
    rm(list = "tags")
    dc.di.rsd.distribution1
  }

  output$dc.di.rsd.distribution1 <- renderPlotly({
    ggplotly(dc.di.rsd.distribution1(),
             tooltip = c("Peak", "RSD", "Group"),
             source = "dc.di.rsd.distribution1")
  })

})


# download plot
output$dc.di.rsd.distribution2 <- downloadHandler(
  filename = function() { "RSD.distributation.before.integration.pdf" },
  content = function(file) {
    device <- function(..., width , height) {
      grDevices::pdf(..., width = input$dc.di.rsd.distribution2.width,
                     height = input$dc.di.rsd.distribution2.height)
    }
    ggsave(file, plot = dc.di.rsd.distribution2(), device = device)
  }, contentType = "pdf")



observeEvent(eventExpr = input$DCdiShowRSD2, {
  dc.di.rsd.distribution2 <- function(){
    par(mar = c(5,5,4,2))
    if(is.null(dc.ms1.di$data)) return(NULL)
    if(is.null(input$dc.di.group4)){
      group <- unique(dc.sample.info.raw$data[,"group"])
      if(any(group == "QC")){
        group <- "QC"
      }
    }else{
      group <- input$dc.di.group4
    }

    tags <- dc.ms1.di$data[,-match(dc.sample.info.raw$data[,"sample.name"], colnames(dc.ms1.di$data)), drop = FALSE]

    dc.di.rsd.distribution2 <- rsdDistribution2(sample = dc.ms1.di$data,
                                                rsd.tol = input$dc.di.rsd.distribution.tol,
                                                tags = tags,
                                                sample.info = dc.sample.info.raw$data,
                                                alpha = 0.5, size = 2,
                                                group = group,
                                                title = "After integration")
    rm(list = "tags")
    dc.di.rsd.distribution2

  }

  output$dc.di.rsd.distribution2 <- renderPlotly({
    ggplotly(dc.di.rsd.distribution2(),
             tooltip = c("Peak", "RSD", "Group"),
             source = "dc.di.rsd.distribution2")
  })

})


# download plot
output$dc.di.rsd.distribution2 <- downloadHandler(
  filename = function() { "RSD.distributation.after.integration.pdf" },
  content = function(file) {
    device <- function(..., width , height) {
      grDevices::pdf(..., width = input$dc.di.rsd.distribution2.width,
                     height = input$dc.di.rsd.distribution2.height)
    }
    ggsave(file, plot = dc.di.rsd.distribution2(), device = device)
  }, contentType = "pdf")




###Error if there are no data integration, warning user click Next befor click submit
observeEvent(eventExpr = input$dc.di.2.os, {
  if(is.null(dc.ms1.di$data)){
    shinyalert::shinyalert(title = "Error",
                           text = "Please click Submit first.",
                           type = "error")
  }

})

##jump to Data normalization page from zero value processing page
observeEvent(input$dc.di.2.os, {
  if(!is.null(dc.ms1.di$data)){
    updateTabsetPanel(session, "DCtab",
                      selected = "DCos")
  }
})






















#-------------------------------------------------------------------------------
#outlier sample filtering
##save parameter
dc.os.params <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$dc.os.submit.button, {
  dcOSparams <- DCosParam(
    input$dc.os.pca.log,
    input$dc.os.pca.scale,
    input$dc.os.pca.center,
    input$dc.os.pca.ci.tol,
    input$dc.os.zero.tol)
  dc.os.params$data <- dcOSparams

  save(dcOSparams, file = file.path("user_data", user_input$username,
                                    input$DCprojectID,
                                    "data_cleaning",
                                    "dcOSparams"))

})


##PCA to find the outlier samples
##group information for selection
dc.group.name <- reactive({
  dc.group.name <- unique(dc.sample.info.raw$data[,"group"])
  if(any(dc.group.name == "QC")){
    dc.group.name <- c("QC", dc.group.name[-which(dc.group.name == "QC")])
  }
  dc.group.name
})

output$dc.os.group.area1 <- renderUI({
  fluidPage(
    shinyWidgets::multiInput(inputId = "dc.os.group1",
                             label = "Show which group",
                             choices = dc.group.name(),
                             selected = dc.group.name()[1],
                             options = list(enable_search = FALSE,
                                            non_selected_header = "No",
                                            selected_header = "Selected")
    )
  )
})


output$dc.os.group.area2 <- renderUI({
  fluidPage(
    shinyWidgets::multiInput(inputId = "dc.os.group2",
                             label = "Show which group",
                             choices = dc.group.name(),
                             selected = dc.group.name()[1],
                             options = list(enable_search = FALSE,
                                            non_selected_header = "No",
                                            selected_header = "Selected:")
    )
  )
})


output$dc.os.group.area3 <- renderUI({
  fluidPage(
    shinyWidgets::multiInput(inputId = "dc.os.group3",
                             label = "Show which group",
                             choices = dc.group.name(),
                             selected = dc.group.name()[1],
                             options = list(enable_search = FALSE,
                                            non_selected_header = "No",
                                            selected_header = "Selected:")
    )
  )
})


output$dc.os.group.area4 <- renderUI({
  fluidPage(
    shinyWidgets::multiInput(inputId = "dc.os.group4",
                             label = "Show which group",
                             choices = dc.group.name(),
                             selected = dc.group.name()[1],
                             options = list(enable_search = FALSE,
                                            non_selected_header = "No",
                                            selected_header = "Selected:")
    )
  )
})





dc.os.pca <- eventReactive(eventExpr = input$dc.os.submit.button,{
  if(is.null(dc.ms1.di$data) | is.null(dc.sample.info.raw$data)) return(NULL)
  dc.peak.table <- dc.ms1.di$data
  dc.sample.info <- dc.sample.info.raw$data

  dc.sample <- dc.peak.table[,match(dc.sample.info[,"sample.name"],
                                    colnames(dc.peak.table)), drop = FALSE]
  dc.tags <- dc.peak.table[,-match(dc.sample.info[,"sample.name"],
                                   colnames(dc.peak.table)), drop = FALSE]

  ##log
  dc.sample <- sxtLog(sample = dc.sample, method = input$dc.os.pca.log)
  ##scale
  dc.sample <- sxtScale(sample = dc.sample,
                        method = input$dc.os.pca.scale,
                        center = input$dc.os.pca.center)

  dc.os.pca <- prcomp(data.frame(t(dc.sample)),
                      retx = TRUE,
                      center = FALSE,
                      scale = FALSE)
  dc.os.pca
})


dc.os.outlier.index1 <- eventReactive(eventExpr = input$dc.os.submit.button, {
  dc.os.outlier.index1 <- try(pcaFindOutlier(dc.os.pca()))
  if(class(dc.os.outlier.index1) == "try-error"){
    dc.os.outlier.index1 <- which(1 > 5)
  }
  dc.os.outlier.index1
})

output$dc.os.outlier.name.pca <- renderText({
  if(length(dc.os.outlier.index1()) > 0){
    return(
      paste(paste(names(dc.os.outlier.index1()), collapse = "&"),
            " are outlier samples.", sep = "")
    )
  }else{
    return("No outlier samples.")
  }
})


dc.os.pca.plot <- function(){
  pcaOutlierPlot(pca.object = dc.os.pca(),
                 outlier.idx = dc.os.outlier.index1(),
                 alpha = 0.8, size = 2, text = "",
                 title = "PCA score plot")
}



output$dc.os.pca.plot <- renderPlotly({
  if(is.null(dc.os.pca())) return(NULL)
  ggplotly(dc.os.pca.plot(),
           tooltip = c("Sample", "PC1", "PC2", "Group"),
           source = "dc.os.pca.plot")
})

##download plot
output$dc.os.pca.plot.download <- downloadHandler(
  filename = function() { "PCA.score.plot.for.outlier.samples.pdf" },
  content = function(file) {
    pdf(file = file,
        width = input$dc.os.pca.plot.width,
        height = input$dc.os.pca.plot.height)
    dc.os.pca.plot()
    dev.off()
  }, contentType = "pdf")



##find outlier samples using zero ratio
dc.os.sample.zero.ratio <- eventReactive(eventExpr = input$dc.os.submit.button, {
  if(is.null(dc.ms1.di$data) | is.null(dc.sample.info.raw$data)) return(NULL)
  dc.peak.table <- dc.ms1.di$data
  dc.sample.info <- dc.sample.info.raw$data

  dc.sample <- dc.peak.table[,match(dc.sample.info[,"sample.name"],
                                    colnames(dc.peak.table)), drop = FALSE]
  dc.tags <- dc.peak.table[,-match(dc.sample.info[,"sample.name"],
                                   colnames(dc.peak.table)), drop = FALSE]
  dc.os.sample.zero.ratio <- apply(dc.sample, 2, function(x){
    sum(x == 0)*100/nrow(dc.sample)
  })
  dc.os.sample.zero.ratio
})

dc.os.outlier.index2 <- eventReactive(eventExpr = input$dc.os.submit.button, {
  which(dc.os.sample.zero.ratio() > input$dc.os.zero.tol)
})

dc.os.zero.plot <- function(){
  DCosZeroRatioPlot(object = dc.os.sample.zero.ratio(),
                    outlier.idx = dc.os.outlier.index2(),
                    zero.ratio.tol = input$dc.os.zero.tol,
                    sample.info = dc.sample.info.raw$data,
                    alpha = 0.8, size = 2, title = "Zero value ratio")
}


output$dc.os.zero.plot <- renderPlotly({
  if(is.null(dc.os.zero.plot())) return(NULL)
  ggplotly(dc.os.zero.plot(),
           tooltip = c("Sample", "Injection.order", "Zero.ratio", "Group"),
           source = "dc.os.zero.plot")
})

##download plot
output$dc.os.zero.plot.download <- downloadHandler(
  filename = function() { "Zero.value.ratio.plot.pdf" },
  content = function(file) {
    pdf(file = file,
        width = input$dc.os.zero.plot.width,
        height = input$dc.os.zero.plot.height)
    dc.os.zero.plot()
    dev.off()
  }, contentType = "pdf")




###outlier sample summary
dc.os.outlier.index <- eventReactive(input$dc.os.submit.button,{
  union(x = dc.os.outlier.index1(), y = dc.os.outlier.index2())
})

observeEvent(eventExpr = input$dc.os.submit.button,{
  if(length(dc.os.outlier.index()) == 0){
    output$dc.os.result.area <- renderUI({
      fluidPage(
        "No outliers"
      )
    })
  }else{
    sample.name <- dc.sample.info.raw$data[,"sample.name"]
    note1 <- note2 <- rep("No", length(sample.name))
    if(length(dc.os.outlier.index1()) > 0) note1[dc.os.outlier.index1()] <- "Yes"
    if(length(dc.os.outlier.index2()) > 0) note2[dc.os.outlier.index2()] <- "Yes"
    temp <- data.frame(sample.name, note1, note2, stringsAsFactors = FALSE)
    colnames(temp) <- c("Sample", "PCA", "Zero value")
    temp <- temp[dc.os.outlier.index(),]

    output$dc.os.outliers <- DT::renderDataTable(
      DT::datatable(temp,
                    selection = "single",
                    escape = FALSE,
                    filter = "none",
                    rownames = FALSE,
                    extensions = list("ColReorder" = NULL,
                                      "Buttons" = NULL,
                                      "FixedColumns" = list(leftColumns=2)
                    ),
                    options = list(
                      pageLength = 10,
                      dom = 'Bfrtip',
                      # autoWidth = TRUE,
                      lengthMenu = list(c(10, 50, -1), c('10', '50', 'All')),
                      ColReorder = TRUE,
                      buttons =
                        list(
                          list(
                            extend = 'collection',
                            # buttons = c('csv', 'excel'),
                            buttons = list(list(extend="csv",filename = "Outlier.samples"),
                                           list(extend='excel',filename = "Outlier.samples")
                            ),
                            text = 'Download'
                          ),
                          I('colvis')
                        ),
                      scrollX = TRUE
                    )
      ),
      server = FALSE
    )


    output$dc.os.result.area <- renderUI({
      fluidPage(
        column(width = 6,
               shinyWidgets::multiInput(inputId = "osDeleteSampleName",
                                        label = "Outlier samples",
                                        choices = as.character(temp[,1]),
                                        # selected = dc.group.name()[1],
                                        options = list(enable_search = FALSE,
                                                       non_selected_header = "No selected:",
                                                       selected_header = "Selected:")
               ),
               actionButton(inputId = 'osDeleteSample',
                            "Delete",
                            styleclass = "info",
                            icon = icon('play-circle')),
               helpText("Please select the sample names and click",
                        strong("Delete"), ", and then click Submit again to delete outlier samples.")
        ),
        column(width = 6,
               h5("Outlier sample information"),
               DT::dataTableOutput(outputId = "dc.os.outliers"),
               textOutput(outputId = "os.delete.sample.info")
        )
        # )
      )
    })
  }
})


observeEvent(eventExpr = input$osDeleteSample,{
  output$os.delete.sample.info <- renderText({
    paste(paste(input$osDeleteSampleName, collapse = ", "),
          " will be removed from peak table.", sep = "")
  })
})



dc.ms1.os <- reactiveValues(data = NULL)
dc.sample.info.os <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$dc.os.submit.button, {
  if(is.null(dc.ms1.di$data) | is.null(dc.sample.info.raw$data)) return(NULL)
  dc.ms1.os$data <- dc.ms1.di$data
  dc.sample.info.os$data <- dc.sample.info.raw$data
  if(!is.null(input$osDeleteSampleName)) {
    dc.ms1.os$data <-
      dc.ms1.os$data[,-match(input$osDeleteSampleName, colnames(dc.ms1.os$data)), drop = FALSE]

    dc.sample.info.os$data <-
      dc.sample.info.raw$data[-match(input$osDeleteSampleName,
                                     dc.sample.info.raw$data[,"sample.name"]), ,drop = FALSE]

  }
})


##
output$dc.ms1.os <- DT::renderDataTable(
  DT::datatable(dc.ms1.os$data,
                selection = list(mode = 'single', target = 'column'),
                # selection = "single",
                escape = FALSE,
                filter = "none",
                rownames= FALSE,
                extensions = list("ColReorder" = NULL,
                                  "Buttons" = NULL,
                                  "FixedColumns" = list(leftColumns=2)
                ),
                options = list(
                  pageLength = 10,
                  dom = 'Bfrtip',
                  # autoWidth = TRUE,
                  lengthMenu = list(c(10, 50, -1), c('10', '50', 'All')),
                  ColReorder = TRUE,
                  buttons =
                    list(
                      list(
                        extend = 'collection',
                        # buttons = c('csv', 'excel'),
                        buttons = list(list(extend="csv",filename = "Data.after.outlier.removal"),
                                       list(extend='excel',filename = "Data.after.outlier.removal")
                        ),
                        text = 'Download'
                      ),
                      I('colvis')
                    ),
                  scrollX = TRUE
                )
  ),
  server = FALSE
)



###box plot for each sample
observeEvent(eventExpr = input$DCosShowSample, {
  peak.table <- dc.ms1.os$data
  sample.info <- dc.sample.info.os$data
  sample <- peak.table[,match(sample.info[,1], colnames(peak.table)), drop = FALSE]
  tags <- peak.table[,-match(sample.info[,1], colnames(peak.table)), drop = FALSE]

  if (is.null(input$dc.ms1.os_columns_selected) |
      input$dc.ms1.os_columns_selected <= ncol(tags)) {
    dc.single.sample.plot.os <- function(){
      par( mar = c(5,5,4,2))
      ggplot2::ggplot(data = data.frame("x" = 1, "y" = 1, stringsAsFactors = FALSE), ggplot2::aes(x = x, y = y)) +
        ggplot2::theme_bw() +
        ggplot2::annotate(geom = "text", x = 1, y = 1, label = "Plese select a sample.")
    }

  }else{
    sample.idx <- as.numeric(input$dc.ms1.os_columns_selected)
    dc.single.sample.plot.os <- function(){
      par(mar = c(5,5,4,2))
      plot <- osSingleSample(sample = sample,
                             sample.info = sample.info,
                             sample.idx = sample.idx,
                             alpha = 0.8, size = 2,
                             title = "")

      plot
    }

    # rm(list = c("peak.table", "sample.info", "sample", "tags"))
  }


  output$dc.single.sample.plot.os <- renderPlot({
    # ggplotly(dc.single.sample.plot.os(),
    #          tooltip = c("Sample", "Group"),
    #          source = "dc.single.sample.plot.os")
    dc.single.sample.plot.os()
  })

  output$dc.single.sample.plot.os.download <- downloadHandler(
    filename = function() { "Sample.boxplot.pdf" },
    content = function(file) {
      device <- function(..., width , height) {
        grDevices::pdf(..., width = input$dc.single.sample.plot.os.width,
                       height = input$dc.single.sample.plot.os.height)
      }
      ggsave(file, plot = dc.single.sample.plot.os(), device = device)
    }, contentType = "pdf")

})



###Error if there are no outlier sample processing, warning user click Next befor click submit
observeEvent(eventExpr = input$dc.os.2.qa2, {
  if(is.null(dc.ms1.os$data)){
    shinyalert::shinyalert(title = "Error",
                           text = "Please click Submit first.",
                           type = "error")
  }

})

##jump to data quality outlier sample processing page from Data integration
observeEvent(input$dc.os.2.qa2, {
  if(!is.null(dc.ms1.os$data)){
    updateTabsetPanel(session, "DCtab",
                      selected = "DCdataQualityAfter")
  }
})





















#-------------------------------------------------------------------------------
#data quality assessment, after data cleaning
## peak profile
dc.data.profile.plot2 <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$dc.os.2.qa2, {
  if(is.null(dc.ms1.os$data)) return(NULL)
  dc.data.profile.plot2$data <- dataInformation(object = dc.ms1.os$data)
})

# dc.data.profile.plot2 <- function(){
#   if(is.null(dc.ms1.os$data)) return(NULL)
#   dc.data.profile.plot2 <- dataInformation(object = dc.ms1.os$data)
# }

output$dc.data.profile.plot2 <- renderPlotly({
  if(is.null(dc.data.profile.plot2$data)) return(NULL)
  ggplotly(dc.data.profile.plot2$data, tooltip = c(
    "rt",
    "mz","int.log"),
    source = "dc.data.profile.plot2")
})


## zero ratio in peaks
dc.peak.zero.ratio.plot2 <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$dc.os.2.qa2, {
  if(is.null(dc.ms1.os$data)) return(NULL)
  dc.peak.zero.ratio.plot2$data <- zeroPeakPlot(data = dc.ms1.os$data,
                                                text = FALSE)
})


output$dc.peak.zero.ratio.plot2 <- renderPlotly({
  if(is.null(dc.peak.zero.ratio.plot2$data)) return(NULL)
  ggplotly(dc.peak.zero.ratio.plot2$data, tooltip = c(
    "Index",
    "zero.ratio","Peak"),
    source = "dc.peak.zero.ratio.plot2")
})


## zero ratio in Samples
dc.sample.zero.ratio.plot2 <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$dc.os.2.qa2, {
  if(is.null(dc.ms1.os$data)) return(NULL)
  dc.sample.zero.ratio.plot2$data <- zeroSamplePlot(data = dc.ms1.os$data,
                                                    text = FALSE)
})



output$dc.sample.zero.ratio.plot2 <- renderPlotly({
  if(is.null(dc.sample.zero.ratio.plot2$data)) return(NULL)
  ggplotly(dc.sample.zero.ratio.plot2$data, tooltip = c(
    "Index",
    "zero.ratio","Sample"),
    source = "dc.sample.zero.ratio.plot2")
})


###group information in da_qa2_ui
dc.group.name2 <- reactive({
  if(is.null(dc.sample.info.os$data)) return(NULL)
  dc.group.name2 <- unique(dc.sample.info.os$data[,"group"])
  dc.group.name2
})


output$dc.qa2.group.area <- renderUI({
  fluidPage(
    shinyWidgets::multiInput(inputId = "dc.qa2.group",
                             label = "Show which group",
                             choices = dc.group.name2(),
                             selected = dc.group.name2()[1],
                             options = list(enable_search = FALSE,
                                            non_selected_header = "No",
                                            selected_header = "Selected")
    )
  )
})



###RSD distribution
dc.peak.rsd.plot2 <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$dc.os.2.qa2, {
  if(is.null(input$dc.qa2.group)){
    group <-  unique(dc.sample.info.os$data[,"group"])[1]
  }else{
    group <- input$dc.qa2.group
  }
  dc.peak.rsd.plot2$data <- rsdDistribution(data = dc.ms1.os$data,
                                            sample.info = dc.sample.info.os$data,
                                            group = group,
                                            rsd.tol = 30)
})

observeEvent(eventExpr = input$dc.qa2.group, {
  dc.peak.rsd.plot$data <- rsdDistribution(data = dc.ms1.os$data,
                                           sample.info = dc.sample.info.os$data,
                                           group = input$dc.qa2.group,
                                           rsd.tol = 30)
})


dc.peak.rsd.plot.for.report2 <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$dc.os.2.qa2, {
  if(is.null(dc.ms1.os$data)) return(NULL)
  if(any(unique(dc.sample.info.os$data[,"group"]) == "QC")){
    group <- "QC"
  }else{
    group <- unique(dc.sample.info.os$data[,"group"])[1]
  }
  dc.peak.rsd.plot.for.report2$data <- rsdDistribution(data = dc.ms1.os$data,
                                                       sample.info = dc.sample.info.os$data,
                                                       group = group,
                                                       rsd.tol = 30)
})



output$dc.peak.rsd.plot2 <- renderPlotly({
  if(is.null(dc.peak.rsd.plot2$data)) return(NULL)
  ggplotly(dc.peak.rsd.plot2$data,
           #        tooltip = c(
           # "Index",
           # "zero.ratio","Sample"),
           source = "dc.peak.rsd.plot2")
})




#-------------------------------------------------------------------------------
#PCA score plot, should be fixed
dc.qa2.pca.object <- eventReactive(eventExpr = input$dc.os.2.qa2,{
  if(!is.null(dc.ms1.os$data) & !is.null(dc.sample.info.os$data)){
    dc.peak.table <- dc.ms1.os$data
    dc.sample.info <- dc.sample.info.os$data
    dc.sample <- dc.peak.table[,match(dc.sample.info[,"sample.name"],
                                      colnames(dc.peak.table)), drop = FALSE]
    dc.tags <- dc.peak.table[,-match(dc.sample.info[,"sample.name"],
                                     colnames(dc.peak.table)), drop = FALSE]
    ##log
    dc.sample <- sxtLog(sample = dc.sample, method = "log10")
    ##scale
    dc.sample <- sxtScale(sample = dc.sample,
                          method = "pareto",
                          center = TRUE)

    dc.qa2.pca.object <- prcomp(data.frame(t(dc.sample)),
                                retx = TRUE,
                                center = FALSE,
                                scale = FALSE)
    dc.qa2.pca.object
  }
})

output$dc.qa2.pca.score.plot <- renderPlotly({
  if(!is.null(dc.sample.info.os$data)){
    batch.info <- dc.sample.info.os$data[,c(1,4)]
    class.info <- dc.sample.info.os$data[,c(1,3)]
    dc.qa2.pca.score.plot <- pcaScorePlot(pca.object = dc.qa2.pca.object(),
                                          batch.info = batch.info,
                                          class.info = class.info)
    plotly::ggplotly(dc.qa2.pca.score.plot,
                     source = "dc.qa2.pca.score.plot")
  }
})

###PCA score plot for analysis report
dc.qa2.pca.object.for.report <- eventReactive(eventExpr = input$dc.os.2.qa2,{
  if(!is.null(dc.ms1.os$data) & !is.null(dc.sample.info.os$data)){
    dc.peak.table <- dc.ms1.os$data
    dc.sample.info <- dc.sample.info.os$data
    dc.sample <- dc.peak.table[,match(dc.sample.info[,"sample.name"],
                                      colnames(dc.peak.table)), drop = FALSE]
    dc.tags <- dc.peak.table[,-match(dc.sample.info[,"sample.name"],
                                     colnames(dc.peak.table)), drop = FALSE]
    ##log
    dc.sample <- sxtLog(sample = dc.sample, method = "log10")
    ##scale
    dc.sample <- sxtScale(sample = dc.sample,
                          method = "pareto",
                          center = TRUE)

    dc.qa2.pca.object.for.report <- prcomp(data.frame(t(dc.sample)),
                                           retx = TRUE,
                                           center = FALSE,
                                           scale = FALSE)
    dc.qa2.pca.object.for.report
  }
})


dc.qa2.pca.score.plot.for.report <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$dc.os.2.qa2, {
  if(is.null(dc.sample.info.os$data)) return(NULL)
  batch.info <- dc.sample.info.os$data[,c(1,4)]
  class.info <- dc.sample.info.os$data[,c(1,3)]
  dc.qa2.pca.score.plot.for.report$data <- pcaScorePlot(pca.object = dc.qa2.pca.object.for.report(),
                                                        batch.info <- dc.sample.info.os$data[,c(1,4)],
                                                        class.info <- dc.sample.info.os$data[,c(1,3)])
})


###Boxplots for QC samples
dc.qc.int.boxplot2 <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$dc.os.2.qa2, {
  if(is.null(dc.ms1.os$data)) return(NULL)
  dc.qc.int.boxplot2$data <- qcIntBoxplot(data = dc.ms1.os$data,
                                          sample.info = dc.sample.info.os$data)
})


output$dc.qc.int.boxplot2 <- renderPlotly({
  ggplotly(dc.qc.int.boxplot2$data,
           source = "dc.qc.int.boxplot2")
})


###QC correlation
output$dc.qc.cor.plot2 <- renderPlotly({
  if(!is.null(dc.ms1.os$data)){
    qcCorPlot(data = dc.ms1.os$data,
              sample.info = dc.sample.info.os$data)
  }
})


dc.qc.cor2 <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$dc.os.2.qa2,{
  if(!is.null(dc.ms1.os$data)){
    dc.qc.cor2$data <- qcCor(data = dc.ms1.os$data,
                             sample.info = dc.sample.info.os$data)
  }
})


output$dc.qc.cor2 <- DT::renderDataTable(
  DT::datatable(dc.qc.cor$data,
                class = 'cell-border stripe',
                editable = FALSE,
                # selection = "single",
                selection = list(mode = 'single', target = 'row'),
                # caption = 'Raw data containing MVs',
                escape = FALSE,
                # filter = "top",
                filter = "none",
                rownames = TRUE,
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
                        buttons = list(list(extend="csv",filename = "QC.sample.correlation"),
                                       list(extend='excel',filename = "QC.sample.correlation")),
                        text = 'Download'
                      ),
                      I('colvis')
                    ),
                  scrollX = TRUE
                )
  ),
  server = FALSE
)



##jump to result download from data quality assessment page after cleaning
observeEvent(input$dc.qa2.2.download, {
  updateTabsetPanel(session, "DCtab",
                    selected = "DCdownload")

})





















#-------------------------------------------------------------------------------
#####download result
###generate analysis report
#
#
# observe({
#   shinyjs::disable(id = "dc.report.download")
#   if (any(dir(user.path()) == "Analysis.Report.of.Data.Cleaning.html")) {
#     shinyjs::enable("dc.report.download")
#   }
# })



#
####generate analysis report
observeEvent(eventExpr = input$dc.generate.analysis.report, {
  now.path <- getwd()
  user.path <- file.path("user_data", user_input$username, input$DCprojectID, "data_cleaning")
  tempReport <- file.path(now.path, user.path, "DCreport.temp.Rmd")
  file.copy("data/markdown/DCreport.Rmd", tempReport, overwrite = TRUE)

  params <- list(  dc.ba.info = dc.ba.info$data,
                   dc.data.info = dc.data.info$data,
                   dc.data.profile.plot = dc.data.profile.plot$data,
                   dc.data.profile.plot2 = dc.data.profile.plot2$data,
                   dc.peak.mv.ratio.plot = dc.peak.mv.ratio.plot$data,
                   dc.sample.mv.ratio.plot = dc.sample.mv.ratio.plot$data,
                   dc.peak.zero.ratio.plot = dc.peak.zero.ratio.plot$data,
                   dc.peak.zero.ratio.plot2 = dc.peak.zero.ratio.plot2$data,
                   dc.sample.zero.ratio.plot = dc.sample.zero.ratio.plot$data,
                   dc.sample.zero.ratio.plot2 = dc.sample.zero.ratio.plot2$data,
                   dc.qa1.pca.score.plot.for.report = dc.qa1.pca.score.plot.for.report$data,
                   dc.qa2.pca.score.plot.for.report = dc.qa2.pca.score.plot.for.report$data,
                   dc.qc.int.boxplot = dc.qc.int.boxplot$data,
                   dc.qc.int.boxplot2 = dc.qc.int.boxplot2$data,
                   dc.peak.rsd.plot.for.report = dc.peak.rsd.plot.for.report2$data,
                   dc.peak.rsd.plot.for.report2 = dc.peak.rsd.plot.for.report2$data,
                   dc.mv.params = dc.mv.params$data,
                   dc.zero.params = dc.zero.params$data,
                   dc.dn.params = dc.dn.params$data,
                   dc.di.params = dc.di.params$data,
                   dc.os.params = dc.os.params$data,
                   dc.rsd.comparation = dc.rsd.comparation(),
                   dc.di.rsd.comparation = dc.di.rsd.comparation(),
                   dc.os.pca.plot = dc.os.pca.plot(),
                   dc.os.zero.plot = dc.os.zero.plot()
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
                                                             "Analysis.Report.of.Data.Cleaning.html"),
                                     params = params,
                                     envir = new.env(parent = globalenv())
                   )
                 })
               })
})


####download analysis report
output$dc.report.download <- downloadHandler(
  filename = "Analysis.Report.of.Data.Cleaning.html",
  content = function(file){
    file.copy(file.path("user_data", user_input$username, input$DCprojectID, "data_cleaning", "Analysis.Report.of.Data.Cleaning.html"), file)
  }
)

# observe({
#   # shinyjs::hide(id = "dc.result.download")
#   useShinyjs()
#   shinyjs::disable(id = "dc.result.download")
#   if (any(dir(user.path()) == "Data_Cleaning_Result.zip")) {
#     # shinyjs::show("dc.result.download")
#     useShinyjs()
#     shinyjs::enable(id = "dc.result.download")
#   }
# })
#
####generate analysis result
observeEvent(eventExpr = input$dc.generate.analysis.result, {

  if(!is.null(dc.ms1.os$data)){
    #ZIP data
    withProgress(message = 'Generate analysis result...',
                 detail = 'This may take a while',
                 # style= "old",
                 value = 0, {
                   lapply(1:length(1), function(idx){
                     incProgress(1/length(1),
                                 detail = "This may take a while")

                     now.path <- getwd()
                     temp.path <- file.path(now.path, "user_data", user_input$username, input$DCprojectID, "data_cleaning","Data_Cleaning_Result")
                     dir.create(temp.path)
                     # load(file.path(user.path(), "dc.ms1.os"))
                     readr::write_csv(dc.ms1.os$data,
                                      file.path(temp.path, "MS1.peak.table.after.data.cleaning.csv"))

                     readr::write_csv(dc.sample.info.os$data,
                                      file.path(temp.path, "Sample.information.after.data.cleaning.csv"))

                     setwd(file.path("user_data", user_input$username, input$DCprojectID, "data_cleaning"))
                     temp.error <- try(zip::zip(zipfile = file.path(now.path, "user_data", user_input$username, input$DCprojectID, "data_cleaning",
                                                                    "Data_Cleaning_Result.zip"),
                                                files = "Data_Cleaning_Result",
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
output$dc.result.download <- downloadHandler(
  filename = "Data_Cleaning_Result.zip",
  content = function(file){
    file.copy(file.path("user_data", user_input$username,
                        input$DCprojectID,
                        "data_cleaning",
                        "Data_Cleaning_Result.zip"), file)
  }
)




data1 <- readr::read_csv("MS1.peak.table.after.zero.processing.csv")
data2 <- readr::read_csv("Data.after.normalization.csv")

