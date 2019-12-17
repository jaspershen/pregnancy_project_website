##data cleaning

temp.file <- dir(file.path("D:/study/R/my git/metCube/version1.0.2/data/demo_data/Data_Cleaning"),
                 full.names = TRUE)
# temp.file <- "F:/data pre-processing/demo data for metCube/GCMS/MTBLS321/GC_demo_data.csv"
# temp.file <- dir(file.path("F:/data pre-processing/tujia"),
#                  full.names = TRUE)
temp.file <- grep("batch", temp.file, value = TRUE)

dc.ms1.raw <- lapply(temp.file, function(x){
  as.data.frame(readr::read_csv(x))
})

# dc.sample.info.raw <- read.csv("F:/data pre-processing/demo data for metCube/GCMS/MTBLS321/GC_demo_sample_info.csv",
#                                stringsAsFactors = FALSE)

dc.sample.info.raw <- read.csv("D:/study/R/my git/metCube/version1.0.2/data/demo_data/Data_Cleaning/sample.information.csv",
                               stringsAsFactors = FALSE)
# dc.sample.info.raw <- read.csv("F:/data pre-processing/tujia/sample.info.demo-1.csv",
#                                stringsAsFactors = FALSE)

check.result <- checkData(peak.table = dc.ms1.raw,
                          sample.info = dc.sample.info.raw, step = "cleaning")

check.result


dc.data.info <- paste("There are", length(dc.ms1.raw), "batches.",
      "And there are",
      ifelse(any(dc.sample.info.raw$class == "QC"),
             "QC sample", "no QC samples."), "in your data.")

dc.data.info

dc.batch.alignment.result1 <- roughAlign(peak.table = dc.ms1.raw,
                            combine.mz.tol = 25,
                            combine.rt.tol = 30)


dc.ba.mz.plot <- baMZplot(dc.batch.alignment.result1)

ggplotly(dc.ba.mz.plot,
         tooltip = c("mz", "mz.error"),
         source = "dc.ba.mz.plot")



dc.ba.rt.plot <- baRTplot(dc.batch.alignment.result1)
ggplotly(dc.ba.rt.plot,
         tooltip = c("rt", "rt.error"),
         source = "dc.ba.rt.plot")


dc.ba.int.plot <- baINTplot(dc.batch.alignment.result1)
ggplotly(dc.ba.int.plot, tooltip = c(
  "int",
  "int.error"),
  source = "dc.ba.int.plot")



dc.batch.alignment.result2 <- accurateAlign(peak.table = dc.ms1.raw,
                               simple.data = dc.batch.alignment.result1)





dc.ba.info <- getBatchAlignmentInfo(dc.ms1.raw,
                                    dc.batch.alignment.result1,
                                    dc.batch.alignment.result2)


dc.ba.info

dc.data.profile.plot <- dataInformation(object = dc.batch.alignment.result2)

dc.data.profile.plot


dc.peak.mv.ratio.plot <- mvPeakPlot(data = dc.batch.alignment.result2)
ggplotly(dc.peak.mv.ratio.plot, tooltip = c(
  "Index",
  "MV.ratio","Peak"),
  source = "dc.peak.mv.ratio.plot")


dc.sample.mv.ratio.plot <- mvSamplePlot(data = dc.batch.alignment.result2)
ggplotly(dc.sample.mv.ratio.plot,
         tooltip = c(
           "Index",
           "MV.ratio","Sample"),
         source = "dc.sample.mv.ratio.plot")



dc.peak.zero.ratio.plot <-   zeroPeakPlot(data = dc.batch.alignment.result2,
                                          text = FALSE)

ggplotly(dc.peak.zero.ratio.plot,
         tooltip = c(
           "Index",
           "zero.ratio","Peak"),
         source = "dc.peak.zero.ratio.plot")


dc.sample.zero.ratio.plot <- zeroSamplePlot(data = dc.batch.alignment.result2,
                                              text = FALSE)

ggplotly(dc.sample.zero.ratio.plot,
         tooltip = c(
           "Index",
           "zero.ratio","Sample"),
         source = "dc.sample.zero.ratio.plot")





  dc.group.name <- unique(dc.sample.info.raw[,"group"])
  dc.group.name




  dc.peak.rsd.plot <- rsdDistribution(data = dc.batch.alignment.result2,
                                      sample.info = dc.sample.info.raw,
                                      group = c("0", "1"),
                                      rsd.tol = 30)
  ggplotly(dc.peak.rsd.plot)




dc.peak.rsd.plot.for.report <- rsdDistribution(data = dc.batch.alignment.result2,
                                               sample.info = dc.sample.info.raw,
                                               group = "0",
                                               rsd.tol = 30)

ggplotly(dc.peak.rsd.plot.for.report)





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
      mv.ratio.sample <- apply(dc.sample, 2, function(x) sum(is.na(x))/ncol(dc.sample))

      idx1 <- which(mv.ratio.peak > 0.5)
      idx2 <- which(mv.ratio.sample > 0.5)
      if(length(idx1) > 0){
        dc.sample <- dc.sample[-idx1,]
        dc.tags <- dc.tags[-idx1,]
      }
      if(length(idx2) > 0){
        dc.sample <- dc.sample[,-idx2]
        dc.sample.info <- dc.sample.info[-idx2,]
      }

      temp <- impute::impute.knn(data = as.matrix(dc.sample), k = 10)
      dc.sample <- as.data.frame(temp$data)
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

  dc.qa1.pca.object <- pca.object
  rm(list = c("dc.peak.table", "dc.sample", "dc.tags", "pca.object"))
  rm(list = c("temp1"))



  temp <- try({
    pcaScorePlot(pca.object = dc.qa1.pca.object,
                 batch.info = dc.sample.info.raw[,c(1,4)],
                 class.info = dc.sample.info.raw[,c(1,3)])
  })


  ggplotly(dc.qa1.pca.score.plot)





  dc.qa1.pca.score.plot.for.report <- pcaScorePlot(pca.object = dc.qa1.pca.object,
                                                   batch.info = batch.info,
                                                   class.info = class.info)

  ggplotly(dc.qa1.pca.score.plot.for.report)

  dc.qc.int.boxplot <- qcIntBoxplot(data = dc.batch.alignment.result2,
               sample.info = dc.sample.info.raw)
  ggplotly(dc.qc.int.boxplot,
           source = "dc.qc.int.boxplot")


  temp <- qcCorPlot(data = dc.batch.alignment.result2,
            sample.info = dc.sample.info.raw)

  dc.qc.cor <- qcCor(data = dc.batch.alignment.result2,
                     sample.info = dc.sample.info.raw)





dc.qa1.pca.object <- pcaAnalysis(data = dc.batch.alignment.result2,
                                 log.method = "log10",
                                 scale.method = "auto",
                                 center = TRUE)
dc.sample <-
  dc.batch.alignment.result2[,match(dc.sample.info.raw[,"sample.name"], colnames(dc.batch.alignment.result2))]
dc.tags <-
  dc.batch.alignment.result2[,-match(dc.sample.info.raw[,"sample.name"], colnames(dc.batch.alignment.result2))]

dc.ms1.after.mv <- mvImputation(sample = dc.sample,
                                     tags = dc.tags,
                                     sample.info = dc.sample.info.raw,
                                     mv.peak.remove.tol = 50,
                                     method = "PPCA",
                                     rowmax = 0.5,
                                     colmax = 0.5,
                                k = 10, bpca.npcs = 2, ppca.npcs = 2)



dc.sample <-
  dc.ms1.after.mv[,match(dc.sample.info.raw[,"sample.name"], colnames(dc.ms1.after.mv))]
dc.tags <-
  dc.ms1.after.mv[,-match(dc.sample.info.raw[,"sample.name"], colnames(dc.ms1.after.mv))]

dc.ms1.after.zero <- zeroFilter(sample = dc.sample,
                                     tags = dc.tags,
                                     sample.info = dc.sample.info.raw,
                                     zero.peak.remove.tol = 50)

dc.peak.table <- dc.ms1.after.zero
dc.sample.info <- dc.sample.info.raw

dc.sample <- dc.peak.table[,match(dc.sample.info[,"sample.name"], colnames(dc.peak.table)), drop = FALSE]
dc.tags <- dc.peak.table[,-match(dc.sample.info[,"sample.name"], colnames(dc.peak.table)), drop = FALSE]

dc.ms1.dn <- dataNormalization(sample = dc.sample,
                                    tags = dc.tags,
                                    sample.info = dc.sample.info,
                                    method = "svr", multiple = 5, dimension2 = TRUE)


dc.boxplot.dn1 <- dnSampleBoxplot(sample = dc.ms1.after.zero,
                                  sample.info = dc.sample.info.raw,
                                  # group = input$dc.dn.group1,
                                  group = "QC",
                                  title = "Befor normalization")


ggplotly(dc.boxplot.dn1)



dc.boxplot.dn2 <- dnSampleBoxplot(sample = dc.ms1.dn,
                                  sample.info = dc.sample.info.raw,
                                  # group = input$dc.dn.group1,
                                  group = "QC",
                                  title = "After normalization")

dc.boxplot.dn2





tags <- dc.ms1.dn[,-match(dc.sample.info.raw[,"sample.name"], colnames(dc.ms1.dn)), drop = FALSE]

dc.rsd.comparation <- rsdBeforeAfter(sample1 = dc.ms1.after.zero,
                                     sample2 = dc.ms1.dn,
                                     tags = tags,
                                     sample.info = dc.sample.info.raw,
                                     alpha = 0.5, size = 2,
                                     group = "QC",
                                     title = "RSD comparation")
dc.rsd.comparation


tags <- dc.ms1.after.zero[,-match(dc.sample.info.raw[,"sample.name"],
                           colnames(dc.ms1.after.zero)), drop = FALSE]

dc.rsd.distribution1 <- rsdDistribution2(sample = dc.ms1.after.zero,
                                         rsd.tol = 30,
                                         tags = tags,
                                         sample.info = dc.sample.info.raw,
                                         alpha = 0.5, size = 2,
                                         group = "QC",
                                         title = "Before normalization")


dc.rsd.distribution2 <- rsdDistribution2(sample = dc.ms1.dn,
                                         rsd.tol = 30,
                                         tags = tags,
                                         sample.info = dc.sample.info.raw,
                                         alpha = 0.5, size = 2,
                                         group = group,
                                         title = "After normalization")

dc.rsd.distribution2


dc.peak.table <- dc.ms1.dn
dc.sample.info <- dc.sample.info.raw
dc.sample <- dc.peak.table[,match(dc.sample.info.raw[,"sample.name"], colnames(dc.peak.table)), drop = FALSE]
dc.tags <- dc.peak.table[,-match(dc.sample.info.raw[,"sample.name"], colnames(dc.peak.table)), drop = FALSE]


dc.ms1.di <- dataIntegration(sample = dc.sample,
                                  sample.info = dc.sample.info,
                                  method = "qc.mean")



dc.ms1.di <- data.frame(dc.tags, dc.ms1.di,
                             stringsAsFactors = FALSE)



dc.boxplot.di1 <- diSampleBoxplot(sample = dc.ms1.dn,
                                  sample.info = dc.sample.info.raw,
                                  # group = input$dc.di.group1,
                                  group = "QC",
                                  title = "Befor integration")


dc.boxplot.di2 <- diSampleBoxplot(sample = dc.ms1.di,
                                  sample.info = dc.sample.info.raw,
                                  # group = input$dc.di.group1,
                                  group = group,
                                  title = "After integration")



tags <- dc.ms1.di[,-match(dc.sample.info.raw[,"sample.name"], colnames(dc.ms1.di)), drop = FALSE]

dc.di.rsd.comparation <- rsdBeforeAfter(sample1 = dc.ms1.dn,
                                        sample2 = dc.ms1.di,
                                        tags = tags,
                                        sample.info = dc.sample.info.raw,
                                        alpha = 0.5, size = 2,
                                        group = "QC",
                                        title = "RSD comparation")
dc.di.rsd.comparation



tags <- dc.ms1.dn[,-match(dc.sample.info.raw[,"sample.name"], colnames(dc.ms1.dn)), drop = FALSE]

dc.di.rsd.distribution1 <- rsdDistribution2(sample = dc.ms1.dn,
                                            rsd.tol = 30,
                                            tags = tags,
                                            sample.info = dc.sample.info.raw,
                                            alpha = 0.5, size = 2,
                                            group = "QC",
                                            title = "Before integration")

tags <- dc.ms1.di$data[,-match(dc.sample.info.raw$data[,"sample.name"], colnames(dc.ms1.di$data)), drop = FALSE]

dc.di.rsd.distribution2 <- rsdDistribution2(sample = dc.ms1.di,
                                            rsd.tol = 30,
                                            tags = tags,
                                            sample.info = dc.sample.info.raw,
                                            alpha = 0.5, size = 2,
                                            group = "QC",
                                            title = "After integration")

dc.di.rsd.distribution2





dc.peak.table <- dc.ms1.di
dc.sample.info <- dc.sample.info.raw

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

dc.os.pca <- prcomp(data.frame(t(dc.sample)),
                    retx = TRUE,
                    center = FALSE,
                    scale = FALSE)


dc.os.outlier.index1 <- pcaFindOutlier(dc.os.pca)


dc.os.pca.plot <- pcaOutlierPlot(pca.object = dc.os.pca,
               outlier.idx = dc.os.outlier.index1,
               alpha = 0.8, size = 2, text = "",
               title = "PCA score plot")


dc.os.pca.plot




dc.peak.table <- dc.ms1.di
dc.sample.info <- dc.sample.info.raw

dc.sample <- dc.peak.table[,match(dc.sample.info[,"sample.name"],
                                  colnames(dc.peak.table)), drop = FALSE]
dc.tags <- dc.peak.table[,-match(dc.sample.info[,"sample.name"],
                                 colnames(dc.peak.table)), drop = FALSE]
dc.os.sample.zero.ratio <- apply(dc.sample, 2, function(x){
  sum(x == 0)*100/nrow(dc.sample)
})

dc.os.outlier.index2 <- which(dc.os.sample.zero.ratio > 50)


dc.os.zero.plot <- DCosZeroRatioPlot(object = dc.os.sample.zero.ratio,
                                       outlier.idx = dc.os.outlier.index2,
                                       zero.ratio.tol = 50,
                                       sample.info = dc.sample.info.raw,
                                       alpha = 0.8, size = 2, title = "Zero value ratio")


dc.os.zero.plot


dc.os.outlier.index <- union(x = dc.os.outlier.index1, y = dc.os.outlier.index2)



sample.name <- dc.sample.info.raw[,"sample.name"]
note1 <- note2 <- rep("No", length(sample.name))
if(length(dc.os.outlier.index1) > 0) note1[dc.os.outlier.index1] <- "Yes"
if(length(dc.os.outlier.index2) > 0) note2[dc.os.outlier.index2] <- "Yes"
temp <- data.frame(sample.name, note1, note2, stringsAsFactors = FALSE)
colnames(temp) <- c("Sample name", "PCA score plot", "Zero ratio > tolerance")
temp <- temp[dc.os.outlier.index,]



dc.ms1.os <- dc.ms1.di
dc.sample.info.os <- dc.sample.info.raw


p <- qcCorPlot(data = dc.batch.alignment.result2,
          sample.info = dc.sample.info.raw)




##metabolite identification
mi.ms1.raw <- readr::read_csv("D:/study/R/my git/metCube/version1.0.2/data/demo_data/Metabolite_Identification/data.csv")
mi.ms1.raw <- as.data.frame(mi.ms1.raw)

temp.file <- dir("D:/study/R/my git/metCube/version1.0.2/data/demo_data/Metabolite_Identification", full.names = TRUE)
temp.file <- temp.file[grep("mgf", temp.file)]
mi.ms2.raw <- lapply(1:length(temp.file), function(idx){
    readMGF(file = temp.file[idx])
})






    temp.ms2.data <- do.call(c, mi.ms2.raw)
    names(temp.ms2.data) <- paste("peak", 1:length(temp.ms2.data), sep = "")
    mi.ms2.data <- combineMS1MS2(ms1.peak.table = mi.ms1.raw,
                                 ms2.data = temp.ms2.data,
                                 mz.tol = 25,
                                 rt.tol = 10,
                                 ms2.type ="mgf")







    mi.ms2.tags <- mi.ms1.raw[,c(1:3)]
    ms2.name <- unname(unlist(lapply(mi.ms2.data, function(x) x[[1]][1,])))



    mi.ms2.tags <- mi.ms1.raw[,c(1:3)]
    colnames(mi.ms2.tags)[2] <- "mzmed"
    ms2.name <- unname(unlist(lapply(mi.ms2.data, function(x) x[[1]][1,])))
    ms1.name <- mi.ms2.tags$name
    temp.idx <- match(ms2.name , ms1.name)
    spec <- vector(mode = "list", length = length(ms1.name))
    names(spec) <- ms1.name
    spec[temp.idx] <- mi.ms2.data
    spec <- lapply(spec, function(x){
      if(is.null(x)) return(x)
      x[[2]]
    })


    load("data/database/zhuMetlib.rda", envir = environment())
    library = zhuMetlib



    # load("mi.ms2.tags")
    # load("spec")
    # load("library")
    # load("pol")
    # load("ce")
    # load("adduct.table")

    load("data/database/hilic.pos.rda", envir = environment())
    adduct.table <- hilic.pos
    rm(list = c("hilic.pos"))


    mi.ms2.match.result <- lapply(1:1, function(idx){
      MetID(info = mi.ms2.tags,
            spec = spec,
            dp.cutoff = 0.8,
            lib = library,
            pol = "pos",
            ce = "30",
            adduct = adduct.table,
            d.out = ".",
            lc = 'HILIC',
            ms2.match.plot = FALSE)
    })[[1]]



    colnames(mi.ms2.match.result)[1] <- "name"
    colnames(mi.ms2.match.result)[2] <- "mz"
    colnames(mi.ms2.tags)[2] <- "mz"
    load("data/database/inHouse.compound.rda", envir = environment())
    mi.ms2.match.result$hits.reverse[mi.ms2.match.result$hits.reverse == ""] <- NA
    mi.ms2.match.result$hits.forward[mi.ms2.match.result$hits.forward == ""] <- NA
    mi.ms2.match.result <- readAnnotation(data = mi.ms2.match.result,
                                          rt.filter = FALSE,
                                          inHouse.compound = inHouse.compound)

    colnames(mi.ms2.match.result)[1] <- "name"

    mi.ms2.match.result <- mi.ms2.match.result[,c(colnames(mi.ms2.tags),
                                                  c("Metabolite.name","KEGG.ID", "adduct", "mz.error", "ms2.sim", "labid"))]
    colnames(mi.ms2.match.result)[which(colnames(mi.ms2.match.result) == "Metabolite.name")] <- "Compound.name"
    colnames(mi.ms2.match.result)[which(colnames(mi.ms2.match.result) == "adduct")] <- "Adduct"
    colnames(mi.ms2.match.result)[which(colnames(mi.ms2.match.result) == "KEGG.ID")] <- "ID"
    colnames(mi.ms2.match.result)[which(colnames(mi.ms2.match.result) == "ms2.sim")] <- "Dot product"
    colnames(mi.ms2.match.result)[which(colnames(mi.ms2.match.result) == "labid")] <- "Lab.ID"








####one data cleaning
param.table <- xlsx::read.xlsx("D:/study/R/my git/metCube/version1.0.2/data/demo_data/parameter_table.xlsx",
                               sheetIndex = 1, as.data.frame = TRUE)
    oneDCparameter <- as.character(param.table[,2])
    oneDCparameter <- oneDCparameter[!is.na(oneDCparameter)]

    oneDCparameter <- sapply(oneDCparameter, list)
    oneDCparameter <- sapply(oneDCparameter, list)
    oneDCparameter[[2]] <- as.numeric(oneDCparameter[[2]])
    oneDCparameter[[3]] <- as.numeric(oneDCparameter[[3]])
    oneDCparameter[[4]] <- as.numeric(oneDCparameter[[4]])
    oneDCparameter[[5]] <- paraTrans(ui.para = oneDCparameter[[5]])
    oneDCparameter[[6]] <- as.numeric(oneDCparameter[[6]])
    oneDCparameter[[7]] <- as.numeric(oneDCparameter[[7]])
    oneDCparameter[[8]] <- as.numeric(oneDCparameter[[8]])
    oneDCparameter[[9]] <- as.numeric(oneDCparameter[[9]])
    oneDCparameter[[10]] <- as.logical(oneDCparameter[[10]])
    oneDCparameter[[11]] <- as.numeric(oneDCparameter[[11]])
    oneDCparameter[[12]] <- as.numeric(oneDCparameter[[12]])
    oneDCparameter[[13]] <- as.character(oneDCparameter[[13]])
    oneDCparameter[[14]] <- paraTrans(oneDCparameter[[14]])
    oneDCparameter[[15]] <- as.logical(oneDCparameter[[15]])
    oneDCparameter[[16]] <- as.logical(oneDCparameter[[16]])
    oneDCparameter[[17]] <- as.numeric(strsplit(oneDCparameter[[17]], split = ",")[[1]])
    # oneDCparameter[[17]] <- as.character(oneDCparameter[[17]])
    oneDCparameter[[18]] <- as.numeric(oneDCparameter[[18]])
    oneDCparameter[[19]] <- as.numeric(oneDCparameter[[19]])
    # oneDCparameter[[20]] <- as.logical(oneDCparameter[[20]])
    oneDCparameter[[21]] <- paraTrans(oneDCparameter[[21]])
    oneDCparameter[[22]] <- paraTrans(oneDCparameter[[22]])
    oneDCparameter[[23]] <- paraTrans(oneDCparameter[[23]])
    oneDCparameter[[24]] <- as.logical(oneDCparameter[[24]])
    oneDCparameter[[25]] <- as.numeric(oneDCparameter[[25]])
    oneDCparameter[[26]] <- as.numeric(oneDCparameter[[26]])
    oneDCparameter[[27]] <- as.logical(oneDCparameter[[27]])
    oneDCparameter[[28]] <- as.logical(oneDCparameter[[28]])




   # load("temp.file")
   # load("temp")
   # oneDCparameter <- temp
 temp.file <- dir("D:/study/R/my git/metCube/version1.0.2/user_data/shen/test/one_data_cleaning")
 save(oneDCparameter, file = "oneDCparameter")
 save(temp.file, file = "temp.file")
   metcleaning.info <- try({MetCleaning::MetCleaning(
      data = grep("batch", x = temp.file, value = TRUE),
      sample.information = "sample.info.csv",
      polarity = oneDCparameter[[1]],
      hasIS = "no",
      hasQC = "yes",
      #batch alignment
      combine.mz.tol = oneDCparameter[[2]],
      combine.rt.tol = oneDCparameter[[3]],
      #MVFilter para
      mv.filter = TRUE,
      obs.mv.cutoff = 0,
      var.mv.cutoff = 1 - oneDCparameter[[3]]/100,
      #MVimputation
      imputation.method = oneDCparameter[[5]],
      k = oneDCparameter[[6]],
      rowmax = oneDCparameter[[7]],
      colmax = oneDCparameter[[8]],
      maxp = 1500,
      #ZeroFilter para
      zero.filter = TRUE,
      obs.zero.cutoff = 0,
      var.zero.cutoff = 1 - oneDCparameter[[12]]/100,
      #DataNormalization
      normalization = TRUE,
      method = oneDCparameter[[14]],
      multiple = oneDCparameter[[19]],
      threads = 2,
      #PeakIdentification
      hmdb.matching = FALSE,
      show = 5,
      mass.tolerance = 30,
      mz.tolerance = 30,
      rt.tolerance = 180,
      #DataOverview para
      met.plot = FALSE,
      path = "D:/study/R/my git/metCube/version1.0.2/user_data/shen/test/one_data_cleaning",
      # worklist.from = "manual",
      #other slection
      qc.outlier.filter = TRUE,
      subject.outlier.filter = TRUE,
      integration = TRUE)}, silent = TRUE)



   temp.path <- file.path(one.user.path,"Data_Cleaning_Result")

   dir.create(temp.path)


   temp.result <- c("1 MV overview", "2 MV filter", "3 Zero overview",
                    "4 Zero filter", "5 QC outlier filter", "6 Subject outlier finder",
                    "7 Normalization result", "8 Batch effect",
                    "10 RSD overview", "data_after_pre.csv")

   ##copy data



                  lapply(1:length(temp.result), function(idx){
                    file.copy(from = file.path(one.user.path, temp.result[idx]),
                              to = temp.path, overwrite = TRUE, recursive = TRUE)
                  })


                  temp.error <- try(zip::zip(zipfile = file.path(one.user.path,
                                                                 "Data_Cleaning_Result.zip"),
                                             files = file.path(one.user.path,"Data_Cleaning_Result"),
                                             recurse = TRUE
                  ))








                                 lapply(1:length(temp.result), function(idx){
                                   unlink(file.path(one.user.path, temp.result[idx]), recursive = TRUE)
                                 })


                  unlink(file.path(one.user.path, "Data_Cleaning_Result"), recursive = TRUE)
                  unlink(file.path(one.user.path, "intermediate"), recursive = TRUE)

























#---------------------------------------------------------
#differential analysis
# da.ms1.raw <- readr::read_csv("F:/data pre-processing/tujia/batch1.demo.csv")
da.ms1.raw <- lapply(1:1, function(x)
  as.data.frame(readr::read_csv("D:/study/R/my git/metCube/version1.0.2/data/demo_data/Differential_Analysis/data.dis.csv")))



# da.ms1.raw <- as.data.frame(da.ms1.raw)

# da.sample.info.raw <- readr::read_csv("F:/data pre-processing/tujia/sample.info.demo-1.csv")
da.sample.info.raw <- readr::read_csv("D:/study/R/my git/metCube/version1.0.2/data/demo_data/Differential_Analysis/sample.info.dis.csv")

da.sample.info.raw <- as.data.frame(da.sample.info.raw)


check.result <- checkData(peak.table = da.ms1.raw,
                          sample.info = da.sample.info.raw,
                          step = "biomarker")

check.result
# temp.idx <- match(colnames(data)[-c(1:3)], sample.info$sample.name)
#
# sample.info <- sample.info[temp.idx,]
#
# write.csv(sample.info, "sample.info.csv", row.names = FALSE)

da.ua.params <- DAuaParam( da.ua.control = "Control",
                                da.ua.case = "Case",
                                daUAlog = "no",
                                daUAscale = "no",
                                daUAcenter = FALSE,
                                daUAfcWhich = "mean",
                                daUAhypothesisTesting = "t",
                                daUAalternative = "two.sided",
                                daUApaired = FALSE,
                                daUAadjust = "fdr",
                                daUApCutoff = 0.05,
                                daUAfcCutoff = 2)

da.ua.params

sample.info <- da.sample.info.raw
data <- da.ms1.raw[[1]]
sample <- data[,match(sample.info[,1], colnames(data))]
tags <- data[,-match(sample.info[,1], colnames(data))]
sample <- sxtLog(sample = sample,  method = "no")
sample <- sxtScale(sample = sample, method = "no", center = FALSE)

control.group <- "Control"
case.group <- "Case"
control.group.name <- sample.info[,"sample.name"][which(sample.info[,"group"]==control.group)]
case.group.name <- sample.info[,"sample.name"][which(sample.info[,"group"]==case.group)]

control.group.idx <- match(control.group.name, colnames(sample))
case.group.idx <- match(case.group.name, colnames(sample))

daUAfc <- apply(sample, 1, function(x){
  mean(as.numeric(x[case.group.idx]), na.rm = TRUE)/mean(as.numeric(x[control.group.idx]), na.rm = TRUE)
})


daUAfc[is.na(daUAfc)] <- 1
daUAfc[is.nan(daUAfc)] <- 1
daUAfc[is.infinite(daUAfc)] <- max(daUAfc, na.rm = TRUE)
names(daUAfc) <- tags[,"name"]



daUAp <- apply(sample, 1, function(x){
  control <- as.numeric(x[control.group.idx])
  case <- as.numeric(x[case.group.idx])
  if(all(is.na(control))) control[is.na(control)] <- 1
  if(all(is.na(case))) case[is.na(case)] <- 1
  control[is.na(control)] <- mean(control, na.rm = TRUE)
  case[is.na(case)] <- mean(case, na.rm = TRUE)
  temp <- try({t.test(x = control,
                 y = case,
                 alternative = "two.sided",
                 paired = FALSE)}, silent = TRUE)
  if(class(temp) == "try-error"){
    1
  }else{
    temp$p.value
  }
})


names(daUAp) <- tags[,"name"]


daUAp <- p.adjust(daUAp, method = "fdr")


da.ms1.p.fc <- data.frame(names(daUAfc), daUAfc, daUAp, stringsAsFactors = FALSE)


daUApCutoff <- 0.05
daUAfcCutoff <- 2
temp.idx1 <- which(daUAp < daUApCutoff & daUAfc > daUAfcCutoff)
temp.idx2 <- which(daUAp < daUApCutoff & daUAfc < 1/daUAfcCutoff)
temp.idx <- sort(c(temp.idx1, temp.idx2))
sample.info <- da.sample.info.raw
data <- da.ms1.raw
sample <- data[,match(sample.info[,1], colnames(data))]
tags <- data[,-match(sample.info[,1], colnames(data))]
tags <- data.frame(tags, "p.value" = daUAp, "FC" = daUAfc, stringsAsFactors = FALSE)
da.ua.marker <- data.frame(tags, sample, stringsAsFactors = FALSE)[temp.idx,]



da.volcano.plot <-   volcanoPlot(object = da.ms1.p.fc, control.group.name = 'control',
                                 case.group.name = "case",
                                 p.adjust.method = "fdr",
                                 p.cutoff = daUApCutoff, fc.cutoff = daUAfcCutoff)

da.volcano.plot









da.ma.params <- DAmaParam(
  da.ma.control = "Control",
  da.ma.case = "Case",
  daMAlog = "log10",
  daMAscale = "auto",
  daMAcenter = TRUE,
  daMAvipCutoff = 1,
  daMAncomp = 3,
  daMAhcaClusteringDistanceRows = "euclidean",
  daMAhcaClusteringDistanceCols = "euclidean",
  daMAhcaClusteringMethod = "ward.D")

da.ma.params

data <- da.ms1.raw[[1]]
sample.info <- da.sample.info.raw
control.group <- "Control"
case.group <- "Case"
sample.info <- sample.info[sample.info$group %in% c(control.group, case.group),]
sample <- data[,match(sample.info$sample.name, colnames(data))]


sample <- sxtLog(sample = sample, method = "log10")
##scale
sample <- sxtScale(sample = sample, method = "auto", center = TRUE)


daMApca <- prcomp(data.frame(t(sample)),
                       retx = TRUE,
                       center = FALSE,
                       scale = FALSE)



sample.info <- da.sample.info.raw
control.group <- "Conteol"
case.group <- "Case"
sample.info <- sample.info[sample.info$group %in% c(control.group, case.group),]
daMApcaPlot <- pcaScorePlot2(pca.object = daMApca, sample.info = sample.info)

daMApcaPlot






##PLS analysis
data <- da.ms1.raw[[1]]
sample.info <- da.sample.info.raw
control.group <- "Control"
case.group <- "Case"
sample.info <- sample.info[sample.info$group %in% c(control.group, case.group),]
sample <- data[,match(sample.info$sample.name, colnames(data))]

##log
sample <- sxtLog(sample = sample, method = 'log10')
##scale
sample <- sxtScale(sample = sample, method = 'auto', center = TRUE)


pls.Y <- as.numeric(as.factor(sample.info[,"group"]))

ncompa <- min(nrow(sample), ncol(sample))
if(ncompa > 30) ncompa <- 30


pls.object <- plsdepot::plsreg1(t(sample), pls.Y, comps = ncompa)
daMApls <- pls.object




plsQ2barplot(daMApls, comps.number = 10)



data <- da.ms1.raw[[1]]
sample.info <- da.sample.info.raw
control.group <- "Control"
case.group <- "Case"
sample.info <- sample.info[sample.info$group %in% c(control.group, case.group),]
sample <- data[,match(sample.info$sample.name, colnames(data))]

##log
sample <- sxtLog(sample = sample, method = 'log10')
##scale
sample <- sxtScale(sample = sample, method = 'auto', center = TRUE)


pls.Y <- as.numeric(as.factor(sample.info[,"group"]))

ncompa <- 3
if(ncompa > min(c(nrow(sample), ncol(sample)))) ncompa <- min(c(nrow(sample), ncol(sample)))
# pls.object <- plsdepot::plsreg1(t(sample), pls.Y, comps = ncompa)

dummy <- SXTdummy(pls.Y)
int.dummy <- dummy
pls.object1 <- plsdepot::plsreg1(t(sample), pls.Y, comps = ncompa)
pls.object2 <- plsdepot::plsreg2(t(sample), int.dummy, comps = ncompa)

daMApls2 <- vector(mode = "list", length = 2)
names(daMApls2) <- c("data1", "data2")

daMApls2$data1 <- pls.object1
daMApls2$data2 <- pls.object2



plotly::ggplotly(plsQ2R2barplot(pls.object = daMApls2$data1),
         # tooltip = c("Sample", "Injection.order", "Batch"),
         source = "daMAplsQ2R2Plot")

daMAplsPlot <- plsScorePlot(pls.object = daMApls2$data1, sample.info = sample.info)
daMAplsPlot


vip <- apply(daMApls2$data2$VIP, 1, mean)
temp.idx1 <- which(daUAp < daUApCutoff & daUAfc > daUAfcCutoff)
temp.idx2 <- which(daUAp < daUApCutoff & daUAfc < 1/daUAfcCutoff)
temp.idx3 <- which(vip > daMAvipCutoff)
temp.idx <- sort(unique(c(temp.idx1, temp.idx2)))
temp.idx <- unique(intersect(temp.idx, temp.idx3))
sample.info <- da.sample.info.raw
data <- da.ms1.raw[[1]]
sample <- data[,match(sample.info[,1], colnames(data))]
tags <- data[,-match(sample.info[,1], colnames(data))]
tags <- data.frame(tags, "p.value" = daUAp,
                   "FC" = daUAfc,
                   "VIP" = vip,
                   stringsAsFactors = FALSE)
da.ma.marker <- data.frame(tags, sample, stringsAsFactors = FALSE)[temp.idx,]


data <- da.ms1.raw[[1]]
sample.info <- da.sample.info.raw
control.group <- "Control"
case.group <- "Case"
sample.info <- sample.info[sample.info$group %in% c(control.group, case.group),]
sample <- data[,match(sample.info$sample.name, colnames(data))]

##log
sample <- sxtLog(sample = sample, method = "log10")
##scale
sample <- sxtScale(sample = sample, method = "auto", center = TRUE)

group <- c(control.group, case.group)

daMAhca <- heatMap(sample = sample,
        sample.info = sample.info,
        group = group
        # int.col = c(input$daMAhcaLowCol, input$daMAhcaMiddleCol, input$daMAhcaHighCol),
        # color = c(input$daMAhcaGroup1Col, input$daMAhcaGroup2Col),
        # show_rownames = input$daMAhcaShowRowNames,
        # show_colnames = input$daMAhcaShowColNames,
        # cluster_rows = input$daMAhcaClusterRows,
        # cluster_cols = input$daMAhcaClusterCols,
        # clustering_distance_rows = input$daMAhcaClusteringDistanceRows,
        # clustering_distance_cols = input$daMAhcaClusteringDistanceCols,
        # clustering_method = input$daMAhcaClusteringMethod,
)


daMAhca


da.ms1.validation <- lapply(1:1, function(x)
  as.data.frame(readr::read_csv("D:/study/R/my git/metCube/version1.0.2/data/demo_data/Differential_Analysis/data.val.csv")))


da.sample.info.validation <- readr::read_csv("D:/study/R/my git/metCube/version1.0.2/data/demo_data/Differential_Analysis/sample.info.val.csv")
da.sample.info.validation <- as.data.frame(da.sample.info.validation)


check.result <- checkData(peak.table = da.ms1.validation,
                          sample.info = da.sample.info.validation,
                          step = "biomarker")

check.result


sample.info <- da.sample.info.raw
control.group <- "Control"
case.group <- "Case"
sample.info <- sample.info[sample.info$group %in% c(control.group, case.group),]
marker <- da.ma.marker

data1 <- da.ms1.raw[[1]]
sample1 <- data1[,match(sample.info$sample.name, colnames(data1))]
rownames(sample1) <- data1$name
sample1 <- sample1[which(rownames(sample1) %in% marker$name),]


##log
sample1 <- sxtLog(sample = sample1, method = "log10")
##scale
sample1 <- sxtScale(sample = sample1, method = "auto", center = TRUE)

daValidationPCA1 <- prcomp(data.frame(t(sample1)),
                                retx = TRUE,
                                center = FALSE,
                                scale = FALSE)



daValidationPCAplot1 <- pcaScorePlot2(pca.object = daValidationPCA1, sample.info = sample.info)
daValidationPCAplot1

sample.info <- da.sample.info.validation
control.group <- "Control"
case.group <- "Case"
sample.info <- sample.info[sample.info$group %in% c(control.group, case.group),]
marker <- da.ma.marker

data2 <- da.ms1.validation[[1]]
sample2 <- data2[,match(sample.info$sample.name, colnames(data2))]
rownames(sample2) <- data2$name
sample2 <- sample2[which(rownames(sample2) %in% marker$name),]

sample2 <- sxtLog(sample = sample2, method = "log10")
##scale
sample2 <- sxtScale(sample = sample2, method = "auto", center = TRUE)

daValidationPCA2 <- prcomp(data.frame(t(sample2)),
                                retx = TRUE,
                                center = FALSE,
                                scale = FALSE)

daValidationPCAplot2 <- pcaScorePlot2(pca.object = daValidationPCA2, sample.info = sample.info)
daValidationPCAplot2




sample.info <- da.sample.info.raw
control.group <- "Control"
case.group <- "Case"
sample.info <- sample.info[sample.info$group %in% c(control.group, case.group),]
marker <- da.ma.marker

data1 <- da.ms1.raw[[1]]
sample1 <- data1[,match(sample.info$sample.name, colnames(data1))]
rownames(sample1) <- data1$name
sample1 <- sample1[which(rownames(sample1) %in% marker$name),]


##log
sample1 <- sxtLog(sample = sample1, method = "log10")
##scale
sample1 <- sxtScale(sample = sample1, method = "auto",
                    center = TRUE)

pls.Y <- as.numeric(as.factor(sample.info[,"group"]))

pls.object <- plsdepot::plsreg1(t(sample1), pls.Y, comps = 3)
daValidationPLS1 <- pls.object


sample.info <- da.sample.info.validation
control.group <- "Control"
case.group <- "Case"
sample.info <- sample.info[sample.info$group %in% c(control.group, case.group),]
marker <- da.ma.marker

data2 <- da.ms1.validation[[1]]
sample2 <- data2[,match(sample.info$sample.name, colnames(data2))]
rownames(sample2) <- data2$name
sample2 <- sample2[which(rownames(sample2) %in% marker$name),]

sample2 <- sxtLog(sample = sample2, method = "log10")
##scale
sample2 <- sxtScale(sample = sample2, method = "auto", center = TRUE)

pls.Y <- as.numeric(as.factor(sample.info[,"group"]))

pls.object <- plsdepot::plsreg1(t(sample2), pls.Y, comps = 3)
daValidationPLS2 <- pls.object



sample.info <- da.sample.info.raw
# control.group <- input$da.ma.control
# case.group <- input$da.ma.case
sample.info <- sample.info[sample.info$group %in% c(control.group, case.group),]
daValidationPLSplot1 <- plsScorePlot(pls.object = daValidationPLS1, sample.info = sample.info)

daValidationPLSplot1


sample.info <- da.sample.info.validation
# control.group <- input$da.ma.control
# case.group <- input$da.ma.case
sample.info <- sample.info[sample.info$group %in% c(control.group, case.group),]
daValidationPLSplot2 <- plsScorePlot(pls.object = daValidationPLS2, sample.info = sample.info)
daValidationPLSplot2







sample.info <- da.sample.info.raw
# control.group <- input$da.ma.control
# case.group <- input$da.ma.case
sample.info <- sample.info[sample.info$group %in% c(control.group, case.group),]
marker <- da.ma.marker

data1 <- da.ms1.raw[[1]]
sample1 <- data1[,match(sample.info$sample.name, colnames(data1))]
rownames(sample1) <- data1$name
sample1 <- sample1[which(rownames(sample1) %in% marker$name),]


##log
sample1 <- sxtLog(sample = sample1, method = "log10")
##scale
sample1 <- sxtScale(sample = sample1, method = "auto",
                    center = TRUE)

group <- c(control.group, case.group)

daValidationHCA1 <- heatMap(sample = sample1,
                                 sample.info = sample.info,
                                 group = group,
                                 # int.col = c(input$daMAhcaLowCol, input$daMAhcaMiddleCol, input$daMAhcaHighCol),
                                 # color = c(input$daMAhcaGroup1Col, input$daMAhcaGroup2Col),
                                 # show_rownames = input$daMAhcaShowRowNames,
                                 # show_colnames = input$daMAhcaShowColNames,
                                 # cluster_rows = input$daMAhcaClusterRows,
                                 # cluster_cols = input$daMAhcaClusterCols,
                                 # clustering_distance_rows = input$daMAhcaClusteringDistanceRows,
                                 # clustering_distance_cols = input$daMAhcaClusteringDistanceCols,
                                 # clustering_method = input$daMAhcaClusteringMethod
)



daValidationHCA1



sample.info <- da.sample.info.validation
# control.group <- input$da.ma.control
# case.group <- input$da.ma.case
sample.info <- sample.info[sample.info$group %in% c(control.group, case.group),]
marker <- da.ma.marker

data2 <- da.ms1.validation[[1]]
sample2 <- data2[,match(sample.info$sample.name, colnames(data2))]
rownames(sample2) <- data2$name
sample2 <- sample2[which(rownames(sample2) %in% marker$name),]

sample2 <- sxtLog(sample = sample2, method = "log10")
##scale
sample2 <- sxtScale(sample = sample2, method = "auto", center = TRUE)

group <- c(control.group, case.group)

daValidationHCA2 <- heatMap(sample = sample2,
                                 sample.info = sample.info,
                                 group = group,
                                 # int.col = c(input$daMAhcaLowCol, input$daMAhcaMiddleCol, input$daMAhcaHighCol),
                                 # color = c(input$daMAhcaGroup1Col, input$daMAhcaGroup2Col),
                                 # show_rownames = input$daMAhcaShowRowNames,
                                 # show_colnames = input$daMAhcaShowColNames,
                                 # cluster_rows = input$daMAhcaClusterRows,
                                 # cluster_cols = input$daMAhcaClusterCols,
                                 # clustering_distance_rows = input$daMAhcaClusteringDistanceRows,
                                 # clustering_distance_cols = input$daMAhcaClusteringDistanceCols,
                                 # clustering_method = input$daMAhcaClusteringMethod
)


daValidationHCA2

##model construction
  sample.info <- da.sample.info.raw
  # control.group <- "0"
  # case.group <- "1"
  sample.info <- sample.info[sample.info$group %in% c(control.group, case.group),]
  marker <- da.ma.marker

  data1 <- da.ms1.raw[[1]]
  sample1 <- data1[,match(sample.info$sample.name, colnames(data1))]
  rownames(sample1) <- data1$name
  sample1 <- sample1[which(rownames(sample1) %in% marker$name),]

  ##log
  sample1 <- sxtLog(sample = sample1, method = "log10")
  ##scale
  sample1 <- sxtScale(sample = sample1, method = "auto",
                      center = TRUE)


  sample.info2 <- da.sample.info.validation
  sample.info2 <- sample.info2[sample.info2$group %in% c(control.group, case.group),]

  data2 <- da.ms1.validation[[1]]
  sample2 <- data2[,match(sample.info2$sample.name, colnames(data2))]
  rownames(sample2) <- data2$name
  sample2 <- sample2[which(rownames(sample2) %in% marker$name),]

  sample2 <- sxtLog(sample = sample2, method = "log10")
  ##scale
  sample2 <- sxtScale(sample = sample2, method = "auto", center = TRUE)
  Y2 <- as.numeric(as.factor(sample.info2[,"group"]))
  Y2 <- Y2 - 1




  ##prediction model
  Y <- as.numeric(as.factor(sample.info[,"group"]))
  Y <- Y - 1

  daValidationPredictionModel <- "pls"

  if(daValidationPredictionModel == "pls"){
    prediction.model <- pls::plsr(Y ~ t(sample1), scale = FALSE,
                                  validation = "CV", ncomp = 3, method = "oscorespls")
    prediction.Y <- predict(prediction.model, t(sample1), ncomp = 3)
    daValidationROC1 <- pROC::roc(Y, prediction.Y, ci = TRUE)

      prediction.Y2 <- predict(prediction.model, t(sample2), ncomp = 3)
      daValidationROC2 <- pROC::roc(Y2, prediction.Y2, ci = TRUE)

  }

  daValidationROClplot1 <- ROCplot(roc.object = daValidationROC1)
  daValidationROClplot1


  daValidationROClplot2 <- ROCplot(roc.object = daValidationROC2)
  daValidationROClplot2


  now.path <- getwd()
  user.path <- file.path("user_data", "test", "test", "differential_analysis")
  tempReport <- file.path(now.path, user.path, "DAreport.temp.Rmd")
  file.copy("data/markdown/DAreport.Rmd", tempReport, overwrite = TRUE)


  da.validation.params <- DAvalidationParam( daValidationPredictionModel = "pls")
  da.validation.params

  daMAhcaHeatmap <- daMAhca
  daValidationHCAheatmap1 <- daValidationHCA1
  daValidationHCAheatmap2 <- daValidationHCA2

  params <- list(##parameters
    da.ua.params = da.ua.params,
    da.ma.params = da.ma.params,
    da.validation.params = da.validation.params,
    da.vocano.plot = da.volcano.plot,
    daMApcaPlot = daMApcaPlot,
    daMAplsPlot = daMAplsPlot,
    daMAhcaHeatmap = daMAhcaHeatmap,
    daValidationPCAplot1 = daValidationPCAplot1,
    daValidationPCAplot2 = daValidationPCAplot2,
    daValidationPLSplot1 = daValidationPLSplot1,
    daValidationPLSplot2 = daValidationPLSplot2,
    daValidationHCAheatmap1 = daValidationHCAheatmap1,
    daValidationHCAheatmap2 = daValidationHCAheatmap2,
    daValidationROClplot1 = daValidationROClplot1,
    daValidationROClplot2 = daValidationROClplot2
  )



  lapply(1:length(tempReport), function(idx){
    rmarkdown::render(tempReport,
                      output_file = file.path(now.path, user.path,
                                              "Analysis.Report.of.Differential.Metabolite.Discovery.html"),
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  })
