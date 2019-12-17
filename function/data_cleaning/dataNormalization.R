dataNormalization <- function(sample,
                              tags,
                              sample.info,
                              method,
                              optimization,
                              begin,
                              end,
                              step,
                              dimension1,
                              dimension2,
                              multiple,
                              threads = 2){

  if(method == "no"){
    peak.table <- cbind(tags, sample)
    peak.table[which(is.na(peak.table), arr.ind = TRUE)] <- 0
    return(peak.table)
  }

  if(method == "mean"){
    sample <- apply(sample, 2, function(x){
      x/mean(x)
    })
    sample <-as.data.frame(sample)
  }

  if(method == "median"){
    sample <- apply(sample, 2, function(x){
      x/median(x)
    })
    sample <-as.data.frame(sample)
  }

  if(method == "total"){
    sample <- apply(sample, 2, function(x){
      x/sum(x)
    })
    sample <-as.data.frame(sample)
  }


  if(method == "loess" | method == "svr"){
    ##split batch
    return.result <- splitBatch(sample = sample, sample.info = sample.info)
    sample <- return.result[[1]]
    QC <- return.result[[2]]
    tags1 <- t(tags)

    if(method == "loess"){

      result <- pbapply::pblapply(1:length(sample), function(idx1){

        SXTloessNor(sample = t(sample[[idx1]]),
                    QC = t(QC[[idx1]]),
                    tags = tags1,
                    sample.order = sample.info$injection.order[match(colnames(sample[[idx1]]), sample.info$sample.name)],
                    QC.order = sample.info$injection.order[match(colnames(QC[[idx1]]), sample.info$sample.name)],
                    optimization = optimization,
                    begin = begin,
                    end = end,
                    step = step,
                    dimension1 = dimension1,
                    batch = idx1)
      })

      sample.nor <- lapply(result, function(x) x[[1]])
      QC.nor <- lapply(result, function(x) x[[2]])
      sample.nor <- t(do.call(rbind, sample.nor))
      QC.nor <- t(do.call(rbind, QC.nor))
      sample <- cbind(sample.nor, QC.nor)
      sample <- sample[,match(sample.info$sample.name, colnames(sample))]
    }else{
      shiny::withProgress(
        message = "SVR normalization...",
        value = 0,{
          result <- pbapply::pblapply(1:length(sample), function(idx2){
            shiny::incProgress(1/length(sample),
                               detail = paste("Batch", idx2)
            )

            SXTsvrNor(sample = t(sample[[idx2]]),
                      QC = t(QC[[idx2]]),
                      tags = tags1,
                      sample.order = sample.info$injection.order[match(colnames(sample[[idx2]]), sample.info$sample.name)],
                      QC.order = sample.info$injection.order[match(colnames(QC[[idx2]]), sample.info$sample.name)],
                      #used data
                      multiple = multiple,
                      dimension1 = dimension2,
                      threads = threads
                      #parameters setting
            )
          })
        })
      sample.nor <- lapply(result, function(x) x[[1]])
      QC.nor <- lapply(result, function(x) x[[2]])
      sample.nor <- t(do.call(rbind, sample.nor))
      QC.nor <- t(do.call(rbind, QC.nor))
      sample <- cbind(sample.nor, QC.nor)
      sample <- sample[,match(sample.info$sample.name, colnames(sample))]
    }
  }


  peak.table <- cbind(tags, sample)
  peak.table[which(is.na(peak.table), arr.ind = TRUE)] <- 0
  return(peak.table)

}




####LOESS normalization function
SXTloessNor <- function(sample,
                        QC,
                        tags,
                        sample.order,
                        QC.order,
                        #used data
                        optimization = TRUE,
                        begin = 0.5,
                        end = 1,
                        step = 0.2,
                        dimension1 = TRUE,
                        batch = 1
                        #parameters setting
){

  cat("LOESS normalization is finished: %\n")
  # Sys.sleep(1)
  QC.nor <- NULL
  sample.nor <- NULL
  best.span <- NULL
  best.degree <- NULL

  shiny::withProgress(
    message = "LOESS normalization...",
    detail = paste("Batch", batch, sep = " "),
    value = 0,{
      for (i in 1:ncol(QC)) {
        shiny::incProgress(1/ncol(QC)
                           # detail = paste("Batch", idx1)
        )

        if (optimization) {
          para <- cvMSE( unlist(QC[,i]),QC.order,
                         begin1 = begin, end1 = end, step1 = step)
          loess.reg <-
            loess(unlist(QC[,i]) ~ QC.order,span = para[2],degree = para[1])
          best.span[i] <- para[2]
          best.degree[i] <- para[1]
        }
        else {
          loess.reg <- loess(unlist(QC[,i]) ~ QC.order)
        }

        predict.QC <- summary(loess.reg)$fitted
        QC.nor1 <- QC[,i] / predict.QC

        #if the predict value is 0, then set the ratio to 0
        QC.nor1[is.nan(unlist(QC.nor1))] <- 0
        QC.nor1[is.infinite(unlist(QC.nor1))] <- 0
        QC.nor1[is.na(unlist(QC.nor1))] <- 0
        QC.nor1[which(unlist(QC.nor1) < 0)] <- 0

        predict.sample <-
          predict(loess.reg, data.frame(QC.order = c(sample.order)))
        sample.nor1 <- sample[,i] / predict.sample
        sample.nor1[is.nan(unlist(sample.nor1))] <- 0
        sample.nor1[is.infinite(unlist(sample.nor1))] <- 0
        sample.nor1[is.na(unlist(sample.nor1))] <- 0
        sample.nor1[which(unlist(sample.nor1) < 0)] <- 0

        QC.nor <- cbind(QC.nor,QC.nor1)
        sample.nor <- cbind(sample.nor,sample.nor1)

        count <- floor(ncol(sample) * c(seq(0,1,0.01)))
        if (any(i == count)) {
          cat(ceiling(i * 100 / ncol(sample)))
          cat(" ")
        }

      }
    }
  )


  cat("\n")
  cat("Normalized sample and qc data are got\n")

  colnames(QC.nor) <- colnames(sample.nor) <- tags["name",]

  QC.median <- apply(QC,2,median)

  if (dimension1) {
    QC.nor <- t(t(QC.nor) * QC.median)
    sample.nor <- t(t(sample.nor) * QC.median)
  }

  return.result <- list("sample" = sample.nor, "QC" = QC.nor)

}


#cvMSE is loess parameter optimization function
cvMSE <- function(qc,QC.order,begin1,end1,step1) {
  mse <- NULL
  nmse <- NULL
  cvmse <- NULL
  cvmse2 <- NULL

  para <- seq(begin1,end1,by = step1)
  for (i in 1:2) {
    for (j in para) {
      for (k in 2:(length(qc) - 1)) {
        loess.reg <- loess(qc[-k] ~ QC.order[-k],span = j,degree = i)
        predict.qc <- predict(loess.reg,QC.order[k])
        mse[k] <- (qc[k] - predict.qc) ^ 2
        nmse[k] <- (qc[k] - mean(qc)) ^ 2
      }
      cvmse1 <- rbind(j,mean(mse,na.rm = TRUE) / mean(nmse,na.rm = TRUE))
      cvmse2 <- cbind(cvmse2,cvmse1)
      mse <- NULL
      nmse <- NULL
    }

    cvmse3 <- rbind(i,cvmse2)
    cvmse <- cbind(cvmse,cvmse3)
    cvmse3 <- NULL
    cvmse2 <- NULL
  }
  return(cvmse[,which.min(cvmse[3,])])
}


# return.result <- splitBatch(sample = dc.sample, sample.info = dc.sample.info)
# sample <- t(return.result[[1]][[1]])
# QC <- t(return.result[[2]][[1]])
# tags <- t(dc.tags)
#
# sample.order <- dc.sample.info$injection.order[match(rownames(sample), dc.sample.info$sample.name)]
# QC.order <- dc.sample.info$injection.order[match(rownames(QC), dc.sample.info$sample.name)]
#
#
# sample.nor <- SXTsvrNor(sample = sample, QC = QC, tags = tags, sample.order = sample.order,
#                         QC.order = QC.order, multiple = 5, dimension1 = TRUE, threads = 2)



##############svr normalization function
SXTsvrNor <- function(sample,
                      QC,
                      tags,
                      sample.order,
                      QC.order,
                      #used data
                      multiple = 5,
                      dimension1 = TRUE,
                      threads = 1
                      #parameters setting
) {
  #
  ichunks <- split((1:ncol(sample)), 1:threads)
  svr.data <- BiocParallel::bplapply(ichunks,
                                     FUN = svr.function,
                                     # BPPARAM = BiocParallel::MulticoreParam(workers = threads,
                                     #                                   progressbar = TRUE),
                                     BPPARAM = BiocParallel::SnowParam(workers = threads,
                                                                       progressbar = TRUE),
                                     sample = sample,
                                     QC = QC,
                                     sample.order = sample.order,
                                     QC.order = QC.order,
                                     multiple = multiple)

  sample.nor <- lapply(svr.data, function(x) {
    x[[1]]
  })

  QC.nor <- lapply(svr.data, function(x) {
    x[[2]]
  })

  index <- lapply(svr.data, function(x) {
    x[[3]]
  })


  sample.nor <- do.call(cbind, sample.nor)
  QC.nor <- do.call(cbind, QC.nor)

  index <- unlist(index)

  sample.nor <- sample.nor[,order(index)]
  QC.nor <- QC.nor[,order(index)]

  QC.median <- apply(QC, 2, median)
  if (dimension1) {
    QC.nor <- t(t(QC.nor) * QC.median)
    sample.nor <- t(t(sample.nor) * QC.median)
  }

  return.result <- list("sample" = sample.nor, "QC" = QC.nor)
}



#
# temp <- svr.function(index = ichunks[[2]],
#                      sample = sample, QC = QC,
#                      sample.order = sample.order, QC.order = QC.order, multiple = 5)


setGeneric(name = "svr.function",
           def = function(index,
                          sample,
                          QC,
                          sample.order,
                          QC.order,
                          multiple){
             # library(e1071)
             # colnames(sample) <- colnames(QC) <- tags["name",]
             sample <- sample[,index, drop = FALSE]
             QC <- QC[,index, drop = FALSE]
             # cat("SVR normalization is finished: %\n")
             data.order <- c(sample.order, QC.order)



             data.nor <- lapply(c(1:ncol(sample)), function(i){

               if (multiple != 1) {
                 correlation <- abs(cor(x = rbind(sample, QC)[,i], y = rbind(sample, QC))[1,])
                 # cor.peak <-
                 # as.numeric(which(QC.cor[, i] %in% rev(sort(QC.cor[-i, i]))[1:as.numeric(multiple)]))
                 cor.peak <- match(names(sort(correlation, decreasing = TRUE)[1:6][-1]),
                                   names(correlation))
                 rm(list = "correlation")
                 svr.reg <- e1071::svm(QC[, cor.peak], QC[, i])
               } else{
                 svr.reg <- e1071::svm(unlist(QC[, i]) ~ QC.order)
               }

               predict.QC <- summary(svr.reg)$fitted
               QC.nor1 <- QC[, i] / predict.QC

               #if the predict value is 0, then set the ratio to 0
               QC.nor1[is.nan(unlist(QC.nor1))] <- 0
               QC.nor1[is.infinite(unlist(QC.nor1))] <- 0
               QC.nor1[is.na(unlist(QC.nor1))] <- 0
               QC.nor1[which(unlist(QC.nor1) < 0)] <- 0

               if (multiple != 1) {
                 predict.sample <- predict(svr.reg, sample[, cor.peak])
               } else{
                 predict.sample <-
                   predict(svr.reg, data.frame(QC.order = c(sample.order)))
               }

               sample.nor1 <- sample[, i] / predict.sample
               sample.nor1[is.nan(unlist(sample.nor1))] <- 0
               sample.nor1[is.infinite(unlist(sample.nor1))] <- 0
               sample.nor1[is.na(unlist(sample.nor1))] <- 0
               sample.nor1[which(unlist(sample.nor1) < 0)] <- 0

               return(list(sample.nor1, QC.nor1))

             })





             # for(i in 1:ncol(sample)){
             #   cat(i, " ")
             #   if (multiple != 1) {
             #     correlation <- abs(cor(x = rbind(sample, QC)[,i], y = rbind(sample, QC))[1,])
             #     # cor.peak <-
             #     # as.numeric(which(QC.cor[, i] %in% rev(sort(QC.cor[-i, i]))[1:as.numeric(multiple)]))
             #     cor.peak <- match(names(sort(correlation, decreasing = TRUE)[1:6][-1]),
             #                       names(correlation))
             #     rm(list = "correlation")
             #     svr.reg <- e1071::svm(QC[, cor.peak], QC[, i])
             #   } else{
             #     svr.reg <- e1071::svm(unlist(QC[, i]) ~ QC.order)
             #   }
             #
             #   predict.QC <- summary(svr.reg)$fitted
             #   QC.nor1 <- QC[, i] / predict.QC
             #
             #   #if the predict value is 0, then set the ratio to 0
             #   QC.nor1[is.nan(unlist(QC.nor1))] <- 0
             #   QC.nor1[is.infinite(unlist(QC.nor1))] <- 0
             #   QC.nor1[is.na(unlist(QC.nor1))] <- 0
             #   QC.nor1[which(unlist(QC.nor1) < 0)] <- 0
             #
             #   if (multiple != 1) {
             #     predict.sample <- predict(svr.reg, sample[, cor.peak])
             #   } else{
             #     predict.sample <-
             #       predict(svr.reg, data.frame(QC.order = c(sample.order)))
             #   }
             #
             #   sample.nor1 <- sample[, i] / predict.sample
             #   sample.nor1[is.nan(unlist(sample.nor1))] <- 0
             #   sample.nor1[is.infinite(unlist(sample.nor1))] <- 0
             #   sample.nor1[is.na(unlist(sample.nor1))] <- 0
             #   sample.nor1[which(unlist(sample.nor1) < 0)] <- 0
             # }



             sample.nor <- lapply(data.nor, function(x) x[[1]])
             QC.nor <- lapply(data.nor, function(x) x[[2]])
             rm(list = "data.nor")
             sample.nor <- t(do.call(rbind, sample.nor))
             QC.nor <- t(do.call(rbind, QC.nor))

             colnames(sample.nor) <- colnames(QC.nor) <- colnames(sample)
             rm(list = c("sample", "QC"))

             svr.data <-
               list(sample.nor = sample.nor,
                    QC.nor = QC.nor,
                    index = index)
             rm(list = c("sample.nor", "QC.nor"))
             return(svr.data)

           })













###single peak plot for data normalization
dnSinglePeak <- function(sample, tags, sample.info, peak.idx,
                         # col = "dodgerblue",
                         alpha = 0.5, size = 2,
                         text = FALSE,
                         title = ""){
  sample <- as.data.frame(sample)
  tags <- as.data.frame(tags)
  sample.info <- as.data.frame(sample.info)
  if(any(colnames(sample.info) == "injection.order")){
    injection.order <- as.numeric(sample.info[,"injection.order"])
  }else{
    injection.order <- c(1:nrow(sample.info))
  }

  x <- as.numeric(sample[peak.idx,])
  sample.name <- colnames(sample)
  batch <- as.character(sample.info[,"batch"])
  class <- as.character(sample.info[,"class"])

  temp <- data.frame(sample.name, injection.order, x, batch, class, stringsAsFactors = FALSE)

  colnames(temp) <- c("Sample", "Injection.order", "Intensity", "Batch", "Class")
  # my.theme <- ggplot2::theme_bw()+
  #   ggplot2::theme(axis.title.x = ggplot2::element_text(size = 18),
  #                  axis.title.y = ggplot2::element_text(size = 18)) +
  #   ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15),
  #                  axis.text.y = ggplot2::element_text(size = 15)) +
  #   ggplot2::theme(legend.title = ggplot2::element_text(size = 12)) +
  #   ggplot2::theme(legend.text = ggplot2::element_text(size = 10))


  par(mar = c(5,5,4,2))
  plot <-  ggplot2::ggplot(temp, ggplot2::aes(x = Injection.order, y = Intensity,
                                              label = Sample, label2 = Batch, label3 = Class)) +
    ggplot2::geom_point(alpha = alpha,
                        # colour = col,
                        size = size,
                        ggplot2::aes(colour = Batch, shape = Class)) +
    ggplot2::labs(x = "Injection order",
                  y = "Intensity") +
    ggplot2::ggtitle(title) +
    my.theme
    # ggplot2::ylim(c(0.25*10^6, 3.75*10^6))
  # ggplot2::theme(legend.justification=c(0,1), legend.position=c(0,1), legend.background = element_rect(fill=alpha('white', 0)))

  if(text) {
    plot <- plot+ggplot2::geom_text(aes(x = Injection.order, y = Intensity, label = Sample),
                                    check_overlap = TRUE, size = 3, hjust = 1, vjust = 1)
  }

  return(plot)
}






#####box plot for each sample
###single peak plot for data normalization
dnSampleBoxplot <- function(sample,
                            sample.info,
                            group,
                            # col = "dodgerblue",
                            # alpha = 0.5,
                            # size = 2,
                            text = FALSE,
                            title = ""){
  sample <- as.data.frame(sample)
  sample.info <- as.data.frame(sample.info)

  if(any(colnames(sample.info) == "injection.order")){
    sample.info <- sample.info[order(sample.info$injection.order),]
  }

  sample.info <- sample.info[sample.info[,"group"] %in% group,]
  sample <- sample[,match(sample.info[,1], colnames(sample))]
  sample <- sxtScale(sample = sample, method = "auto", center = TRUE)

  temp <- apply(sample, 2, list)
  temp <- lapply(temp, unlist)

  temp <- mapply(function(x, y, z){
    list(data.frame("Sample" = rep(x, nrow(sample)),
                    "Intensity" = y,
                    "Batch" = rep(z, nrow(sample)),
                    stringsAsFactors = FALSE))
  },
  x = colnames(sample),
  y = temp,
  z = as.character(sample.info[,"batch"]))

  temp <- do.call(rbind, temp)

  temp$Sample <- factor(temp$Sample,
                        levels = sample.info$sample.name,
                        ordered = TRUE)

  # my.theme <- ggplot2::theme_bw()+
  #   ggplot2::theme(axis.title.x = ggplot2::element_text(size = 18),
  #                  axis.title.y = ggplot2::element_text(size = 18)) +
  #   ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15),
  #                  axis.text.y = ggplot2::element_text(size = 15)) +
  #   ggplot2::theme(legend.title = ggplot2::element_text(size = 12)) +
  #   ggplot2::theme(legend.text = ggplot2::element_text(size = 10))

  temp[,"Sample"] <- as.factor(temp[,"Sample"])

  par(mar = c(5,5,4,2))
  plot <-  ggplot2::ggplot(temp, ggplot2::aes(x = Sample, y = Intensity,
                                              color = Batch
                                              # label = Sample
  )) +
    ggplot2::geom_boxplot() +
    # ggplot2::geom_dotplot(
    #                       binaxis='y',
    #                       stackdir='center',
    #                       dotsize=0.1
    #                       ) +
    ggplot2::labs(x = "Sample",
                  y = "Intensity") +
    ggplot2::ggtitle(title) +
    my.theme
  # ggplot2::theme(legend.justification=c(0,1), legend.position=c(0,1), legend.background = element_rect(fill=alpha('white', 0)))


  return(plot)
}








#####box plot for each sample
###single peak plot for data normalization
rsdBeforeAfter <- function(sample1, sample2,
                           tags,
                           sample.info,
                           group,
                           # col = "dodgerblue",
                           alpha = 0.5,
                           size = 2,
                           text = FALSE,
                           title = ""){
  sample1 <- as.data.frame(sample1)
  sample2 <- as.data.frame(sample2)
  sample.info <- as.data.frame(sample.info)

  sample.info <- sample.info[sample.info[,"group"] == group,]
  sample1 <- sample1[,match(sample.info[,1], colnames(sample1))]
  sample2 <- sample2[,match(sample.info[,1], colnames(sample2))]

  rsd1 <- apply(sample1, 1, function(x){
    sd(x)*100/mean(x)
  })

  rsd2 <- apply(sample2, 1, function(x){
    sd(x)*100/mean(x)
  })


  change <- rep(NA, length(rsd1))
  change[rsd2 > rsd1] <- "Increase"
  change[rsd2 == rsd1] <- "No change"
  change[rsd2 < rsd1] <- "Decrease"


  temp <- data.frame(tags[,"name"], rsd1, rsd2, change, stringsAsFactors = FALSE)
  colnames(temp) <- c("Peak", "Before", "After", "Change")

  # my.theme <- ggplot2::theme_bw()+
  #   ggplot2::theme(axis.title.x = ggplot2::element_text(size = 18),
  #                  axis.title.y = ggplot2::element_text(size = 18)) +
  #   ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15),
  #                  axis.text.y = ggplot2::element_text(size = 15)) +
  #   ggplot2::theme(legend.title = ggplot2::element_text(size = 12)) +
  #   ggplot2::theme(legend.text = ggplot2::element_text(size = 10))


  par(mar = c(5,5,4,2))
  plot <-  ggplot2::ggplot(temp, ggplot2::aes(x = Before, y = After,
                                              color = Change,
                                              label = Peak)) +
    ggplot2::geom_point(alpha = alpha, size = size) +
    ggplot2::labs(x = "RSD (%, before normalization)",
                  y = "RSD (%, after normalization)") +
    ggplot2::ggtitle(title) +
    ggplot2::geom_abline(intercept = 0,slope = 1) +
    my.theme
  # ggplot2::theme(legend.justification=c(0,1), legend.position=c(0,1), legend.background = element_rect(fill=alpha('white', 0)))


  return(plot)
}





#####box plot for each sample
###single peak plot for data normalization
rsdDistribution2 <- function(sample,
                            tags,
                            sample.info,
                            group,
                            # col = "dodgerblue",
                            rsd.tol = 30,
                            alpha = 0.5,
                            size = 2,
                            # text = FALSE,
                            title = ""){
  sample <- as.data.frame(sample)
  sample.info <- as.data.frame(sample.info)

  sample.info <- sample.info[sample.info[,"group"] == group,]
  sample <- sample[,match(sample.info[,1], colnames(sample))]

  rsd <- apply(sample, 1, function(x){
    sd(x)*100/mean(x)
  })

  Group <- rep(NA, length(rsd))
  Group[rsd >= 30] <- ">=30"
  Group[rsd < 30] <- "<30"

  temp <- data.frame(1:length(rsd),tags[,"name"], rsd, Group, stringsAsFactors = FALSE)
  colnames(temp) <- c("Index","Peak", "RSD", "Group")

  # my.theme <- ggplot2::theme_bw()+
  #   ggplot2::theme(axis.title.x = ggplot2::element_text(size = 18),
  #                  axis.title.y = ggplot2::element_text(size = 18)) +
  #   ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15),
  #                  axis.text.y = ggplot2::element_text(size = 15)) +
  #   ggplot2::theme(legend.title = ggplot2::element_text(size = 12)) +
  #   ggplot2::theme(legend.text = ggplot2::element_text(size = 10))


  par(mar = c(5,5,4,2))
  plot <-  ggplot2::ggplot(temp, ggplot2::aes(x = Index, y = RSD,
                                              label = Peak,
                                              color = Group)) +
    ggplot2::geom_point(alpha = alpha, size = size) +
    ggplot2::labs(x = "Peaks",
                  y = "RSD (%)",
                  colour = "RSD value") +
    ggplot2::ggtitle(title) +
    my.theme +
    ggplot2::geom_hline(yintercept = rsd.tol)
  # ggplot2::theme(legend.justification=c(0,1), legend.position=c(0,1), legend.background = element_rect(fill=alpha('white', 0)))

  num1 <- length(rsd)
  num2 <- sum(rsd < rsd.tol)
  text <- paste(num2, " out of ", num1, " peaks with RSDs less than ", rsd.tol, "%", sep = "")
  plot <- plot+ggplot2::annotate(geom = "text",
                                 x = -Inf, y = Inf, vjust = 2, hjust = -0.2,
                                 label = text)
  return(plot)
}









